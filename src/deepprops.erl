%% @doc A set of utility routines used to operate on deep proplists.
%%
%% This single module has been designed to hit three primary goals:
%% <ll>
%% <li>allow you to access deeply nested properties inside proplists;</li>
%% <li>admit you to mutate proplists;</li>
%% <li>allow you to access a group of properties with a single call.</li>
%% </ll>

-module(deepprops).

-export([
    get/2,
    get/3,
    require/2,
    set/3,
    extract/2,
    extract/3,
    append/3,
    list/2,
    values/2,
    split/2,
    defaults/2
]).

%% Properties deep access

-type path() :: term() | [term(), ...].

-spec get(Path, Proplist) -> Result when
    Path     :: path(),
    Proplist :: proplists:proplist(),
    Result   :: term() | undefined.

get(Path, Proplist) ->
    get(Path, Proplist, undefined).

%% @doc Retrieves value of a property located and possibly deeply nested inside the property list
%% `Proplist' under path `Path'.
%%
%% Path may be a single `Key' thus denoting that the property located on the top level of a
%% property list. As well as a list of keys it may be noting that the property is located inside a
%% property list which is located inside another property list and so on until the top level `Proplist'
%% is finally reached.
%%
%% When there is no property under the `Path' the `Default' shall be the result of a call or
%% `undefined' if `Default' has been not specified.
%%
%% Finally for the sake of clarity the following code will run with no exceptions:
%% ```
%% Proplist = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
%% Result = 4,
%% Result = deepprops:get([top, middle], Proplist).
%% '''

-spec get(Path, Proplist, Default) -> Result when
    Path     :: path(),
    Proplist :: proplists:proplist(),
    Result   :: term() | Default,
    Default  :: term().

get(Path, Proplist, Default) ->
    do_get(keynormalize(Path), Proplist, Default).

do_get([], Value, _) ->
    Value;
do_get([Key | Rest], [Key | _], Default) ->
    do_get(Rest, true, Default);
do_get([Key | Rest], [{Key, Value} | _], Default) ->
    do_get(Rest, Value, Default);
do_get(Path, [_ | Left], Default) ->
    do_get(Path, Left, Default);
do_get(_, _, Default) ->
    Default.

-spec require(Path, Proplist) -> Result when
    Path     :: path(),
    Proplist :: proplists:proplist(),
    Result   :: term() | no_return().

%% @doc Retrieves value of a property located and possibly deeply nested inside the property list
%% `Proplist' under path `Path'. The only difference with `get/2` is in the case when no value is
%% present under the given key. In such situations `{novalue, Path}` exception will be thrown.
%%
%% @see get/2

require(Path, Proplist) ->
    case get(Path, Proplist, Unique = make_ref()) of
        Unique ->
            error({novalue, Path});
        Value ->
            Value
    end.

%% @doc Sets value of a property to the `Value' and returns new property list.
%%
%% Property located and possibly deeply nested inside the property list `Proplist' under path `Path'.
%%
%% Path may be a single key `Key' thus denoting that the property located on the top level of a
%% property list. As well as a list of keys it may be noting that the property is located inside a
%% property list which is located inside another property list and so on until the top level `Proplist'
%% is finally reached.
%%
%% If there is already such property exists the one's value will be replaced with new `Value'.
%% Otherwise the property will be appended to the deepest enclosing proplist addressed by the `Path'.
%%
%% And finally let us clarify with the following valid example:
%% ```
%% Proplist = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
%% Result = [ {top, [ {level, [ {thing, new}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
%% Result = deepprops:set([top, level, thing], new, Proplist).
%% '''

-spec set(Path, Value, Proplist) -> Result when
    Path     :: path(),
    Value    :: term(),
    Proplist :: proplists:proplist(),
    Result   :: proplists:proplist().

set(Path, Value, Proplist) ->
    do_set(keynormalize(Path), Value, Proplist).

%% private
do_set([], Entry, _) ->
    Entry;
do_set([Key | Rest], Entry, Proplist = [{_, _} | _]) ->
    With = [ {K, do_set(Rest, Entry, V)} || P = {K, V} <- Proplist, keymatch(Key, P) ],
    Without = [ P || P <- Proplist, not keymatch(Key, P) ],
    case With of
        [E|_] -> [E | Without];
        []    -> [{Key, do_set(Rest, Entry, [])} | Without]
    end;
do_set([Key | Rest], Entry, Value) ->
    [{Key, do_set(Rest, Entry, Value)}].

%% @doc Appends the new entry `Value' to the list located under a property and returns new property list.
%%
%% Property located and possibly deeply nested inside the property list `Proplist' under path `Path'.
%%
%% Path may be a single key `Key' thus denoting that the property located on the top level of a
%% property list. As well as a list of keys it may be noting that the property is located inside a
%% property list which is located inside another property list and so on until the top level `Proplist'
%% is finally reached.
%%
%% If there is no such property under `Path' the new one will be created and set to the `[Value]'.
%% Otherwise the `Value' will be appended to the head of the list which is value of the property.
%% Please be careful when appending value to anything but a list because malformed list will then
%% be created
%%
%% And finally let us clarify everything with the following valid example:
%% ```
%% Proplist = [ {top, [ {middle, [4]} ]}, {last, 5} ],
%% Result = [ {top, [ {middle, [new, 4]}, {last, 5} ],
%% Result = deepprops:append([top, middle], new, Proplist).
%% '''

-spec append(Path, Value, Proplist) -> Result when
    Path     :: path(),
    Value    :: proplists:property(),
    Proplist :: proplists:proplist(),
    Result   :: proplists:proplist().

append(Path, Entry, Acc) ->
    do_append(keynormalize(Path), Entry, Acc).

do_append([], Entry, Acc) ->
    [Entry | Acc];
do_append([Key | Rest], Entry, Proplist = [{_, _} | _]) ->
    With = [ {K, do_append(Rest, Entry, V)} || P = {K, V} <- Proplist, keymatch(Key, P) ],
    Without = [ P || P <- Proplist, not keymatch(Key, P) ],
    case With of
        [E|_] -> [E | Without];
        []    -> [{Key, do_append(Rest, Entry, [])} | Without]
    end;
do_append([Key | Rest], Entry, Value) ->
    [{Key, do_append(Rest, Entry, Value)}].

-spec extract(Path, Proplist) -> Result when
    Path     :: path(),
    Proplist :: proplists:proplist(),
    Result   :: {Value, proplists:proplist()},
    Value    :: term().

extract(Path, Proplist) ->
    extract(Path, Proplist, undefined).

%% @doc Extracts the property from the property list `Proplist' and return its value and new property
%% list with the property removed.
%%
%% Property located and possibly deeply nested inside the property list `Proplist' under path `Path'.
%%
%% Path may be a single key `Key' thus denoting that the property located on the top level of a
%% property list. As well as a list of keys it may be noting that the property is located inside a
%% property list which is located inside another property list and so on until the top level `Proplist'
%% is finally reached.
%%
%% If there is such property found then the result will be the tuple `{Value, Rest}' where `Value' is
%% value of the propery and `Rest is property list formed after the original with found property removed
%% from the deepest enclosing property list addressed by the `Path'.
%%
%% On the other hand if no such property actually the tuple `{Default, Proplist}' returned where
%% `Proplist' is the original property list untoched and `Default' is equal to `undefined' when no
%% `Default' value have been passed.
%%
%% And finally let us clarify everything with the following:
%% ```
%% Proplist = [ {top, [ {level, [ {thing, 2}, {where, 3} ]} ]}, {last, 5} ],
%% Result = 3,
%% Rest = [ {top, [ {level, [ {thing, 2} ]} ]}, {last, 5} ],
%% {Result, Rest} = deepprops:extract([top, level, where], Proplist).
%% '''

-spec extract(Path, Proplist, Default) -> Result when
    Path     :: path(),
    Proplist :: proplists:proplist(),
    Result   :: {Value, proplists:proplist()},
    Value    :: term() | Default,
    Default  :: term().

extract(Path, Proplist, Default) ->
    do_extract(keynormalize(Path), Proplist, Default).

do_extract([], Proplist, _) ->
    {Proplist, []};
do_extract([Key], Proplist = [{_, _} | _], Default) ->
    Values = [ V || P = {_, V} <- Proplist, keymatch(Key, P) ],
    Rest = [ P || P <- Proplist, not keymatch(Key, P) ],
    case Values of
        []       -> {Default, Rest};
        [Single] -> {Single, Rest};
        List     -> {List, Rest}
    end;
do_extract([Key | RestKeys], Proplist = [{_, _} | _], Default) ->
    WithIt = lists:append([ V || P = {_, V} <- Proplist, keymatch(Key, P) ]),
    Without = [ P || P <- Proplist, not keymatch(Key, P) ],
    {Value, Rest} = do_extract(RestKeys, WithIt, Default),
    {Value, [{Key, Rest} | Without]};
do_extract(_, Proplist, Default) ->
    {Default, Proplist}.

%% @private
keymatch(Key, Key) -> true;
keymatch(Key, {Key, _}) -> true;
keymatch(_, _) -> false.

%% Properties multiple access

%% @doc Retrieves values of one or more properties with a single invocation. These values form a list
%% which strictly preserve order of properties accessed.
%%
%% This function performs much like group `get' call. In other words the result of this function will be
%% equal to sequential application of `deepprops:get/3' in the way close to folding.
%%
%% Properties located and possibly deeply nested inside the property list `Proplist' addressed by one
%% or more paths in `Paths'. A path then may be a single `PurePath' (explained further) which is semantically
%% equal to `{PurePath, undefined}'. As well as a tuple `{PurePath, Default}' may be specified thus
%% denoting that in the case of absence of the property the `Default' will be the value in the list of values.
%% Therefore `undefined' will be that value when `Default' is not specified.
%%
%% `PurePath' may be a single key `Key' thus denoting that the property located on the top level of a
%% property list. As well as a list of keys it may be noting that the property is located inside a
%% property list which is located inside another property list and so on until the top level `Proplist'
%% is finally reached.
%%
%% Then following final clarification example:
%% ```
%% Proplist = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
%% Result = [ 2, def, 5 ],
%% Result = deepprops:values([ [top, level, thing], {[top, down], def}, last ], Proplist).
%% '''
%%
%% In the latter example the value `def' was returned for path `[top, down]'. If the default value was not
%% specified through `{[top, down], def}' then the `undefined' would be returned instead.
%%
%% @see get/3

-spec values(Paths, Proplist) -> Results when
    Proplist :: proplists:proplist(),
    Paths    :: [Path],
    Path     :: PurePath | {PurePath, Default},
    PurePath :: path(),
    Default  :: term(),
    Results  :: [Result],
    Result   :: term() | Default | undefined.

values(Keys, Proplist) ->
    values(Keys, Proplist, []).

values([], _, Acc) ->
    lists:reverse(Acc);
values([{Key, Default} | Rest], Proplist, Acc) ->
    values(Rest, Proplist, [get(keynormalize(Key), Proplist, Default) | Acc]);
values([Key | Rest], Proplist, Acc) ->
    values(Rest, Proplist, [get(keynormalize(Key), Proplist) | Acc]).

-spec split(Paths, Proplist) -> Result when
    Proplist :: proplists:proplist(),
    Paths    :: [Path],
    Path     :: PurePath | {PurePath, Default},
    PurePath :: path(),
    Default  :: term(),
    Result   :: {[Value], Rest},
    Value    :: term(),
    Rest     :: proplists:proplist().

%% @doc Extracts values of one or more properties with a single invocation and returns them along with
%% new property list with these properties removed. These values form a list which strictly preserve
%% order of properties accessed.
%%
%% This function performs much like group `extract' call. In other words the result of this function will be
%% equal to sequential application of `deepprops:extract/3' in the way close to folding.
%%
%% Properties located and possibly deeply nested inside the property list `Proplist' addressed by one
%% or more paths in `Paths'. A path then may be a single `PurePath'as well as a tuple `{PurePath, Default}'
%% thus denoting that in the case of absence of the property the `Default' will be the value in the list
%% of values. Therefore `undefined' will be that value when `Default' is not specified.
%%
%% The function call will return tuple `{Values, Rest}' where `Values' is list of values being explained
%% and the `Rest' is property list with properties addressed by `Paths' removed from the corresponding
%% deepest enclosing property lists. Obviously the absent properties are not removed.
%%
%% `PurePath' may be a single key `Key' thus denoting that the property located on the top level of a
%% property list. As well as a list of keys it may be noting that the property is located inside a
%% property list which is located inside another property list and so on until the top level `Proplist'
%% is finally reached.
%%
%% Finally goes the clarification example:
%% ```
%% Proplist = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
%% Result = [ 2, def, 5 ],
%% Rest = [ {top, [ {level, [ {where, 3} ]}, {middle, 4} ]} ],
%% {Result, Rest} = deepprops:split([ [top, level, thing], {[top, down], def}, last ], Proplist).
%% '''
%%
%% @see extract/3

split(Keys, Proplist) ->
    do_split(Keys, {[], Proplist}).

do_split([], {Values, Rest}) ->
    {lists:reverse(Values), Rest};
do_split([{Key, Default} | Keys], {Acc, Proplist}) ->
    {Value, Rest} = extract(keynormalize(Key), Proplist, Default),
    do_split(Keys, {[Value | Acc], Rest});
do_split([Key | Keys], {Acc, Proplist}) ->
    {Value, Rest} = extract(keynormalize(Key), Proplist),
    do_split(Keys, {[Value | Acc], Rest}).

%% @doc Returns plain sublist of the property list with properties under `Paths' being
%% retrieved.
%%
%% Properties located and possibly deeply nested inside the property list `Proplist' addressed by one
%% or more paths in `Paths'. A `Path' then may be a single key `Key' thus denoting that the property
%% located on the top level of a property list. As well as a list of keys it may be noting that the
%% property is located inside a property list which is located inside another property list and so on
%% until the top level `Proplist' is finally reached.
%%
%% Worth noting that when property is not present under specific `Path' the resulting list misses this
%% property totally. Thus length of the result may be less than the length of `Paths'. But the ordering
%% is still preserved even in such cases.
%%
%% Finally for the sake of clarity it is guaranteed that the following code will run with no exceptions:
%% ```
%% Proplist = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
%% Result = [ {[top, level, thing], 2}, {last, 5} ],
%% Result = deepprops:list([ [top, level, thing], [top, down], last ], Proplist).
%% '''

-spec list(Paths, Proplist) -> Result when
    Proplist :: proplists:proplist(),
    Paths    :: [path()],
    Result   :: [Prop],
    Prop     :: proplists:property().

list(Keys, Proplist) ->
    list(Keys, Proplist, []).

list([Key | Rest], Proplist, Acc) ->
    Norm = keynormalize(Key),
    case get(Norm, Proplist) of
        undefined -> list(Rest, Proplist, Acc);
        Value     -> list(Rest, Proplist, [{Key, Value} | Acc])
    end;

list([], _, Acc) ->
    lists:reverse(Acc).

%% @private
keynormalize(Key = [_ | _]) -> Key;
keynormalize(Key) -> [Key].

%% @doc Retrieves the property list formed by substitution any missing properties with
%% default ones from `Defaults'.
%%
%% If `Defaults' contains a property which is present in `Proplist' too last one left untouched.
%% Otherwise the `Proplist' is populated with property from `Defaults'. It may seem like merging of
%% two proplists with respect to values in the `Proplist'.

-spec defaults(Defaults, Proplist) -> Result when
    Proplist :: proplists:proplist(),
    Defaults :: proplists:proplist(),
    Result   :: proplists:proplist().

defaults([E = {Head, _} | Defaults], Proplist) ->
    case keydefined(Head, Proplist) of
        true -> defaults(Defaults, Proplist);
        _    -> defaults(Defaults, [E | Proplist])
    end;

defaults([Head | Defaults], Proplist) ->
    defaults([{Head, true} | Defaults], Proplist);

defaults([], Proplist) ->
    Proplist.

%% @private
keydefined(Key, [Key | _])      -> true;
keydefined(Key, [{Key, _} | _]) -> true;
keydefined(Key, [_ | Rest])     -> keydefined(Key, Rest);
keydefined(_Key, [])            -> false.

%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

set_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [ {top, [ {level, [ {thing, new}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    ?assertEqual(R, set([top, level, thing], new, L)).

set_top_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [ {top, one}, {last, 5} ],
    ?assertEqual(R, set(top, one, L)).

set_new_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [ {top, [ {level, [ {thing2, [{thing3, new}]}, {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    ?assertEqual(R, set([top, level, thing2, thing3], new, L)).

set_empty_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]} ]}, {last, 5} ],
    R = [ {[], new}, {top, [ {level, [ {thing, 2}, {where, 3} ]} ]}, {last, 5} ],
    ?assertEqual(R, set([], new, L)).

get_plist_test() ->
    L = [ {top, [ {level, [ {thing2, [{thing3, new}]}, {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [{thing3, new}],
    ?assertEqual(R, get([top, level, thing2], L)).

get_prop_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = 4,
    ?assertEqual(R, get([top, middle], L)).

get_top_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = 5,
    ?assertEqual(R, get(last, L)).

get_missing_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = undefined,
    ?assertEqual(R, get([top, middle, further], L)).

require_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    ?assertEqual(2, require([top, level, thing], L)),
    ?assertEqual(5, require(last, L)),
    Missing = [top, random],
    ?assertError({novalue, Missing}, require(Missing, L)).

append_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, [3]} ]}, {middle, [4]} ]}, {last, 5} ],
    R = [ {top, [ {middle, [new, 4]}, {level, [ {thing, 2}, {where, [3]} ]} ]}, {last, 5} ],
    ?assertEqual(R, append([top, middle], new, L)).

defaults_test() ->
    L = [ {level, 1}, {something, 2}, {somewhere, 3}, {last, 5} ],
    R = [ {someone, there}, {level, 1}, {something, 2}, {somewhere, 3}, {last, 5} ],
    Defs = [ {somewhere, none}, {someone, there} ],
    ?assertEqual(R, defaults(Defs, L)).

values_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [ 2, def, 5 ],
    ?assertEqual(R, values([ [top, level, thing], {[top, down], def}, last ], L)).

list_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [ {[top, level, thing], 2}, {last, 5} ],
    ?assertEqual(R, list([ [top, level, thing], [top, down], last ], L)).

extract_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = {3, [ {top, [ {level, [ {thing, 2} ]}, {middle, 4} ]}, {last, 5} ]},
    ?assertEqual(R, extract([top, level, where], L)).

extract_empty_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = {undefined, L},
    ?assertEqual(R, extract([], L)).

extract_missing_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = {undefined, [ {top, [ {level, [ {where, 3}, {thing, 2} ]}, {middle, 4} ]}, {last, 5} ]},
    ?assertEqual(R, extract([top, level, where, does, it], L)).

split_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = {[ 2, def, 5 ], [ {top, [ {level, [ {where, 3} ]}, {middle, 4} ]} ]},
    ?assertEqual(R, split([ [top, level, thing], {[top, down], def}, last ], L)).

-endif.
