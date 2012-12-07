-module(deepprops_eunit).

-include_lib("eunit/include/eunit.hrl").

set_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [ {top, [ {level, [ {thing, new}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    ?assertEqual(R, deepprops:set([top, level, thing], new, L)).

set_top_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [ {top, one}, {last, 5} ],
    ?assertEqual(R, deepprops:set(top, one, L)).

set_new_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [ {top, [ {level, [ {thing2, [{thing3, new}]}, {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    ?assertEqual(R, deepprops:set([top, level, thing2, thing3], new, L)).

set_empty_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]} ]}, {last, 5} ],
    R = [ {[], new}, {top, [ {level, [ {thing, 2}, {where, 3} ]} ]}, {last, 5} ],
    ?assertEqual(R, deepprops:set([], new, L)).

get_plist_test() ->
    L = [ {top, [ {level, [ {thing2, [{thing3, new}]}, {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [{thing3, new}],
    ?assertEqual(R, deepprops:get([top, level, thing2], L)).

get_prop_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = 4,
    ?assertEqual(R, deepprops:get([top, middle], L)).

get_top_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = 5,
    ?assertEqual(R, deepprops:get(last, L)).

get_missing_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = undefined,
    ?assertEqual(R, deepprops:get([top, middle, further], L)).

require_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    ?assertEqual(2, deepprops:require([top, level, thing], L)),
    ?assertEqual(5, deepprops:require(last, L)),
    Missing = [top, random],
    ?assertError({novalue, Missing}, deepprops:require(Missing, L)).

append_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, [3]} ]}, {middle, [4]} ]}, {last, 5} ],
    R = [ {top, [ {middle, [new, 4]}, {level, [ {thing, 2}, {where, [3]} ]} ]}, {last, 5} ],
    ?assertEqual(R, deepprops:append([top, middle], new, L)).

defaults_test() ->
    L = [ {level, 1}, {something, 2}, {somewhere, 3}, {last, 5} ],
    R = [ {someone, there}, {level, 1}, {something, 2}, {somewhere, 3}, {last, 5} ],
    Defs = [ {somewhere, none}, {someone, there} ],
    ?assertEqual(R, deepprops:defaults(Defs, L)).

values_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [ 2, def, 5 ],
    ?assertEqual(R, deepprops:values([ [top, level, thing], {[top, down], def}, last ], L)).

list_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = [ {[top, level, thing], 2}, {last, 5} ],
    ?assertEqual(R, deepprops:list([ [top, level, thing], [top, down], last ], L)).

extract_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = {3, [ {top, [ {level, [ {thing, 2} ]}, {middle, 4} ]}, {last, 5} ]},
    ?assertEqual(R, deepprops:extract([top, level, where], L)).

extract_empty_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = {undefined, L},
    ?assertEqual(R, deepprops:extract([], L)).

extract_missing_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = {undefined, [ {top, [ {level, [ {where, 3}, {thing, 2} ]}, {middle, 4} ]}, {last, 5} ]},
    ?assertEqual(R, deepprops:extract([top, level, where, does, it], L)).

split_test() ->
    L = [ {top, [ {level, [ {thing, 2}, {where, 3} ]}, {middle, 4} ]}, {last, 5} ],
    R = {[ 2, def, 5 ], [ {top, [ {level, [ {where, 3} ]}, {middle, 4} ]} ]},
    ?assertEqual(R, deepprops:split([ [top, level, thing], {[top, down], def}, last ], L)).
