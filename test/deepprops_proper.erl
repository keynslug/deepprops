-module(deepprops_proper).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%

spec_test_() ->
    MFAs = [
        {get, 2},
        {get, 3},
        {set, 3},
        {extract, 2},
        {split, 2},
        {list, 2},
        {values, 2},
        {defaults, 2}
    ],
    {timeout, 3600, [{inparallel, [
        {atom_to_list(F), ?_assert(proper:check_spec({deepprops, F, A}, [
            {to_file, user}, {max_size, 24}
        ]))} || {F, A} <- MFAs
    ]}]}.

proper_test_() ->
    PropErOpts = [{to_file, user}, {max_size, 16}],
    {timeout, 3600, ?_assertEqual([], proper:module(?MODULE, PropErOpts))}.

prop_get_trues() ->
    ?FORALL(
        {Dpl, Key},
        {list(dpl_shortkey(8)), dpl_shortkey(8)},
        ?IMPLIES(
            lists:member(Key, Dpl),
            true =:= deepprops:get(Key, Dpl)
        )
    ).

prop_set_get() ->
    ?FORALL(
        {Dpl, Key, Value},
        {dpl(dpl_key()), dpl_path(dpl_key()), dpl_value()},
        Value =:= deepprops:get(Key, deepprops:set(Key, Value, Dpl))
    ).

prop_extract_get() ->
    numtests(20, ?FORALL(
        {Dpl, Key},
        {dpl(dpl_shortkey(8)), dpl_path(dpl_shortkey(8))},
        ?IMPLIES(
            undefined =/= deepprops:get(Key, Dpl),
            begin
                {Value0, Dpl0} = deepprops:extract(Key, Dpl),
                Value0 =:= deepprops:get(Key, Dpl) andalso
                    undefined =:= deepprops:get(Key, Dpl0)
            end
        )
    )).

prop_defaults() ->
    ?FORALL(
        {DplA, DplB},
        {dpl(dpl_key()), dpl(dpl_key())},
        begin
            DplR = deepprops:defaults(DplB, DplA),
            Ks = [element(1, E) || E <- DplR],
            lists:all(fun (K) ->
                V = deepprops:get(K, DplR),
                V =/= undefined andalso (
                    V =:= deepprops:get(K, DplA) orelse
                    V =:= deepprops:get(K, DplB)
                )
            end, Ks)
        end
    ).

%%

dpl_path(K, S) when S > 0 ->
    oneof([
        K,
        ?LETSHRINK([Size], [integer(1, S)], vector(Size, K))
    ]);

dpl_path(K, _) ->
    K.

dpl_shortkey(N) ->
    ?LET(V, integer($a, $a + N), list_to_atom([V])).

dpl_key() ->
    oneof([
        atom(),
        binary(),
        byte()
    ]).

dpl_value() ->
    any().

dpl_path(K) ->
    ?SIZED(S, dpl_path(K, S)).

%%

dpl(K, S) when S > 0 ->
    ?LAZY(?LETSHRINK(
        [Dpl, Path, Value],
        [dpl(K, S - 1), K, oneof([dpl_value(), dpl(K, S - 2)])],
        {'$call', deepprops, set, [Path, Value, Dpl]}
    ));

dpl(_, _) ->
    [].

dpl(K) ->
    ?SIZED(S, dpl(K, S)).
