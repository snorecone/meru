-module(meru_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

meru_test() ->
    meru:start(),
    compile:file("../examples/mountain.erl"),
    % construct 2 mountain records

    Chimbo = mountain:new([
        {name, <<"Chimborazo">>},
        {range, <<"Cordillera Occidental">>},
        {planet, <<"Earth">>},
        {height, 6267},
        {type, <<"volcano">>}
    ]),
    Oly = mountain:new([
        {name, <<"Olympus Mons">>},
        {range, <<"Amazonis">>},
        {planet, <<"Mars">>},
        {height, 21171},
        {type, <<"volcano">>}
    ]),

    % put our mountains in the store
    {ok, ChimboKey, Chimbo} = mountain:put(Chimbo),
    {ok, OlyKey, Oly}       = mountain:put(Oly),

    % update chimborazo
    ChimboUpdate = mountain:new([
        {lakes, [<<"Rio Chambo Dam">>]}
    ]),
    MergedChimbo = mountain:merge(Chimbo, ChimboUpdate, [{lake_merge, union}]),
    {ok, ChimboKey, MergedChimbo} = mountain:put_merge(ChimboKey, ChimboUpdate, [{lake_merge, union}]),

    % get the mountains out by key or tuple
    {ok, Chimbo2} = mountain:get(ChimboKey),
    {ok, Chimbo2} = mountain:get({<<"Chimborazo">>, <<"Cordillera Occidental">>}),
    {ok, Oly} = mountain:get(OlyKey),
    {ok, Oly} = mountain:get({<<"Olympus Mons">>, <<"Amazonis">>}),

    % deleting a deleted record should return not found
    {ok, ChimboKey} = mountain:delete({<<"Chimborazo">>, <<"Cordillera Occidental">>}),
    {ok, ChimboKey} = mountain:delete(ChimboKey),
    {ok, OlyKey} = mountain:delete(OlyKey),

    % with transaction
    meru_riak:transaction(meru_riak_pool_default, fun (Pid) ->
        {ok, ChimboKey, Chimbo} = mountain:put(Pid, Chimbo),
        {ok, OlyKey, Oly}       = mountain:put(Pid, Oly),

        {ok, ChimboKey, MergedChimbo} = mountain:put_merge(Pid, ChimboKey, ChimboUpdate, [{lake_merge, union}]),

        {ok, Chimbo2} = mountain:get(Pid, ChimboKey),
        {ok, Chimbo2} = mountain:get(Pid, {<<"Chimborazo">>, <<"Cordillera Occidental">>}),
        {ok, Oly} = mountain:get(Pid, OlyKey),
        {ok, Oly} = mountain:get(Pid, {<<"Olympus Mons">>, <<"Amazonis">>}),

        {ok, ChimboKey} = mountain:delete(Pid, {<<"Chimborazo">>, <<"Cordillera Occidental">>}),
        {ok, ChimboKey} = mountain:delete(Pid, ChimboKey),
        {ok, OlyKey} = mountain:delete(Pid, OlyKey)
    end).
