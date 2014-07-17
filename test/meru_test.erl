-module(meru_test).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

meru_test() ->
    meru:start(),
    {ok, mountain} = compile:file("../examples/mountain.erl"),

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
    end),
    application:stop(meru).

meru_migration_test() ->
    application:set_env(meru, riak_pools, [
                                           {old_pool, [{host, "localhost"}, {port, 8087},
                                                       {pool_size, 1}, {pool_max_overflow, 10}]},
                                           {new_pool, [{host, "localhost"}, {port, 8087},
                                                       {pool_size, 1}, {pool_max_overflow, 10}]}
                                          ]),
    meru:start(),
    {ok, wave} = compile:file("../examples/wave.erl"),
    OldPool = meru_riak_pool_old_pool,
    NewPool = meru_riak_pool_new_pool,
    OldBucket = <<"oldwaves">>,
    NewBucket = <<"waves">>,
    CreatedAt1 = os:timestamp(),
    CreatedAt2 = os:timestamp(),
    OldWave1 = wave:new([{length, 1}, {amplitude, 1}, {created_at, CreatedAt1}]),
    OldWave2 = wave:new([{length, 2}, {amplitude, 2}, {created_at, CreatedAt2}]),

    % set up fixtures
    lists:foreach(fun(K) ->
                          meru_riak:delete(OldPool, OldBucket, wave:make_key(K)),
                          meru_riak:delete(NewPool, NewBucket, wave:make_key(K)),
                          meru_riak:put(OldPool, riakc_obj:new(OldBucket, wave:make_key(K), term_to_binary(wave:record_to_proplist(K))))
                  end, [OldWave1, OldWave2]),

    % magically pull one out
    {ok, NewWave1} = wave:get({1,1}),

    % the migrated property is set
    {wave, 1, 1, undefined, true, CreatedAt1, undefined} = NewWave1,

    % we can also pull it out directly from the new pool/bucket
    {ok, RObj1} = meru_riak:get(NewPool, NewBucket, wave:make_key(OldWave1)),
    NewWave1 = wave:proplist_to_record(binary_to_term(riakc_obj:get_value(RObj1))),

    % update a record that has been migrated
    wave:put_merge(wave:new([{amplitude, 1}, {length, 1}, {color, blue}]), []),
    {ok, {wave, 1, 1, blue, true, CreatedAt1, {_, _, _}}} = wave:get(OldWave1),

    % update a record that has not been migrated
    wave:put_merge(wave:new([{amplitude, 2}, {length, 2}, {color, red}]), []),
    {ok, {wave, 2, 2, red, true, CreatedAt2, {_, _, _}}} = wave:get(OldWave2),

    application:stop(meru).


