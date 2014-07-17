-module(wave).
-compile({parse_transform, meru_transform}).

-meru_pool(new_pool).
-meru_bucket(<<"waves">>).
-meru_record(wave).
-meru_keyfun(make_key).
-meru_mergefun(merge).
-meru_migration({old_pool, <<"oldwaves">>, migrate}).

-export([
    merge/3,
    make_key/1,
    migrate/1
]).

-record(wave, {
    length,
    amplitude,
    color,
    migrated,
    created_at,
    updated_at
}).

merge(notfound, NewWave, _MergeOpts) ->
    NewWave;
merge(#wave{ created_at = CreatedAt }, NewWave, _MergeOpts) ->
    NewWave#wave{ created_at = CreatedAt, updated_at = os:timestamp(), migrated = true }.

make_key(#wave{ length = Length, amplitude = Amplitude }) ->
    make_key({Length, Amplitude});
make_key({Length, Amplitude}) ->
    <<Length:32/integer,Amplitude:32/integer>>;
make_key(Key) when is_binary(Key) ->
    Key.

migrate(Wave) ->
    io:format(user, "migrating ~p~n", [Wave]),
    Wave#wave{ migrated = true }.
