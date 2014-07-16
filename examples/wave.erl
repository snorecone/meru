-module(wave).
-compile({parse_transform, meru_transform}).

-meru_pool(new_pool).
-meru_bucket(<<"waves">>).
-meru_record(wave).
-meru_keyfun(make_key).
-meru_mergefun(merge).
-meru_migration({old_pool, <<"oldwaves">>}).

-export([
    merge/3
]).

-record(wave, {
    length,
    amplitude,
    migrated,
    created_at,
    updated_at
}).

merge(notfound, NewWave, _) ->
    NewWave;
merge(#wave{ created_at = CreatedAt }, NewWave, MergeOpts) ->
    case proplists:get_bool(meru_migration, MergeOpts) of
        true ->
            NewWave#wave{ created_at = CreatedAt, migrated = true };
        false ->
            NewWave#wave{ created_at = CreatedAt }
    end.

make_key(#wave{ length = Length, amplitude = Amplitude }) ->
    make_key({Length, Amplitude});
make_key({Length, Amplitude}) ->
    <<Length:32/integer,Amplitude:32/integer>>.
