#!/usr/bin/env escript
%%! -noshell -noinput
-compile({no_auto_import,[get/1,put/2]}).

main([]) ->
    % proplist_to_record/1
    P2R = {proplist_to_record, 1, fun (Proplist) ->
        list_to_tuple(
            ['___RECORD_NAME___' | 
                lists:reverse(
                    lists:foldl(fun ({K, Default}, Acc) ->
                        [proplists:get_value(K, Proplist, Default) | Acc]
                    end, [], lists:zip('___RECORD_FIELDS___', '___RECORD_DEFAULTS___')))])
    end},

    % record_to_proplist/1
    R2P = {record_to_proplist, 1, fun (Record) ->
        lists:zip('___RECORD_FIELDS___', tl(tuple_to_list(Record)))
    end},

    % new/0
    New0 = {new, 0, fun () ->
        new([])
    end},

    % new/1
    New1 = {new, 1, fun (Proplist) ->
        proplist_to_record(Proplist)
    end},

    % get/1
    Get1 = {get, 1, fun (Key) ->
        '___RIAK___':transaction(fun (Pid) ->
            get(Pid, Key)
        end)
    end},

    % get/2
    Get2 = {get, 2, fun (Pid, Key) ->
        case '___RIAK___':get(Pid, '___BUCKET___', '___KEYFUN___'(Key)) of
            {ok, RObj} ->
                {ok, proplist_to_record(binary_to_term(riakc_obj:get_value(RObj)))};
            Error ->
                Error
        end
    end},

    % put/1
    Put1 = {put, 1, fun (Record) ->
        '___RIAK___':transaction(fun (Pid) ->
            put(Pid, Record)
        end)
    end},

    % put/2
    Put2 = {put, 2, fun (Pid, Record) ->
        Key = '___KEYFUN___'(Record),
        case '___RIAK___':put(Pid, riakc_obj:new('___BUCKET___', Key, 
            term_to_binary(record_to_proplist(Record)))) of
            ok -> {ok, Key, Record};
            Error -> Error
        end
    end},

    % put_merge/2
    PutMerge2 = {put_merge, 2, fun (Record, Options) ->
        '___RIAK___':transaction(fun (Pid) ->
            put_merge(Pid, Record, Options)
        end)
    end},

    % put_merge/3
    PutMerge3 = {put_merge, 3, fun (Pid, Record, Options) when is_pid(Pid) ->
        case get(Pid, Record) of
            {ok, OldRecord} -> put(Pid, '___MERGEFUN___'(OldRecord, Record, Options));
            _Error -> put(Pid, '___MERGEFUN___'(notfound, Record, Options))
        end;
    (Key, Record, Options) ->
        '___RIAK___':transaction(fun (Pid) ->
            put_merge(Pid, Key, Record, Options)
        end)
    end},

    % put_merge/4
    PutMerge4 = {put_merge, 4, fun (Pid, Key, Record, Options) ->
        case get(Pid, Key) of
            {ok, OldRecord} -> put(Pid, '___MERGEFUN___'(OldRecord, Record, Options));
            _Error -> put(Pid, '___MERGEFUN___'(notfound, Record, Options))
        end
    end},

    % delete/1
    Delete1 = {delete, 1, fun (Record) ->
        '___RIAK___':transaction(fun (Pid) ->
            delete(Pid, Record)
        end)
    end},

    % delete/2
    Delete2 = {delete, 2, fun (Pid, Record) ->
        Key = '___KEYFUN___'(Record),
        case '___RIAK___':delete(Pid, '___BUCKET___', Key) of
            ok -> {ok, Key};
            Error -> Error
        end
    end},

    % print the ast for each fun
    lists:foreach(fun ({FName, Arity, F}) ->
        {env, [_, _, _, Forms]} = erlang:fun_info(F, env),
        Fun = [{function, 1, FName, Arity, Forms}],
        io:format("~p~n--------------------~n", [FName]),
        io:format("~p~n~n", [Fun]),
        io:format("---------------------~n~n~n")
    end, [P2R, R2P, New0, New1, Get1, Get2, Put1, Put2, PutMerge2, PutMerge3, PutMerge4, Delete1, Delete2]).

'___KEYFUN___'(_) -> ok.
'___MERGEFUN___'(_, _, _) -> ok.
proplist_to_record(_) -> ok.
record_to_proplist(_) -> ok.
get(_) -> ok.
get(_,_) -> ok.
put(_) -> ok.
put(_,_) -> ok.
new(_) -> ok.
delete(_,_) -> ok.
put_merge(_,_,_) -> ok.
put_merge(_,_,_,_) -> ok.
