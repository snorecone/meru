#!/usr/bin/env escript
%%! -noshell -noinput
-compile({no_auto_import,[get/1]}).

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
    Get = {get, 1, fun (Key) ->
        case '___RIAK___':get('___BUCKET___', '___KEYFUN___'(Key)) of
            {ok, RObj} ->
                {ok, proplist_to_record(binary_to_term(riakc_obj:get_value(RObj)))};
            Error ->
                Error
        end
    end},
    
    % put/1
    Put = {put, 1, fun (Record) ->
        Key = '___KEYFUN___'(Record),
        case '___RIAK___':put(riakc_obj:new('___BUCKET___', Key, 
            term_to_binary(record_to_proplist(Record)))) of
            ok -> {ok, Key};
            Error -> Error
        end
    end},
    
    % put_merge/2
    PutMerge2 = {put_merge, 2, fun (Record, Options) ->
        case get(Record) of
            {ok, OldRecord} -> put('___MERGEFUN___'(OldRecord, Record, Options));
            _Error -> put('___MERGEFUN___'(notfound, Record, Options))
        end
    end},
    
    % put_merge/3
    PutMerge3 = {put_merge, 3, fun (Key, Record, Options) ->
        case get(Key) of
            {ok, OldRecord} -> put('___MERGEFUN___'(OldRecord, Record, Options));
            _Error -> put('___MERGEFUN___'(notfound, Record, Options))
        end
    end},
    
    % delete/1
    Delete = {delete, 1, fun (Record) ->
        Key = '___KEYFUN___'(Record),
        case '___RIAK___':delete('___BUCKET___', Key) of
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
    % end, [P2R, R2P, Get, Put, PutMerge, Delete]).
    end, [P2R, R2P, New0, New1, Get, Put, PutMerge2, PutMerge3, Delete]).

'___KEYFUN___'(_) -> ok.
'___MERGEFUN___'(_, _, _) -> ok.
proplist_to_record(_) -> ok.
record_to_proplist(_) -> ok.
get(_) -> ok.
put(_) -> ok.
new(_) -> ok.

