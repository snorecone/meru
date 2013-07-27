-module(meru_riak).
-compile({inline, [do_call/2, do_call/3]}).

-export([
    start_link/1,
    get/2,
    get/3,
    put/1,
    put/2,
    delete/2,
    delete/3,
    mapred/2,
    mapred/3,
    call/2,
    call/3,
    transaction/1
]).

%%
%% API.
%%
start_link([Host, Port]) ->
    riakc_pb_socket:start_link(Host, Port).

get(Bucket, Key) ->
    do_call(get, [Bucket, Key]).

get(Pid, Bucket, Key) ->
    do_call(Pid, get, [Bucket, Key]).

put(RObj) ->
    do_call(put, [RObj]).

put(Pid, RObj) ->
    do_call(Pid, put, [RObj]).

delete(Bucket, Key) ->
    do_call(delete, [Bucket, Key]).

delete(Pid, Bucket, Key) ->
    do_call(Pid, delete, [Bucket, Key]).

mapred(Inputs, Query) ->
    do_call(mapred, [Inputs, Query]).

mapred(Pid, Inputs, Query) ->
    do_call(Pid, mapred, [Inputs, Query]).

call(Method, Args) ->
    do_call(Method, Args).

call(Pid, Method, Args) when is_pid(Pid) ->
    do_call(Pid, Method, Args).

transaction(Fun) ->
    poolboy:transaction(?MODULE, Fun).

%%
%% private
%%
do_call(Method, Args) ->
    poolboy:transaction(?MODULE, fun (Worker) -> 
        erlang:apply(riakc_pb_socket, Method, [Worker | Args]) end).

do_call(Pid, Method, Args) ->
    erlang:apply(riakc_pb_socket, Method, [Pid | Args]).
