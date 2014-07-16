-module(meru_riak).
-compile({inline, [do_call/3]}).

-export([
    start_link/1,
    get/3,
    put/2,
    delete/3,
    mapred/3,
    call/3,
    transaction/2
]).

%%
%% API.
%%
start_link([Host, Port]) ->
    riakc_pb_socket:start_link(Host, Port).

get(Pool, Bucket, Key) when is_atom(Pool) ->
    do_call(Pool, get, [Bucket, Key]);
get(Pid, Bucket, Key) when is_pid(Pid) ->
    do_call(Pid, get, [Bucket, Key]).

put(Pool, RObj) when is_atom(Pool) ->
    do_call(Pool, put, [RObj]);
put(Pid, RObj) when is_pid(Pid) ->
    do_call(Pid, put, [RObj]).

delete(Pool, Bucket, Key) when is_atom(Pool) ->
    do_call(Pool, delete, [Bucket, Key]);
delete(Pid, Bucket, Key) when is_pid(Pid) ->
    do_call(Pid, delete, [Bucket, Key]).

mapred(Pool, Inputs, Query) when is_atom(Pool) ->
    do_call(Pool, mapred, [Inputs, Query]);
mapred(Pid, Inputs, Query) when is_pid(Pid) ->
    do_call(Pid, mapred, [Inputs, Query]).

call(Pool, Method, Args) when is_atom(Pool) ->
    do_call(Pool, Method, Args);
call(Pid, Method, Args) when is_pid(Pid) ->
    do_call(Pid, Method, Args).

transaction(Pool, Fun) ->
    poolboy:transaction(Pool, Fun).

%%
%% private
%%
do_call(Pool, Method, Args) when is_atom(Pool) ->
    poolboy:transaction(Pool, fun (Worker) ->
        erlang:apply(riakc_pb_socket, Method, [Worker | Args]) end);
do_call(Pid, Method, Args) when is_pid(Pid) ->
    erlang:apply(riakc_pb_socket, Method, [Pid | Args]).
