-module(meru_riak).
-compile({inline, [call/2]}).

-export([
    start_link/1,
    get/2,
    put/1,
    delete/2,
    call/2
]).

%%
%% API.
%%
start_link([Host, Port]) ->
    riakc_pb_socket:start_link(Host, Port).

get(Bucket, Key) ->
    call(get, [Bucket, Key]).

put(RObj) ->
    call(put, [RObj]).

delete(Bucket, Key) ->
    call(delete, [Bucket, Key]).

call(Method, Args) ->
    poolboy:transaction(?MODULE, fun (Worker) -> 
        erlang:apply(riakc_pb_socket, Method, [Worker | Args]) end).

%%
%% private
%%