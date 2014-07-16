-module(meru).

-export([
    start/0,
    pool_name/1
]).

start() ->
    application:start(protobuffs),
    application:start(riak_pb),
    application:start(riakc),
    application:start(poolboy),
    application:start(?MODULE).

pool_name(Name) ->
    list_to_atom("meru_riak_pool_" ++ atom_to_list(Name)).

%%
%% private
%%
