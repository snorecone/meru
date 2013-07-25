-module(meru).

-export([
    start/0
]).

start() ->
    application:start(protobuffs),
    application:start(riak_pb),
    application:start(riakc),
    application:start(poolboy),
    application:start(?MODULE).

%%
%% private
%%
