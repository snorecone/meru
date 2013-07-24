-module(meru_sup).

-behaviour(supervisor).

-export([
    start_link/1,
    init/1
]).

%%
%% API
%%
start_link(Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%%
%% supervisor callbacks
%%
init([]) ->
    {ok, {{one_for_one, 10, 10}, [
        {meru_riak_sup, {meru_riak_sup, start_link, []}, permanent, infinity, supervisor, [meru_riak_sup]}
    ]}}.
