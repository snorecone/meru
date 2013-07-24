-module(meru_riak_sup).

-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

%%
%% API
%%
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%
%% supervisor callbacks
%%
init([]) ->
    Host = meru_app:get_env(riak_host, "localhost"),
    Port = meru_app:get_env(riak_port, 8087),
    PoolSize = meru_app:get_env(riak_pool_size, 10),
    PoolMaxOverflow = meru_app:get_env(riak_pool_max_overflow, 20),
    PoolSpecs = [poolboy:child_spec(meru_riak, [
            {name, {local, meru_riak}},
            {worker_module, meru_riak},
            {size, PoolSize},
            {max_overflow, PoolMaxOverflow}
        ],
        [Host, Port]
    )],
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.
