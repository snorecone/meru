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
    PoolSpecs = [pool_spec(Riak) || Riak <- meru_app:get_env(riak_pools, default_riak_pools())],
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

pool_spec({Name0, Opts}) ->
    Name = meru:pool_name(Name0),
    Host = proplists:get_value(host, Opts, "localhost"),
    Port = proplists:get_value(port, Opts, 8087),
    PoolSize = proplists:get_value(pool_size, Opts, 10),
    PoolMaxOverflow = proplists:get_value(pool_max_overflow, Opts, 20),
    poolboy:child_spec(Name, [
            {name, {local, Name}},
            {worker_module, meru_riak},
            {size, PoolSize},
            {max_overflow, PoolMaxOverflow}
        ],
        [Host, Port]
    ).

default_riak_pools() ->
    [
     {default,
      [{host, "localhost"},
      {port, 8087},
      {pool_size, 10},
      {pool_max_overflow, 20}]}
    ].
