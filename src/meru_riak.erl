-module(meru_riak).

-behavior(gen_server).

-export([
    start_link/1,
    get/2,
    put/1,
    delete/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    connection
}).

%%
%% API.
%%
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

get(Bucket, Key) ->
    call_transaction({riak_command, get, [Bucket, Key]}).

put(RObj) ->
    call_transaction({riak_command, put, [RObj]}).

delete(Bucket, Key) ->
    call_transaction({riak_command, delete, [Bucket, Key]}).

%%
%% gen_server
%%
init([Host, Port]) ->
    {ok, Connection} = riakc_pb_socket:start_link(Host, Port),
    {ok, #state{ connection = Connection }}.

handle_call({riak_command, Command, Args}, _From, #state{ connection = Connection } = State) ->
    Reply = erlang:apply(riakc_pb_socket, Command, [Connection | Args]),
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% private
%%
call_transaction(Msg) ->
    poolboy:transaction(?MODULE, fun (Worker) -> gen_server:call(Worker, Msg) end).
