-module(meru_app).

-behavior(application).

-export([
    get_env/2,
    get_env/3,
    start/2,
    stop/1
]).

%%
%% API
%%

get_env(Key, Default) ->
    get_env(meru, Key, Default).

get_env(App, Key, Default) ->
    case application:get_env(App, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

%%
%% application callbacks
%%

start(_StartType, _StartArgs) ->
    meru_sup:start_link([]).

stop(_State) ->
    ok.

%%
%% private
%%

