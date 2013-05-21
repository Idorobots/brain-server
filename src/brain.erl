%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc brain.

-module(brain).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0, get_env/1]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the brain server.
start() ->
    brain_deps:ensure(),
    ensure_started(crypto),
    lager:start(),
    application:start(brain).


%% @spec stop() -> ok
%% @doc Stop the brain server.
stop() ->
    application:stop(brain).

get_env(Name) ->
    {ok, Value} = application:get_env(?MODULE, Name),
    Value.
