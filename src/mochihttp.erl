%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc mochihttp.

-module(mochihttp).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the mochihttp server.
start() ->
    mochihttp_deps:ensure(),
    ensure_started(crypto),
    application:start(mochihttp).


%% @spec stop() -> ok
%% @doc Stop the mochihttp server.
stop() ->
    application:stop(mochihttp).
