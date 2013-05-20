%% @author Mochi Media <dev@mochimedia.com>
%% @copyright brain Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the brain application.

-module(brain_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for brain.
start(_Type, _StartArgs) ->
    brain_deps:ensure(),
    brain_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for brain.
stop(_State) ->
    ok.
