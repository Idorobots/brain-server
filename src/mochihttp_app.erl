%% @author Mochi Media <dev@mochimedia.com>
%% @copyright mochihttp Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the mochihttp application.

-module(mochihttp_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for mochihttp.
start(_Type, _StartArgs) ->
    mochihttp_deps:ensure(),
    mochihttp_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mochihttp.
stop(_State) ->
    ok.
