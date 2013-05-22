-module(brain_cowboy).

-export([setup/1, teardown/0]).

setup(Options) ->
    lager:info("Starting Cowboy server..."),
    application:start(ranch),
    application:start(cowboy),
    Dispatch = cowboy_router:compile([{'_', [{"/wspoll/:timeout", brain_cowboy_websocket_handler, []},
                                             {"/poll/:timeout", brain_cowboy_poll_handler, []},
                                             {"/:path", brain_cowboy_simple_handler, []}]}]),
    Port = proplists:get_value(port, Options),
    cowboy:start_http(http, brain:get_env(max_connections),
                      [{port, Port}],
                      [{env, [{dispatch, Dispatch}]}]),
    lager:info("Cowboy started!").

teardown() ->
    application:stop(cowboy).
