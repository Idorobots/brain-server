-module(brain_cowboy).

-export([setup/1, teardown/0]).

setup(Options) ->
    lager:info("Starting Cowboy server..."),
    application:start(ranch),
    application:start(cowboy),
    Dispatch = cowboy_router:compile([{'_', [{"/poll/:timeout", brain_cowboy_poll_handler, []},
                                             {"/:path", brain_cowboy_simple_handler, []}]}]),
    cowboy:start_http(http, 100,
                      [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]),
    lager:info("Cowboy started!").

teardown() ->
    application:stop(cowboy).
