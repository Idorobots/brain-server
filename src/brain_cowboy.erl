-module(brain_cowboy).

-export([setup/1, teardown/0]).

setup(_Options) -> % TODO Actually use options
    application:start(ranch),
    application:start(cowboy),
    Dispatch = cowboy_router:compile([{'_', [{"/[:path]", brain_cowboy_handler, []}]}]),
    io:format("Starting listener"),
    cowboy:start_http(http, 100,
                      [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]).

teardown() ->
    application:stop(cowboy).
