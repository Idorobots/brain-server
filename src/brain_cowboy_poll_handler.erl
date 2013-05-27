-module(brain_cowboy_poll_handler).
-behaviour(cowboy_loop_handler).

-export([init/3, info/3, terminate/3]).

%% Cowboy loop handler callbacks
init({tcp, http}, Request, _Options) ->
    lager:info("Received a poll/ request!"),
    Chunksize = 8 * brain:get_env(chunksize),
    Chunk = <<97:Chunksize>>,
    {Timeout, Req} = cowboy_req:binding(timeout, Request),
    Req2 = reply(Req, "Polling every " ++ binary_to_list(Timeout) ++ " seconds..."),
    T = binary_to_integer(Timeout) * 1000,
    erlang:send_after(T, self(), {feed, T}),
    {loop, Req2, Chunk, hibernate}.

terminate(_Reason, _Request, _State) ->
    ok.

%% Cowboy loop handler handlers
info(Message = {feed, Timeout}, Request, State) ->
    Chunk = State,
    feed(Request, binary_to_list(Chunk)),
    erlang:send_after(Timeout, self(), Message),
    {loop, Request, State, hibernate};

info(Message, Request, State) ->
    lager:warning("Unhandled message: ~p", [Message]),
    {ok, Request, State}.

%% Internal functions
reply(Request, Msg) ->
    {ok, Req} = cowboy_req:chunked_reply(200, [{<<"content-encoding">>, <<"utf-8">>}], Request),
    feed(Req, Msg),
    Req.

feed(Request, Msg) ->
    lager:info("Sent a chunk of data!"),
    ok = cowboy_req:chunk(Msg, Request).
