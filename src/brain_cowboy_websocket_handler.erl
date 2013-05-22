-module(brain_cowboy_websocket_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).

-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

%% Cowboy websocket handler callbacks
init({tcp, http}, _Request, _Options) ->
    lager:info("Received a wspoll/ request!"),
    {upgrade, protocol, cowboy_websocket}.

websocket_terminate(_Reason, _Request, _State) ->
    ok.

%% Cowboy websocket handler handlers
websocket_init(_Transport, Request, _Options) ->
    Chunksize = 8 * brain:get_env(chunksize),
    Chunk = <<97:Chunksize>>,
    {Timeout, Req} = cowboy_req:binding(timeout, Request),
    erlang:send_after(100, self(), {welcome, Timeout}),
    {ok, Req, Chunk}.

websocket_handle({text, Msg}, Request, State) ->
    lager:info("Received a chunk of data: ~p", [Msg]),
    {ok, Request, State};

websocket_handle(Frame, Request, State) ->
    lager:warning("Received an unhandled frame type: ~p", [Frame]),
    {ok, Request, State}.

websocket_info({welcome, Timeout}, Request, State) ->
    T = binary_to_integer(Timeout) * 1000,
    erlang:send_after(T, self(), {feed, T}),
    feed(Request, State, <<"Polling every ", Timeout/binary, " seconds...">>);

websocket_info({feed, Timeout}, Request, State) ->
    erlang:send_after(Timeout, self(), {feed, Timeout}),
    Chunk = State,
    feed(Request, State, Chunk);

websocket_info(Info, Request, State) ->
    lager:warning("Unhandled message: ~p", [Info]),
    {ok, Request, State}.

%% Internal functions
feed(Request, State, Message) ->
    lager:info("Sent a chunk of data!"),
    {reply, {text, Message}, Request, State}.
