-module(brain_cowboy_poll_handler).

-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Request, Options) ->
    {ok, Request, undefined}.

handle(Request, State) ->
    {Timeout, Req} = cowboy_req:binding(timeout, Request),
    reply(Req, "Polling every " ++ binary_to_list(Timeout) ++ " seconds...", State).

reply(Request, Msg, State) ->
    {ok, Reply} = cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}], Msg, Request),
    {ok, Reply, State}.

terminate(_Reason, _Request, _State) ->
    ok.
