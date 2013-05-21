-module(brain_cowboy_handler).

-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Request, _Options) ->
    {ok, Request, undefined}.

handle(Request, State) ->
    case cowboy_req:binding(path, Request) of
        {<<"poll">>, Req}      -> reply(Req, "Polling every # seconds...", State);
        {<<"static">>, Req}    -> reply(Req, "Static...", State);
        {<<"hibernate">>, Req} -> reply(Req, "Hibernate...", State);
        {Path, Req}        -> reply(Req, Path, State)
    end.

reply(Request, Msg, State) ->
    {ok, Reply} = cowboy_req:reply(200, [{<<"content-encoding">>, <<"utf-8">>}], Msg, Request),
    {ok, Reply, State}.

terminate(_Reason, _Request, _State) ->
    ok.