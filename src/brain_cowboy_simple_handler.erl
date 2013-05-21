-module(brain_cowboy_simple_handler).
-behaviour(cowboy_loop_handler).

-export([init/3, info/3, terminate/3]).

init({tcp, http}, Request, _Options) ->
    Chunksize = 8 * brain:get_env(chunksize),
    Chunk = <<97:Chunksize>>,
    State = Chunk,
    case cowboy_req:binding(path, Request) of
        {<<"static">>, Req}    -> lager:info("Received a static/ request!"),
                                  Req2 = reply(Req, "Static..."),
                                  {loop, Req2, State};
        {<<"hibernate">>, Req} ->  lager:info("Received a hibernate/ request!"),
                                   Req2 = reply(Req, "Hibernate..."),
                                   {loop, Req2, State, hibernate};
        {Path, Req}            -> Req2 = reply(Req, "Unknown path: " ++ binary_to_list(Path)),
                                  {ok, Req2, State}
    end.

info(Message, Request, State) ->
    lager:warning("Unhandled message: ~w", [Message]),
    {loop, Request, State}.

reply(Request, Msg) ->
    {ok, Req} = cowboy_req:chunked_reply(200, Request),
    feed(Req, Msg),
    Req.

feed(Request, Msg) ->
    ok = cowboy_req:chunk(Msg, Request).

terminate(_Reason, _Request, _State) ->
    ok.
