-module(brain_mochi).

-export([setup/1, teardown/0]).
-export([ resume/3]).

setup(Options) ->
    Chunksize = 8 * brain:get_env(chunksize),
    Chunk = <<97:Chunksize>>,
    Loop = fun (Req) -> loop(Req, Chunk) end,
    mochiweb_http:start([{max, brain:get_env(max_connections)},
                         {name, ?MODULE},
                         {loop, Loop} | Options]).

teardown() ->
    mochiweb_http:stop(?MODULE).

loop(Req, Chunk) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case string:tokens(Path, "/") of
                ["static" | _Rest] ->
                    lager:info("Received a static/ request!"),
                    Response = Req:ok({"text/html; charset=utf-8",
                                       [{"Server", "Mochiweb-Test"}],
                                       chunked}),
                    Response:write_chunk("Static...\n");
                ["hibernate" | Rest] ->
                    lager:info("Received a hibernate/ request!"),
                    Reentry = mochiweb_http:reentry({?MODULE, loop}),
                    Response = Req:ok({"text/html; charset=utf-8",
                                       [{"Server", "Mochiweb-Test"}],
                                       chunked}),
                    Response:write_chunk("Hibernate...\n"),
                    erlang:hibernate(?MODULE, resume, [Req, Rest, Reentry]);
                ["poll", Timeout | _Rest] ->
                    lager:info("Received a poll/ request!"),
                    Response = Req:ok({"text/html; charset=utf-8",
                                       [{"Server","Mochiweb-Test"}],
                                       chunked}),
                    T = try list_to_integer(Timeout) of
                            Num -> Num
                        catch
                            error:_ -> 5
                        end,
                    Response:write_chunk(io_lib:format("Polling every ~p seconds ~n", [T])),
                    feed(Response, Chunk, T);
                ["wspoll", Timeout | _Rest] ->
                    lager:info("Received a wspoll/ request!"),
                    WsVersion = mochiweb_websocket:upgrade_connection(Req),
                    Socket = Req:get(socket),

                    T = try list_to_integer(Timeout) of
                            Num -> Num
                        catch
                            error:_ -> 5
                        end,
                    Msg = list_to_binary(io_lib:format("Poling every ~p seconds...", [T])),
                    mochiweb_websocket:send(Socket, Msg, WsVersion),
                    wsfeed(Socket, Chunk, WsVersion, T);
                _ ->
                    Req:not_found()
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

feed(Response, Chunk, T) ->
    timer:sleep(T*1000),
    case Response:write_chunk(Chunk) of
        ok -> lager:info("Sent a chunk of data!"),
              feed(Response, Chunk, T);
        _  -> lager:info("Connection closed!")
    end.

wsfeed(Socket, Chunk, WsVersion, T) ->
    timer:sleep(T*1000),
    case mochiweb_websocket:send(Socket, Chunk, WsVersion) of
        ok -> lager:info("Sent a chunk of data!"),
              wsfeed(Socket, Chunk, WsVersion, T);
        _  -> lager:info("Connection closed!")
    end.

%% Process resumes here after hibernation
resume(Req, _Path, Reentry) ->
    receive
        _ -> Response = Req:ok({"text/html; charset=utf-8",
                                [{"Server", "Brain-Server"}],
                                chunked}),
             Response:write_chunk("Resumed...")
    end,
    Reentry(Req).
