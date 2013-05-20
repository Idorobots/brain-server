-module(brain_mochi).

-export([loop/2, setup/1, teardown/0]).
-export([ resume/3]).

setup(Options) ->
    Chunksize = 8 * brain:get_env(chunksize),
    Chunk = <<97:Chunksize>>,
    Loop = fun (Req) -> ?MODULE:loop(Req, Chunk) end,
    %% Set max connections to 1kk to REALLY stress test the server.
    mochiweb_http:start([{max, 1000000}, {name, ?MODULE}, {loop, Loop} | Options]).

teardown() ->
    mochiweb_http:stop(?MODULE).

loop(Req, Chunk) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case string:tokens(Path, "/") of
                ["static" | _Rest] ->
                    Response = Req:ok({"text/html; charset=utf-8",
                                       [{"Server", "Mochiweb-Test"}],
                                       chunked}),
                    Response:write_chunk("Static...\n");
                ["hibernate" | Rest] ->
                    Reentry = mochiweb_http:reentry({?MODULE, loop}),
                    Response = Req:ok({"text/html; charset=utf-8",
                                       [{"Server", "Mochiweb-Test"}],
                                       chunked}),
                    Response:write_chunk("Hibernate...\n"),
                    erlang:hibernate(?MODULE, resume, [Req, Rest, Reentry]);
                ["poll", Id | _Rest] ->
                    Response = Req:ok({"text/html; charset=utf-8",
                                       [{"Server","Mochiweb-Test"}],
                                       chunked}),
                    N = try list_to_integer(Id) of Num -> Num catch error:_ -> 5 end,
                    Response:write_chunk(io_lib:format("Polling every ~w seconds ~n", [N])),
                    feed(Response, Chunk, N);
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

feed(Response, Chunk, N) ->
    receive
    after N * 1000 -> Response:write_chunk(Chunk)
    end,
    feed(Response, Chunk, N).

%% Resumes here after hibertation
resume(Req, _Path, Reentry) ->
    receive
        _ -> Response = Req:ok({"text/html; charset=utf-8",
                                [{"Server", "Brain-Server"}],
                                chunked}),
             Response:write_chunk("Resumed...")
    end,
    Reentry(Req).
