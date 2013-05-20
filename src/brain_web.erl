%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for brain.

-module(brain_web).
-define(CHUNK, <<97:4096>>).

-export([start/1, stop/0, loop/2, resume/3]).

%% External API
%% External API
start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    % we'll set our maximum to 1 million connections. (default: 2048)
    mochiweb_http:start([{max, 1000000}, {name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case string:tokens(Path, "/") of
                 ["static" | Rest] ->
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
                ["poll",  Id | Rest] ->
                    Response = Req:ok({"text/html; charset=utf-8",
                                      [{"Server","Mochiweb-Test"}],
                                      chunked}),
                    N = try list_to_integer(Id) of Num -> Num catch error:_ -> 5 end,
                    Response:write_chunk(io_lib:format("Polling every ~w seconds ~n", [N])),
                    %% router:login(list_to_atom(Id), self()),
                    feed(Response, N);
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

feed(Response, N) ->
    receive
        %{router_msg, Msg} ->
        %    Html = io_lib:format("Recvd msg #~w: '~s'<br/>", [N, Msg]),
        %    Response:write_chunk(Html);
    after  N*1000->
        Response:write_chunk(?CHUNK)
    end,
    feed(Response, N).

%% Resumes here after hibertation
resume(Req, Path, Reentry) ->
    receive
        Msg -> Response = Req:ok({"text/html; charset=utf-8",
                                 [{"Server", "Mochiweb-Test"}],
                                 chunked}),
               Response:write_chunk("Resumed...")
    end,
    Reentry(Req).

%% Internal API
get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
