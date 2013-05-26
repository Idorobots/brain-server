%% @author Łukasz Lalik <lukasz.lalik@zadane.pl>
%% @copyright 2012 Zadane.pl

%% @doc Websockets module for Mochiweb.

-module(mochiweb_websocket).
-author('lukasz.lalik@zadane.pl').
-export([start/1, start_link/1, stop/0, stop/1]).
-export([loop/4, upgrade_connection/1, request/4]).
-export([after_response/4, reentry/1, send/3]).
-export([parse_range_request/1, range_skip_length/2]).

-define(REQUEST_RECV_TIMEOUT, 3000000).   %% timeout waiting for request line
-define(HEADERS_RECV_TIMEOUT, 30000).    %% timeout waiting for headers

-define(MAX_HEADERS, 1000).
-define(DEFAULTS, [{name, ?MODULE},
                   {port, 8888}]).

parse_options(Options) ->
    {loop, HttpLoop} = proplists:lookup(loop, Options),
    Loop = {?MODULE, loop, [HttpLoop]},
    Options1 = [{loop, Loop} | proplists:delete(loop, Options)],
    mochilists:set_defaults(?DEFAULTS, Options1).

stop() ->
    mochiweb_socket_server:stop(?MODULE).

stop(Name) ->
    mochiweb_socket_server:stop(Name).

%% @spec start(Options) -> ServerRet
%%     Options = [option()]
%%     Option = {name, atom()} | {ip, string() | tuple()} | {backlog, integer()}
%%              | {nodelay, boolean()} | {acceptor_pool_size, integer()}
%%              | {ssl, boolean()} | {profile_fun, undefined | (Props) -> ok}
%%              | {link, false}
%% @doc Start a mochiweb server.
%%      profile_fun is used to profile accept timing.
%%      After each accept, if defined, profile_fun is called with a proplist of a subset of the mochiweb_socket_server state and timing information.
%%      The proplist is as follows: [{name, Name}, {port, Port}, {active_sockets, ActiveSockets}, {timing, Timing}].
%% @end
start(Options) ->
    mochiweb_socket_server:start(parse_options(Options)).

start_link(Options) ->
    mochiweb_socket_server:start_link(parse_options(Options)).

loop(Socket, Body, State, WsVersion) ->
    ok = mochiweb_socket:setopts(Socket, [{packet, 0}, {active, once}]),
    proc_lib:hibernate(?MODULE, request, [Socket, Body, State, WsVersion]).
    %request(Socket, Body, State, WsVersion).

upgrade_connection(Req) ->
    % Headers for HYBI handshake
    SecKey  = Req:get_header_value("sec-websocket-key"),
    Sec1Key = Req:get_header_value("Sec-WebSocket-Key1"),
    Sec2Key = Req:get_header_value("Sec-WebSocket-Key2"),
    Origin = Req:get_header_value(origin),

    if not (SecKey == undefined) ->
          hybi_handshake(Req, SecKey);

      (not (Sec1Key == undefined)) and (not (Sec2Key == undefined)) ->
          Body = Req:recv(8),
          hixie_handshake(Req, Sec1Key, Sec2Key, Body, Origin);

       true ->
          mochiweb_socket:close(Req:get(socket)),
          exit(normal)
    end.
   

hybi_handshake(Req, SecKey) ->
  Challenge = handshake_key(SecKey),
  Req:respond({101, [{"Connection", "Upgrade"},
                     {"Upgrade", "websocket"},
                     {"Sec-Websocket-Accept", Challenge}], ""}),
  hybi.

hixie_handshake(Req, Key1, Key2, Body, Origin) ->
  Ikey1 = [D || D <- Key1, $0 =< D, D =< $9],
  Ikey2 = [D || D <- Key2, $0 =< D, D =< $9],
  Blank1 = length([D || D <- Key1, D =:= 32]),
  Blank2 = length([D || D <- Key2, D =:= 32]),
  Part1 = erlang:list_to_integer(Ikey1) div Blank1,
  Part2 = erlang:list_to_integer(Ikey2) div Blank2,
  Ckey = <<Part1:4/big-unsigned-integer-unit:8, Part2:4/big-unsigned-integer-unit:8, Body/binary>>,
  Challenge = erlang:md5(Ckey),

  Location = lists:concat(["ws://", 
                           Req:get_header_value("Host"),
                           Req:get(path)]),

  Req:respond({101, [{"Upgrade", "WebSocket"},
                     {"Connection", "Upgrade"},
                     {"Sec-WebSocket-Origin", Origin},
                     {"Sec-WebSocket-Location", Location}],
                    Challenge}),
  hixie.



send(Socket, Payload, hybi) ->
    Len = payload_length(iolist_size(Payload)),
    Data = <<1:1, 0:3, 1:4, 0:1, Len/bits, Payload/binary>>,
    mochiweb_socket:send(Socket, Data);

send(Socket, Payload, hixie) ->
    Data = <<0, Payload/binary, 255>>,
    mochiweb_socket:send(Socket, Data).

request(Socket, Body, WsVersion) ->
    request(Socket, Body, [], WsVersion).
request(Socket, Body, State, WsVersion) ->
    receive
        {tcp_closed, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);
        {ssl_closed, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);
        {tcp_error, _, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);

        {tcp, _, WsFrames} ->
            {M, F} = Body,
            case WsVersion of
              hybi ->
                Reply = fun(close) ->
                                mochiweb_socket:close(Socket),
                                exit(normal);
                            (Payload) ->
                                M:F(Socket, Payload, State, WsVersion)
                        end,

                try parse_frames(Socket, WsFrames, []) of
                    Parsed -> process_frames(Parsed, Reply, [])
                catch
                    _:_ -> Reply(close)
                end;

              hixie ->
                try handle_frames(WsFrames, []) of
                    Payload -> M:F(Socket, Payload, State, WsVersion)
                catch
                    _:_ ->
                      mochiweb_socket:close(Socket),
                      exit(normal)
                end
                
            end;

        Other ->
            io:format(">>>>>> WS INVALID REQUEST: ~p~n", [Other]),
            handle_invalid_request(Socket)
    end.

process_frames([], Reply, Acc) ->
    Reply(lists:reverse(Acc));

process_frames([{Opcode, Payload} | Rest], Reply, Acc) ->
    case Opcode of
        8 ->
            Reply(lists:reverse(Acc)),
            Reply(close);

        _ ->
            process_frames(Rest, Reply, [Payload | Acc])
    end.

reentry(Body) ->
    fun (Socket, State, WsVersion) ->
            ?MODULE:after_response(Body, Socket, State, WsVersion)
    end.

headers(Socket, Request, Headers, _Body, ?MAX_HEADERS) ->
    %% Too many headers sent, bad request.
    ok = mochiweb_socket:setopts(Socket, [{packet, 0}]),
    handle_invalid_request(Socket, Request, Headers);
headers(Socket, Request, Headers, Body, HeaderCount) ->
    ok = mochiweb_socket:setopts(Socket, [{active, once}]),
    receive
        {Protocol, _, http_eoh} when Protocol == http orelse Protocol == ssl ->
            Req = new_request(Socket, Request, Headers),
            call_body(Body, Req),
            ?MODULE:after_response(Body, Req);
        {Protocol, _, {http_header, _, Name, _, Value}} when Protocol == http orelse Protocol == ssl ->
            headers(Socket, Request, [{Name, Value} | Headers], Body,
                    1 + HeaderCount);
        {tcp_closed, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);
        _Other ->
            handle_invalid_request(Socket, Request, Headers)
    after ?HEADERS_RECV_TIMEOUT ->
        mochiweb_socket:close(Socket),
        exit(normal)
    end.

call_body({M, F, A}, Req) ->
    erlang:apply(M, F, [Req | A]);
call_body({M, F}, Req) ->
    M:F(Req);
call_body(Body, Req) ->
    Body(Req).

-spec handle_invalid_request(term()) -> no_return().
handle_invalid_request(Socket) ->
    handle_invalid_request(Socket, {'GET', {abs_path, "/"}, {0,9}}, []),
    exit(normal).

-spec handle_invalid_request(term(), term(), term()) -> no_return().
handle_invalid_request(Socket, Request, RevHeaders) ->
    Req = new_request(Socket, Request, RevHeaders),
    Req:respond({400, [], []}),
    mochiweb_socket:close(Socket),
    exit(normal).

new_request(Socket, Request, RevHeaders) ->
    ok = mochiweb_socket:setopts(Socket, [{packet, 0}]),
    mochiweb:new_request({Socket, Request, lists:reverse(RevHeaders)}).

after_response(Body, Socket, State, WsVersion) ->
    %erlang:garbage_collect(),
    ?MODULE:loop(Socket, Body, State, WsVersion).
    % case Req:should_close() of
    %     true ->
    %         mochiweb_socket:close(Socket),
    %         exit(normal);
    %     false ->
    %         Req:cleanup(),
    %         erlang:garbage_collect(),
    %         ?MODULE:loop(Socket, Body)
    % end.

parse_range_request("bytes=0-") ->
    undefined;
parse_range_request(RawRange) when is_list(RawRange) ->
    try
        "bytes=" ++ RangeString = RawRange,
        Ranges = string:tokens(RangeString, ","),
        lists:map(fun ("-" ++ V)  ->
                          {none, list_to_integer(V)};
                      (R) ->
                          case string:tokens(R, "-") of
                              [S1, S2] ->
                                  {list_to_integer(S1), list_to_integer(S2)};
                              [S] ->
                                  {list_to_integer(S), none}
                          end
                  end,
                  Ranges)
    catch
        _:_ ->
            fail
    end.

range_skip_length(Spec, Size) ->
    case Spec of
        {none, R} when R =< Size, R >= 0 ->
            {Size - R, R};
        {none, _OutOfRange} ->
            {0, Size};
        {R, none} when R >= 0, R < Size ->
            {R, Size - R};
        {_OutOfRange, none} ->
            invalid_range;
        {Start, End} when 0 =< Start, Start =< End, End < Size ->
            {Start, End - Start + 1};
        {_OutOfRange, _End} ->
            invalid_range
    end.

%%
%% Websockets internal functions for RFC6455 (hybi)
%%

% RFC6455 Handshake
handshake_key(Key) ->
    BinKey = list_to_binary(Key),
    Bin = <<BinKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>,
    base64:encode(crypto:sha(Bin)).

parse_frames(_, <<>>, Acc) ->
    lists:reverse(Acc);

parse_frames(S, <<Fin:1, 
               Rsv:3, 
               Opcode:4, 
               Mask:1, 
               PayloadLen:7, 
               MaskKey:4/binary,
               Payload:PayloadLen/binary-unit:8,
               Rest/binary>>,
             Acc) when PayloadLen < 126 ->

    Payload2 = extract_payload(MaskKey, Payload),
    parse_frames(S, Rest, [{Opcode, Payload2} | Acc]);

parse_frames(S, <<Fin:1, 
               Rsv:3, 
               Opcode:4, 
               Mask:1, 
               126:7, 
               PayloadLen:16,
               MaskKey:4/binary,
               Payload:PayloadLen/binary-unit:8,
               Rest/binary>>,
             Acc) ->


    Payload2 = extract_payload(MaskKey, Payload),
    parse_frames(S, Rest, [{Opcode, Payload2} | Acc]);

parse_frames(Socket, <<Fin:1, 
               Rsv:3, 
               Opcode:4, 
               Mask:1, 
               126:7, 
               PayloadLen:16,
               MaskKey:4/binary,
               _/binary-unit:8>> = PartFrame,
             Acc) ->
    
    ok = mochiweb_socket:setopts(Socket, [{packet, 0}, {active, once}]),
    receive
        {tcp_closed, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);
        {ssl_closed, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);
        {tcp_error, _, _} ->
            mochiweb_socket:close(Socket),
            exit(normal);

        {tcp, _, Continuation} ->
          parse_frames(Socket, <<PartFrame/binary, Continuation/binary>>, Acc);
            
        Other ->
            mochiweb_socket:close(Socket),
            exit(normal)
    after
      5000 -> 
        mochiweb_socket:close(Socket),
        exit(normal)
    end;

parse_frames(S, <<Fin:1,
               Rsv:3, 
               Opcode:4, 
               Mask:1, 
               127:7, 
               0:1, 
               PayloadLen:63, 
               MaskKey:4/binary,
               Payload:PayloadLen/binary-unit:8,
               Rest/binary>>,
             Acc) ->

    Payload2 = extract_payload(MaskKey, Payload),
    parse_frames(S, Rest, [{Opcode, Payload2} | Acc]).

extract_payload(MaskKey, Payload) ->
    websocket_unmask(Payload, MaskKey, <<>>).

% Unmasks RFC 6455 message
websocket_unmask(<<O:32, Rest/bits>>, MaskKey, Acc) ->
    <<MaskKey2:32>> = MaskKey,
    T = O bxor MaskKey2,
    websocket_unmask(Rest, MaskKey, <<Acc/binary, T:32>>);
websocket_unmask(<<O:24>>, MaskKey, Acc) ->
    <<MaskKey2:24, _:8>> = MaskKey,
    T = O bxor MaskKey2,
    <<Acc/binary, T:24>>;
websocket_unmask(<<O:16>>, MaskKey, Acc) ->
    <<MaskKey2:16, _:16>> = MaskKey,
    T = O bxor MaskKey2,
    <<Acc/binary, T:16>>;
websocket_unmask(<<O:8>>, MaskKey, Acc) ->
    <<MaskKey2:8, _:24>> = MaskKey,
    T = O bxor MaskKey2,
    <<Acc/binary, T:8>>;
websocket_unmask(<<>>, MaskKey, Acc) ->
    Acc.

payload_length(N) ->
    case N of
        N when N =< 125 -> << N:7 >>;
        N when N =< 16#ffff -> << 126:7, N:16 >>;
        N when N =< 16#7fffffffffffffff -> << 127:7, N:64 >>
    end.


%%
%% Websockets internal functions for hixie-76 websocket version
%%

handle_frames(<<>>, Frames) ->
  lists:reverse(Frames);
handle_frames(<<0, T/binary>>, Frames) ->
  {Frame, Rest} = handle_frame(T, <<>>),
  handle_frames(Rest, [Frame | Frames]).

handle_frame(<<255, Rest/binary>>, Buffer) ->
  {Buffer, Rest};
handle_frame(<<H, T/binary>>, Buffer) ->
  handle_frame(T, <<Buffer/binary, H>>).