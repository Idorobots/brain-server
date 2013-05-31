-module(stats).

-export([all/0]).

-define(CH, ["data/cowboy_http_1k",
             "data/cowboy_http_2k",
             "data/cowboy_http_3k",
             "data/cowboy_http_4k",
             "data/cowboy_http_5k",
             "data/cowboy_http_6k",
             "data/cowboy_http_7k",
             "data/cowboy_http_8k",
             "data/cowboy_http_9k",
             "data/cowboy_http_10k"]).

-define(CW, ["data/cowboy_ws_1k",
             "data/cowboy_ws_2k",
             "data/cowboy_ws_3k",
             "data/cowboy_ws_4k",
             "data/cowboy_ws_5k",
             "data/cowboy_ws_6k",
             "data/cowboy_ws_7k",
             "data/cowboy_ws_8k",
             "data/cowboy_ws_9k",
             "data/cowboy_ws_10k"]).

-define(MH, ["data/mochiweb_http_1k",
             "data/mochiweb_http_2k",
             "data/mochiweb_http_3k",
             "data/mochiweb_http_4k",
             "data/mochiweb_http_5k",
             "data/mochiweb_http_6k",
             "data/mochiweb_http_7k",
             "data/mochiweb_http_8k",
             "data/mochiweb_http_9k",
             "data/mochiweb_http_10k"]).

-define(MW, ["data/mochiweb_ws_1k",
             "data/mochiweb_ws_2k",
             "data/mochiweb_ws_3k",
             "data/mochiweb_ws_4k",
             "data/mochiweb_ws_5k",
             "data/mochiweb_ws_6k",
             "data/mochiweb_ws_7k",
             "data/mochiweb_ws_8k",
             "data/mochiweb_ws_9k",
             "data/mochiweb_ws_10k"]).

-define(METRICS, [cpu_utilization, total, context_switches, io]).

-define(CPUS, [1, 2]).

all() ->
    {prepare(?CH),  prepare(?CW), prepare(?MH), prepare(?MW)}.

prepare(Files) ->
    AllData = lists:map(fun(File) ->
                                case file:script(File) of
                                    {ok, Data} -> Data;
                                    {error, _} -> io:format("Error while reading ~p~n", [File])
                                end
                        end,
                        Files),
    lists:map(fun(Metric) ->
                      ByMetric = lists:map(fun(Data) ->
                                                   {Metric, D} = lists:keyfind(Metric, 1, Data),
                                                   D
                                           end,
                                           AllData),
                      {Metric, analyze(Metric, ByMetric)}
              end,
              ?METRICS).

analyze(total, AllData) ->
    Max = maxes(AllData),
    Avg = avgs(AllData),
    {Max, Avg};

analyze(context_switches, AllData) ->
    Max = maxes(AllData),
    Avg = avgs(AllData),
    {Max, Avg};

analyze(io, AllData) ->
    In = lists:map(fun(Data) ->
                           lists:map(fun([{input, Value}, {output, _}]) ->
                                             Value
                                     end,
                                     Data)
                   end,
                   AllData),
    Out = lists:map(fun(Data) ->
                            lists:map(fun([{input, _}, {output, Value}]) ->
                                              Value
                                      end,
                                      Data)
                    end,
                    AllData),
    InMax = maxes(In),
    InAvg = avgs(In),
    OutMax = maxes(Out),
    OutAvg = avgs(Out),
    [{InMax, InAvg}, {OutMax, OutAvg}];

analyze(cpu_utilization, AllData) ->
    lists:map(fun(CPU) ->
                      CpuData = lists:map(fun(Data) ->
                                                  lists:map(fun(Value) ->
                                                                    {CPU, V} = lists:keyfind(CPU, 1, Value),
                                                                    V
                                                            end,
                                                            Data)
                                          end,
                                          AllData),

                      CpuMax = maxes(CpuData),
                      CpuAvg = avgs(CpuData),
                      {CpuMax, CpuAvg}
              end,
              ?CPUS).

maxes(ListOfLists) ->
    lists:map(fun(List) -> lists:foldl(fun max/2, 0, List) end, ListOfLists).

avgs(ListOfLists) ->
    lists:map(fun(List) -> lists:sum(List) / length(List) end, ListOfLists).
