-module(brain_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Port = brain:get_env(port),
    Processes = [web_specs(brain_web, Port)],
    Strategy = {one_for_one, 10, 10},
    {ok, {Strategy, Processes}}.

web_specs(Mod, Port) ->
    WebConfig = [{ip, {0,0,0,0}}, {port, Port}],
    {Mod,
     {Mod, start, [WebConfig]},
     permanent,
     5000,
     worker,
     dynamic}.
