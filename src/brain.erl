-module(brain).
-export([start/0, stop/0, get_env/1]).

start() ->
    crypto:start(),
    lager:start(),
    application:start(brain).

stop() ->
    application:stop(brain).

%% External functions
get_env(Name) ->
    {ok, Value} = application:get_env(?MODULE, Name),
    Value.
