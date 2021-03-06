-module(brain_web).

-export([start/1, stop/0]).

%% External API
start(Options) ->
    case brain:get_env(backend) of
        mochiweb -> brain_mochi:setup(Options);
        cowboy   -> brain_cowboy:setup(Options);
        Other    -> lager:warning("Backend ~w not implemented. Using Mochiweb instead.", [Other]),
                    brain_mochi:setup(Options)
    end.

stop() ->
    case brain:get_env(backend) of
        mochiweb -> brain_mochi:teardown();
        cowboy   -> brain_cowboy:teardown();
        _        -> brain_mochi:teardown()
    end.
