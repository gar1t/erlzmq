-module(zmq_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    zmq:start_link().

stop(_State) ->
    ok.

