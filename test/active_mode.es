#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin
main(_) ->
    zmq:start_link(),

    %% Demonstrate active=true for bind socket.
    push_pull([{active, true}]),

    %% Demonstrate active=parts for bind socket.
    push_pull([{active, parts}]).

push_pull(PullOpts) ->
    %% Setup our sockets.
    {ok, Pull} = zmq:socket(pull, PullOpts),
    zmq:bind(Pull, "inproc://queue"),
    {ok, Push} = zmq:socket(push),
    zmq:connect(Push, "inproc://queue"),

    %% Clear our message queue and send a multi-part message.
    lib:flush_receive(),
    zmq:send(Push, <<"part 1">>, [sndmore]),
    zmq:send(Push, <<"part 2">>, [sndmore]),
    zmq:send(Push, <<"part 3">>),

    %% Wait for the messages to be delivered and print what we have.
    timer:sleep(100),
    io:format("~p: ~p~n", [PullOpts, process_info(self(), messages)]),

    %% Cleanup.
    zmq:close(Push),
    zmq:close(Pull),
    timer:sleep(100).
