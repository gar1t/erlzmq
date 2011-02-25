#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(["crash"]) ->
    zmq:start_link(),
    {ok, C} = zmq_reqclient2:start_link("tcp://127.0.0.1:5555"),
    erlang:exit(C, stop),
    timer:sleep(100);

main([Addr]) ->
    zmq:start_link(),
    {ok, C} = zmq_reqclient2:start_link("tcp://" ++ Addr),
    Rep = zmq_reqclient2:req(C, term_to_binary({msg, "hello"}), 1000),
    io:format("~p~n", [Rep]),
    timer:sleep(100);

main(_) ->
    io:format("usage: reqclient2.es <HOST>:<PORT> | crash~n").
