-module(zmq_reqclient2).

-behaviour(gen_server).

-export([start_link/1, req/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {sock}).

start_link(Addr) ->
    gen_server:start_link(?MODULE, [Addr], []).

req(Client, Msg, Timeout) ->
    gen_server:call(Client, {req, Msg, Timeout}).

init([Addr]) ->
    {ok, S} = zmq:socket(req),
    ok = zmq:connect(S, Addr),
    {ok, #state{sock=S}}.

handle_call({req, Msg, Timeout}, _From, #state{sock=S}=State) ->
    zmq:send(S, Msg),
    receive
        {zmq, _, Rep} -> {reply, Rep, State}
    after
        Timeout -> erlang:exit(timeout)
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
