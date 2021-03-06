%%%-------------------------------------------------------------------
%%% File: $Id$
%%%-------------------------------------------------------------------
%%% @doc Erlang bindings for ZeroMQ.
%%%
%%% @author Dhammika Pathirana <dhammika at gmail dot com>
%%% @author Serge Aleynikov <saleyn at gmail dot com>.
%%% @copyright 2010 Dhammika Pathirana and Serge Aleynikov
%%% @version {@version}
%%% @end
%%%-------------------------------------------------------------------
%%% @type zmq_socket().  Opaque 0MQ socket type.
%%% @type zmq_sockopt() = {hwm, integer()}
%%%                     | {swap, integer()}
%%%                     | {affinity, integer()}
%%%                     | {identity, string()}
%%%                     | {subscribe, string()}
%%%                     | {unsubscibe, string()}
%%%                     | {rate, integer()}
%%%                     | {recovery_ivl, integer()}
%%%                     | {mcast_loop, boolean()}
%%%                     | {sndbuf, integer()}
%%%                     | {rcvbuf, integer()}
%%%                     | {rcvmore, boolean()}
%%%                     | {linger, boolean()}
%%%                     | {reconnect_ivl, integer()}
%%%                     | {backlog, integer()}
%%%                     | {recovery_ivl_msec, integer()}
%%%                     | {active, true | false | parts}.     
%%%           0MQ socket options. See 0MQ man pages for details.
%%%           One additional options `active' indicates to the driver
%%%           that incoming messages must be automatically delivered 
%%%           to the process owner's mailbox as {zmq, Socket, Msg} 
%%%           instead of explicitely requiring recv/1 call. If `active'
%%%           is `parts', messages are delivered as a list of binary
%%%           parts, one for each part received.
%%% @end
%%% @type zmq_sendopt() = sndmore.
%%%           Send options. See 0MQ man pages for details.
%%% @end
%%%-------------------------------------------------------------------
-module(zmq).
-author("dhammika@gmail.com").
-author("saleyn@gmail.com").
-id("$Id$").

-behaviour(gen_server).

%% ZMQ API
-export([start/0, start/1, start_link/0, start_link/1,
         socket/1, socket/2, close/1, setsockopt/2, getsockopt/2,
         bind/2, connect/2, send/2, send/3, recv/1,
         socket_int_to_type/1, format_error/1]).

-export([port/0]).

%% gen_server callbacks.
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("zmq.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(state, {port}).

%%%===================================================================
%%% ZMQ API
%%%===================================================================

%%--------------------------------------------------------------------
%% @equiv start(1)
%% @end
%%--------------------------------------------------------------------
start() ->
    start(1).

%%--------------------------------------------------------------------
%% @doc Start the server.
%% @spec (IoThreads) -> {ok, Pid} | {error, Error} | ignore
%% @end
%%--------------------------------------------------------------------
start(IoThreads) when is_integer(IoThreads) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [IoThreads], []).

%%--------------------------------------------------------------------
%% @equiv start_link(1)
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link(1).

%%--------------------------------------------------------------------
%% @doc Start the server as part of a supervision tree.
%% @spec (IoThreads) -> {ok, Pid} | {error, Error} | ignore
%% @end
%%--------------------------------------------------------------------
start_link(IoThreads) when is_integer(IoThreads) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [IoThreads], []).

socket(Type) when is_atom(Type) ->
    socket(Type, []).

%%--------------------------------------------------------------------
%% @doc Create a 0MQ socket.
%% @spec (Type, Options) -> {ok, Socket::zmq_socket()} | {error, Reason}
%%          Type = pair | pub | sub | req | rep | 
%%                 xreq | xrep | upstream | downstream
%%          Options = [Option]
%%          Option  = {active, true | false | parts}
%%                  | {zmq_sockopt(), Value}
%% @end
%%--------------------------------------------------------------------
socket(Type, Options) when is_atom(Type), is_list(Options) ->
    EncType = encode_msg_socket(Type),
    % We are using direct call to the driver to create the socket,
    % because we need the driver to know the socket owner's pid, so
    % that it can deliver messages to its mailbox in the passive mode
    case gen_server:call(?MODULE, port) of
    Port when is_port(Port) ->
        ok;
    Else ->
        Port = undefined,
        throw(Else)
    end,

    {ok, S} = driver(Port, EncType),
    try
        EncOpts = encode_setsockopts(S, Options),
        case driver(Port, EncOpts) of
        ok -> 
            {ok, {Port, S}};
        {error, Why} ->
            throw(Why)
        end
    catch _:Error ->
        driver(Port, encode_close(S)),
        {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc Close a 0MQ socket.
%% @spec (Socket::zmq_socket()) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
close({Port, Socket}) when is_integer(Socket) ->
    Msg = encode_close(Socket),
    driver(Port, Msg);
close(Socket) when is_integer(Socket) ->
    gen_server:call(?MODULE, {close, Socket}).

%%--------------------------------------------------------------------
%% @doc Set socket options.
%% @spec (Socket::zmq_socket(), Options) -> ok | {error, Reason}
%%          Options = [{zmq_sockopt(), Value}]
%% @end
%%--------------------------------------------------------------------
setsockopt(Socket, Opts) when is_integer(Socket), is_list(Opts) ->
    gen_server:call(?MODULE, {setsockopt, Socket, Opts});
setsockopt({Port, S}, Opts) when is_integer(S), is_list(Opts)->
    Msg = encode_setsockopts(S, Opts),
    driver(Port, Msg).

%%--------------------------------------------------------------------
%% @doc Get socket option.
%% @spec (Socket::zmq_socket(), Option) -> {ok, Value} | {error, Reason}
%%          Option = zmq_sockopt()
%% @end
%%--------------------------------------------------------------------
getsockopt(Socket, Option) when is_integer(Socket), is_atom(Option) ->
    Msg = encode_getsockopt(Socket, Option),
    gen_server:call(?MODULE, {getsockopt, Msg});
% Experimantal support of direct port communication
getsockopt({Port, S}, Option) when is_atom(Option)->
    Msg = encode_getsockopt(S, Option),
    driver(Port, Msg).

%%--------------------------------------------------------------------
%% @doc Bind a 0MQ socket to address.
%% @spec (Socket::zmq_socket(), Address) -> ok | {error, Reason}
%%          Address = string() | binary()
%% @end
%%--------------------------------------------------------------------
bind(Socket, Address) when is_integer(Socket), is_list(Address) ->
    bind(Socket, list_to_binary(Address));
bind(Socket, Address) when is_integer(Socket), is_binary(Address) ->
    gen_server:call(?MODULE, {bind, Socket, Address});
bind({Port, S}, Address) when is_integer(S), is_list(Address) ->
    bind({Port, S}, list_to_binary(Address));
bind({Port, S}, Address) when is_integer(S), is_binary(Address) ->
    Msg = encode_bind(S, Address),
    driver(Port, Msg).

%%--------------------------------------------------------------------
%% @doc Connect a 0MQ socket to address.
%% @spec (Socket::zmq_socket(), Address) -> ok | {error, Reason}
%%          Address = string() | binary()
%% @end
%%--------------------------------------------------------------------
connect(Socket, Address) when is_integer(Socket), is_list(Address) ->
    connect(Socket, list_to_binary(Address));
connect(Socket, Address) when is_integer(Socket), is_binary(Address) ->
    gen_server:call(?MODULE, {connect, Socket, Address});
% Experimantal support of direct port communication
connect({Port, S}, Address) when is_list(Address)->
    connect({Port, S}, list_to_binary(Address));
connect({Port, S}, Address) when is_binary(Address) ->
    Msg = encode_connect(S, Address),
    driver(Port, Msg).

%%--------------------------------------------------------------------
%% @equiv send(S, Msg, [])
%% @end
%%--------------------------------------------------------------------
send(Socket, Data) ->
    send(Socket, Data, []).

%%--------------------------------------------------------------------
%% @doc Send a message to a given 0MQ socket.
%% @spec (Socket::zmq_socket(), Msg::binary(), Flags) -> ok | {error, Reason}
%%          Flags = [zmq_sendopt()]
%% @end
%%--------------------------------------------------------------------
send(Socket, Data, Flags)
        when is_integer(Socket), is_binary(Data), is_list(Flags) ->
    gen_server:call(?MODULE, {send, Socket, Data, Flags});
% Experimantal support of direct port communication
send({Port, S}, Data, Flags) ->
    Msg = encode_msg_send(S, Data, Flags),
    driver(Port, Msg).

%%--------------------------------------------------------------------
%% @doc Receive a message from a given 0MQ socket.
%% @spec (Socket::zmq_socket()) -> {ok, binary()} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
recv(Socket) when is_integer(Socket) ->
    gen_server:call(?MODULE, {recv, Socket});
% Experimantal support of direct port communication
recv({Port, S}) ->
    Msg = encode_msg_recv(S),
    driver(Port, Msg).

%% Experimental functions for direct communications with port 
%% bypassing serialization through ?MODULE server.

port() ->
    gen_server:call(?MODULE, port).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Handle start.
%%
%% @spec init(Args) ->
%%          {ok, State} |
%%          {ok, State, Timeout} |
%%          {ok, State, hibernate} |
%%          {stop, Reason} |
%%          ignore
%% @end
%%--------------------------------------------------------------------
init([IoThreads]) ->
    process_flag(trap_exit, true),
    DirName = re:replace(filename:dirname(code:which(?MODULE)), 
                         "/?[^/]+/\\.\\.", "", [{return,list}]),
    SearchDir = filename:join(filename:dirname(DirName), "priv"),
    ?log("init, lib path: ~s", [SearchDir]),
    try erl_ddll:load(SearchDir, ?DRIVER_NAME) of
    ok ->
        Port = open_port({spawn_driver, ?DRIVER_NAME}, [binary]),
        init_context(Port, IoThreads),
        {ok, #state{port=Port}};
    {error, Reason} ->
        throw(erl_ddll:format_error(Reason))
    catch _:Error ->
        {stop, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle synchronous call.
%%
%% @spec handle_call(Request, From, State) ->
%%          {reply, Reply, NewState} |
%%          {reply, Reply, NewState, Timeout} |
%%          {reply, Reply, NewState, hibernate} |
%%          {noreply, NewState} |
%%          {noreply, NewState, Timeout} |
%%          {noreply, NewState, hibernate} |
%%          {stop, Reason, Reply, NewState} |
%%          {stop, Reason, NewState}
%% @end
%%-------------------------------------------------------------------

% No need to support context termination - context allocation
% is handled on port creation, and the resource will be 
% automatically reclaimed upon death of the port driver.  
%handle_call({term}, _From, State) ->
%    ?log("~p", [term]),
%    Message = <<(?ZMQ_TERM):8>>,
%    Reply = driver(State#state.port, Message),
%    {reply, Reply, State};

handle_call({close, Socket}, _From, State) ->
    ?log("~p", [close]),
    do_call(State, encode_close(Socket));

handle_call({setsockopt, Socket, Options}, _From, State) ->
    ?log("~p", [socketopt]),
    do_call(State, encode_setsockopts(Socket, Options));

handle_call({getsockopt, EncodedOpt}, _From, State) ->
    do_call(State, EncodedOpt);

handle_call({bind, Socket, Address}, _From, State) ->
    ?log("~p addr:~s", [bind, binary_to_list(Address)]),
    do_call(State, encode_bind(Socket, Address));

handle_call({connect, Socket, Address}, _From, State) ->
    ?log("~p addr:~s", [connect, binary_to_list(Address)]),
    do_call(State, encode_connect(Socket, Address));

handle_call({send, Socket, Data, Flags}, _From, State) ->
    ?log("~p", [send]),
    do_call(State, encode_msg_send(Socket, Data, Flags));

handle_call({recv, Socket}, _From, State) ->
    ?log("~p", [recv]),
    do_call(State, encode_msg_recv(Socket));

handle_call(port, _From, #state{port = Port} = State) ->
    {reply, Port, State};

handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle asynchronous call.
%%
%% @spec handle_cast(Msg, State) ->
%%          {noreply, NewState} |
%%          {noreply, NewState, Timeout} |
%%          {noreply, NewState, hibernate} |
%%          {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle message.
%%
%% @spec handle_info(Info, State) ->
%%          {noreply, NewState} |
%%          {noreply, NewState, Timeout} |
%%          {noreply, NewState, hibernate} |
%%          {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    ?log("unhandled message: ~p\n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle termination/shutdown.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    port_close(State#state.port),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle code change.
%%
%% @spec code_change(OldVsn, State, Extra) -> 
%%          {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Format error atom returned by the driver.
%% @spec (Code::atom()) -> string()
%% @end
%%--------------------------------------------------------------------
format_error(enotsup)           -> "Not supported";
format_error(eprotonosupport)   -> "Protocol not supported";
format_error(enobufs)           -> "No buffer space available";
format_error(enetdown)          -> "Network is down";
format_error(eaddrinuse)        -> "Address in use";
format_error(eaddrnotavail)     -> "Address not available";
format_error(efsm)              -> "Operation cannot be accomplished in current state";
format_error(enocompatproto)    -> "The protocol is not compatible with the socket type";
format_error(E) when is_atom(E) -> inet:format_error(E);
format_error(E) when is_list(E) -> E;
format_error(E) when is_tuple(E)-> io_lib:format("~p", [E]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_context(Port, IoThreads) ->
    ?log("~p, io threads:~B", [init, IoThreads]),
    Message = <<(?ZMQ_INIT):8, IoThreads:32>>,
    case driver(Port, Message) of
        ok              -> ok;
        {error, Error}  -> throw(format_error(Error))
    end.

encode_msg_socket(Type) ->
    <<(?ZMQ_SOCKET):8, (socket_type_to_int(Type)):8>>.

socket_type_to_int(pair)            -> ?ZMQ_PAIR;
socket_type_to_int(pub)             -> ?ZMQ_PUB;
socket_type_to_int(sub)             -> ?ZMQ_SUB;
socket_type_to_int(push)            -> ?ZMQ_PUSH;
socket_type_to_int(pull)            -> ?ZMQ_PULL;
socket_type_to_int(req)             -> ?ZMQ_REQ;
socket_type_to_int(rep)             -> ?ZMQ_REP;
socket_type_to_int(xreq)            -> ?ZMQ_XREQ;
socket_type_to_int(xrep)            -> ?ZMQ_XREP;
socket_type_to_int(upstream)        -> ?ZMQ_UPSTREAM;
socket_type_to_int(downstream)      -> ?ZMQ_DOWNSTREAM.

socket_int_to_type(?ZMQ_PAIR)       -> pair;
socket_int_to_type(?ZMQ_PUB)        -> pub;
socket_int_to_type(?ZMQ_SUB)        -> sub;
socket_int_to_type(?ZMQ_PUSH)       -> push;
socket_int_to_type(?ZMQ_PULL)       -> pull;
socket_int_to_type(?ZMQ_REQ)        -> req;
socket_int_to_type(?ZMQ_REP)        -> rep;
socket_int_to_type(?ZMQ_XREQ)       -> xreq;
socket_int_to_type(?ZMQ_XREP)       -> xrep.

encode_close(Socket) ->
    <<(?ZMQ_CLOSE):8, Socket:32>>.

encode_setsockopts(Socket, Options) when length(Options) =< 255 ->
    Opts = [encode_setsockopt(V) || V <- Options],
    <<(?ZMQ_SETSOCKOPT):8, Socket:32, (length(Opts)):8, (list_to_binary(Opts))/binary>>.

encode_setsockopt({hwm, V}) when is_integer(V) ->
    <<?ZMQ_HWM, 8, V:64/native>>;
encode_setsockopt({swap, V}) when is_integer(V) ->
    <<?ZMQ_SWAP, 8, V:64/native>>;
encode_setsockopt({affinity, V}) when is_integer(V) ->
    <<?ZMQ_AFFINITY, 8, V:64/native>>;
encode_setsockopt({identity, V}) when is_list(V), length(V) =< 255 ->
    Bin = list_to_binary(V),
    <<?ZMQ_IDENTITY, (byte_size(Bin)):8, Bin/binary>>;
encode_setsockopt({identity, V}) when is_binary(V), byte_size(V) =< 255 ->
    <<?ZMQ_IDENTITY, (byte_size(V)):8, V/binary>>;
% Note that 0MQ doesn't limit the size of subscribe/unsubscribe options,
% but we do for simplicity.
encode_setsockopt({subscribe, V}) when is_list(V), length(V) =< 255 ->
    Bin = list_to_binary(V),
    <<?ZMQ_SUBSCRIBE, (byte_size(Bin)):8, Bin/binary>>;
encode_setsockopt({subscribe, V}) when is_binary(V), byte_size(V) =< 255 ->
    <<?ZMQ_SUBSCRIBE, (byte_size(V)):8, V/binary>>;
encode_setsockopt({unsubscribe, V}) when is_list(V), length(V) =< 255 ->
    Bin = list_to_binary(V),
    <<?ZMQ_UNSUBSCRIBE, (byte_size(Bin)):8, Bin/binary>>;
encode_setsockopt({unsubscribe, V}) when is_binary(V), byte_size(V) =< 255 ->
    <<?ZMQ_UNSUBSCRIBE, (byte_size(V)):8, V/binary>>;
encode_setsockopt({rate, V}) when is_integer(V) ->
    <<?ZMQ_RATE, 8, V:64/native>>;
encode_setsockopt({recovery_ivl,  V}) when is_integer(V) ->
    <<?ZMQ_RECOVERY_IVL, 8, V:64/native>>;
encode_setsockopt({recovery_ivl_msec,V}) when is_integer(V) ->
    <<?ZMQ_RECOVERY_IVL_MSEC, 8, V:64/native>>;
encode_setsockopt({mcast_loop, V}) when is_boolean(V) ->
    <<?ZMQ_MCAST_LOOP, 8, (bool_to_int(V)):64/native>>;
encode_setsockopt({sndbuf, V}) when is_integer(V) ->
    <<?ZMQ_SNDBUF, 8, V:64/native>>;
encode_setsockopt({rcvbuf, V}) when is_integer(V) ->
    <<?ZMQ_RCVBUF, 8, V:64/native>>;
%encode_setsockopt({type, V}) when is_atom(V) ->
%    <<?ZMQ_TYPE, 1, socket_type_to_int(V)>>;
encode_setsockopt({linger, V}) when is_boolean(V) ->
    <<?ZMQ_LINGER, 4, (bool_to_int(V)):32/native>>;
encode_setsockopt({reconnect_ivl, V}) when is_integer(V) ->
    <<?ZMQ_RECONNECT_IVL, 4, V:32/native>>;
encode_setsockopt({backlog, V}) when is_integer(V) ->
    <<?ZMQ_BACKLOG, 4, V:32/native>>;
% Driver's internal socket options
encode_setsockopt({active, V}) ->
    <<?ZMQ_ACTIVE, 1, (active_to_int(V))>>.

bool_to_int(true)  -> 1;
bool_to_int(false) -> 0.

active_to_int(true) -> 1;
active_to_int(false) -> 0; 
active_to_int(parts) -> 2. 

sockopt_to_int(hwm)                 -> ?ZMQ_HWM;
sockopt_to_int(swap)                -> ?ZMQ_SWAP;
sockopt_to_int(affinity)            -> ?ZMQ_AFFINITY;
sockopt_to_int(identity)            -> ?ZMQ_IDENTITY;
sockopt_to_int(subscribe)           -> ?ZMQ_SUBSCRIBE;
sockopt_to_int(unsubscribe)         -> ?ZMQ_UNSUBSCRIBE;
sockopt_to_int(rate)                -> ?ZMQ_RATE;
sockopt_to_int(recovery_ivl)        -> ?ZMQ_RECOVERY_IVL;
sockopt_to_int(mcast_loop)          -> ?ZMQ_MCAST_LOOP;
sockopt_to_int(sndbuf)              -> ?ZMQ_SNDBUF;
sockopt_to_int(rcvbuf)              -> ?ZMQ_RCVBUF;
sockopt_to_int(rcvmore)             -> ?ZMQ_RCVMORE;
sockopt_to_int(linger)              -> ?ZMQ_LINGER;
sockopt_to_int(reconnect_ivl)       -> ?ZMQ_RECONNECT_IVL;
sockopt_to_int(backlog)             -> ?ZMQ_BACKLOG;
sockopt_to_int(recovery_ivl_msec)   -> ?ZMQ_RECOVERY_IVL_MSEC;
sockopt_to_int(active)              -> ?ZMQ_ACTIVE.

encode_getsockopt(Socket, Option) ->
    O = sockopt_to_int(Option),
    <<(?ZMQ_GETSOCKOPT):8, Socket:32, O:32>>.

encode_bind(Socket, Address) ->
    <<(?ZMQ_BIND):8, Socket:32, Address/binary>>.
encode_connect(Socket, Address) ->
    <<(?ZMQ_CONNECT):8, Socket:32, Address/binary>>.

send_flags_to_int([]) -> 0;
send_flags_to_int([H|T]) ->
    send_flags_to_int(T) bor
    case H of
        sndmore -> ?ZMQ_SNDMORE;
        _ -> throw({unknown_send_option, 0})
    end.

encode_msg_send(Socket, Data, Flags) ->
    F = send_flags_to_int(Flags),
    <<(?ZMQ_SEND):8, Socket:32, F:32, Data/binary>>.
encode_msg_recv(Socket) ->
    <<(?ZMQ_RECV):8, Socket:32>>.

do_call(#state{} = State, Message) ->
    Reply = driver(State#state.port, Message),
    {reply, Reply, State}.

driver(Port, Message) ->
    ?log("port command ~p", [Message]),
    port_command(Port, Message),
    receive
        zok ->
            ok;
        {zok, Term} ->
            {ok, Term};
        Err = {error, _} ->
            Err
    end.

%%%----------------------------------------------------------------------------
%%% Test Cases
%%%----------------------------------------------------------------------------

-ifdef(EUNIT).

setsockopts_test() ->
    zmq:start(),
    {ok, S} = zmq:socket(xrep, []),
    ?assertMatch(ok,          zmq:setsockopt(S, [{hwm, 100}])),
    ?assertMatch({ok, 100},   zmq:getsockopt(S, hwm)),
    ?assertMatch(ok,          zmq:setsockopt(S, [{swap, 10}])),
    ?assertMatch({ok, 10},    zmq:getsockopt(S, swap)),
    ?assertMatch(ok,          zmq:setsockopt(S, [{affinity, 1}])),
    ?assertMatch({ok, 1},     zmq:getsockopt(S, affinity)),
    ?assertMatch(ok,          zmq:setsockopt(S, [{identity, <<"a">>}])),
    ?assertMatch({ok,<<"a">>},zmq:getsockopt(S, identity)),
    ?assertMatch(ok,          zmq:setsockopt(S, [{rate, 10000}])),
    ?assertMatch({ok, 10000}, zmq:getsockopt(S, rate)),
    ?assertMatch(ok,          zmq:setsockopt(S, [{recovery_ivl, 1234}])),
    ?assertMatch({ok, 1234},  zmq:getsockopt(S, recovery_ivl)),
    ?assertMatch(ok,          zmq:setsockopt(S, [{mcast_loop, true}])),
    ?assertMatch({ok, true},  zmq:getsockopt(S, mcast_loop)),
    ?assertMatch(ok,          zmq:setsockopt(S, [{sndbuf, 12300}])),
    ?assertMatch({ok, 12300}, zmq:getsockopt(S, sndbuf)),
    ?assertMatch(ok,          zmq:setsockopt(S, [{rcvbuf, 22300}])),
    ?assertMatch({ok, 22300}, zmq:getsockopt(S, rcvbuf)),
    ?assertMatch(ok,          zmq:setsockopt(S, [{linger, true}])),
    ?assertMatch({ok, true},  zmq:getsockopt(S, linger)),
    ?assertMatch({ok, false}, zmq:getsockopt(S, rcvmore)),
    ?assertMatch(ok,          zmq:setsockopt(S, [{reconnect_ivl, 3000}])),
    ?assertMatch({ok, 3000},  zmq:getsockopt(S, reconnect_ivl)),
    ?assertMatch(ok,          zmq:setsockopt(S, [{recovery_ivl_msec, 5000}])),
    ?assertMatch({ok, 5000},  zmq:getsockopt(S, recovery_ivl_msec)),
    ?assertMatch(ok,          zmq:setsockopt(S, [{active, false}])),
    ?assertMatch({ok, false}, zmq:getsockopt(S, active)),
    ok.

pushpull_test() ->
    zmq:start(), 
    {ok, Push} = zmq:socket(push, [{active, false}]),
    {ok, Pull} = zmq:socket(pull, [{active, false}]),
    ?assertMatch(ok, zmq:bind(Pull,"tcp://127.0.0.1:2000")),
    ?assertMatch(ok, zmq:connect(Push, "tcp://127.0.0.1:2000")),
    ?assertMatch(ok, zmq:send(Push, <<"Hello World!">>)),
    ?assertMatch({ok, <<"Hello World!">>}, zmq:recv(Pull)).

-endif.
