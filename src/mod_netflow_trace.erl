-module(mod_netflow_trace).
 
-behaviour(gen_module).
-behaviour(gen_server).
 
%% API
-export([start_link/1, handle_packet/2]).
 
%% gen_module callbacks
-export([start/1, stop/0]).
 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
 
-include("netspire.hrl").
-include("netflow/netflow_v5.hrl").
 
-record(state, {fd, trace_dir, timestamp}).
 
start(Options) ->
    TraceDir = proplists:get_value(trace_dir, Options, []),
    ?INFO_MSG("Starting dynamic module ~p~n", [?MODULE]),
    ChildSpec = {?MODULE,
                 {?MODULE, start_link, [TraceDir]},
                 temporary,
                 1000,
                 worker,
                 [?MODULE]},
    supervisor:start_child(netspire_sup, ChildSpec).
 
stop() ->
    ?INFO_MSG("Stopping dynamic module ~p~n", [?MODULE]),
    gen_server:call(?MODULE, stop),
    supervisor:terminate_child(netspire_sup, ?MODULE),
    supervisor:delete_child(netspire_sup, ?MODULE).
 
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).
 
handle_packet(SrcIP, Pdu) ->
    gen_server:cast(?MODULE, {netflow, Pdu, SrcIP}).
 
timestamp() ->
    {{Year, Month, Day}, _} = erlang:localtime(),
    TimeStamp = io_lib:format("~4..0w-~2..0w-~2..0w", [Year, Month, Day]),
    lists:flatten(TimeStamp).
 
open_trace_file(TraceDir) ->
    TimeStamp = timestamp(),
    Filename = TraceDir ++ "/netflow-" ++ TimeStamp ++ ".log",
    case file:open(Filename, [append, raw]) of
        {ok, Fd} ->
            #state{fd = Fd, trace_dir = TraceDir, timestamp = TimeStamp};
        Error ->
            ?ERROR_MSG("Can't open log file: ~p~n", [Error])
    end.
 
init([TraceDir]) ->
    netspire_netflow:add_packet_handler(?MODULE, []),
    {ok, _} = timer:send_interval(1000, self(), check_if_need_rotation),
    State = open_trace_file(TraceDir),
    {ok, State}.
 
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
 
handle_cast({netflow, Pdu, _SrcIP}, State) ->
    {Header, Records} = Pdu,
    Time = Header#nfh_v5.unix_secs,
    F = fun(R) ->
            SrcIP = netspire_util:int_to_ip4(R#nfrec_v5.src_addr),
            DstIP = netspire_util:int_to_ip4(R#nfrec_v5.dst_addr),
            SrcPort = R#nfrec_v5.src_port,
            DstPort = R#nfrec_v5.dst_port,
            Octets = R#nfrec_v5.d_octets,
            file:write(State#state.fd, io_lib:format("~p ~s ~s ~p ~p ~p~n",
                    [Time, inet_parse:ntoa(SrcIP),
                        inet_parse:ntoa(DstIP), SrcPort, DstPort, Octets]))
    end,
    lists:foreach(F, Records),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(check_if_need_rotation, State) ->
    TimeStamp = timestamp(),
    if
        State#state.timestamp =/= TimeStamp ->
            file:close(State#state.fd),
            State1 = open_trace_file(State#state.trace_dir),
            ?INFO_MSG("Netflow trace was rotated~n", []),
            {noreply, State1};
        true ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, State) ->
    file:close(State#state.fd),
    netspire_netflow:delete_packet_handler(?MODULE).
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
