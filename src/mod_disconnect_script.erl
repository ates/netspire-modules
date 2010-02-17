-module(mod_disconnect_script).

-behaviour(gen_module).

%% API
-export([disconnect/1]).

%% gen_module callbacks
-export([start/1, stop/0]).

-include("netspire.hrl").
-include("radius/radius.hrl").

start(_Options) ->
    ?INFO_MSG("Starting dynamic module ~p~n", [?MODULE]),
    netspire_hooks:add(disconnect_client, ?MODULE, disconnect).

stop() ->
    ?INFO_MSG("Stopping dynamic module ~p~n", [?MODULE]),
    netspire_hooks:delete(disconnect_client, ?MODULE, disconnect).

disconnect(Session) ->
    SID = Session#session.id,
    IP = inet_parse:ntoa(Session#session.ip),
    Script = gen_module:get_option(?MODULE, disconnect_script),
    Cmd = Script ++ " " ++ SID ++ " " ++ IP,
    ?INFO_MSG("Disconnecting ~s:~s~n", [SID, IP]),
    case call_external_prog(Cmd) of
        {0, _} ->
            ?INFO_MSG("Client ~s:~s was disconnected~n", [SID, IP]);
        {N, Output} ->
            ?ERROR_MSG("Disconnect failed. Exit code: ~p. Error: ~p~n", [N, Output])
    end.

call_external_prog(Cmd) ->
    call_external_prog(Cmd, "").

call_external_prog(Cmd, Dir) ->
    call_external_prog(Cmd, Dir, []).

call_external_prog(Cmd, Dir, Env) ->
    CD = if Dir =:= "" -> [];
        true -> [{cd, Dir}]
    end,
    SetEnv = if Env =:= [] -> [];
        true -> [{env, Env}]
        end,
    Opt = CD ++ SetEnv ++ [stream, exit_status, use_stdio,
               stderr_to_stdout, in, eof],
    P = open_port({spawn, Cmd}, Opt),
    get_data(P, []).

get_data(P, D) ->
    receive
    {P, {data, D1}} ->
        get_data(P, [D1|D]);
    {P, eof} ->
        port_close(P),
        receive
        {P, {exit_status, N}} ->
            {N, normalize(lists:flatten(lists:reverse(D)))}
        end
    end.

normalize([$\r, $\n | Cs]) ->
    [$\n | normalize(Cs)];
normalize([$\r | Cs]) ->
    [$\n | normalize(Cs)];
normalize([C | Cs]) ->
    [C | normalize(Cs)];
normalize([]) ->
    [].
