-module(mod_disconnect_pod).

-behaviour(gen_module).

-export([start/1, stop/0, disconnect/1]).

-include("netspire.hrl").
-include("radius/radius.hrl").

-define(POD_DEFAULT_PORT, 3799).
-define(DISCONNECT_REQUEST, 40).
-define(DISCONNECT_ACK, 41).
-define(DISCONNECT_NAK, 42).

start(_Options) ->
    ?INFO_MSG("Starting dynamic module ~p~n", [?MODULE]),
    netspire_hooks:add(disconnect_client, ?MODULE, disconnect).

stop() ->
    ?INFO_MSG("Stop dynamic module ~p~n", [?MODULE]),
    netspire_hooks:delete(disconnect_client, ?MODULE, disconnect).

generate_packet(SID, IP, Secret) ->
    Ident = 0,
    Code = <<?DISCONNECT_REQUEST:8>>,
    Attrs = radius:encode_attributes([{"Acct-Session-Id", SID}, {"Framed-IP-Address", IP}]),
    Length = <<(20 + byte_size(Attrs)):16>>,
    % Auth: Code + Ident + Length + 16 zero octets + Attrs + Secret
    Auth = erlang:md5([Code, Ident, Length, <<0:128>>, Attrs, Secret]),
    [Code, Ident, Length, Auth, Attrs].

send_disconnect_packet(Packet, NasIP) ->
    case gen_udp:open(0, [binary]) of
        {ok, Socket} ->
            gen_udp:send(Socket, inet_parse:ntoa(NasIP), ?POD_DEFAULT_PORT, Packet),
            waiting_for_acknowledge(),
            gen_udp:close(Socket);
        {error, Reason} ->
            ?ERROR_MSG("Cannot send Disconnect Message due to ~p~n", [Reason])
    end.

waiting_for_acknowledge() ->
    receive
        {udp, _Socket, _IP, _InPortNo, Bin} ->
            {ok, Packet} = radius:decode_packet(Bin),
            case Packet#radius_packet.code of
                ?DISCONNECT_ACK ->
                    ?INFO_MSG("Received Disconnect-ACK from NAS~n", []);
                ?DISCONNECT_NAK ->
                    ?ERROR_MSG("Received Disconnect-NAK from NAS~n", [])
            end
    after 5000 ->
            ?ERROR_MSG("Not received reply from NAS~n", [])
    end.

disconnect(Session) ->
    IP = Session#session.ip,
    SID = Session#session.id,
    {nas_spec, NasIP, _, NasSecret, _} = Session#session.nas_spec,
    Packet = generate_packet(SID, IP, NasSecret),
    ?INFO_MSG("Disconnecting ~s:~s~n", [SID, inet_parse:ntoa(IP)]),
    send_disconnect_packet(Packet, NasIP).

