-module(mod_disconnect_pod).

-behaviour(gen_module).

%% API
-export([disconnect/5]).

%% gen_module callbacks
-export([start/1, stop/0]).

-include("netspire.hrl").

start(_Options) ->
    ?INFO_MSG("Starting dynamic module ~p~n", [?MODULE]),
    netspire_hooks:add(disconnect_client, ?MODULE, disconnect).

stop() ->
    ?INFO_MSG("Stop dynamic module ~p~n", [?MODULE]),
    netspire_hooks:delete(disconnect_client, ?MODULE, disconnect).

disconnect(_, UserName, SID, IP, NasSpec) ->
    Attrs = [{"User-Name", UserName}, {"Acct-Session-Id", SID}, {"Framed-IP-Address", IP}],
    case radclient:request(disconnect, NasSpec, Attrs) of
        {ok, _} -> {ok, []};
        {error, A} when is_list(A) ->
            case radius:attribute_value("Error-Cause", A) of
                undefined -> {error, A};
                Value ->
                    {error, format_error(Value)}
            end;
        {error, Reason} -> {error, Reason}
    end.

%% As per RFC 3576 (Error-Cause attribute values)
format_error(Code) ->
    case Code of
        201 -> "Residual Session Context Removed";
        202 -> "Invalid EAP Packet (Ignored)";
        401 -> "Unsupported Attribute";
        402 -> "Missing Attribute";
        403 -> "NAS Identification Mismatch";
        404 -> "Invalid Request";
        405 -> "Unsupported Service";
        406 -> "Unsupported Extension";
        501 -> "Administratively Prohibited";
        502 -> "Request Not Routable (Proxy)";
        503 -> "Session Context Not Found";
        504 -> "Session Context Not Removable";
        505 -> "Other Proxy Processing Error";
        506 -> "Resources Unavailable";
        507 -> "Request Initiated";
        _ -> "Unknown error"
    end.
