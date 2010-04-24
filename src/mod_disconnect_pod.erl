-module(mod_disconnect_pod).

-behaviour(gen_module).

-export([start/1, stop/0, disconnect/5]).

-include("netspire.hrl").

start(_Options) ->
    ?INFO_MSG("Starting dynamic module ~p~n", [?MODULE]),
    netspire_hooks:add(disconnect_client, ?MODULE, disconnect).

stop() ->
    ?INFO_MSG("Stop dynamic module ~p~n", [?MODULE]),
    netspire_hooks:delete(disconnect_client, ?MODULE, disconnect).

disconnect(_, UserName, SID, IP, NasSpec) ->
    Attrs = [{"User-Name", UserName}, {"Acct-Session-Id", SID}, {"Framed-IP-Address", IP}],
    radclient:request(disconnect, NasSpec, Attrs).
