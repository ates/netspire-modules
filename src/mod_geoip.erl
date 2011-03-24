-module(mod_geoip).

-behaviour(gen_module).

-export([start/1, stop/0, match/2]).

-include("netspire.hrl").

-record(geoip_entry, {from, to, code}).

start(Options) ->
    ?INFO_MSG("Starting dynamic module ~p~n", [?MODULE]),
    File = proplists:get_value(file, Options),
    case file:open(File, [read]) of
        {ok, Fd} ->
            ets:new(geoip, [named_table, public, {keypos, 2}]),
            ?INFO_MSG("Loading GeoIP database from ~s~n", [File]),
            {T, V} = timer:tc(fun() -> load_csv(Fd, 0) end, []),
            ?INFO_MSG("Loaded ~p GeoIP entries for ~p seconds~n", [V, T / 1000000]),
            netspire_hooks:add(geoip_match, ?MODULE, match);
        {error, Reason} ->
            ?ERROR_MSG("Cannot open ~s due to ~s~n", [File, file:format_error(Reason)])
    end.

stop() ->
    ?INFO_MSG("Stop dynamic module ~p~n", [?MODULE]),
    ets:delete(geoip),
    netspire_hooks:delete(geoip_match, ?MODULE, geoip_match).

load_csv(Fd, N) ->
    case io:get_line(Fd, "") of
        eof ->
            file:close(Fd), N;
        Line ->
            [[_, _, N1, N2, Code, _]] = csv:read(string:strip(Line, both, $\n)),
            R = #geoip_entry{from = list_to_integer(N1), to = list_to_integer(N2), code = Code},
            ets:insert(geoip, R),
            load_csv(Fd, N + 1)
    end.

match(_, IP) ->
    N = ip:ip2long(IP),
    MatchHead = #geoip_entry{from = '$1', to = '$2', code = '$3'},
    MatchSpec = [{MatchHead, [{'>=', N, '$1'}, {'=<', N, '$2'}], ['$3']}],
    case ets:select(geoip, MatchSpec, 1) of
        {[Code], _} -> Code;
        _ -> undefined
    end.
