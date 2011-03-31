%% This is http://code.google.com/p/ip2country-erlang on steroids.
%% Database: http://software77.net/geo-ip/?DL=1
%% Speed: over 200000 lookups in a second.

-module(mod_geoip).

-behaviour(gen_module).
-behaviour(gen_server).

%% API
-export([start_link/1, match/2]).

%% gen_module callbacks
-export([start/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-include("netspire.hrl").

-record(state, {tree}).

start(Options) ->
    ?INFO_MSG("Starting dynamic module ~p~n", [?MODULE]),
    ChildSpec = {?MODULE,
                 {?MODULE, start_link, [Options]},
                 permanent,
                 brutal_kill,
                 worker,
                 [?MODULE]
                },
    supervisor:start_child(netspire_sup, ChildSpec).

stop() ->
    ?INFO_MSG("Stop dynamic module ~p~n", [?MODULE]),
    gen_server:call(?MODULE, stop),
    netspire_hooks:delete_all(?MODULE),
    supervisor:terminate_child(netspire_sup, ?MODULE),
    supervisor:delete_child(netspire_sup, ?MODULE).

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).

match(_, IP) ->
    gen_server:call(?MODULE, {lookup, IP}).

init([Options]) ->
    File = proplists:get_value(file, Options),
    ?INFO_MSG("Loading GeoIP data from ~s~n", [File]),
    case load_csv_file(File) of
        {error, Reason} -> {error, Reason};
        Tree ->
            netspire_hooks:add(geoip_match, ?MODULE, match),
            {ok, #state{tree = Tree}}
    end.

handle_call({lookup, IP}, _From, State) ->
    Reply = lookup(State#state.tree, IP),
    {reply, Reply, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.

handle_info(_Request, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%%
%% Internal functions
%%

load_csv_file(Filename) ->
    Fun = fun(Line) ->
            case string:left(Line, 1) of
                "\n" -> skip;
                "#" -> skip;
                _ ->
                    Params = string:tokens(Line, ",\n\""),
                    try
                        [Min, Max, _, _, Ctry, _, _] = Params,
                        {list_to_integer(Min), list_to_integer(Max), Ctry}
                    catch
                        _:_ ->
                            ?ERROR_MSG("Cannot read GeoIP data due to incorrect file format~n", []),
                            throw(badarg)
                    end
            end
    end,
    for_each_line_in_file(Filename, Fun, []).

lookup(Tree, IP) when is_list(IP) ->
    lookup(Tree, ip:ip2long(IP));
lookup({Min, Max, Country, _Left, _Right}, IP) when IP >= Min, IP =< Max ->
    Country;
lookup({_Min, Max, _Country, _Left, Right}, IP) when IP >= Max ->
    lookup(Right, IP);
lookup({Min, _Max, _Country, Left, _Right}, IP) when IP =< Min ->
    lookup(Left, IP);
lookup(undefined, _) ->
    false.

for_each_line_in_file(Name, Proc, Acc) ->
    case file:open(Name, [read]) of
        {ok, Device} ->
            for_each_line(Device, Proc, Acc);
        {error, Reason} ->
            ?ERROR_MSG("Cannot open file ~s due to ~s~n", [Name, file:format_error(Reason)])
    end.

for_each_line(Device, Proc, Acc) ->
    case io:get_line(Device, "") of
        eof ->
            file:close(Device),
            build_tree(lists:keysort(1, Acc));
        Line ->
            case Proc(Line) of
                skip ->
                    for_each_line(Device, Proc, Acc);
                Value ->
                    Acc1 = [Value|Acc],
                    for_each_line(Device, Proc, Acc1)
            end
    end.

build_tree([]) ->
    undefined;
build_tree(L) ->
    {Left, Right} = lists:split(round(length(L) / 2), L),
    [{Min, Max, Country}|LeftSide] = lists:reverse(Left),
    {Min, Max, Country, build_tree(lists:reverse(LeftSide)), build_tree(Right)}.
