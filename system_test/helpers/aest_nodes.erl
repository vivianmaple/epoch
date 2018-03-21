-module(aest_nodes).

-behaviour(gen_server).

%=== EXPORTS ===================================================================

%% Common Test API exports
-export([ct_setup/1]).
-export([ct_cleanup/1]).

%% QuickCheck API exports
-export([eqc_setup/2]).
-export([eqc_cleanup/1]).

%% Generic API exports
-export([setup_nodes/2]).
-export([start_node/2]).
-export([stop_node/2]).
-export([request/3]).
-export([request/4]).
-export([wait_for_height/4]).
-export([assert_synchronized/2]).

%% Behaviour gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

%=== MACROS ====================================================================

-define(BACKENDS, [aest_docker]).
-define(CALL_TAG, ?MODULE).
-define(CT_CONF_KEY, node_manager).
-define(CALL_TIMEOUT, 60000).
-define(NODE_STOP_TIMEOUT, 30).
-define(NODE_TEARDOWN_TIMEOUT, 0).
-define(DEFAULT_HTTP_TIMEOUT, 3000).

%=== COMMON TEST API FUNCTIONS =================================================

ct_setup(Config) ->
    {data_dir, DataDir} = proplists:lookup(data_dir, Config),
    {priv_dir, PrivDir} = proplists:lookup(priv_dir, Config),
    ct:log("Node logs can be found here: ~n<a href=\"file://~s\">~s</a>",
        [PrivDir, PrivDir]
    ),
    LogFun = fun(Fmt, Args) -> ct:log(Fmt, Args) end,
    case start(DataDir, PrivDir, LogFun) of
        {ok, Pid} -> [{?CT_CONF_KEY, Pid} | Config];
        {error, Reason} ->
            erlang:error({system_test_setup_failed, [{reason, Reason}]})
    end.

ct_cleanup(Config) ->
    Pid = ctx2pid(Config),
    call(Pid, cleanup),
    call(Pid, stop),
    wait_for_exit(Pid, 120000).

%=== QICKCHECK API FUNCTIONS ===================================================

eqc_setup(DataDir, TempDir) ->
    case start(DataDir, TempDir, undefined) of
        {ok, Pid} -> Pid;
        {error, Reason} ->
            erlang:error({system_test_setup_failed, [{reason, Reason}]})
    end.

eqc_cleanup(Pid) ->
    call(Pid, cleanup),
    call(Pid, stop),
    wait_for_exit(Pid, 120000).

%=== GENERIC API FUNCTIONS =====================================================

setup_nodes(NodeSpecs, Ctx) ->
    call(ctx2pid(Ctx), {setup_nodes, NodeSpecs}).

start_node(NodeName, Ctx) ->
    call(ctx2pid(Ctx), {start_node, NodeName}).

stop_node(NodeName, Ctx) ->
    call(ctx2pid(Ctx), {stop_node, NodeName}).

request(NodeName, Path, Ctx) ->
    request(NodeName, Path, #{}, Ctx).

request(NodeName, Path, Query, Ctx) ->
    Addr = call(ctx2pid(Ctx), {get_service_address, NodeName, ext_http}),
    case http_get(Addr, Path, Query) of
        {error, Reason} -> throw(Reason);
        {ok, 200, Response} -> Response
    end.

wait_for_height(MinHeight, NodeNames, Timeout, Ctx) ->
    Pid = ctx2pid(Ctx),
    Addrs = [call(Pid, {get_service_address, N, ext_http}) || N <- NodeNames],
    StartTime = os:timestamp(),
    wait_for_height(MinHeight, Addrs, [], 500, StartTime, Timeout).

wait_for_height(_Height, [], [], _Delay, _StartTime, _Timeout) -> ok;
wait_for_height(Height, [], Rem, Delay, StartTime, Timeout) ->
    timer:sleep(Delay),
    wait_for_height(Height, lists:reverse(Rem), [], Delay, StartTime, Timeout);
wait_for_height(Height, [Addr | Addrs], Rem, Delay, StartTime, Timeout) ->
    case http_get(Addr, [v2, 'block-by-height'], #{height => Height}) of
        {ok, 200, _} ->
            wait_for_height(Height, Addrs, Rem, Delay, StartTime, Timeout);
        _ ->
            Now = os:timestamp(),
            Delta = timer:now_diff(Now, StartTime),
            case Delta > (Timeout * 1000) of
                true -> throw(timeout);
                false ->
                    wait_for_height(Height, Addrs, [Addr | Rem],
                                    Delay, StartTime, Timeout)
            end
    end.

assert_synchronized(_NodeNames, _Ctx) ->
    ok.

%=== BEHAVIOUR GEN_SERVER CALLBACK FUNCTIONS ===================================

init([DataDir, TempDir, LogFun]) ->
    process_flag(trap_exit, true), % Make sure terminate always cleans up
    mgr_setup(DataDir, TempDir, LogFun).

handle_call(Request, From, State) ->
    try
        handlex(Request, From, State)
    catch
        throw:Reason ->
            {reply, {'$error', Reason, erlang:get_stacktrace()}, State}
    end.

handlex({get_service_address, NodeName, Service}, _From, State) ->
    {reply, mgr_get_service_address(NodeName, Service, State), State};
handlex({setup_nodes, NodeSpecs}, _From, State) ->
    {reply, ok, mgr_setup_nodes(NodeSpecs, State)};
handlex({start_node, NodeName}, _From, State) ->
    {reply, ok, mgr_start_node(NodeName, State)};
handlex({stop_node, NodeName}, _From, State) ->
    {reply, ok, mgr_stop_node(NodeName, State)};
handlex(cleanup, _From, State) ->
    {reply, ok, mgr_cleanup(State)};
handlex(stop, _From, State) ->
    {stop, normal, ok, State};
handlex(Request, From, _State) ->
    error({unknown_request, Request, From}).

handle_info(_Msg, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    mgr_cleanup(State),
    ok.

%=== INTERNAL FUNCTIONS ========================================================

log(#{log_fun := undefined}, _Fmt, _Args) -> ok;
log(#{log_fun := LogFun}, Fmt, Args) -> LogFun(Fmt, Args).

uid() ->
    iolist_to_binary([[io_lib:format("~2.16.0B",[X])
                       || <<X:8>> <= crypto:strong_rand_bytes(8) ]]).

ctx2pid(Pid) when is_pid(Pid) -> Pid;
ctx2pid(Props) when is_list(Props) ->
    case proplists:lookup(?CT_CONF_KEY, Props) of
        {?CT_CONF_KEY, Pid} when is_pid(Pid) -> Pid;
        _ ->
            erlang:error({system_test_not_setup, []})
    end.

call(Pid, Msg) ->
    case gen_server:call(Pid, Msg, ?CALL_TIMEOUT) of
        {'$error', Reason, Stacktrace} ->
            erlang:raise(throw, Reason, Stacktrace);
        Reply ->
            Reply
    end.

start(DataDir, TempDir, LogFun) ->
    {ok, _} = application:ensure_all_started(hackney),
    gen_server:start_link(?MODULE, [DataDir, TempDir, LogFun], []).

wait_for_exit(Pid, Timeout) ->
    Ref = erlang:monitor(process, Pid),
    receive {'DOWN', Ref, process, Pid, _Reason} -> ok
    after Timeout -> error({process_not_stopped, Pid})
    end.

http_get(Addr, Path, Query) ->
    http_get(Addr, Path, Query, #{}).

http_get(Addr, Path, Query, Opts) ->
    Timeout = maps:get(timeout, Opts, ?DEFAULT_HTTP_TIMEOUT),
    HttpOpts = [{recv_timeout, Timeout}],
    case hackney:request(get, url(Addr, Path, Query), [], <<>>, HttpOpts) of
        {error, _Reason} = Error -> Error;
        {ok, Status, _RespHeaders, ClientRef} ->
            {ok, Status, hackney_json_body(ClientRef)}
    end.

url(Base, Path, QS) when is_list(Path) ->
    hackney_url:make_url(Base, [to_binary(P) || P <- Path], maps:to_list(QS));
url(Base, Item, QS) ->
    url(Base, [Item], QS).

to_binary(Term) when is_atom(Term) -> atom_to_binary(Term, utf8);
to_binary(Term)                    -> Term.

hackney_json_body(ClientRef) ->
    case hackney:body(ClientRef) of
        {error, _Reason} = Error -> Error;
        {ok, BodyJson} ->
            jsx:decode(BodyJson, [{labels, attempt_atom}, return_maps])
    end.

%--- NODE MANAGER PROCESS FUNCTION ---------------------------------------------

mgr_setup(DataDir, TempDir, LogFun) ->
    TestId = uid(),
    BackendOpts = #{
        test_id => TestId,
        log_fun => LogFun,
        data_dir => DataDir,
        temp_dir => TempDir
    },
    Backends = mgr_setup_backends(?BACKENDS, BackendOpts),
    {ok, BackendOpts#{backends => Backends, nodes => #{}}}.

mgr_setup_backends(BackendMods, Opts) ->
    lists:foldl(fun(Mod, Acc) -> Acc#{Mod => Mod:start(Opts)} end,
                #{}, BackendMods).

mgr_cleanup(State) ->
    State2 = mgr_safe_stop_all(?NODE_TEARDOWN_TIMEOUT, State),
    State3 = mgr_safe_delete_all(State2),
    mgr_safe_stop_backends(State3).

mgr_get_service_address(NodeName, Service, #{nodes := Nodes}) ->
    #{NodeName := {Mod, NodeState}} = Nodes,
    Mod:get_service_address(Service, NodeState).

mgr_setup_nodes(NodeSpecs, State) ->
    lists:foldl(fun mgr_setup_node/2, State, NodeSpecs).

mgr_setup_node(#{backend := Mod, name := Name} = NodeSpec, State) ->
    #{backends := Backends, nodes := Nodes} = State,
    #{Mod := BackendState} = Backends,
    NodeState = Mod:setup_node(NodeSpec, BackendState),
    State#{nodes := Nodes#{Name => {Mod, NodeState}}}.

mgr_start_node(NodeName, #{nodes := Nodes} = State) ->
    #{NodeName := {Mod, NodeState}} = Nodes,
    NodeState2 = Mod:start_node(NodeState),
    State#{nodes := Nodes#{NodeName := {Mod, NodeState2}}}.

mgr_stop_node(NodeName, #{nodes := Nodes} = State) ->
    #{NodeName := {Mod, NodeState}} = Nodes,
    Opts = #{soft_timout => ?NODE_STOP_TIMEOUT},
    NodeState2 = Mod:stop_node(NodeState, Opts),
    State#{nodes := Nodes#{NodeName := {Mod, NodeState2}}}.

mgr_safe_stop_backends(#{backends := Backends} = State) ->
    maps:map(fun(Mod, BackendState) ->
        try
            Mod:stop(BackendState)
        catch
            _:E ->
                log(State, "Error while stopping backend ~p: ~p", [Mod, E])
        end
    end, Backends),
    State#{backends := #{}}.


mgr_safe_stop_all(Timeout, #{nodes := Nodes1} = State) ->
    Opts = #{soft_timeout => Timeout},
    Nodes2 = maps:map(fun(Name, {Backend, NodeState}) ->
        try
            {Backend, Backend:stop_node(NodeState, Opts)}
        catch
            _:E ->
                log(State, "Error while stopping node ~p: ~p", [Name, E]),
                {Backend, NodeState}
        end
    end, Nodes1),
    State#{nodes := Nodes2}.

mgr_safe_delete_all(#{nodes := Nodes1} = State) ->
    maps:map(fun(Name, {Backend, NodeState}) ->
        try
            {Backend, Backend:delete_node(NodeState)}
        catch
            _:E ->
                log(State, "Error while stopping node ~p: ~p", [Name, E]),
                {Backend, NodeState}
        end
    end, Nodes1),
    State#{nodes := #{}}.