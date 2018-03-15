-module(aest_utils).

-behaviour(gen_server).

%=== EXPORTS ===================================================================

%% Common Test API exports
-export([ct_setup/1]).
-export([ct_cleanup/1]).

%% Generic API exports
-export([setup_nodes/2]).
-export([start_node/2]).
-export([wait_for_height/4]).
-export([assert_synchronized/2]).

%% Behaviour gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

%=== MACROS ====================================================================

-define(CALL_TAG, ?MODULE).
-define(CT_CONF_KEY, node_manager).

-define(TEST_CTX_FIRST_LOCAL_PORT, 3001).

-define(STOP_TIMEOUT, 30). % In seconds

%=== COMMON TEST API FUNCTIONS =================================================

ct_setup(Config) ->
    {data_dir, DataDir} = proplists:lookup(data_dir, Config),
    {priv_dir, PrivDir} = proplists:lookup(priv_dir, Config),
    ct:log("Node logs can be found here: ~n<a href=\"file://~s\">~s</a>",
        [PrivDir, PrivDir]
    ),
    case setup(DataDir, PrivDir) of
        {ok, Pid} -> [{?CT_CONF_KEY, Pid} | Config];
        {error, Reason} ->
            erlang:error({system_test_setup_failed, [{reason, Reason}]})
    end.

ct_cleanup(Config) ->
    Pid = ctx2pid(Config),
    call(Pid, cleanup),
    call(Pid, stop),
    wait_for_exit(Pid, 120000).

%=== GENERIC API FUNCTIONS =====================================================

setup_nodes(NodeSpecs, Ctx) ->
    call(ctx2pid(Ctx), {setup_nodes, NodeSpecs}).

start_node(NodeName, Ctx) ->
    call(ctx2pid(Ctx), {start_node, NodeName}).

wait_for_height(_MinHeight, _NodeNames, _Timeout, Ctx) ->
    _Nodes = call(ctx2pid(Ctx), get_nodes),
    ok.

assert_synchronized(_NodeNames, _Ctx) ->
    ok.

%=== BEHAVIOUR GEN_SERVER CALLBACK FUNCTIONS ===================================

init([DataDir, TempDir]) ->
    process_flag(trap_exit, true), % Make sure terminate always cleans up
    mgr_setup(DataDir, TempDir).

handle_call(Request, From, State) ->
    try
        handlex(Request, From, State)
    catch
        throw:Reason ->
            {reply, {'$error', Reason}, State}
    end.

handlex({setup_nodes, NodeSpecs}, _From, State) ->
    {reply, ok, mgr_setup_nodes(NodeSpecs, State)};
handlex({start_node, NodeName}, _From, State) ->
    {reply, ok, mgr_start_node(NodeName, State)};
handlex(get_nodes, _From, #{nodes := Nodes} = State) ->
    {reply, Nodes, State};
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

terminate(_Reason, State) -> mgr_cleanup(State).

%=== INTERNAL FUNCTIONS ========================================================

uid() ->
    iolist_to_binary([[io_lib:format("~2.16.0B",[X])
                       || <<X:8>> <= crypto:strong_rand_bytes(16) ]]).

ctx2pid(Pid) when is_pid(Pid) -> Pid;
ctx2pid(Props) when is_list(Props) ->
    case proplists:lookup(?CT_CONF_KEY, Props) of
        {?CT_CONF_KEY, Pid} when is_pid(Pid) -> Pid;
        _ ->
            erlang:error({system_test_not_setup, []})
    end.

start(DataDir, TempDir) ->
    gen_server:start_link(?MODULE, [DataDir, TempDir], []).

call(Pid, Msg) ->
    case gen_server:call(Pid, Msg, ?STOP_TIMEOUT * 1000 * 2) of
        {'$error', Reason} ->
            ct:fail({error, Reason});
        Reply ->
            Reply
    end.

setup(DataDir, TempDir) ->
    aest_docker_api:start(),
    start(DataDir, TempDir).

wait_for_exit(Pid, Timeout) ->
    Ref = erlang:monitor(process, Pid),
    receive {'DOWN', Ref, process, Pid, _Reason} -> ok
    after Timeout -> error({process_not_stopped, Pid})
    end.

%--- NODE MANAGER PROCESS FUNCTION ---------------------------------------------

mgr_setup(DataDir, TempDir) ->
    TestId = uid(),
    TestCtx = #{next_port => ?TEST_CTX_FIRST_LOCAL_PORT,
                test_id => TestId,
                temp_dir => TempDir,
                data_dir => DataDir},
    {ok, #{ctx => TestCtx, nodes => #{}}}.

mgr_cleanup(#{nodes := Nodes0} = State0) ->
    State1 = #{nodes := Nodes1} = maps:fold(fun(Name, NodeState, State) ->
        #{spec := #{backend := Backend, name := Name}} = NodeState,
        #{ctx := Ctx, nodes := Nodes} = State,
        {ok, NodeState1, Ctx1} = Backend:stop_node(NodeState, ?STOP_TIMEOUT, Ctx),
        State#{ctx := Ctx1, nodes := Nodes#{Name := NodeState1}}
    end, State0, Nodes0),
    maps:fold(fun(Name, #{spec := #{backend := Backend}} = NodeState, State) ->
        #{ctx := Ctx, nodes := Nodes} = State,
        {ok, _NodeState1, Ctx1} = Backend:delete_node(NodeState, Ctx),
        State#{ctx := Ctx1, nodes := maps:remove(Name, Nodes)}
    end, State1, Nodes1).

mgr_setup_nodes(NodeSpecs, State) ->
    #{ctx := TestCtx, nodes := Nodes} = State,
    %% TODO: Add some validation of the specs
    %%  e.g. name clash, required keys...
    {PrepNodes, TestCtx2} = lists:foldl(
        fun(#{name := Name, backend := Backend} = Spec, {Acc, Ctx}) ->
            {ok, NodeState, NewCtx} = Backend:prepare_node(Spec, Ctx),
            {Acc#{Name => NodeState}, NewCtx}
        end, {#{}, TestCtx}, NodeSpecs),
    AllNodes = maps:merge(Nodes, PrepNodes),
    {Nodes2, TestCtx3} = maps:fold(
        fun(Name, #{spec := #{backend := Backend}} = NodeState, {Acc, Ctx}) ->
            {ok, NewNodeState, NewCtx} =
                Backend:setup_node(NodeState, AllNodes, Ctx),
            {Acc#{Name => NewNodeState}, NewCtx}
        end, {Nodes, TestCtx2}, PrepNodes),
    State#{ctx := TestCtx3, nodes := Nodes2}.

mgr_start_node(NodeName, State) ->
    #{ctx := TestCtx, nodes := Nodes} = State,
    #{spec := #{backend := Backend}} = NodeState = maps:get(NodeName, Nodes),
    {ok, NodeState2, TestCtx2} = Backend:start_node(NodeState, TestCtx),
    Nodes2 = Nodes#{NodeName := NodeState2},
    State#{ctx := TestCtx2, nodes := Nodes2}.
