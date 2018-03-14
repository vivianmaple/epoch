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

%=== MACROS ====================================================================

-define(CALL_TAG, ?MODULE).
-define(CT_CONF_KEY, node_manager).

-define(TEST_CTX_FIRST_LOCAL_PORT, 3001).

-define(STOP_TIMEOUT, 3). % In seconds

%=== COMMON TEST API FUNCTIONS =================================================

ct_setup(Config) ->
    {data_dir, DataDir} = proplists:lookup(data_dir, Config),
    {priv_dir, PrivDir} = proplists:lookup(priv_dir, Config),
    case setup(DataDir, PrivDir) of
        {ok, Pid} -> [{?CT_CONF_KEY, Pid} | Config];
        {error, Reason} ->
            erlang:error({system_test_setup_failed, [{reason, Reason}]})
    end.

ct_cleanup(Config) ->
    case cleanup(Config) of
        ok -> ok;
        {error, Reason} ->
            erlang:error({system_test_cleanup_failed, [{reason, Reason}]})
    end.

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
    mgr_setup(DataDir, TempDir).

handle_call(Request, From, State) ->
    try
        handlex(Request, From, State)
    catch
        Class:Reason ->
            {reply, {error, Class, Reason, erlang:get_stacktrace()}, State}
    end.

handlex({setup_nodes, NodeSpecs}, _From, State) ->
    call_reply(mgr_setup_nodes(NodeSpecs, State), State);
handlex({start_node, NodeName}, _From, State) ->
    call_reply(mgr_start_node(NodeName, State), State);
handlex(get_nodes, _From, #{nodes := Nodes} = State) ->
    {reply, Nodes, State};
handlex(cleanup, _From, State) ->
    call_reply(mgr_cleanup(State), State);
handlex(Request, From, _State) ->
    error({unknown_request, Request, From}).

handle_info(_Msg, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

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
    gen_server:start(?MODULE, [DataDir, TempDir], []).

stop(Pid) ->
    gen_server:stop(Pid).

call(Pid, Msg) ->
    case gen_server:call(Pid, Msg, 60000) of
        {error, Class, Reason, _Stacktrace} ->
            ct:fail({Class, Reason});
        Reply ->
            Reply
    end.

setup(DataDir, TempDir) ->
    aest_docker:start(),
    start(DataDir, TempDir).

cleanup(Ctx) ->
    Pid = ctx2pid(Ctx),
    Result = call(Pid, cleanup),
    stop(Pid),
    Result.

call_reply(ok, OldState) ->
    {reply, ok, OldState};
call_reply({ok, NewState}, _OldState) ->
    {reply, ok, NewState};
call_reply({ok, Result, NewState}, _OldState) ->
    {reply, {ok, Result}, NewState};
call_reply(Error, OldState) ->
    {reply, Error, OldState}.

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
        #{ctx := Ctx, nodes := Nodes} = State,
        {ok, NodeState1, Ctx1} = aest_backend:stop_node(NodeState, ?STOP_TIMEOUT, Ctx),
        State#{ctx := Ctx1, nodes := Nodes#{Name := NodeState1}}
    end, State0, Nodes0),
    State2 = maps:fold(fun(Name, NodeState, State) ->
        #{ctx := Ctx, nodes := Nodes} = State,
        case aest_backend:delete_node(NodeState, Ctx) of
            {error, _Reason} ->
                %% Maybe we should log something ?
                State;
            {ok, NodeState1, Ctx1} ->
                State#{ctx := Ctx1, nodes := Nodes#{Name := NodeState1}}
        end
    end, State1, Nodes1),
    {ok, State2}.

mgr_setup_nodes(NodeSpecs, State) ->
    #{ctx := TestCtx, nodes := Nodes} = State,
    %% TODO: Add some validation of the specs
    %%  e.g. name clash, required keys...
    {PrepNodes, TestCtx2} = lists:foldl(
        fun(#{name := Name} = Spec, {Acc, Ctx}) ->
            {ok, NodeState, NewCtx} =
                aest_backend:prepare_node(Spec, Ctx),
            {Acc#{Name => NodeState}, NewCtx}
        end, {#{}, TestCtx}, NodeSpecs),
    AllNodes = maps:merge(Nodes, PrepNodes),
    {Nodes2, TestCtx3} = maps:fold(
        fun(Name, NodeState, {Acc, Ctx}) ->
            {ok, NewNodeState, NewCtx} =
                aest_backend:setup_node(NodeState, AllNodes, Ctx),
            {Acc#{Name => NewNodeState}, NewCtx}
        end, {Nodes, TestCtx2}, PrepNodes),
    {ok, State#{ctx := TestCtx3, nodes := Nodes2}}.

mgr_start_node(NodeName, State) ->
    #{ctx := TestCtx, nodes := Nodes} = State,
    case maps:find(NodeName, Nodes) of
        error -> {error, node_not_found};
        {ok, NodeState} ->
            case aest_backend:start_node(NodeState, TestCtx) of
                {error, _Reason} = Error -> Error;
                {ok, NodeState2, TestCtx2} ->
                    Nodes2 = Nodes#{NodeName := NodeState2},
                    {ok, State#{ctx := TestCtx2, nodes := Nodes2}}
            end
    end.
