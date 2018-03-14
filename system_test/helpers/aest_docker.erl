-module(aest_docker).

%=== EXPORTS ===================================================================

%% API exports
-export([prepare_node/2]).
-export([setup_node/3]).
-export([delete_node/2]).
-export([start_node/2]).
-export([stop_node/3]).

%=== MACROS ====================================================================

-define(CONFIG_FILE_TEMPLATE, "epoch.yaml.mustache").
-define(EPOCH_CONFIG_FILE, "/home/epoch/epoch.yaml").

%=== GENERIC API FUNCTIONS =====================================================

prepare_node(#{name := Name} = NodeSpec, TestCtx) ->
    NodeState = #{
        spec => NodeSpec,
        hostname => Name,
        ports => [3013, 3113, 3114]
    },
    {ok, NodeState, TestCtx}.

setup_node(NodeState, NodeStates, TestCtx) ->
    #{test_id := TestId,
      next_port := NextPort,
      data_dir := DataDir,
      temp_dir := TempDir} = TestCtx,
    #{spec := NodeSpec, hostname := Hostname, ports := Ports} = NodeState,
    #{peers := PeerNames, source := {pull, Image}} = NodeSpec,
    ConfigFileName = format("epoch_~s.yaml", [Hostname]),
    ConfigFilePath = filename:join(TempDir, ConfigFileName),
    TemplateFile = filename:join(DataDir, ?CONFIG_FILE_TEMPLATE),
    ExpandedPeers = [maps:to_list(V)
                     || {K, V} <- maps:to_list(NodeStates),
                     lists:member(K, PeerNames)],
    Context = [{epoch_config, maps:to_list(NodeState#{peers => ExpandedPeers})}],
    ok = write_template(TemplateFile, ConfigFilePath, Context),
    ContName = format("~s_~s", [Hostname, TestId]),
    {NextPort2, PortMapping} = lists:foldl(fun(Port, {Next, Mapping}) ->
        {Next + 1, [{tcp, Next, Port}|Mapping]}
    end, {NextPort, []}, Ports),
    DockerConfig = #{
        hostname => ContName,
        image => Image,
        env => #{"EPOCH_CONFIG" => ?EPOCH_CONFIG_FILE},
        volumes => [{ro, ConfigFilePath, ?EPOCH_CONFIG_FILE}],
        ports => PortMapping
    },
    #{'Id' := ContId} = aest_docker_api:create_container(ContName, DockerConfig),
    NodeState2 = NodeState#{
        container_name => ContName,
        container_id => ContId,
        config_path => ConfigFilePath
    },
    {ok, NodeState2, TestCtx#{next_port := NextPort2}}.

delete_node(#{container_id := ID} = NodeState, TestCtx) ->
    aest_docker_api:delete_container(ID),
    {ok, NodeState, TestCtx}.

start_node(#{container_id := ID, spec := Spec} = NodeState, TestCtx) ->
    aest_docker_api:start_container(ID),
    #{'Id' := ID} = Info = aest_docker_api:inspect(ID),
    ct:log(info, "Container ~p [~s] started", [maps:get(name, Spec), ID]),
    {ok, NodeState#{ports => get_in(Info, ['NetworkSettings', 'Ports'])}, TestCtx}.

stop_node(#{container_id := ID, spec := Spec} = NodeState, Timeout, TestCtx) ->
    ct:log(info, "Container ~p [~s] stopped ", [maps:get(name, Spec), ID]),
    aest_docker_api:stop_container(ID, Timeout),
    {ok, NodeState, TestCtx}.

%=== INTERNAL FUNCTIONS ========================================================

format(Fmt, Args) ->
    iolist_to_binary(io_lib:format(Fmt, Args)).

write_template(TemplateFile, OutputFile, Context) ->
    Template = bbmustache:parse_file(TemplateFile),
    Data = bbmustache:compile(Template, Context, [{key_type, atom}]),
    file:write_file(OutputFile, Data).

get_in(Map, [Key])      -> maps:get(Key, Map);
get_in(Map, [Key|Keys]) -> get_in(maps:get(Key, Map), Keys).
