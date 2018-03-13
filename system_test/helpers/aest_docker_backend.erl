-module(aest_docker_backend).

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
    case aest_docker:create_container(ContName, DockerConfig) of
        {error, _Reason} = Error -> Error;
        {ok, #{'Id' := ContId}} ->
            NodeState2 = NodeState#{
                container_name => ContName,
                container_id => ContId,
                config_path => ConfigFilePath
            },
            {ok, NodeState2, TestCtx#{next_port := NextPort2}}
    end.

delete_node(NodeState, TestCtx) ->
    #{container_id := Id} = NodeState,
    case aest_docker:delete_container(Id) of
        {error, _Reason} = Error -> Error;
        ok -> {ok, NodeState, TestCtx}
    end.

start_node(NodeState, TestCtx) ->
    #{container_id := Id} = NodeState,
    case aest_docker:start_container(Id) of
        {error, _Reason} = Error -> Error;
        ok ->
            Info = aest_docker:inspect(Id),
            {ok, NodeState#{ports => get_in(Info, ['NetworkSettings', 'Ports'])}, TestCtx}
    end.

stop_node(NodeState, Timeout, TestCtx) ->
    #{container_id := Id} = NodeState,
    case aest_docker:stop_container(Id, Timeout) of
        {error, _Reason} = Error -> Error;
        ok -> {ok, NodeState, TestCtx}
    end.

%=== INTERNAL FUNCTIONS ========================================================

format(Fmt, Args) ->
    iolist_to_binary(io_lib:format(Fmt, Args)).

write_template(TemplateFile, OutputFile, Context) ->
    Template = bbmustache:parse_file(TemplateFile),
    Data = bbmustache:compile(Template, Context, [{key_type, atom}]),
    file:write_file(OutputFile, Data).

get_in(Map, [Key])      -> maps:get(Key, Map);
get_in(Map, [Key|Keys]) -> get_in(maps:get(Key, Map), Keys).
