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

prepare_node(NodeSpec, TestCtx) ->
    #{name := Hostname} = NodeSpec,
    ExtAddr = format("http://~s:3013/", [Hostname]),
    NodeState = #{
        spec => NodeSpec,
        hostname => Hostname,
        ext_port => 3013,
        ext_addr => ExtAddr
    },
    {ok, NodeState, TestCtx}.

setup_node(NodeState, NodeStates, TestCtx) ->
    #{test_id := TestId,
      next_port := HostPort,
      data_dir := DataDir,
      temp_dir := TempDir} = TestCtx,
    #{spec := NodeSpec, hostname := Hostname, ext_port := ExtPort} = NodeState,
    #{peers := PeerNames, source := {pull, Image}} = NodeSpec,
    HostAddr = format("http://~s:~w/", [Hostname, HostPort]),
    ConfigFileName = format("epoch_~s.yaml", [Hostname]),
    ConfigFilePath = filename:join(TempDir, ConfigFileName),
    TemplateFile = filename:join(DataDir, ?CONFIG_FILE_TEMPLATE),
    ExpandedPeers = [maps:to_list(V)
                     || {K, V} <- maps:to_list(NodeStates),
                     lists:member(K, PeerNames)],
    Context = [{epoch_config, maps:to_list(NodeState#{peers => ExpandedPeers})}],
    ok = write_template(TemplateFile, ConfigFilePath, Context),
    ContName = format("~s_~s", [Hostname, TestId]),
    DockerConfig = #{
        hostname => ContName,
        image => Image,
        env => #{"EPOCH_CONFIG" => ?EPOCH_CONFIG_FILE},
        volumes => [{ro, ConfigFilePath, ?EPOCH_CONFIG_FILE}],
        ports => [{tcp, HostPort, ExtPort}]
    },
    case aest_docker:create_container(ContName, DockerConfig) of
        {error, _Reason} = Error -> Error;
        {ok, Res} ->
            #{'Id' := ContId} = Res,
            NodeState2 = NodeState#{
                container_name => ContName,
                container_id => ContId,
                host_port => HostPort,
                host_addr => HostAddr,
                config_path => ConfigFilePath
            },
            {ok, NodeState2, TestCtx#{next_port := HostPort + 1}}
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
        ok -> {ok, NodeState, TestCtx}
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
