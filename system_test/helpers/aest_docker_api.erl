%% For a description of the API used see
%% https://docs.docker.com/engine/api/v1.32/
%%

-module(aest_docker_api).

%=== EXPORTS ===================================================================

%% API exports
-export([start/0]).
-export([info/0]).
-export([create_network/1]).
-export([prune_networks/0]).
-export([create_container/2]).
-export([delete_container/1]).
-export([start_container/1]).
-export([stop_container/2]).
-export([inspect/1]).

%=== MACROS ====================================================================

-define(BASE_URL, <<"http+unix://%2Fvar%2Frun%2Fdocker.sock/">>).
-define(CONTAINER_STOP_TIMEOUT, 30).
-define(CONTAINER_KILL_TIMEOUT_EXTRA, 5).

%=== API FUNCTIONS =============================================================

start() ->
    {ok, _} = application:ensure_all_started(hackney),
    ok.

info() ->
    {ok, 200, Info} = docker_get([info]),
    Info.

create_network(Config) ->
    BodyObj = maps:fold(fun create_network_object/3, #{}, Config),
    case docker_post([networks, create], #{}, BodyObj) of
        {ok, 201, Response} -> Response;
        {ok, 500, Response} ->
            throw({docker_error, maps:get(message, Response)})
    end.

prune_networks() ->
    case docker_post([networks, prune], #{}) of
        {ok, 200, Response} -> Response;
        {ok, 500, Response} ->
            throw({docker_error, maps:get(message, Response)})
    end.

create_container(Name, #{image := Image} = Config) ->
    BodyObj = maps:fold(fun create_container_object/3, #{}, Config),
    case docker_post([containers, create], #{name => Name}, BodyObj) of
        {ok, 201, Response} -> Response;
        {ok, 404, _Response} -> throw({no_such_image, Image});
        {ok, 500, Response} ->
            throw({docker_error, maps:get(message, Response)})
    end.

delete_container(ID) ->
    case docker_delete([containers, ID]) of
        {ok, 204, _} -> ok;
        {ok, 500, Response} ->
            throw({docker_error, maps:get(message, Response)})
    end.

start_container(ID) ->
    case docker_post([containers, ID, start]) of
        {ok, 204, _} -> ok;
        {ok, 304, _} -> throw({container_already_started, ID});
        {ok, 500, Response} ->
            throw({docker_error, maps:get(message, Response)})
    end.

stop_container(ID, Opts) ->
    SoftTimeout = maps:get(soft_timeout, Opts, ?CONTAINER_STOP_TIMEOUT),
    HardTimeout = maps:get(hard_timeout, Opts,
                           SoftTimeout + ?CONTAINER_KILL_TIMEOUT_EXTRA),
    case docker_post([containers, ID, stop], #{t => SoftTimeout},
                     undefined, HardTimeout * 1000) of
        {ok, 204, _} -> ok;
        {ok, 304, _} -> throw({container_not_started, ID});
        {ok, 500, Response} ->
            throw({docker_error, maps:get(message, Response)});
        {error, timeout} ->
            throw({container_stop_timeout, ID})
    end.

inspect(ID) ->
    {ok, 200, Info} = docker_get([containers, ID, json]),
    Info.

%=== INTERNAL FUNCTIONS ========================================================

create_network_object(name, Name, Body) ->
    Body#{'Name' => json_string(Name)};
create_network_object(driver, Driver, Body) ->
    Body#{'Driver' => json_string(Driver)}.

create_container_object(command, Cmd, Body) when is_list(Cmd) ->
    JsonCmd = [json_string(V) || V <- Cmd],
    put_in(['Cmd'], JsonCmd, Body);
create_container_object(command, Cmd, Body) ->
    put_in(['Cmd'], json_string(Cmd), Body);
create_container_object(network, NetId, Body) ->
    put_in(['HostConfig', 'NetworkMode'], NetId, Body);
create_container_object(hostname, Hostname, Body) ->
    Body#{'Hostname' => json_string(Hostname)};
create_container_object(image, Image, Body) ->
    Body#{'Image' => json_string(Image)};
create_container_object(env, Env, Body) ->
    EnvList = [json_string(K ++ "=" ++ V) || {K, V} <- maps:to_list(Env)],
    Body#{'Env' => EnvList};
create_container_object(volumes, VolSpecs, Body) ->
    {Volumes, Bindings} = lists:foldl(fun
        ({rw, HostVol, NodeVol}, {VolAcc, BindAcc}) ->
            {VolAcc#{json_string(NodeVol) => #{}},
             [format("~s:~s", [HostVol, NodeVol]) | BindAcc]};
        ({ro, HostVol, NodeVol}, {VolAcc, BindAcc}) ->
            {VolAcc#{json_string(NodeVol) => #{}},
             [format("~s:~s:ro", [HostVol, NodeVol]) | BindAcc]}
    end, {#{}, []}, VolSpecs),
    HostConfig = maps:get('HostConfig', Body, #{}),
    HostConfig2 = HostConfig#{'Binds' => Bindings},
    Body#{'HostConfig' => HostConfig2, 'Volumes' => Volumes};
create_container_object(ports, PortSpecs, Body) ->
    {Exposed, Bindings} = lists:foldl(fun
        ({Proto, HostPort, ContainerPort}, {ExpAcc, BindAcc}) ->
            Key = format("~w/~s", [ContainerPort, Proto]),
            PortStr = format("~w", [HostPort]),
            HostSpec = [#{'HostPort' => PortStr}],
            {ExpAcc#{Key => #{}}, BindAcc#{Key => HostSpec}}
    end, {#{}, #{}}, PortSpecs),
    HostConfig = maps:get('HostConfig', Body, #{}),
    HostConfig2 = HostConfig#{'PortBindings' => Bindings},
    Body#{'HostConfig' => HostConfig2, 'ExposedPorts' => Exposed};
create_container_object(Key, _Value, _Body) ->
    error({unknown_create_param, Key}).

put_in([Key], Val, Map) -> Map#{Key => Val};
put_in([Key | Keys], Val, Map) ->
    maps:put(Key, put_in(Keys, Val, maps:get(Key, Map, #{})), Map).

format(Fmt, Args) ->
    iolist_to_binary(io_lib:format(Fmt, Args)).

docker_get(Path) ->
    case hackney:request(get, url(Path), [], <<>>, []) of
        {error, _Reason} = Error -> Error;
        {ok, Status, _RespHeaders, ClientRef} ->
            case docker_fetch_json_body(ClientRef) of
                {error, _Reason} = Error -> Error;
                {ok, Response} -> {ok, Status, Response}
            end
    end.

docker_delete(Path) ->
    case hackney:request(delete, url(Path), [], <<>>, []) of
        {error, _Reason} = Error -> Error;
        {ok, Status, _RespHeaders, ClientRef} ->
            case docker_fetch_json_body(ClientRef) of
                {error, _Reason} = Error -> Error;
                {ok, Response} -> {ok, Status, Response}
            end
    end.

docker_post(Path) -> docker_post(Path, #{}).

docker_post(Path, Query) -> docker_post(Path, Query, undefined).

docker_post(Path, Query, BodyObj) -> docker_post(Path, Query, BodyObj, 5000).

docker_post(Path, Query, BodyObj, Timeout) ->
    BodyJSON = encode(BodyObj),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    case hackney:request(post, url(Path, Query), Headers, BodyJSON, [{recv_timeout, Timeout}]) of
        {error, _Reason} = Error -> Error;
        {ok, Status, _RespHeaders, ClientRef} ->
            case docker_fetch_json_body(ClientRef) of
                {error, _Reason} = Error -> Error;
                {ok, Response} -> {ok, Status, Response}
            end
    end.

docker_fetch_json_body(ClientRef) ->
    case hackney:body(ClientRef) of
        {error, _Reason} = Error -> Error;
        {ok, BodyJson} -> decode(BodyJson)
    end.

decode(<<>>) -> {ok, undefined};
decode(JsonStr) ->
    try jsx:decode(JsonStr, [{labels, attempt_atom}, return_maps]) of
        JsonObj -> {ok, JsonObj}
    catch
        error:badarg -> {error, bad_json}
    end.

encode(undefined) -> <<>>;
encode(JsonObj)   -> jsx:encode(JsonObj, []).

json_string(Atom) when is_atom(Atom) -> Atom;
json_string(Bin) when is_binary(Bin) -> Bin;
json_string(Str) when is_list(Str) -> list_to_binary(Str);
json_string(Num) when is_number(Num) -> format("~w", [Num]).

url(Path) -> url(Path, #{}).

url(Path, QS) when is_list(Path) ->
    hackney_url:make_url(?BASE_URL, [to_binary(P) || P <- Path], maps:to_list(QS));
url(Item, QS) ->
    url([Item], QS).

to_binary(Term) when is_atom(Term) -> atom_to_binary(Term, utf8);
to_binary(Term)                    -> Term.
