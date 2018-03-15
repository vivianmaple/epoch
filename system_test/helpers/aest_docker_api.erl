-module(aest_docker_api).

%=== EXPORTS ===================================================================

%% API exports
-export([start/0]).
-export([info/0]).
-export([create_container/2]).
-export([delete_container/1]).
-export([start_container/1]).
-export([stop_container/2]).
-export([inspect/1]).

%=== MACROS ====================================================================

-define(BASE_URL, <<"http+unix://%2Fvar%2Frun%2Fdocker.sock/">>).

%=== API FUNCTIONS =============================================================

start() ->
    {ok, _} = application:ensure_all_started(hackney),
    ok.

info() ->
    {ok, 200, Info} = docker_get([info]),
    Info.

create_container(Name, #{image := Image} = Config) ->
    BodyObj = maps:fold(fun create_body_object/3, #{}, Config),
    case docker_post([containers, create], #{name => Name}, BodyObj) of
        {ok, 201, Response} -> Response;
        {ok, 404, _Response} -> throw({no_such_image, Image})
    end.

delete_container(ID) ->
    {ok, 204, _} = docker_delete([containers, ID]),
    ok.

start_container(ID) ->
    case docker_post([containers, ID, start]) of
        {ok, 204, _} -> ok;
        {ok, 304, _} -> throw({container_already_started, ID})
    end.

stop_container(ID, Timeout) ->
    case docker_post([containers, ID, stop], #{t => Timeout}, undefined, Timeout * 1000) of
        {ok, 204, _}     -> ok;
        {ok, 304, _}     -> throw({container_not_started, ID});
        {error, timeout} -> throw({container_stop_timeout, ID})
    end.

inspect(ID) ->
    {ok, 200, Info} = docker_get([containers, ID, json]),
    Info.

%=== INTERNAL FUNCTIONS ========================================================

create_body_object(hostname, Hostname, Body) ->
    Body#{'Hostname' => json_string(Hostname)};
create_body_object(image, Image, Body) ->
    Body#{'Image' => json_string(Image)};
create_body_object(env, Env, Body) ->
    EnvList = [json_string(K ++ "=" ++ V) || {K, V} <- maps:to_list(Env)],
    Body#{'Env' => EnvList};
create_body_object(volumes, VolSpecs, Body) ->
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
create_body_object(ports, PortSpecs, Body) ->
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
create_body_object(Key, _Value, _Body) ->
    error({unknown_create_param, Key}).

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
json_string(Str) when is_list(Str) -> list_to_binary(Str).

url(Path) -> url(Path, #{}).

url(Path, QS) when is_list(Path) ->
    hackney_url:make_url(?BASE_URL, [to_binary(P) || P <- Path], maps:to_list(QS));
url(Item, QS) ->
    url([Item], QS).

to_binary(Term) when is_atom(Term) -> atom_to_binary(Term, utf8);
to_binary(Term)                    -> Term.
