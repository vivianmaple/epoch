%%% -*- erlang-indent-level: 4 -*-
%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%%-------------------------------------------------------------------
-module(docker_utils).


%=== EXPORTS ===================================================================

%% API exports
-export([start/0]).
-export([info/0]).
-export([create_container/2]).
-export([delete_container/1]).
-export([start_container/1]).
-export([stop_container/2]).


%=== MACROS ====================================================================

-define(BASE_URL, <<"http+unix://%2Fvar%2Frun%2Fdocker.sock/">>).
-define(DOCKER_INFO, [<<"info">>]).
-define(DOCKER_CONTAINERS_CREATE, [<<"containers/create">>]).
-define(DOCKER_CONTAINERS_DELETE(ID_OR_NAME), [<<"containers/">>, ID_OR_NAME]).
-define(DOCKER_CONTAINERS_START(ID_OR_NAME),
        [<<"containers/">>, ID_OR_NAME, <<"/start">>]).
-define(DOCKER_CONTAINERS_STOP(ID_OR_NAME),
        [<<"containers/">>, ID_OR_NAME, <<"/stop">>]).

-define(JSX_DECODE_OPTS, [{labels, attempt_atom}, return_maps]).
-define(JSX_ENCODE_OPTS, []).


%=== API FUNCTIONS =============================================================

start() ->
    case application:ensure_all_started(hackney) of
        {ok, _} -> ok;
        Error -> Error
    end.

info() ->
    case docker_get(?DOCKER_INFO) of
        {error, _Reason} = Error -> Error;
        {ok, 200, Response} -> {ok, Response};
        {ok, Status, Response} ->
            {error, {unexpected_status, Status, Response}}
    end.

create_container(Name, #{image := Image} = Config) ->
    BodyObj = maps:fold(fun create_body_object/3, #{}, Config),
    case docker_post(?DOCKER_CONTAINERS_CREATE, #{name => Name}, BodyObj) of
        {error, _Reason} = Error -> Error;
        {ok, 409, _Response} -> {error, container_conflict};
        {ok, 201, Response} -> {ok, Response};
        {ok, Status, Response} ->
            {error, {unexpected_status, Status, Response}}
    end.

delete_container(NameOrId) ->
    Path = ?DOCKER_CONTAINERS_DELETE(NameOrId),
    case docker_delete(Path) of
        {error, _Reason} = Error -> Error;
        {ok, 404, _Response} -> {error, container_not_found};
        {ok, 409, _Response} -> {error, container_conflict};
        {ok, 204, _Response} -> ok;
        {ok, Status, Response} ->
            {error, {unexpected_status, Status, Response}}
    end.

start_container(NameOrId) ->
    Path = ?DOCKER_CONTAINERS_START(NameOrId),
    case docker_post(Path) of
        {error, _Reason} = Error -> Error;
        {ok, 404, _Response} -> {error, container_not_found};
        {ok, 304, _Response} -> {error, container_already_started};
        {ok, 204, _Response} -> ok;
        {ok, Status, Response} ->
            {error, {unexpected_status, Status, Response}}
    end.

stop_container(NameOrId, Timeout) ->
    Path = ?DOCKER_CONTAINERS_STOP(NameOrId),
    case docker_post(Path, #{t => Timeout}) of
        {error, _Reason} = Error -> Error;
        {ok, 404, _Response} -> {error, container_not_found};
        {ok, 304, _Response} -> {error, container_already_stopped};
        {ok, 204, _Response} -> ok;
        {ok, Status, Response} ->
            {error, {unexpected_status, Status, Response}}
    end.


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
        ({Proto, HostPort, ExtPort}, {ExpAcc, BindAcc}) ->
            Key = format("~w/~s", [ExtPort, Proto]),
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
    Url = hackney_url:make_url(?BASE_URL, Path, []),
    case hackney:request(get, Url, [], <<>>, []) of
        {error, _Reason} = Error -> Error;
        {ok, Status, _RespHeaders, ClientRef} ->
            case docker_fetch_json_body(ClientRef) of
                {error, _Reason} = Error -> Error;
                {ok, Response} -> {ok, Status, Response}
            end
    end.

docker_delete(Path) ->
    Url = hackney_url:make_url(?BASE_URL, Path, []),
    case hackney:request(delete, Url, [], <<>>, []) of
        {error, _Reason} = Error -> Error;
        {ok, Status, _RespHeaders, ClientRef} ->
            case docker_fetch_json_body(ClientRef) of
                {error, _Reason} = Error -> Error;
                {ok, Response} -> {ok, Status, Response}
            end
    end.

docker_post(Path) -> docker_post(Path, #{}).

docker_post(Path, Query) -> docker_post(Path, Query, undefined).

docker_post(Path, Query, BodyObj) ->
    Url = hackney_url:make_url(?BASE_URL, Path, maps:to_list(Query)),
    case encode(BodyObj) of
        {error, _Reason} = Error -> Error;
        {ok, BodyJson} ->
            Headers = [{<<"Content-Type">>, <<"application/json">>}],
            case hackney:request(post, Url, Headers, BodyJson, []) of
                {error, _Reason} = Error -> Error;
                {ok, Status, _RespHeaders, ClientRef} ->
                    case docker_fetch_json_body(ClientRef) of
                        {error, _Reason} = Error -> Error;
                        {ok, Response} -> {ok, Status, Response}
                    end
            end
    end.

docker_fetch_json_body(ClientRef) ->
    case hackney:body(ClientRef) of
        {error, _Reason} = Error -> Error;
        {ok, BodyJson} -> decode(BodyJson)
    end.

decode(<<>>) -> {ok, undefined};
decode(JsonStr) ->
    try jsx:decode(JsonStr, ?JSX_DECODE_OPTS) of
        JsonObj -> {ok, JsonObj}
    catch
        error:badarg -> {error, bad_json}
    end.

encode(undefined) -> {ok, <<>>};
encode(JsonObj) ->
    try jsx:encode(JsonObj, ?JSX_ENCODE_OPTS) of
        JsonStr -> {ok, JsonStr}
    catch
        error:badarg -> {error, bad_json}
    end.

json_string(Atom) when is_atom(Atom) -> Atom;
json_string(Bin) when is_binary(Bin) -> Bin;
json_string(Str) when is_list(Str) -> list_to_binary(Str).

