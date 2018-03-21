-module(aest_sync_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% Test cases
-export([new_node_join_old_network/1,
         crash_and_continue_sync/1]).

-import(aest_nodes, [
    setup_nodes/2,
    start_node/2,
    stop_node/2,
    request/4,
    wait_for_height/4,
    assert_synchronized/2
]).

%=== MACROS ====================================================================

-define(MINING_TIMEOUT, 10000).

-define(NODE1, #{
    name    => node1,
    peers   => [node2],
    backend => aest_docker,
    source  => {pull, "aetrnty/epoch:v0.8.0"}
}).

-define(NODE2, #{
    name    => node2,
    peers   => [node1],
    backend => aest_docker,
    source => {pull, "aetrnty/epoch:v0.8.0"}
}).

-define(NODE3, #{
    name    => node3,
    peers   => [node1],
    backend => aest_docker,
    source  => {pull, "aetrnty/epoch:local"}
}).

%=== COMMON TEST FUNCTIONS =====================================================

all() ->
    [ new_node_join_old_network
    %% , crash_and_continue_sync 
    ].

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

%=== TEST CASES ================================================================

new_node_join_old_network(Cfg) ->
    Length = 10,
    setup_nodes([?NODE1, ?NODE2, ?NODE3], Cfg),
    start_node(node1, Cfg),
    start_node(node2, Cfg),
    wait_for_height(Length, [node1, node2], Length * ?MINING_TIMEOUT, Cfg),
    Top1 = request(node1, [v2, 'top'], #{}, Cfg),
    ct:log("Node 1 top: ~p~n", [Top1]),
    Height = request(node2, [v2, 'block-by-height'], #{height => Length}, Cfg),
    ct:log("Node 2 at height ~p: ~p~n", [Length, Height]),
    start_node(node3, Cfg),
    wait_for_height(Length + 1, [node3], ?MINING_TIMEOUT, Cfg),
    Height = request(node3, [v2, 'block-by-height'], #{height => Length}, Cfg),
    ok.

crash_and_continue_sync(Cfg) ->
    %% Create a chain long enough to need some time to fetch
    Length = 10,
    setup_nodes([?NODE1, ?NODE2], Cfg),
    start_node(node1, Cfg),
    timer:sleep(Length * ?MINING_TIMEOUT),
    Top1 = request(node1, [v2, 'top'], #{}, Cfg),
    ct:log("Node 1 top: ~p~n", [Top1]),

    %% Start fetching the chain and crash node1 while doing so
    start_node(node2, Cfg),
    wait_for_height(1, [node2], 5000, Cfg),
    %% we are fetching blocks crash now
    stop_node(node2, Cfg),
    Top2 = request(node2, [v2, 'top'], #{}, Cfg),
    ct:log("Node 2 top: ~p~n", [Top2]),
    case maps:get(height, Top2) >= maps:get(height,Top1) of
         true -> {skip, already_synced_when_crashed};
         false -> ok
    end.
