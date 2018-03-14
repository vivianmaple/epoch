-module(aest_sync_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% Test cases
-export([new_node_join_old_network/1]).

-import(aest_utils, [
    setup_nodes/2,
    start_node/2,
    wait_for_height/4,
    assert_synchronized/2
]).

%=== MACROS ====================================================================

-define(MINING_TIMOUT, 10000).

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
    [new_node_join_old_network].

init_per_testcase(_TC, Config) ->
    aest_utils:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_utils:ct_cleanup(Config).

%=== TEST CASES ================================================================

new_node_join_old_network(Cfg) ->
    setup_nodes([?NODE1, ?NODE2, ?NODE3], Cfg),
    start_node(node1, Cfg),
    start_node(node2, Cfg),
    wait_for_height(20, [node1, node2], 20 * ?MINING_TIMOUT, Cfg),
    assert_synchronized([node1, node2], Cfg),
    start_node(node3, Cfg),
    wait_for_height(20, [node3], 5 * ?MINING_TIMOUT, Cfg),
    assert_synchronized([node1, node3], Cfg).
