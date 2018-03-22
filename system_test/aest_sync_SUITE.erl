-module(aest_sync_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% Test cases
-export([new_node_joins_network/1,
         docker_keeps_data/1,
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

-define(MINING_TIMEOUT, 4*10000).

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
    [ new_node_joins_network
    , docker_keeps_data
    , crash_and_continue_sync 
    ].

init_per_testcase(_TC, Config) ->
    %% Some parameters depend on the speed and capacity of the docker containers:
    aest_nodes:ct_setup([ {blocks_per_second, 3},
                          {node_startup_time, 5000}  %% Time it takes to get the node to respond to http
                          | Config]).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

%=== TEST CASES ================================================================

%% A few tests that verify that our assumptions are right for docker timings and
%% API.

new_node_joins_network(Cfg) ->
    Length = 10,
    BlocksPerSecond = proplists:get_value(blocks_per_second, Cfg),
    NodeStartupTime = proplists:get_value(node_startup_time, Cfg), 

    setup_nodes([?NODE1, ?NODE2, ?NODE3], Cfg),
    start_node(node1, Cfg),
    start_node(node2, Cfg),
    wait_for_height(Length, [node1, node2], Length * ?MINING_TIMEOUT, Cfg),
    Top1 = request(node1, [v2, 'top'], #{}, Cfg),
    ct:log("Node 1 top: ~p~n", [Top1]),
    Height = request(node2, [v2, 'block-by-height'], #{height => Length}, Cfg),
    ct:log("Node 2 at height ~p: ~p~n", [Length, Height]),
    start_node(node3, Cfg),
    wait_for_height(0, [node3], NodeStartupTime, Cfg),
    ct:log("Node 3 ready to go"),

    %% get them with N blocks a second
    timer:sleep((Length div BlocksPerSecond) * 1000),

    wait_for_height(Length, [node3], 1000, Cfg),
    ct:log("Node 3 on same height"),
    Height = request(node3, [v2, 'block-by-height'], #{height => Length}, Cfg),
    ok.

%% When we stop and restart a node we will be able to read the blocks
%% that we had in the chain before stopping: data is persistent.
docker_keeps_data(Cfg) ->
    Length = 10,
    BlocksPerSecond = proplists:get_value(blocks_per_second, Cfg),
    NodeStartupTime = proplists:get_value(node_startup_time, Cfg), 

    setup_nodes([?NODE1], Cfg),
    start_node(node1, Cfg),
    wait_for_height(Length, [node1], Length * ?MINING_TIMEOUT, Cfg),
    B1 = request(node1, [v2, 'block-by-height'], #{height => Length}, Cfg),
    ct:log("Node 1 at ~p: ~p~n", [Length, B1]),
    stop_node(node1, Cfg),
    timer:sleep(1000),  %% Is this triggering PT-155851463 ?
    start_node(node1, Cfg),
    wait_for_height(0, [node1], NodeStartupTime, Cfg),
    ct:log("Node restarted and ready to go"),
    %% Give it time to read from disk, but not enough to build a new chain of same length
    timer:sleep(?MINING_TIMEOUT),  
    B2 = request(node1, [v2, 'block-by-height'], #{height => Length}, Cfg),
    ct:log("Node 1 after restart on ~p: ~p~n", [Length, B2]),
    B1 = B2.


crash_and_continue_sync(Cfg) ->
    BlocksPerSecond = proplists:get_value(blocks_per_second, Cfg),
    NodeStartupTime = proplists:get_value(node_startup_time, Cfg),
    %% Create a chain long enough to need 10 seconds to fetch it
    Length = BlocksPerSecond * 10,

    setup_nodes([?NODE1, ?NODE2], Cfg),
    start_node(node1, Cfg),
    wait_for_height(Length, [node1], Length * ?MINING_TIMEOUT, Cfg),
    B1 = request(node1, [v2, 'block-by-height'], #{height => Length}, Cfg),
    ct:log("Node 1 at height ~p: ~p~n", [Length, B1]),

    %% Start fetching the chain
    start_node(node2, Cfg),
    wait_for_height(0, [node2], NodeStartupTime, Cfg),
    ct:log("Node 2 ready to go"),

    %% we are fetching blocks crash now
    stop_node(node1, Cfg),
    %% Give node1 possibility to really stop 
    timer:sleep(NodeStartupTime),
    Top2 = request(node2, [v2, 'top'], #{}, Cfg),
    ct:log("Node 2 top: ~p~n", [Top2]),
    Height = maps:get(height, Top2),
    case Height >= Length of
         true -> {skip, already_synced_when_crashed};
         false ->
            start_node(node1, Cfg),
            wait_for_height(Length, [node2], ((Length - Height) div BlocksPerSecond + 1) * 1000, Cfg),
            B2 = request(node2, [v2, 'block-by-height'], #{height => Length}, Cfg),
            ct:log("Node 2 at height ~p: ~p~n", [Length, B2]),
            B1 = B2
    end.