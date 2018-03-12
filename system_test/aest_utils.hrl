%=== MACROS ====================================================================

-define(setup_nodes(NODE_SPECS, CTX),
    begin
        ((fun () ->
            case aest_utils:setup_nodes((NODE_SPECS), (CTX)) of
                ok -> ok;
                {error, Reason} ->
                    erlang:error({setup_node_failed, [
                                    {module, ?MODULE},
                                    {line, ?LINE},
                                    {reason, Reason}]})
            end
        end)())
    end).

-define(start_node(NODE_NAME, CTX),
    begin
        ((fun () ->
            case aest_utils:start_node((NODE_NAME), (CTX)) of
                ok -> ok;
                {error, Reason} ->
                    erlang:error({start_node_failed, [
                                    {module, ?MODULE},
                                    {line, ?LINE},
                                    {reason, Reason}]})
            end
        end)())
    end).

-define(wait_for_height(MIN_HEIGHT, NODE_NAMES, TIMEOUT, CTX),
    begin
        ((fun () ->
            case aest_utils:wait_for_height((MIN_HEIGHT), (NODE_NAMES), (TIMEOUT), (CTX)) of
                ok -> ok;
                {error, Reason} ->
                    erlang:error({wait_for_height_failed, [
                                    {module, ?MODULE},
                                    {line, ?LINE},
                                    {reason, Reason}]})
            end
        end)())
    end).

-define(assert_synchronized(NODE_NAMES, CTX),
    begin
        ((fun () ->
            case aest_utils:assert_synchronized((NODE_NAMES), (CTX)) of
                ok -> ok;
                {error, Reason} ->
                    erlang:error({assert_synchronized_failed, [
                                    {module, ?MODULE},
                                    {line, ?LINE},
                                    {reason, Reason}]})
            end
        end)())
    end).
