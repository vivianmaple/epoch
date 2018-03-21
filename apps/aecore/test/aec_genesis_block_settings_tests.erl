-module(aec_genesis_block_settings_tests).

-include_lib("eunit/include/eunit.hrl").
-include("common.hrl").
-include("blocks.hrl").

-define(TEST_MODULE, aec_genesis_block_settings).

%%%===================================================================
%%% Test cases
%%%===================================================================

preset_accounts_test_() ->
    {foreach,
     fun() ->
         meck:new(?TEST_MODULE, [passthrough]),
         ok
     end,
     fun(ok) ->
         meck:unload(?TEST_MODULE)
     end,
     [ {"Preset accounts parsing: empty file",
        fun() ->
            expect_accounts(<<"">>),
            ?assertEqual([], ?TEST_MODULE:preset_accounts()),
            ok
        end},
       {"Preset accounts parsing: a preset account",
        fun() ->
            expect_accounts([{<<"some pubkey">>, 10}]),
            ?assertEqual([{<<"some pubkey">>, 10}], ?TEST_MODULE:preset_accounts()),
            ok
        end},
       {"Preset accounts parsing: deterministic ordering",
        fun() ->
            Accounts =
                [{<<"Alice">>, 10},
                 {<<"Carol">>, 20},
                 {<<"Bob">>, 42}],
            AccountsOrdered = lists:keysort(1, Accounts),
            expect_accounts(Accounts),
            ?assertEqual(AccountsOrdered, ?TEST_MODULE:preset_accounts()),
            ok
        end},
       {"Preset accounts parsing: preset accounts file missing",
        fun() ->
            FileName = file_path,
            meck:expect(?TEST_MODULE, read_presets, 0, {error, something, FileName}),
            ?assertError({genesis_accounts_file_missing, FileName}, ?TEST_MODULE:preset_accounts()),
            ok
        end}

     ]}.

expect_accounts(B) when is_binary(B) ->
    meck:expect(?TEST_MODULE, read_presets, 0, {ok, B});
expect_accounts(L0) when is_list(L0) ->
    L =
        lists:map(
            fun({PK, Amt}) ->
                {aec_base58c:encode(account_pubkey, PK), Amt}
            end,
            L0),
    expect_accounts(jsx:encode(L)).
