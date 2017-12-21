%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%% Oracles subscription events
%%% @end
%%%-------------------------------------------------------------------

%% API
-module(aeo_subscription).

-export([notify_on_new_transactions/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec notify_on_new_transactions(list(aec_tx_sign:signed_tx())) -> ok.
notify_on_new_transactions([]) ->
    ok;
notify_on_new_transactions([SignedTx | Rest]) ->
    Tx = aec_tx_sign:data(SignedTx),
    case aec_tx_dispatcher:handler(Tx) of
        aeo_query_tx    -> maybe_notify_query_tx(Tx);
        aeo_response_tx -> maybe_notify_response_tx(Tx);
        _Other          -> ok
    end,
    notify_on_new_transactions(Rest).


%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_notify_query_tx(Tx) ->
    case is_query_to_node_oracle(Tx) of
        true  -> aec_events:publish(oracle_query_tx_created, Tx);
        false -> ok
    end.

maybe_notify_response_tx(Tx) ->
    case is_response_to_node_question(Tx) of
        true  -> aec_events:publish(oracle_response_tx_created, Tx);
        false -> ok
    end.

is_query_to_node_oracle(QueryTx) ->
    {ok, Keys} = aec_keys:pubkey(),
    Keys =:= aeo_query_tx:oracle(QueryTx).

is_response_to_node_question(_Signed) ->
    %% TODO: Check if sender_address of interaction
    %% from a transaction is associated with node's pubkey
    %% We need to fetch interaction by id from state tree to check sender_address.
    %% To do this we may need oracle state tree at a given height available.
    true.

