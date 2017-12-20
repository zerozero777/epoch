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
    maybe_notify_query_tx(SignedTx),
    maybe_notify_response_tx(SignedTx),
    notify_on_new_transactions(Rest).


%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_notify_query_tx(SignedTx) ->
    case is_oracle_query_tx(SignedTx)
        andalso is_query_to_node_oracle(SignedTx) of
        true ->
            aec_events:publish(oracle_query_tx_created, aec_tx_sign:data(SignedTx));
        false ->
            ok
    end.

maybe_notify_response_tx(SignedTx) ->
    case is_oracle_response_tx(SignedTx)
        andalso is_response_to_node_question(SignedTx) of
        true ->
            aec_events:publish(oracle_response_tx_created, aec_tx_sign:data(SignedTx));
        false ->
            ok
    end.

is_query_to_node_oracle(Signed) ->
    QueryTx = aec_tx_sign:data(Signed),
    {ok, Keys} = aec_keys:pubkey(),
    Keys =:= aeo_query_tx:oracle(QueryTx).

is_response_to_node_question(Signed) ->
    %% TODO: Check if sender_address of interaction
    %% from a transaction is associated with node's pubkey
    %% We need to fetch interaction by id from state tree to check sender_address.
    %% To do this we may need oracle state tree at a given height available.
    true.

%% TODO: consider moving below to aec_tx.erl
is_oracle_query_tx(Signed) ->
    is_of_type(Signed, <<"oracle_query">>).

is_oracle_response_tx(Signed) ->
    is_of_type(Signed, <<"oracle_response">>).

is_of_type(Signed, ExpectedType) ->
    Tx = aec_tx_sign:data(Signed),
    Mod = aec_tx_dispatcher:handler(Tx),
    Type = Mod:type(),
    ExpectedType =:= Type.
