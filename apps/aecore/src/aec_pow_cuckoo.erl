%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%    A library providing Cuckoo Cycle PoW generation and verification.
%%%    A NIF interface to the C/C++ Cuckoo Cycle implementation of
%%%    John Tromp:  https://github.com/tromp/cuckoo
%%%    White paper: https://github.com/tromp/cuckoo/blob/master/doc/cuckoo.pdf?raw=true
%%% @end
%%%-------------------------------------------------------------------
-module(aec_pow_cuckoo).

-behaviour(aec_pow).

-export([generate/5,
         generate/7,
         verify/4]).


-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-on_load(init/0).

-define(PROOFSIZE, 42).
-define(POW_OK, ok).
-define(POW_TOO_BIG, {error, nonce_too_big}).
-define(POW_TOO_SMALL, {error, nonces_not_ascending}).
-define(POW_NON_MATCHING, {error, endpoinyts_do_not_match_up}).
-define(POW_BRANCH, {error, branch_in_cycle}).
-define(POW_DEAD_END, {error, cycle_dead_ends}).
-define(POW_SHORT_CYCLE, {error, cycle_too_short}).

-type pow_cuckoo_solution() :: [integer()].


%%%=============================================================================
%%% NIF initialization
%%%=============================================================================

init() ->
    ok = erlang:load_nif(filename:join([code:priv_dir(aecore), "aec_pow_cuckoo_nif"]), 0).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Proof of Work generation with default settings, multiple attempts
%%
%% According to my experiments, increasing the number of trims from the default
%% 7 in John Tromp's code does not really affect speed, reducing it causes failure.
%%
%% Measured execution times (seconds) for 7 trims for threads:
%%   1: 44.61 46.92 46.41
%%   2: 15.17 15.75 19.19
%%   3:  9.35 10.92  9.73
%%   4: 10.66  7.83 10.02
%%   5:  7.41  7.47  7.32
%%  10:  7.27  6.70  6.38
%%  20:  6.25  6.74  6.41
%%
%%  Very slow below 3 threads, not improving significantly above 5, let us take 5.
%%------------------------------------------------------------------------------
-spec generate(Data :: aec_sha256:hashable(), Target :: aec_pow:sci_int(),
               Retries :: integer(), Nonce :: integer(),
               MaxNonce :: integer()) -> aec_pow:pow_result().
generate(Data, Target, Retries, Nonce, MaxNonce) ->
    generate(Data, Nonce, MaxNonce, Target, 7, 5, Retries).

%%------------------------------------------------------------------------------
%% Proof of Work generation, all params adjustable
%%------------------------------------------------------------------------------
-spec generate(Data :: aec_sha256:hashable(), Nonce :: integer(), MaxNonce :: integer(),
               Target :: aec_pow:sci_int(), Trims :: integer(),
               Threads :: integer(), Retries :: integer()) ->
                      aec_pow:pow_result().
generate(Data, Nonce, MaxNonce, Target, Trims, Threads, Retries) ->
    Hash = aec_sha256:hash(Data),
    generate_from_hash(Hash, Nonce, MaxNonce, Target, Trims, Threads, Retries).

%%------------------------------------------------------------------------------
%% Proof of Work verification (with difficulty check)
%%------------------------------------------------------------------------------
-spec verify(Data :: aec_sha256:hashable(), Nonce :: integer(),
             Evd :: aec_pow:pow_evidence(), Target :: aec_pow:sci_int()) ->
                    boolean().
verify(Data, Nonce, Evd, Target) when is_list(Evd) ->
    Hash = base64:encode_to_string(aec_sha256:hash(Data)),
    case test_target(Evd, Target) of
        true ->
            {K1, K2} = aec_siphash:create_keypair(Hash, Nonce),
            verify_proof(K1, K2, Evd);
        false ->
            false
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% Proof of Work generation: use the hash provided and try consecutive nonces
%%------------------------------------------------------------------------------
-spec generate_from_hash(binary(), integer(), integer(), aec_pow:sci_int(),
                         integer(), integer(), integer()) ->
                                aec_pow:pow_result().
generate_from_hash(_Hash, _Nonce, _MaxNonce, _Target, _Trims, _Threads, 0) ->
    {error, generation_count_exhausted};
generate_from_hash(_Hash, Nonce, Nonce, _Target, _Trims, _Threads, _Retries) ->
    {error, nonce_range_exhausted};
generate_from_hash(Hash, Nonce, MaxNonce, Target, Trims, Threads, Retries) when Retries > 0 ->
    Nonce32 = Nonce band 16#7fffffff,
    {K1, K2} = aec_siphash:create_keypair(Hash, Nonce32),
    case generate_single(K1, K2, Trims, Threads) of
        {error, no_solutions} ->
            generate_from_hash(Hash, Nonce32 + 1, MaxNonce, Target, Trims, Threads, Retries - 1);
        {ok, Soln} ->
            case test_target(Soln, Target) of
                true ->
                    {ok, {Nonce32, Soln}};
                false ->
                    NewNonce = case Nonce of
                                   16#7fffffff -> 0;
                                   _ -> Nonce32 + 1
                               end,
                    generate_from_hash(Hash, NewNonce, MaxNonce, Target, Trims, Threads, Retries - 1)
            end
    end.

%%------------------------------------------------------------------------------
%% Proof of Work generation, a single attempt
%%------------------------------------------------------------------------------
-spec generate_single(Key1 :: integer(), Key2 :: integer(), Trims :: integer(),
                      Threads :: integer()) ->
                             {'ok', Solution :: pow_cuckoo_solution()} |
                             {'error', 'no_solutions'}.
generate_single(_Key1, _Key2, _Trims, _Threads) ->
    erlang:nif_error(nif_library_not_loaded).

%%------------------------------------------------------------------------------
%% Proof of Work verification (without difficulty check)
%%------------------------------------------------------------------------------
-spec verify_proof_c(Key1 :: integer(), Key2 :: integer(),
                     Soln :: aec_pow:pow_evidence()) -> boolean().
verify_proof_c(_Key1, _Key2, _Soln) ->
    erlang:nif_error(nif_library_not_loaded).

%%------------------------------------------------------------------------------
%% Fetch the size of solution elements
%%------------------------------------------------------------------------------
-spec get_node_size() -> integer().
get_node_size() ->
    erlang:nif_error(nif_library_not_loaded).

%%------------------------------------------------------------------------------
%% Fetch value of EDGEMASK from the C code
%%------------------------------------------------------------------------------
-spec get_edge_mask() -> integer().
get_edge_mask() ->
    erlang:nif_error(nif_library_not_loaded).

%%------------------------------------------------------------------------------
%% White paper, section 9: rather than adjusting the nodes/edges ratio, a
%% hash-based target is suggested: the sha256 hash of the cycle nonces
%% is restricted to be under the target value (0 < target < 2^256).
%%------------------------------------------------------------------------------
-spec test_target(Soln :: pow_cuckoo_solution(), Target :: aec_pow:sci_int()) ->
                             boolean().
test_target(Soln, Target) ->
    NodeSize = get_node_size(),
    Bin = solution_to_binary(lists:sort(Soln), NodeSize * 8, <<>>),
    Hash = aec_sha256:hash(Bin),
    aec_pow:test_target(Hash, Target).


%%------------------------------------------------------------------------------
%% Convert solution (a list of 42 numbers) to a binary
%% in a languauge-independent way
%%------------------------------------------------------------------------------
-spec solution_to_binary(Soln :: pow_cuckoo_solution(), Bits :: integer(),
                         Acc :: binary()) -> binary().
solution_to_binary([], _Bits, Acc) ->
    Acc;
solution_to_binary([H | T], Bits, Acc) ->
    solution_to_binary(T, Bits, <<Acc/binary, H:Bits>>).

%%------------------------------------------------------------------------------
%%
%%------------------------------------------------------------------------------
-spec verify_proof(aec_hiphash:siphash_key(), aec_hiphash:siphash_key(),
                   list(integer())) -> boolean().
verify_proof(K1, K2, Proof) ->
    EdgeMask = get_edge_mask(),
    try
        %% Generate Uv pairs representing endpoints by hashing the proof
        %% XOR points together: for a closed cycle they must match somewhere
        %% making one of the XORs zero.
        {Xor0, Xor1, _, Uvs} =
            lists:foldl(fun(Nonce, _) when Nonce > EdgeMask ->
                                throw(?POW_TOO_BIG);
                           (Nonce, {_Xor0, _Xor1, PrevNonce, _Uvs}) when
                                  PrevNonce =/= no_value andalso Nonce =< PrevNonce ->
                                throw(?POW_TOO_SMALL);
                           (Nonce, {Xor0C, Xor1C, _PrevNonce, UvsC}) ->
                                Uv0 = sipnode(K1, K2, Nonce, 0),
                                Uv1 = sipnode(K1, K2, Nonce, 1),
                                {Xor0C bxor Uv0, Xor1C bxor Uv1, Nonce, [Uv1, Uv0 | UvsC]}
                        end,
                        {0, 0, no_value, []},
                        Proof),
        case Xor0 bor Xor1 of
            0 ->
                %% follow cycle
                case follow_cycle(lists:reverse(Uvs), 0, 0, 0, 0) of
                    ?PROOFSIZE ->
                        true;
                    _ ->
                        ?POW_SHORT_CYCLE
                end;
            _ ->
                %% matching endpoints imply zero xors
                throw(?POW_NON_MATCHING)
        end
    catch
        throw:{error, Reason} ->
            lager:error("Proof verification failed for ~p: ~p", [Proof, Reason]),
            false
    end.

sipnode(K1, K2, Proof, UOrV) ->
    sipnode2(K1, K2, Proof, UOrV) bsl 1 bor UOrV.

sipnode2(K1, K2, Proof, UOrV) ->
    aec_siphash:hash(K1, K2, 2*Proof + UOrV) band get_edge_mask().

%%------------------------------------------------------------------------------
%% @doc
%%   Take a list of endpoint pairs forming edges (odd/even indices).
%%   Select the 1st endpoint (I = 0).
%%     Search (K: even values starting from 2) for matching endpoint value.
%%       If found, mark it with J.
%%         Search further. If another match found it is wrong: a branch found.
%%       If none found: error, dead end
%%     Now select the pair of the matching endpoint (the point where the edge
%%     is pointing to: I = J bxor 1) and continue from there.
%%   A solution is found if we return to the starting node (I = 0 again).
%%   Count and return the number of edges in the cycle
%% @end
%%------------------------------------------------------------------------------
-spec follow_cycle(list(integer()), integer(), integer(), integer(), integer()) ->
                          integer().
follow_cycle(Uvs, I, J, K, CycLength) ->
    case {(K + 2) rem (2 * ?PROOFSIZE), I} of
        {I, 0} ->
            %% must cycle back to start or we would have found branch
            CycLength;
        {I, J} ->
            %% no matching endpoint
            throw(?POW_DEAD_END);
        {I, _} ->
            INew = J bxor 1,
            follow_cycle(Uvs, INew, INew, INew, CycLength + 1);
        {K2, _} ->
            %% find other edge endpoint identical to one at i
            case {lists:nth(K, Uvs) =:= lists:uvs(I, Uvs), I} of
                {true, J} ->
                    %% already found one before
                    follow_cycle(Uvs, I, K2, K2, CycLength);
                {true, _} ->
                    throw(?POW_BRANCH);
                _ ->
                    follow_cycle(Uvs, I, J, K2, CycLength)
            end
    end.
