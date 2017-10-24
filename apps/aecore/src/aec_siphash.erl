%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   SipHash-2-4 specialized to precomputed key and 8 byte nonces
%%% @end
%%%=============================================================================

-module(aec_siphash).

-export([create_keypair/2,
         hash/3]).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-define(MAX64, 16#ffffffffffffffff).

-type hashable() :: non_neg_integer().
-type siphash_key() :: non_neg_integer().
-type sip_quadruple() :: {non_neg_integer(), non_neg_integer(),
                          non_neg_integer(), non_neg_integer()}.%% in fact, uint64

-export_type([hashable/0,
              siphash_key/0]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%%   Generate a siphash keypair from a string/nonce pair
%% @end
%%------------------------------------------------------------------------------
-spec create_keypair(binary(), integer()) ->
                            {siphash_key(), siphash_key()}.
create_keypair(Header, Nonce) ->
    Header2 =
        case size(Header) of
            Sz when Sz < 76 ->
                ZSz = 8*(76 - Sz),
                <<Header/binary, 0:ZSz, Nonce:32/little-unsigned-integer>>;
            _ ->
                HSz = 8*76,
                <<H:HSz, _T/binary>> = Header,
                <<H:HSz, Nonce:32/little-unsigned-integer>>
        end,
    AuxHash = aux_hash(Header2),
    <<K1:64/little-unsigned-integer, K2:64/little-unsigned-integer, _/binary>> =
        AuxHash,
    {K1, K2}.

%%------------------------------------------------------------------------------
%% @doc
%%   Calculate the SipHash-2-4 of Nonce with two precomputed keys
%% @end
%%------------------------------------------------------------------------------
-spec hash(siphash_key(), siphash_key(), hashable()) -> hashable().
hash(K1, K2, Nonce) ->
    V0 = K1 bxor 16#736f6d6570736575,
    V1 = K2 bxor 16#646f72616e646f6d,
    V2 = K1 bxor 16#6c7967656e657261,
    V3 = K2 bxor 16#7465646279746573 bxor Nonce,
    {V01, V11, V21, V31} = sip_round(
                             sip_round(
                               sip_round(
                                 sip_round(
                                   sip_change(Nonce,
                                              sip_round(
                                                sip_round({V0, V1, V2, V3}))))))),
    ((V01 bxor V11) bxor (V21 bxor V31)) band 16#ffffffffffffffff.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% 1:
%% v0 += v1; v2 += v3; v1 = ROTL(v1,13); \
%% v3 = ROTL(v3,16);

%% 2:
%% v1 ^= v0; v3 ^= v2; \
%% v0 = ROTL(v0,32);

%% 3:
%% v2 += v1; v0 += v3; \
%% v1 = ROTL(v1,17);   v3 = ROTL(v3,21); \

%% 4:
%% v1 ^= v2; v3 ^= v0; v2 = ROTL(v2,32); \

-spec sip_round(sip_quadruple()) -> sip_quadruple().
sip_round({_V0, _V1, _V2, _V3} = Vs) ->
    sip_round4(sip_round3(sip_round2(sip_round1(Vs)))).

-spec sip_round1(sip_quadruple()) -> sip_quadruple().
sip_round1({V0, V1, V2, V3}) ->
    {(V0 + V1) band ?MAX64, rotl(V1, 13), (V2 + V3) band ?MAX64, rotl(V3, 16)}.

-spec sip_round2(sip_quadruple()) -> sip_quadruple().
sip_round2({V0, V1, V2, V3}) ->
    {rotl(V0, 32), V1 bxor V0, V2, V3 bxor V2}.

-spec sip_round3(sip_quadruple()) -> sip_quadruple().
sip_round3({V0, V1, V2, V3}) ->
    {(V0 + V3) band ?MAX64, rotl(V1, 17), (V2 + V1) band ?MAX64, rotl(V3, 21)}.

-spec sip_round4(sip_quadruple()) -> sip_quadruple().
sip_round4({V0, V1, V2, V3}) ->
    {V0, V1 bxor V2, rotl(V2, 32), V3 bxor V0}.

-spec sip_change(integer(), sip_quadruple()) -> sip_quadruple().
sip_change(Nonce, {V0, V1, V2, V3}) ->
    {V0 bxor Nonce, V1, V2 bxor 16#ff, V3}.

-spec rotl(integer(), integer()) -> integer().
rotl(X, B) ->
    ((X bsl B) bor (X bsr (64 - B))) band 16#ffffffffffffffff.

%%------------------------------------------------------------------------------
%% @doc
%%   This was blake2b in Tromp's original code.
%%   %% TODO: Use Blake2b here?
%% @end
%%------------------------------------------------------------------------------
-spec aux_hash(binary()) -> <<_:256>>.
aux_hash(Bin) ->
    aec_sha256:hash(Bin).
    %%aec_pow_cuckoo:blake2b_hash(Bin).
