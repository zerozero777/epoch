%%%=============================================================================
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%   Unit tests for the aec_pow_cuckoo module
%%% @end
%%%=============================================================================
-module(aec_siphash_tests).

-ifdef(TEST).

-export([reference_hash/3,
         reference_set_headernonce/2,
         blake2b_hash/1]).

-on_load(init/0).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aec_siphash).


keypair_test_() ->
    {setup,
     fun() ->
             ok
     end ,
     fun(_) ->
             ok
     end,
     [{"Create keypair",
       fun() ->
               ?assertMatch({_, _}, ?TEST_MODULE:create_keypair(<<"header 1 string">>, 1234453)),
               ?assertMatch({_, _}, ?TEST_MODULE:create_keypair(<<"header 1 stringsdjglkshgisdikdshgfjksdfuisfjuszdgfjhzdsfyjusfszjdgfsdjzfhdfhdfhjdfjhbgvjhgbfv">>, 1234453))
       end}
     ]
    }.

hash_test_() ->
    {setup,
     fun() -> ok end ,
     fun(_) -> ok end,
     [{"Calculate hash",
       fun() ->
               {K1, K2} = ?TEST_MODULE:create_keypair(<<"header 1 string">>, 1234453),
               N1 = 15535,
               Res1 = reference_hash(K1, K2, N1),
               ?assertEqual(Res1, ?TEST_MODULE:hash(K1, K2, N1)),

               N2 = 99786,
               Res2 = reference_hash(K1, K2, N2),
               ?assertEqual(Res2, ?TEST_MODULE:hash(K1, K2, N2)),

               N3 = 55222,
               Res3 = reference_hash(K1, K2, N3),
               ?assertEqual(Res3, ?TEST_MODULE:hash(K1, K2, N3)),

               N4 = 300012,
               Res4 = reference_hash(K1, K2, N4),
               ?assertEqual(Res4, ?TEST_MODULE:hash(K1, K2, N4))
       end}
     ]
    }.


%%%=============================================================================
%%% NIF initialization
%%%=============================================================================

init() ->
    ok = erlang:load_nif(filename:join([code:priv_dir(aecore), "siphash_test_nif"]), 0).

-spec reference_hash(integer(), integer(), binary()) -> binary().
reference_hash(_K1, _K2, _Nonce) ->
    erlang:nif_error(nif_library_not_loaded).

-spec reference_set_headernonce(binary(), integer()) -> {integer(), integer()}.
reference_set_headernonce(_Header, _Nonce) ->
    erlang:nif_error(nif_library_not_loaded).


-spec blake2b_hash(binary()) -> <<_:256>>.
blake2b_hash(_Bin) ->
    erlang:nif_error(nif_library_not_loaded).

-endif.
