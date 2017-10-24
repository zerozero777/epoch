///=============================================================================
/// @copyright (C) 2017, Aeternity Anstalt
/// @doc
///   Reference functions for testing the aec_hiphash module
/// @end
///=============================================================================

// #include <inttypes.h> // for SCNx64 macro
#include <stdio.h>    // printf/scanf
// #include <stdlib.h>   // exit
// #include <unistd.h>   // getopt
// #include <assert.h>
#include <string.h>
#include <erl_nif.h>
#include "siphash.h"
#include "cuckoo.h"

#define HEADERLEN 80

int get_uint64(ErlNifEnv* env, const ERL_NIF_TERM from, uint64_t* to);

///=============================================================================
/// API
///=============================================================================

static ERL_NIF_TERM reference_hash_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  u64 k1, k2, nonce;

  if (argc != 3 ||
      !get_uint64(env, argv[0], &k1) ||
      !get_uint64(env, argv[1], &k2) ||
      !get_uint64(env, argv[2], &nonce))
    return enif_make_badarg(env);

  siphash_keys keys;
  keys.k0 = k1;
  keys.k1 = k2;

  u64 res = siphash24(&keys, nonce);
  return enif_make_uint64(env, res);
}

static ERL_NIF_TERM reference_set_headernonce_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    u64 nonce;
    ErlNifBinary in;

    if (argc != 2 ||
        !enif_inspect_binary(env, argv[0], &in) ||
        !get_uint64(env, argv[1], &nonce))
    return enif_make_badarg(env);

    char headernonce[HEADERLEN];
    u32 hdrlen = in.size;
    memcpy(headernonce, in.data, hdrlen);
    memset(headernonce + hdrlen, 0, sizeof(headernonce) - hdrlen);
    ((u32 *)headernonce)[HEADERLEN/sizeof(u32)-1] = htole32(nonce);
    siphash_keys keys;
    setheader(headernonce, sizeof(headernonce), &keys);

    return enif_make_tuple2(env, enif_make_uint64(env, keys.k0),
                            enif_make_uint64(env, keys.k1));
}

static ERL_NIF_TERM blake2b_hash_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary in, out;
  char header[HEADERLEN];

   if (argc != 1 ||
       !enif_inspect_binary(env, argv[0], &in) ||
       in.size == 0) {
      return enif_make_badarg(env);
   }

   size_t hlen;
   if (in.size < HEADERLEN) {
    hlen = in.size;
    memset(header, 0, HEADERLEN);
   } else {
     hlen = HEADERLEN;
   }
   memcpy(header, in.data, hlen);

   char hdrkey[32];
   blake2b((void *)hdrkey, sizeof(hdrkey), (const void *)header, HEADERLEN, 0, 0);

   enif_alloc_binary(32, &out);
   for (int i = 0; i < 32; i++)
     out.data[i] = hdrkey[i];

   return enif_make_binary(env, &out);
}

static ErlNifFunc nif_funcs[] = {
  {"reference_hash",            3, reference_hash_nif,            0},
  {"reference_set_headernonce", 2, reference_set_headernonce_nif, 0},
  {"blake2b_hash",              1, blake2b_hash_nif,              0}
};

ERL_NIF_INIT(aec_siphash_tests, nif_funcs, NULL, NULL, NULL, NULL);

// Fix for clang: force cast from u64 (unsigned long long) to unsigned long on
// 64-bit architectures
int get_uint64(ErlNifEnv* env, const ERL_NIF_TERM from, uint64_t* to) {
  int result;
#if SIZEOF_LONG == 8
  result = enif_get_ulong(env, from, (unsigned long *)to);
#else
  result = enif_get_uint64(env, from, to);
#endif
  return result;
}
