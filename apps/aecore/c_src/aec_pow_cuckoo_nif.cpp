//-------------------------------------------------------------------
// Copyright (C) 2017, Aeternity Anstalt
// Based on Cuckoo Cycle of John Tromp, a memory-hard proof-of-work
//-------------------------------------------------------------------

#include <inttypes.h> // for SCNx64 macro
#include <stdio.h>    // printf/scanf
#include <stdlib.h>   // exit
#include <unistd.h>   // getopt
#include <assert.h>
#include <string.h>
#include <erl_nif.h>
#include "cuckoo_base.h"


extern node_t* generate(u64 key0, u64 key1, int ntrims, int nthreads);
extern int verify(u64 key0, u64 key1, node_t soln[PROOFSIZE]);

int read_solution(ErlNifEnv* env, const ERL_NIF_TERM e_soln, node_t soln[PROOFSIZE]);
int get_uint64(ErlNifEnv* env, const ERL_NIF_TERM from, uint64_t* to);


///=============================================================================
/// API
///=============================================================================

static ERL_NIF_TERM generate_single_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  u64 key0, key1;
  int ntrims, nthreads;

  // decode args
  if (argc != 4 ||
      !get_uint64(env, argv[0], &key0) ||
      !get_uint64(env, argv[1], &key1) ||
      !enif_get_int(env, argv[2], &ntrims) ||
      !enif_get_int(env, argv[3], &nthreads))
    return enif_make_badarg(env);

  node_t* result = generate(key0, key1, ntrims, nthreads);

  if (result) {
    // success: encode result
    ERL_NIF_TERM arr[PROOFSIZE];
    for (int i2 = 0; i2 < PROOFSIZE; i2++) {
#if EDGEBITS > 31
      arr[i2] = enif_make_uint64(env, result[i2]);
#else
      arr[i2] = enif_make_uint(env, result[i2]);
#endif
    }
    ERL_NIF_TERM e_soln = enif_make_list_from_array(env, arr, PROOFSIZE);

    delete[] result;

    // solution found: return {ok, Solution}
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), e_soln);
  } else {
    // failed to find solution, return {error, no_solutions}
    ERL_NIF_TERM failure = enif_make_tuple2(env,
                                            enif_make_atom(env, "error"),
                                            enif_make_atom(env, "no_solutions"));
    return failure;
  }
}

static ERL_NIF_TERM verify_proof_c_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  u64 key0, key1;
  node_t soln[PROOFSIZE];

  // decode args
  if (argc != 3 ||
      !get_uint64(env, argv[0], &key0) ||
      !get_uint64(env, argv[1], &key1) ||
      !read_solution(env, argv[2], soln))
    return enif_make_badarg(env);

  int result = verify(key0, key1, soln);

  if (result != 0)
    return enif_make_atom(env, "true");
  else
    return enif_make_atom(env, "false");
}

static ERL_NIF_TERM get_node_size_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  int size = sizeof(node_t);
  return enif_make_int(env, size);
}

static ERL_NIF_TERM get_edge_mask_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
  int emask = EDGEMASK;
  return enif_make_int(env, emask);
}

static ErlNifFunc nif_funcs[] = {
  {"generate_single", 4, generate_single_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  {"verify_proof_c",  3, verify_proof_c_nif,  0},
  {"get_node_size",   0, get_node_size_nif,   0},
  {"get_edge_mask",   0, get_edge_mask_nif,   0}
};

ERL_NIF_INIT(aec_pow_cuckoo, nif_funcs, NULL, NULL, NULL, NULL);

///=============================================================================
/// Internal functions
///=============================================================================

int read_solution(ErlNifEnv* env, const ERL_NIF_TERM e_soln, node_t c_soln[PROOFSIZE]) {
  ERL_NIF_TERM term, head, tail;

  term = e_soln;
  for (int i = 0; i < PROOFSIZE; i++) {
    if (!enif_get_list_cell(env, term, &head, &tail))
      return false;
    c_soln[i] = head;
    if (
#if EDGEBITS > 31
        !get_uint64(env, head, c_soln + i)
#else
        !enif_get_uint(env, head, c_soln + i)
#endif
        )
      return false;
    term = tail;
  }

  return true;
}

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
