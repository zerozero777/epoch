//-------------------------------------------------------------------
// Copyright (C) 2017, Aeternity Anstalt
// Based on Cuckoo Cycle of John Tromp, a memory-hard proof-of-work:
// https://github.com/tromp/cuckoo
//-------------------------------------------------------------------

#include <inttypes.h> // for SCNx64 macro
#include <stdio.h>    // printf/scanf
#include <stdlib.h>   // exit
#include <unistd.h>   // getopt
#include <assert.h>
#include <string.h>
#include "siphash.h"
#include "mean_miner.hpp"


// arbitrary length of header hashed into siphash key
#define HEADERLEN 80

int aec_printf(const char *format, ...);

node_t* generate(u64 key0, u64 key1, int ntrims, int nthreads) {
  printf("Looking for %d-cycle on cuckoo%d", PROOFSIZE, NODEBITS);
  printf(" with 50%% edges\n");
  bool showcycle = 0;
  bool allrounds = false;

  solver_ctx ctx(nthreads, ntrims & -2, allrounds, showcycle);

  u64 sbytes = ctx.sharedbytes();
  u32 tbytes = ctx.threadbytes();
  int sunit,tunit;
  for (sunit=0; sbytes >= 10240; sbytes>>=10,sunit++) ;
  for (tunit=0; tbytes >= 10240; tbytes>>=10,tunit++) ;
  printf("Using %d%cB bucket memory at %lx,\n", sbytes, " KMGT"[sunit], (u64)ctx.trimmer->buckets);
  printf("%dx%d%cB thread memory at %lx,\n", nthreads, tbytes, " KMGT"[tunit],
         (u64)ctx.trimmer->tbuckets);
  printf("%d-way siphash, and %d buckets.\n", NSIPHASH, NX);

  // Set keys in context
  ctx.trimmer->sip_keys.k0 = key0;
  ctx.trimmer->sip_keys.k1 = key1;
  printf("k0 k1 %lx %lx\n", ctx.trimmer->sip_keys.k0, ctx.trimmer->sip_keys.k1);
  u32 nsols = ctx.solve();

  if (nsols == 0)
    return NULL;
  else {
    node_t* result = new node_t[PROOFSIZE];
    printf("Solution");
    u32* prf = &ctx.sols[0];
    for (u32 i = 0; i < PROOFSIZE; i++) {
      printf(" %jx", (uintmax_t)prf[i]);
      result[i] = prf[i];
    }
    printf("\n");

    return result;
  }
}

int verify(u64 key0, u64 key1, node_t soln[PROOFSIZE]) {
  // Initiate a context with keys
  solver_ctx ctx(1, 2, false, 0);
  ctx.trimmer->sip_keys.k0 = key0;
  ctx.trimmer->sip_keys.k1 = key1;
  printf("k0 %llx k1 %llx\n", ctx.trimmer->sip_keys.k0, ctx.trimmer->sip_keys.k1);

  edge_t nonces[PROOFSIZE];
  for (int i = 0; i < PROOFSIZE; i++) {
    nonces[i] = soln[i];
  }

  int pow_rc = verify(nonces, &ctx.trimmer->sip_keys);

  if (pow_rc == POW_OK) {
#ifdef DEBUG
    printf("Verified with cyclehash ");
    unsigned char cyclehash[32];
    blake2b((void *)cyclehash, sizeof(cyclehash), (const void *)nonces, sizeof(nonces), 0, 0);
    for (int i=0; i<32; i++)
      printf("%02x", cyclehash[i]);
      printf("\n");
#endif
    return true;
  } else {
    aec_printf("FAILED due to %s\n", errstr[pow_rc]);

    return false;
  }
}

int aec_printf(const char *format, ...)
{
#ifdef DEBUG
   va_list arg;
   int done;

   va_start (arg, format);
   done = vfprintf (stdout, format, arg);
   va_end (arg);

   return done;
#else
   return 0;
#endif
}
