#ifndef CHAKRA_MAIN_H
#define CHAKRA_MAIN_H

#include "actors.h"

typedef unsigned long long i64;

typedef struct Capabilites {
  i64 stdio;
} capabilities_t;

const capabilities_t Capabilities = {.stdio = 0};

typedef struct MainActor {
  envelope_t *(*init)(capabilities_t *)
} main_actor_t;

extern main_actor_t MainActor;

extern envelope_t *(*Chakra_boostrap)(main_actor_t *, capabilities_t *);
#endif