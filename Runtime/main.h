#ifndef CHAKRA_MAIN_H
#define CHAKRA_MAIN_H

#include "actors.h"

typedef unsigned long long i64;

typedef struct Capabilities {
  i64 stdio;
} capabilities_t;

extern const capabilities_t Capabilities;

typedef struct MainActor {
  envelope_t *(*init)(capabilities_t *);
} main_actor_t;

extern main_actor_t MainActor;
#endif