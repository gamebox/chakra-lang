/*
 * Types related to Actors
 */
#ifndef CHAKRA_ACTORS_H
#define CHAKRA_ACTORS_H

#include <stdlib.h>

typedef struct ActorId {
  size_t process;
  size_t entity;
} actor_id_t;

typedef struct Msg {
  char *type;
  void *payload;
} msg_t;

typedef struct Envelope {
  actor_id_t actor_id;
  msg_t msg;
} envelope_t;

typedef struct List__Envelope_t {
  envelope_t *item;
  struct List__Envelope_t *next;
} list__envelope_t;

typedef struct TurnResult {
  void *state;
  envelope_t *envelope;
} turn_result_t;

typedef turn_result_t *(actor_receive_t)(void **state, msg_t *msg);

typedef struct Actor {
  turn_result_t *(*init)(void **args);
  actor_receive_t *receive;
} actor_t;

typedef struct RunningActor {
  actor_id_t id;
  actor_t *def;
  void *state;
} running_actor_t;

typedef struct SpawnRequest {
  actor_id_t spawnee;
  actor_t *def;
  int num_init_args;
  void **init_args;
} spawn_request_t;

void actor_id_display(actor_id_t *a);
int actor_id_cmp(actor_id_t a, actor_id_t b);

#endif