/*
* Types related to Actors
*/
#include <stdlib.h>

#ifndef __CHAKRA_ACTORS_H
#define __CHAKRA_ACTORS_H
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

typedef struct TurnResult {
    void *state;
    envelope_t *envelopes;
    size_t num_msgs;
} turn_result_t;

typedef struct Actor {
    turn_result_t* (*init)(void **args);
    turn_result_t* (*receive)(void *state, msg_t *msg);
} actor_t;


typedef struct RunningActor {
    actor_id_t id;
    actor_t *def;
    void *state;
} running_actor_t;


int actor_id_cmp(actor_id_t a, actor_id_t b);

#endif