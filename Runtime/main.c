#include "main.h"

#include <string.h>

#include "actors.h"

/* main.chakra
= %( init )

%(
    spawn,
    print,
    kill,
    string,
) = /stdlib

%( other-actor ) = /root/other

%( join, from-num ) = string

init(caps) =
    %( stdio ) = caps
    (
        %Started( stdio ),
        [
            print(stdio, "Starting Up!"),
            spawn1(other-actor, 0)
        ]
    )

receive(state, msg) =
    ( state, msg ) ?
    | ( %Booted( stdio, other ),  ( "CURRENT", 100) ) ->
        (
            state,
            [
                print(stdio, "All done"),
                kill(other),
                kill(self())
            ]
        )
    | ( %Booted( stdio, ...), ( "CURRENT", n ) ) ->
        (
            state,
            [
                print(stdio, join("Got ", from-num(n))),
                timeout("WAKE")
            ]
        )
    | ( %Booted( other, ...), "WAKE" ) ->
        ( state, [ send(other, "INC") ] )
    | ( %Started( stdio, ...), ( "SPAWNED", other ) ) ->
        (
            %Booted(
                other,
                stdio
            ),
            []
        )
*/

typedef struct DISCRIMINATOR {
  long tag;
};

typedef struct Main_Actor__State_STARTED {
  long tag;
  actor_id_t stdio;
};

typedef struct Main_Actor__State_BOOTED {
  long tag;
  actor_id_t stdio;
  actor_id_t other;
};

envelope_t Chakra_stdlib__print(actor_id_t process_id, char *text) {
  envelope_t env = {.actor_id = process_id,
                    .msg = {.type = "PRINT", .payload = (void *)text}};

  return env;
};

envelope_t Chakra_stdlib__kill(actor_id_t process_id) {
  actor_id_t *id = (actor_id_t *)malloc(sizeof(actor_id_t));
  *id =
      (actor_id_t){.entity = process_id.entity, .process = process_id.process};
  envelope_t env = {.actor_id = {.entity = 0, .process = process_id.process},
                    .msg = {.type = "KILL", .payload = (void *)id}};

  return env;
};

envelope_t Chakra_stdlib__spawn(actor_id_t process_id, actor_t *actor) {
  envelope_t env = {.actor_id = process_id,
                    .msg = {.type = "SPAWN", .payload = (void *)actor}};

  return env;
};

turn_result_t *Main_Actor__init(void **args) {
  actor_id_t *process_id = (actor_id_t *)args[0];
  list__envelope_t *envs = (list__envelope_t *)malloc(sizeof(list__envelope_t));
  envs->item = Chakra_stdlib__spawn(*process_id, &OtherActor);

  int *state = (int *)malloc(sizeof(int));
  *state = 0;

  turn_result_t *res = malloc(sizeof(turn_result_t));
  res->state = (void *)state;
  res->envelopes = envs;

  return res;
}

turn_result_t *Main_Actor__receive(void *state, msg_t *msg) {
  //   ( state, msg ) ?
  // | ( %Booted( state, ...),  ( "CURRENT", 100) ) ->
  struct DISCRIMINATOR *d = (struct DISCRIMINATOR *)state;
  int *n = (int *)msg->payload;
  turn_result_t *res = malloc(sizeof(turn_result_t));

  if (d->tag == 1 && strcmp(msg->type, "CURRENT") == 0 &&
      *n == 100) {  // %Booted( stdio, other )
    //     (
    //         state,
    //         [
    //             print(state.stdio, "All done"),
    //             kill(state.other),
    //             kill(self())
    //         ]
    //     )
    }
  // | ( %Booted( state, ...), ( "CURRENT", n ) ) ->
  if (d->tag == 1 &&
      strcmp(msg->type, "CURRENT") == 0) {  // %Booted( stdio, other )
    //     (
    //         state,
    //         [
    //             print(state.stdio, join("Got ", from-num(n))),
    //             timeout("WAKE")
    //         ]
    //     )
  }
  // | ( %Booted( state, ...), "WAKE" ) ->
  if (d->tag == 1 &&
      strcmp(msg->type, "WAKE") == 0) {  // %Booted( stdio, other )
    //     ( state, [ send(state.other, "INC") ] )
  }
  // | ( %Started( stdio, ...), ( "SPAWNED", other ) ) ->
  if (d->tag == 0 && strcmp(msg->type, "SPAWNED") == 0) {
    //     (
    //         %Booted(
    //             other,
    //             stdio
    //         ),
    //         []
    //     )
    struct Main_Actor__State_STARTED *s =
        (struct Main_Actor__State_STARTED *)state;
    struct Main_Actor__State_BOOTED *s1 =
        (struct Main_Actor__State_BOOTED *)malloc(
            sizeof(struct Main_Actor__State_BOOTED));
    s1->tag = 1;
    s1->stdio = s->stdio;
    actor_id_t *other = (actor_id_t *)msg->payload;
    s1->other = *other;

    res->state = s1;
    res->envelopes = NULL;
  }
  return res;
}

actor_t MainActor = {.init = Main_Actor__init, .receive = Main_Actor__receive};

/* other.chakra
= %(
    init,
    receive,
)

%( send ) = /stdlib

init(initial) =
    ( initial, [] )

receive(state, msg) =
    (state, msg) ?
    | (_, "INC") -> %( 0 + 1, [] )
    | (_, "DEC") -> %( 0 - 1, [] )
    | (_, ("TELL", to, msg-type)) ->
        %(
            state,
            [
                send(to, ( msg-type, state ))
            ]
        )

*/
turn_result_t *Other_Actor__init(void **args) {
  int *initial = (int *)args[0];

  turn_result_t *res = malloc(sizeof(turn_result_t));
  res->state = (void *)initial;
  res->envelopes = NULL;

  return res;
}

turn_result_t *Other_Actor__receive(void *state, msg_t *msg) {
  int *s = (int *)state;
  if (strcmp(msg->type, "INC") == 0) {
    *s = *s + 1;
    turn_result_t *res = malloc(sizeof(turn_result_t));
    res->state = (void *)s;
    res->envelopes = NULL;

    return res;
  }

  if (strcmp(msg->type, "DEC") == 0) {
    *s = *s - 1;
    turn_result_t *res = malloc(sizeof(turn_result_t));
    res->state = (void *)s;
    res->envelopes = NULL;

    return res;
  }

  if (strcmp(msg->type, "TELL") == 0) {
    actor_id_t *to = (actor_id_t *)msg->payload;

    list__envelope_t *envs =
        (list__envelope_t *)malloc(sizeof(list__envelope_t));
    envs->item =
        (envelope_t){.actor_id = *to, .msg = {.type = "INC", .payload = NULL}};

    turn_result_t *res = malloc(sizeof(turn_result_t));
    res->state = state;
    res->envelopes = NULL;

    return res;
  }

  return NULL;
}

actor_t OtherActor = {.init = Other_Actor__init,
                      .receive = Other_Actor__receive};