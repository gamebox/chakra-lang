#include "main.h"

#include <stdio.h>
#include <string.h>

#include "actors.h"
#include "stdlib.h"

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
} DISCRIMINATOR_t;

typedef struct Main_Actor__State_STARTED {
  long tag;
  actor_id_t stdio;
} Main_Actor__State_STARTED_t;

typedef struct Main_Actor__State_BOOTED {
  long tag;
  actor_id_t stdio;
  actor_id_t other;
} Main_Actor__State_BOOTED_t;

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
  turn_result_t *res = malloc(sizeof(turn_result_t));

  if (d->tag == 1 && strcmp(msg->type, "CURRENT") == 0 &&
      *((int *)msg->payload) == 100) {  // %Booted( stdio, other )
    //     (
    //         state,
    //         [
    //             print(state.stdio, "All done"),
    //             kill(state.other),
    //             kill(self())
    //         ]
    //     )
    Main_Actor__State_BOOTED_t *s = (Main_Actor__State_BOOTED_t *)state;
    list__envelope_t *env2 =
        (list__envelope_t *)malloc(sizeof(list__envelope_t));
    *env2 = (list__envelope_t){
        .item = Chakra_stdlib__kill(Chakra_stdlib__self()), .next = NULL};
    list__envelope_t *env1 =
        (list__envelope_t *)malloc(sizeof(list__envelope_t));
    *env1 =
        (list__envelope_t){.item = Chakra_stdlib__kill(s->other), .next = env2};
    list__envelope_t *envs =
        (list__envelope_t *)malloc(sizeof(list__envelope_t));
    *envs = (list__envelope_t){
        .item = Chakra_stdlib__print(s->stdio, "All done"), .next = env1};

    res->state = state;
    res->envelopes = envs;

    return res;
  }
  // | ( %Booted( state, ...), ( "CURRENT", n ) ) ->
  if (d->tag == 1 &&
      strcmp(msg->type, "CURRENT") == 0) {  // %Booted( stdio, other )
    //     (
    //         state,
    //         [
    //             print(state.stdio, join("Got ", from-num(n))),
    //             timeout(1000, "WAKE")
    //         ]
    //     )
    Main_Actor__State_BOOTED_t *s = (Main_Actor__State_BOOTED_t *)state;
    list__envelope_t *env1 =
        (list__envelope_t *)malloc(sizeof(list__envelope_t));
    *env1 = (list__envelope_t){
        .item = Chakra_stdlib__timeout(1000, (void *)"WAKE"), .next = NULL};
    list__envelope_t *envs =
        (list__envelope_t *)malloc(sizeof(list__envelope_t));
    char text[100];
    int *n = (int *)msg->payload;
    sprintf(text, "Got %d", *n);
    *envs = (list__envelope_t){.item = Chakra_stdlib__print(s->stdio, text),
                               .next = env1};
    res->state = state;
    res->envelopes = envs;
    return res;
  }
  // | ( %Booted( state, ...), "WAKE" ) ->
  if (d->tag == 1 &&
      strcmp(msg->type, "WAKE") == 0) {  // %Booted( stdio, other )
    //     ( state, [ send(state.other, "INC") ] )
    Main_Actor__State_BOOTED_t *s = (Main_Actor__State_BOOTED_t *)state;
    list__envelope_t *envs =
        (list__envelope_t *)malloc(sizeof(list__envelope_t));
    *envs = (list__envelope_t){
        .item = Chakra_stdlib__send(s->other, (void *)"INC"), .next = NULL};
    res->state = state;
    res->envelopes = envs;
    return res;
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
    | (_, "INC") -> ( 0 + 1, [] )
    | (_, "DEC") -> ( 0 - 1, [] )
    | (_, ("TELL", to, msg-type)) ->
        (
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