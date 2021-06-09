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
        (
          state,
          [
            send(other, "INC"),
            send(other, ("TELL", self()))
          ]
        )
    | ( %Started( stdio, ...), ( "SPAWNED", other ) ) ->
        (
            %Booted(
                other,
                stdio
            ),
            [ send(other, ("TELL", self())) ]
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
  // actor_id_t self = Chakra_stdlib__self();
  // actor_id_display(&self);
  actor_id_t *actor_id = (actor_id_t *)args[0];
  list__envelope_t *envs = (list__envelope_t *)malloc(sizeof(list__envelope_t));
  int *init = (int *)malloc(sizeof(int));
  *init = 0;
  envelope_t *env = Chakra_stdlib__spawn1(&OtherActor, (void *)init);
  envs->item = env;

  int *state = (int *)malloc(sizeof(int));
  *state = 0;

  turn_result_t *res = malloc(sizeof(turn_result_t));
  res->state = (void *)state;
  res->envelopes = envs;

  return res;
}

turn_result_t *Main_Actor__receive(void *state, msg_t *msg) {
  // printf("Main_Actor__receive: %s\n", msg->type);
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
    char *text = (char *)malloc(9);
    strcpy(text, "All done");
    *envs = (list__envelope_t){.item = Chakra_stdlib__print(s->stdio, text),
                               .next = env1};

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
    // puts("CURRENT");
    int *n = (int *)msg->payload;
    Main_Actor__State_BOOTED_t *s = (Main_Actor__State_BOOTED_t *)state;
    list__envelope_t *env1 =
        (list__envelope_t *)malloc(sizeof(list__envelope_t));
    *env1 = (list__envelope_t){
        .item = Chakra_stdlib__timeout(1000, (void *)"WAKE"), .next = NULL};
    list__envelope_t *envs =
        (list__envelope_t *)malloc(sizeof(list__envelope_t));
    char *text = (char *)malloc(10);
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
    list__envelope_t *env1 =
        (list__envelope_t *)malloc(sizeof(list__envelope_t));
    actor_id_t *self = (actor_id_t *)malloc(sizeof(actor_id_t));
    *self = Chakra_stdlib__self();
    msg_t msg1 = {"TELL", (void *)self};
    *env1 = (list__envelope_t){.item = Chakra_stdlib__send(s->other, msg1),
                               .next = NULL};
    list__envelope_t *envs =
        (list__envelope_t *)malloc(sizeof(list__envelope_t));
    msg_t msg = {"INC", NULL};
    *envs = (list__envelope_t){.item = Chakra_stdlib__send(s->other, msg),
                               .next = env1};
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
    // puts("SPAWNED");
    struct Main_Actor__State_STARTED *s =
        (struct Main_Actor__State_STARTED *)state;
    struct Main_Actor__State_BOOTED *s1 =
        (struct Main_Actor__State_BOOTED *)malloc(
            sizeof(struct Main_Actor__State_BOOTED));
    s1->tag = 1;
    s1->stdio = s->stdio;
    actor_id_t *other = (actor_id_t *)msg->payload;
    // puts("OTHER is ");
    actor_id_display(other);
    s1->other = *other;

    list__envelope_t *envs =
        (list__envelope_t *)malloc(sizeof(list__envelope_t));
    actor_id_t *self = (actor_id_t *)malloc(sizeof(actor_id_t));
    *self = Chakra_stdlib__self();
    msg_t msg = {"TELL", (void *)self};
    *envs = (list__envelope_t){.item = Chakra_stdlib__send(*other, msg),
                               .next = NULL};

    res->state = s1;
    res->envelopes = envs;
  } else {
    res = NULL;
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
    | (_, "INC") -> ( add(0, 1), [] )
    | (_, "DEC") -> ( sub(0, 1), [] )
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
  // printf("Other_Actor__receive: %s state is %d @ <%p>\n", msg->type, *s, s);
  if (strcmp(msg->type, "INC") == 0) {
    int newState = *s + 1;
    *s = newState;
    // printf("INC state is now %d\n", *s);
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
    envelope_t *env = (envelope_t *)malloc(sizeof(envelope_t));
    *env =
        (envelope_t){.actor_id = *to, .msg = {.type = "INC", .payload = NULL}};
    envs->item = env;

    turn_result_t *res = malloc(sizeof(turn_result_t));
    msg_t msg = {"CURRENT", state};
    *envs =
        (list__envelope_t){.item = Chakra_stdlib__send(*to, msg), .next = NULL};
    res->state = state;
    res->envelopes = envs;

    return res;
  }

  return NULL;
}

actor_t OtherActor = {.init = Other_Actor__init,
                      .receive = Other_Actor__receive};