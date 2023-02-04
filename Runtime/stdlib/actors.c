#include "../actors.h"

#include <stdio.h>
#include <stdlib.h>

#include "../main.h"
#include "../process.h"

char *SPAWN_REQUEST = "SPAWN_REQUEST";

envelope_t *Chakra_stdlib__kill(actor_id_t actor_id) {
  actor_id_t *id = (actor_id_t *)malloc(sizeof(actor_id_t));
  *id = (actor_id_t){.entity = actor_id.entity, .process = actor_id.process};
  envelope_t *env = (envelope_t *)malloc(sizeof(envelope_t));
  *env = (envelope_t){.actor_id = {.entity = 0, .process = actor_id.process},
                      .msg = {.type = "KILL", .payload = (void *)id}};

  return env;
};

envelope_t *Chakra_bootstrap(main_actor_t *actor, capabilities_t *caps) {
  // puts("Bootstrap");
  return actor->init(caps);
}

actor_id_t Chakra_stdlib__self() {
  actor_id_t *a = process_actor_running();
  if (a == NULL) {  // Must be parent
    return (actor_id_t){.process = 0, .entity = 0};
  }
  // actor_id_display(a);
  return *a;
}

envelope_t *Chakra_stdlib__spawn(actor_t *actor, void *arg0) {
  // printf("Actor <%p>", actor);
  actor_id_t self = Chakra_stdlib__self();
  actor_id_t recipient = {.process = self.process, .entity = 0};
  void **args = (void **)malloc(sizeof(void *));
  args[0] = arg0;
  spawn_request_t *spawn_request =
      (spawn_request_t *)malloc(sizeof(spawn_request_t));
  *spawn_request = (spawn_request_t){
      .def = actor, .init_args = args, .num_init_args = 1, .spawnee = self};
  envelope_t *env = (envelope_t *)malloc(sizeof(envelope_t));
  *env = (envelope_t){
      .actor_id = recipient,
      .msg = {.type = SPAWN_REQUEST, .payload = (void *)spawn_request}};

  return env;
};

envelope_t *Chakra_stdlib__send(actor_id_t actor_id, msg_t msg) {
  // printf("Sending to");
  // actor_id_display(&actor_id);
  envelope_t *env = (envelope_t *)malloc(sizeof(envelope_t));
  *env = (envelope_t){.actor_id = actor_id, .msg = msg};

  return env;
}