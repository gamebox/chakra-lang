#include "../actors.h"

#include <stdlib.h>

#include "../process.h"

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

envelope_t Chakra_stdlib__send(actor_id_t actor_id, void *msg) {
  return (envelope_t){};
}
actor_id_t Chakra_stdlib__self() { return (actor_id_t){}; }