#include <stdlib.h>

#include "../actors.h"

envelope_t Chakra_stdlib__print(actor_id_t process_id, char *text) {
  envelope_t env = {.actor_id = process_id,
                    .msg = {.type = "PRINT", .payload = (void *)text}};

  return env;
};