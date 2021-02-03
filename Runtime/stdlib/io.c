#include <stdlib.h>

#include "../actors.h"

inline envelope_t *Chakra_stdlib__print(actor_id_t process_id, char *text) {
  envelope_t *env = (envelope_t *)malloc(sizeof(envelope_t));
  *env = (envelope_t){.actor_id = process_id,
                      .msg = {.type = "PRINT", .payload = (void *)text}};

  return env;
};