#include <stdlib.h>

#include "../actors.h"

inline envelope_t *Chakra_stdlib__print(unsigned long long cap, char *text) {
  envelope_t *env = (envelope_t *)malloc(sizeof(envelope_t));
  *env = (envelope_t){.actor_id = NULL,
                      .msg = {.type = "PRINT", .payload = (void *)text}};

  return env;
};