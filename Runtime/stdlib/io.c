#include "../actors.h"
#include "../stdlib.h"

envelope_t *Chakra_stdlib__print(unsigned long long cap, char *text) {
  envelope_t *env = (envelope_t *)malloc(sizeof(envelope_t));
  *env = (envelope_t){.actor_id = {0, 0},
                      .msg = {.type = "PRINT", .payload = (void *)text}};

  return env;
};

const stdlib__io_t Chakra_stdlib__io = {.print = Chakra_stdlib__print};