#include "../actors.h"
#include "../stdlib.h"

envelope_t *Chakra_stdlib__timeout(int ms_timeout, void *msg) {
  envelope_t *wrappeEnv = (envelope_t *)malloc(sizeof(envelope_t));
  *wrappeEnv = (envelope_t){.actor_id = Chakra_stdlib__self(), .msg = msg};
  timeout_command_t *cmd =
      (timeout_command_t *)malloc(sizeof(timeout_command_t));
  *cmd = (timeout_command_t){.timeout_ms = ms_timeout, .env = wrappeEnv};
  envelope_t *env = (envelope_t *)malloc(sizeof(envelope_t));
  *env = (envelope_t){.actor_id = {0, 0}, .msg = {"TIMEOUT", (void *)cmd}};

  return env;
}