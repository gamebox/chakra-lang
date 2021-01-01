#ifndef CHAKRA_STDLIB_H
#define CHAKRA_STDLIB_H

#include "actors.h"

envelope_t Chakra_stdlib__print(actor_id_t process_id, char *text);
envelope_t Chakra_stdlib__kill(actor_id_t process_id);
envelope_t Chakra_stdlib__spawn(actor_id_t process_id, actor_t *actor);
envelope_t Chakra_stdlib__send(actor_id_t actor_id, void *msg);
envelope_t Chakra_stdlib__timeout(int ms_timeout, void *msg);
actor_id_t Chakra_stdlib__self();

#endif