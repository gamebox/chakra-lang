#include <pthread.h>
#include <stdio.h>
#include <string.h>

#include "actors.h"
#include "process.h"
#include "run_table.h"
#include "stdlib.h"

void *child_process(void *arg) {
  int *id = (int *)arg;
  if (id == NULL) {
    // puts("ID is NULL");
    exit(1);
  }
  envelope_t *buf = (envelope_t *)malloc(sizeof(envelope_t));
  process_register(id);
  actor_id_t self = {.process = *id, .entity = 0};

  while (1) {
    int readResult = process_read(buf, 0);
    if (readResult == -1) {
      // printf("Should die: %i", *id);
      break;
    }

    if (readResult == 0) {
      // puts("Wut?");
      break;
    }

    actor_id_t recipient_id = buf->actor_id;
    envelope_t *env_to_handle = NULL;

    if (recipient_id.process == 999) {
      // puts("Received kill message");
      break;
    }

    if (recipient_id.process != *id) {
      // puts("not for me");
      continue;
    }

    // puts("Child got message");
    actor_id_display(&recipient_id);
    puts(buf->msg.type);

    if (recipient_id.entity == 0) {
      // This is a message for the process itself, i.e., this is a command.

      // Create main process:
      // 1. Call the init function on main actor struct, passing in an actor id
      // for this process
      if (strcmp(buf->msg.type, "SPAWN_REQUEST") == 0) {
        if (buf->msg.payload == NULL) {
          break;
        }
        spawn_request_t *spawn_request = (spawn_request_t *)buf->msg.payload;
        if (spawn_request == NULL) {
          break;
        }
        actor_t *Actor = spawn_request->def;
        actor_id_t actor_id = process_add_actor(Actor, NULL);
        running_actor_t *ra = process_mount_actor(&actor_id);
        turn_result_t *res = Actor->init(spawn_request->init_args);
        if (res == NULL || ra == NULL) {
          continue;
        }
        ra->state = res->state;
        // 2. Store the state and def, as well as entity id in a running_actor_t
        // 3. Handle the result(set state, write messages out)
        env_to_handle = res->envelope;
        actor_id_t *payload = (actor_id_t *)malloc(sizeof(actor_id_t));
        *payload = actor_id;
        msg_t spawnMsg = {"SPAWNED", payload};
        envelope_t *spawned_env =
            Chakra_stdlib__send(spawn_request->spawnee, spawnMsg);

        env_to_handle = spawned_env;
      } else if (strcmp(buf->msg.type, "KILL") == 0) {
        actor_id_t *id = (actor_id_t *)buf->msg.payload;
        int delete_result = process_delete_actor(id);
      }
    } else {
      // Mounting
      msg_t msg = buf->msg;
      running_actor_t *a = process_mount_actor(&recipient_id);
      if (a == NULL || a->def == NULL || a->def->receive == NULL) {
        // puts("Wah wah");
        continue;
      }

      turn_result_t *res = a->def->receive(a->state, &msg);
      a->state = res->state;
      env_to_handle = res->envelope;
    }

    process_write(*env_to_handle);

    env_to_handle = NULL;
  }

  // fprintf(stderr, "CHILD %d CRASHED\n", *id);
  // puts("Exiting");
  exit(0);
}