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
      break;
    }

    if (readResult == 0) {
      break;
    }

    actor_id_t recipient_id = buf->actor_id;
    list__envelope_t *envs_to_handle = NULL;

    // puts("--------------------------------------------------");
    // printf("CHILD %d MSG RECEIVED\n", *id);
    // printf("%s\n", buf->msg.type);
    // actor_id_display(&buf->actor_id);
    // puts("--------------------------------------------------");

    if (recipient_id.process != *id) {
      fprintf(stderr, "The message is for process %d\n", recipient_id.process);
      continue;
    }

    if (recipient_id.entity == 0) {
      // This is a message for the process itself, i.e., this is a command.

      // Create main process:
      // 1. Call the init function on main actor struct, passing in an actor id
      // for this process
      if (strcmp(buf->msg.type, "SPAWN_REQUEST") == 0) {
        // puts("Creating actor");
        if (buf->msg.payload == NULL) {
          // puts("Request is NULL");
          break;
        } else {
          // printf("Request @ <%p>\n", buf->msg.payload);
        }
        spawn_request_t *spawn_request = (spawn_request_t *)buf->msg.payload;
        if (spawn_request == NULL) {
          // puts("Request is NULL");
          break;
        }
        actor_t *Actor = spawn_request->def;
        // printf("Actor->init @ <%p>\n", Actor);
        actor_id_t actor_id = process_add_actor(Actor, NULL);
        running_actor_t *ra = process_mount_actor(&actor_id);
        turn_result_t *res = Actor->init(spawn_request->init_args);
        // puts("Actor->init END");
        if (res == NULL || ra == NULL) {
          // puts("SOMETHING WENT WRONG");
          continue;
        }
        ra->state = res->state;
        // 2. Store the state and def, as well as entity id in a running_actor_t
        // 3. Handle the result(set state, write messages out)
        envs_to_handle = res->envelopes;
        actor_id_t *payload = (actor_id_t *)malloc(sizeof(actor_id_t));
        *payload = actor_id;
        msg_t spawnMsg = {"SPAWNED", payload};
        envelope_t *spawned_env =
            Chakra_stdlib__send(spawn_request->spawnee, spawnMsg);

        if (envs_to_handle == NULL) {
          list__envelope_t *e =
              (list__envelope_t *)malloc(sizeof(list__envelope_t));
          *e = (list__envelope_t){spawned_env, NULL};
          envs_to_handle = e;
        } else {
          list__envelope_t *e = envs_to_handle;
          while (e->next != NULL) {
            e = e->next;
          }
          list__envelope_t *e1 =
              (list__envelope_t *)malloc(sizeof(list__envelope_t));
          *e1 = (list__envelope_t){spawned_env, NULL};
          e->next = e1;
        }
        // puts("SPAWN COMPLETE");
      } else if (strcmp(buf->msg.type, "KILL") == 0) {
        // printf("[PROCESS %d]: KILL", *id);
        actor_id_t *id = (actor_id_t *)buf->msg.payload;
        actor_id_display(id);
        int delete_result = process_delete_actor(id);
        // printf("KILLED? %d", delete_result);
      }
    } else {
      // puts("MOUNTING");
      actor_id_display(&recipient_id);
      msg_t msg = buf->msg;
      running_actor_t *a = process_mount_actor(&recipient_id);
      if (a == NULL || a->def == NULL) {
        // puts("Missing def?");
        continue;
      }
      // puts("MOUNTED");
      turn_result_t *res = a->def->receive(a->state, &msg);
      a->state = res->state;
      envs_to_handle = res->envelopes;
    }

    list__envelope_t *env_to_handle = envs_to_handle;
    while (env_to_handle != NULL) {
      // puts("WRITING ENVELOPE");
      if (env_to_handle->item == NULL) {
        // puts("No envelope?");
        pthread_exit(NULL);
      }
      process_write(*env_to_handle->item);
      env_to_handle = env_to_handle->next;
    }

    envs_to_handle = NULL;
  }

  fprintf(stderr, "CHILD %d CRASHED", *id);

  exit(0);
}