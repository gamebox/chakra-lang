#include "actors.h"
#include "run_table.h"

void child_process(int id) {
  envelope_t *buf;
  int next_entity = 1;
  run_table_t *run_table = run_table_new();
  actor_id_t self = {.process = id, .entity = 0};

  while (1) {
    int readResult = process_read(id, buf, 0);
    if (readResult == -1) {
      break;
    }

    if (readResult == 0) {
      break;
    }

    actor_id_t recipient_id = buf->actor_id;
    list__envelope_t *envs_to_handle;

    if (recipient_id.process == id && recipient_id.entity == 0) {
      // This is a message for the process itself, i.e., this is a command.

      // Create main process:
      // 1. Call the init function on main actor struct, passing in an actor id
      // for this process
      if (strcmp(buf->msg.type, "SPAWN") == 0) {
        // putts("Creating main actor");
        void **args = {(void *)&self};
        actor_t *Actor = (actor_t *)buf->msg.payload;
        turn_result_t *res = Actor->init(args);
        // 2. Store the state and def, as well as entity id in a running_actor_t
        running_actor_t *new_a =
            (running_actor_t *)malloc(sizeof(running_actor_t));
        *new_a =
            (running_actor_t){.state = res->state,
                              .def = Actor,
                              .id = {.process = id, .entity = next_entity++}};

        run_table_insert(new_a, run_table);
        // 3. Handle the result(set state, write messages out)
        envs_to_handle = res->envelopes;
      }
    } else {
      msg_t msg = buf->msg;
      running_actor_t *a = run_table_search(recipient_id, run_table);
      if (a == NULL) {
        continue;
      }
      turn_result_t *res = (a->def->receive(a->state, &msg));
      a->state = res->state;
      envs_to_handle = res->envelopes;
    }

    int i = 0;
    list__envelope_t *env_to_handle = envs_to_handle;
    while (env_to_handle != NULL) {
      process_write(env_to_handle->item);
      env_to_handle = env_to_handle->next;
    }

    envs_to_handle = NULL;
  }

  exit(0);
}