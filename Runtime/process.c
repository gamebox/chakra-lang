#include "process.h"

#include <poll.h>
#include <pthread.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

#include "actors.h"
#include "channel.h"
#include "run_table.h"

process_coms_t *coms;
pthread_key_t process_id_key;

int get_process_id() {
  int *id = (int *)pthread_getspecific(process_id_key);
  if (id == NULL) {
    return 0;
  }
  return *id;
}

#define process_id get_process_id()

size_t TimeoutRead(int port, void *buf, size_t size, int mlsec_timeout) {
  // puts("TIMEOUTREAD");
  struct pollfd fd = {.fd = port, .events = POLLIN};

  size_t bytesread = 0;

  while (poll(&fd, 1, mlsec_timeout) == 1) {
    int chunksize = read(port, buf + bytesread, size);
    if (chunksize == -1) {
      return -1;
    }

    bytesread += chunksize;
    size -= chunksize;

    if (size == 0) {
      return bytesread;
    }

    fd.revents = 0;
  }

  // TODO: IsTimeout = true;
  return bytesread;
}

process_coms_t *process_coms_new(size_t num_procs) {
  pthread_key_create(&process_id_key, NULL);
  process_coms_t *coms = (process_coms_t *)malloc(sizeof(process_coms_t));
  channel_t **channels = (channel_t **)malloc(sizeof(channel_t *) * num_procs);
  actor_id_t **running_actors =
      (actor_id_t **)malloc(sizeof(actor_id_t *) * num_procs);
  run_table_t **run_tables =
      (run_table_t **)malloc(sizeof(run_table_t *) * num_procs);
  size_t *next_entities = (size_t *)malloc(sizeof(size_t) * num_procs);

  coms->num_procs = num_procs;
  for (size_t i = 0; i < num_procs; i++) {
    channels[i] = channel_new();
    running_actors[i] = NULL;
    run_tables[i] = run_table_new();
    next_entities[i] = 1;
  }
  coms->channels = channels;
  coms->running_actors = running_actors;
  coms->run_tables = run_tables;
  coms->next_entities = next_entities;

  return coms;
}

int process_register(int *id) {
  // printf("Registering process %d from <%p>\n", *id, id);
  return pthread_setspecific(process_id_key, id);
}

void process_coms_close() {
  if (process_id != 0) {
    return;
  }
  for (size_t i = 0; i < coms->num_procs; i++) {
    channel_close(coms->channels[i]);
    free(coms->channels[i]);
    free(coms->running_actors[i]);
  }
  free(coms);
}

void process_coms_display() {
  // printf("PROCESS_COMS @ <%p>\nProcess | Read FD | Write FD\n", coms);
  for (size_t i = 0; i < coms->num_procs; i++) {
    // printf("%2zu    |    %2d |      %2d\n", i, coms->channels[i]->read_fd,
    //       coms->channels[i]->write_fd);
  }
}

// A process should only read their own buffer
int process_read(envelope_t *buf, int timeout_ms) {
  size_t size = sizeof(envelope_t);
  // printf("[PROCESS %d]: READING %zu bytes from FD %d with %d ms timeout\n",
  //        process_id, size, coms->channels[process_id]->read_fd, timeout_ms);
  int res;
  res = timeout_ms > 0 ? TimeoutRead(coms->channels[process_id]->read_fd, buf,
                                     size, timeout_ms)
                       : read(coms->channels[process_id]->read_fd, buf, size);
  // if (res >= 0) {
  //   printf("[PROCESS %d]: READ\n", process_id);
  // }
  return res;
}

// A process may write to any other process, including themselves
int process_write(envelope_t env) {
  int write_fd;
  if (env.actor_id.process > coms->num_procs) {
    return -1;
  }
  // printf("[PROCESS %d]: Writing `%s` to PROCESS %d on FD %d\n", process_id,
  //        env.msg.type, env.actor_id.process,
  //        coms->channels[env.actor_id.process]->write_fd);
  int res = write(coms->channels[env.actor_id.process]->write_fd, &env,
                  sizeof(envelope_t));
  return res;
}

actor_id_t *process_actor_running() { return coms->running_actors[process_id]; }

actor_id_t process_add_actor(actor_t *actor, void *initial_state) {
  size_t next_entity = coms->next_entities[process_id];
  running_actor_t *new_a = (running_actor_t *)malloc(sizeof(running_actor_t));
  *new_a =
      (running_actor_t){.state = initial_state,
                        .def = actor,
                        .id = {.process = process_id, .entity = next_entity}};

  run_table_insert(new_a, coms->run_tables[process_id]);
  coms->next_entities[process_id]++;

  return new_a->id;
}

int process_delete_actor(actor_id_t *id) {
  int delete_result = run_table_delete(*id, coms->run_tables[process_id]);

  int total_num_running_actors = 0;
  for (int i = 0; i < coms->num_procs; i++) {
    total_num_running_actors =
        total_num_running_actors + coms->run_tables[i]->num_entries;
  }

  if (total_num_running_actors == 0) {
    // puts("PROGRAM SHOULD DIE");
    exit(0);
  } else {
    // printf("%d actors left running after delete", total_num_running_actors);
  }

  return delete_result;
}

running_actor_t *process_mount_actor(actor_id_t *id) {
  // puts("------------");
  // puts("Mounting");
  // actor_id_display(id);
  // puts("------------");

  running_actor_t *a = run_table_search(*id, coms->run_tables[process_id]);
  if (a == NULL) {
    return NULL;
  }
  coms->running_actors[process_id] = id;

  return a;
}