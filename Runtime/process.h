#ifndef CHAKRA_PROCESS_H
#define CHAKRA_PROCESS_H

#include "actors.h"
#include "channel.h"
#include "run_table.h"

typedef struct ProcessComs {
    size_t num_procs;
    channel_t **channels;
    actor_id_t **running_actors;
    run_table_t **run_tables;
    size_t *next_entities;
    size_t num_reading;
} process_coms_t;

extern process_coms_t *coms;

// A process should only read their own buffer
process_coms_t *process_coms_new(size_t num_procs);
int process_register(int *id);
void process_coms_close();
void process_coms_display();
int process_read(envelope_t *buf, int timeout_ms);
int process_write(envelope_t env);
actor_id_t *process_actor_running();
actor_id_t process_add_actor(actor_t *actor, void *initial_state);
int process_delete_actor(actor_id_t *id);
running_actor_t *process_mount_actor(actor_id_t *id);

#endif