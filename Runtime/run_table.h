/*
 * A hashtable implementation for storing references to Actor instances
 * currently running on a process.
 */
#include <stdlib.h>

#include "actors.h"

#ifndef CHAKRA_RUN_TABLE_H
#define CHAKRA_RUN_TABLE_H

typedef struct ActorList {
  running_actor_t *actor;
  struct ActorList *next;
} actor_list_t;

typedef struct RunTable {
  size_t size;
  size_t num_entries;
  actor_list_t **entries;
} run_table_t;

run_table_t *run_table_new();
running_actor_t *run_table_search(actor_id_t id, run_table_t *table);
void run_table_insert(running_actor_t *actor, run_table_t *table);
int run_table_delete(actor_id_t id, run_table_t *table);

#endif