#include "run_table.h"

#include <stdio.h>

#include "actors.h"

const size_t TABLE_INIT_SIZE = 100;

run_table_t *run_table_new() {
  run_table_t *table = (run_table_t *)malloc(sizeof(run_table_t));
  actor_list_t **entries =
      (actor_list_t **)malloc(sizeof(actor_list_t *) * TABLE_INIT_SIZE);
  table->entries = entries;
  table->size = TABLE_INIT_SIZE;

  return table;
}

size_t hash_actor_id(actor_id_t id, size_t num) {
  size_t total = (id.entity + id.process);
  return total / num;
}

running_actor_t *run_table_search(actor_id_t id, run_table_t *table) {
  size_t hash = hash_actor_id(id, table->size);
  // puts("SEARCH");
  // actor_id_display(&id);
  // printf("HASH is %zu in table with %zu entries", hash, table->num_entries);
  actor_list_t *al = table->entries[hash];
  while (al != NULL) {
    if (al->actor != NULL && actor_id_cmp(al->actor->id, id) == 1) {
      return al->actor;
    }
    al = al->next;
  }

  return NULL;
}

void run_table_insert(running_actor_t *actor, run_table_t *table) {
  if (table->num_entries / table->size > .75) {
    // grow table by doubling
    fprintf(stderr, "TODO: IMPLEMENT RUN TABLE GROWING");
  }
  // printf("Inserting actor from <%p>", actor);
  size_t hash = hash_actor_id(actor->id, table->size);
  actor_list_t *al = table->entries[hash];
  while (al != NULL) {
    if (al->actor != NULL && actor_id_cmp(al->actor->id, actor->id)) {
      return;
    }
    al = al->next;
  }
  actor_list_t *new_node = (actor_list_t *)malloc(sizeof(actor_list_t));
  *new_node = (actor_list_t){.actor = actor, .next = table->entries[hash]};
  table->entries[hash] = new_node;
  table->num_entries++;
}

int run_table_delete(actor_id_t id, run_table_t *table) {
  size_t hash = hash_actor_id(id, table->size);
  actor_list_t *al = table->entries[hash];
  actor_list_t *prev = NULL;

  while (al != NULL) {
    if (al->actor != NULL && actor_id_cmp(al->actor->id, id)) {
      if (prev == NULL || prev->next == NULL) {
        table->entries[hash] = al->next;
      } else {
        prev->next = al->next;
      }
      table->num_entries--;
      return 1;
    }
    prev = al;
    al = al->next;
  }

  return 0;
}