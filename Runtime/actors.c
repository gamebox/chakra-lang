#include "actors.h"

#include <stdio.h>

int actor_id_cmp(actor_id_t a, actor_id_t b) {
  if (a.process != b.process || a.entity != b.entity) {
    return 0;
  }

  return 1;
}

void actor_id_display(actor_id_t *a) {
  // printf("Actor Id <%zu.%zu>\n", a->process, a->entity);
}