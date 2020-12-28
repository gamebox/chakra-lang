#include "actors.h"

int actor_id_cmp(actor_id_t a, actor_id_t b) {
    if (a.process != b.process || a.entity != b.entity) {
        return 0;
    }

    return 1;
}