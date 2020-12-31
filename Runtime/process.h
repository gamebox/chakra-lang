#ifndef CHAKRA_PROCESS_H
#define CHAKRA_PROCESS_H

#include "actors.h"
#include "channel.h"

channel_t **process_coms;
// A process should only read their own buffer
int process_read(int process_id, envelope_t *buf, int timeout_ms);
int process_write(envelope_t env);

#endif