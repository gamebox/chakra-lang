#include "channel.h"

#include <stdlib.h>
#include <unistd.h>

#include "process.h"

channel_t *channel_new() {
  int fd1[2];

  if (pipe(fd1) == -1) {
    return NULL;
  }

  struct Channel *c = malloc(sizeof(channel_t));
  c->read_fd = fd1[0];
  c->write_fd = fd1[1];

  return c;
}

int channel_close(channel_t *c) {
  int f = close(c->read_fd);
  int s = close(c->write_fd);

  return f + s;
}