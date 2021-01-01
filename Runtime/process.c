#include <poll.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

#include "actors.h"
#include "channel.h"

size_t TimeoutRead(int port, void *buf, size_t size, int mlsec_timeout) {
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

channel_t **process_coms;
// A process should only read their own buffer
int process_read(int process_id, envelope_t *buf, int timeout_ms) {
  int res;
  res = timeout_ms
            ? TimeoutRead(process_coms[process_id]->read_fd, buf,
                          sizeof(envelope_t), timeout_ms)
            : read(process_coms[process_id]->read_fd, buf, sizeof(envelope_t));
  return res;
}

// A process may write to any other process, including themselves
int process_write(envelope_t env) {
  envelope_t buf[1] = {env};
  int res = write(process_coms[env.actor_id.process]->write_fd, buf,
                  sizeof(envelope_t));
  return res;
}