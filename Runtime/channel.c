#include <stdlib.h>
#include <unistd.h>

#include "channel.h"

void channel_pair_new(channel_t *pair) {
    int fd1[2];  // Used to store two ends of first pipe
    int fd2[2];  // Used to store two ends of second pipe

    if (pipe(fd1)==-1 || pipe(fd2)==-1) {
        exit(1);
    }

    struct Channel c1;
    struct Channel c2;

    c1.read_fd = fd1[0];
    c1.write_fd = fd2[1];

    c2.read_fd = fd2[0];
    c2.write_fd = fd1[1];

    pair[0] = c1;
    pair[1] = c2;
}

channel_t *channel_new() {
    int fd1[2];

    if (pipe(fd1)==-1) {
        return NULL;
    }

    struct Channel *c = malloc(sizeof(channel_t));
    c->read_fd = fd1[0];
    c->write_fd = fd1[1];

    return c;
}

int channel_close(channel_t c) {
    int f = close(c.read_fd);
    int s = close(c.write_fd);

    return f + s;
}