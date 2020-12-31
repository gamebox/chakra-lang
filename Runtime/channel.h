#ifndef CHAKRA_CHANNEL_H
#define CHAKRA_CHANNEL_H

typedef struct Channel {
  int read_fd;
  int write_fd;
} channel_t;

void channel_pair_new(channel_t *pair);
channel_t *channel_new();
int channel_close(channel_t c);

#endif