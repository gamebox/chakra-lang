#define _GNU_SOURCE
#include <errno.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/sysinfo.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "actors.h"
#include "channel.h"
#include "main.h"
#include "process.h"
#include "run_table.h"

int assignToCore(pid_t pid, int cpu) {
  cpu_set_t mask;

  CPU_ZERO(&mask);
  CPU_SET(cpu, &mask);
  int result = sched_setaffinity(pid, sizeof(mask), &mask);

  return result;
}

int main() {
  pid_t processes[32];
  channel_t channel_pairs[32];

  int nprocs = get_nprocs();

  process_coms = malloc(sizeof(channel_t *) * nprocs);

  process_coms[0] = channel_new();

  for (int i = 1; i < nprocs; i++) {
    process_coms[i] = channel_new();
  }
  for (int i = 1; i < nprocs; i++) {
    pid_t p = fork();

    if (p < 0) {  // Fork failure
      fprintf(stderr, "fork Failed");
      return 1;
    }

    if (p > 0) {
      processes[i] = p;
      int assignmentResult = assignToCore(p, i + 1);
      if (assignmentResult < 0) {
        break;
      }
    }

    if (p == 0) {
      child_process(i);
      return 0;
    }
  }

  // Start main actor on first child

  msg_t msg = (msg_t){.type = "SPAWN", .payload = (void *)&MainActor};
  envelope_t *env = (envelope_t *)malloc(sizeof(envelope_t));
  *env = (envelope_t){.msg = msg, .actor_id = {.process = 1, .entity = 0}};

  process_write(*env);

  while (1) {
    int readResult = process_read(0, env, 0);

    if (readResult == -1) {
      break;
    }

    if (env != NULL) {
      if (strcmp(env->msg.type, "PRINT") == 0) {
        char *text = (char *)env->msg.payload;
        puts(text);
      }
    }
  }

  return 0;
}
