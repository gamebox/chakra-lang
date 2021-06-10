#define _GNU_SOURCE
#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if __MACH__
#include <sys/proc.h>
#include <sys/proc_info.h>
#endif
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#include "actors.h"
#include "channel.h"
#include "child.h"
#include "main.h"
#include "process.h"
#include "run_table.h"
#include "sched.h"
#include "stdlib.h"

int main() {
  // puts("Started");
  int nprocs = get_nprocs();
  int process_id = 0;

  coms = process_coms_new(nprocs);
  process_coms_display();
  actor_id_t self = {.process = 0, .entity = 0};

  for (int i = 1; i < nprocs; i++) {
    pthread_t thread_id;
    int *id = (int *)malloc(sizeof(int));
    *id = i;
    pthread_create(&thread_id, NULL, child_process, (void *)id);
    int assignmentResult = set_cpu_affinity(thread_id, i + 1);
    if (assignmentResult < 0) {
      break;
    }
  }

  // Start main actor on first child

  void **args = (void **)malloc(sizeof(void *));
  actor_id_t *actor_id = (actor_id_t *)malloc(sizeof(actor_id_t));
  *actor_id = (actor_id_t){.process = 2, .entity = 0};
  args[0] = (void *)actor_id;
  // printf("MainActor->init @ <%p><%p>\n", &MainActor, &(MainActor.init));
  envelope_t *env = Chakra_bootstrap(&MainActor, &Capabilities);

  int writeResult = process_write(*env);

  // printf("WROTE SPAWN? %d\n", writeResult);

  while (1) {
    // puts("About to read 0");
    int readResult = process_read(env, 1);
    // printf("Read %i", readResult);

    if (readResult == -1) {
      puts("No messages - dying 0");
      break;
    }

    if (readResult == 0) {
      // puts("No msgs 0");
      continue;
    }

    if (env != NULL) {
      if (env->actor_id.process == 999) {
        // puts("Received kill message");
        break;
      }
      // printf("PARENT MSG RECEIVED: `%s`\n", env->msg.type);
      if (strcmp(env->msg.type, "PRINT") == 0) {
        char *text = (char *)env->msg.payload;
        puts(text);
      }

      if (strcmp(env->msg.type, "TIMEOUT") == 0) {
        timeout_command_t *cmd = (timeout_command_t *)env->msg.payload;
        // printf("SHOULD BE SENT AFTER A %d timeout\n", cmd->timeout_ms);
        int writeResult = process_write(*cmd->env);
      }
    }
  }

  process_coms_close();

  return 0;
}
