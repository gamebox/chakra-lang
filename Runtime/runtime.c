#define _GNU_SOURCE
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include <sys/sysinfo.h>
#include <sys/wait.h>
#include <errno.h>
#include <poll.h>
#include "actors.h"
#include "channel.h"
#include "run_table.h"

/*
init =
    (
        0,
        [print("Hello World!")]
    )
*/
turn_result_t *Main_Actor__init(void **args) {
    actor_id_t *process_id = (actor_id_t *) args[0];
    msg_t *msgs = malloc(sizeof(msg_t));
    char *text = "Hello world!";
    msg_t msg = (msg_t) { .type = "PRINT", .payload = (void *) text };

    envelope_t env = { .actor_id = *process_id, .msg = msg };
    envelope_t envs[1] = { env };

    int *state = malloc(sizeof(int));
    *state = 0;

    turn_result_t *res = malloc(sizeof(turn_result_t));
    res->state = (void*) state;
    res->envelopes = envs;
    res->num_msgs = 1;

    return res;
}

const actor_t MainActor = { .init = Main_Actor__init, .receive = NULL };

channel_t **process_coms;

size_t TimeoutRead (int port, void*buf, size_t size, int mlsec_timeout) {
    struct pollfd fd = { .fd = port, .events = POLLIN };

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

int assignToCore(pid_t pid, int cpu);

int assignToCore(pid_t pid, int cpu) {
    cpu_set_t mask;

    CPU_ZERO(&mask);
    CPU_SET(cpu, &mask);
    int result = sched_setaffinity(pid, sizeof(mask), &mask);

    return result;
}

// A process should only read their own buffer
int process_read(int process_id, envelope_t *buf) {
    int res = read(process_coms[process_id]->read_fd, buf, sizeof(envelope_t));
    return res;
}

// A process may write to any other process, including themselves
int process_write(envelope_t env) {
    envelope_t buf[1] = { env };
    int res = write(process_coms[env.actor_id.process]->write_fd, buf, sizeof(envelope_t));
    return res;
}

void child_process(int id) {
    envelope_t* buf;
    int next_entity = 1;
    run_table_t *run_table = run_table_new();

    while(1) {
        int readResult = process_read(id, buf);
        if (readResult == -1) {
            exit(1);
        }

        if (readResult == 0) {
            break;
        }

        actor_id_t recipient_id = buf->actor_id;

        if (recipient_id.process == id && recipient_id.entity == 0) {
            // This is a message for the process itself, i.e., this is a command.

            // Create main process:
            // 1. Call the init function on main actor struct, passing in an actor id for this process
            // 2. Store the state and def, as well as entity id in a running_actor_t
            // 3. Handle the result(set state, write messages out)
            break;
        }

        msg_t msg = buf->msg;
        running_actor_t *a = run_table_search(recipient_id, run_table);
        if (a == NULL) {
            break;
        }
        turn_result_t *res = (a->def->receive(a->state, &msg));
        a->state = res->state;
        int i = 0;
        while (i < res->num_msgs) {
            process_write(res->envelopes[i]);
        }

        buf = NULL;
    }


    exit(0);
}

int main()  {
    pid_t processes[32];
    channel_t channel_pairs[32];

    int nprocs = get_nprocs();

    process_coms = malloc(sizeof(channel_t*) * nprocs);

    process_coms[0] = channel_new();

    for (int i = 1; i < nprocs; i++) {
        process_coms[i] = channel_new();
        pid_t p = fork();

        if (p < 0) { // Fork failure
            fprintf(stderr, "fork Failed" );
            return 1;
        }

        if (p > 0) {
            processes[i] = p;
            int assignmentResult = assignToCore(p, i + 1);
            if (assignmentResult < 0) {
                fprintf(stderr, "Could not set affinity for pid %d to CPU core %d\n", p, i);
                break;
            }
        }

        if (p == 0) {
            child_process(i);
            return 0;
        }
    }

    int i = 1;
    long int blah = (long int) i;
    void *b = (void *) blah;
    char pong_message[100];
    while(i < nprocs) {
        int readResult = TimeoutRead(channel_pairs[i].read_fd, pong_message, sizeof(pong_message), 16);

        if (readResult == -1) {
            printf("No more input from CPU %d, code %d\n", i, readResult);
        }

        puts(pong_message);
        channel_close(channel_pairs[0]);
        i++;
    }

    puts("Done");

    return 0;
}
