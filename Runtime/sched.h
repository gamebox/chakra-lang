#include <stdlib.h>

#if __linux__
#include <sys/sysinfo.h>
#include <sched.h>
#endif

#if __MACH__

typedef struct cpu_set {
  uint32_t count;
} cpu_set_t;

#endif

int set_cpu_affinity(pthread_t pid, int cpu);
int get_nprocs();