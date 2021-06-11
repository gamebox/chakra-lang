#if __linux__
#define _GNU_SOURCE

#include <pthread.h>
#include <sched.h>

int set_cpu_affinity(pthread_t pid, int cpu) {
  cpu_set_t mask;

  CPU_ZERO(&mask);
  CPU_SET(cpu, &mask);
  int res = pthread_setaffinity_np(pid, sizeof(mask), &mask);
  return res;
}

int find_num_procs() { return get_nprocs(); }

#endif

#if __MACH__

#include <stdio.h>
#include <stdlib.h>
#include <sys/sysctl.h>
#include <sys/types.h>

#define SYSCTL_CORE_COUNT "machdep.cpu.core_count"

typedef struct cpu_set {
  uint32_t count;
} cpu_set_t;

int set_cpu_affinity(pid_t pid, int cpu) { return 0; }
void CPU_ZERO(cpu_set_t *mask) { mask->count = 0; }

void CPU_SET(int cpu, cpu_set_t *mask) { mask->count |= 1 << cpu; }

int find_num_procs() {
  int32_t core_count = 0;
  size_t len = sizeof(core_count);
  int res = sysctlbyname(SYSCTL_CORE_COUNT, &core_count, &len, 0, 0);
  return core_count;
  // if (res < 0) {
  //   puts("error");
  //   return res;
  // } else {
  //   printf("core_count is %i", core_count);
  //   return core_count;
  // }
}

#endif