#ifndef CHAKRA_SCHED_H
#define CHAKRA_SCHED_H

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

#endif

#if __MACH__

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

int get_nprocs() {
  int32_t core_count = 0;
  size_t len = sizeof(core_count);
  int res = sysctlbyname(SYSCTL_CORE_COUNT, &core_count, &len, 0, 0);
  if (res < 0) {
    return res;
  } else {
    return core_count;
  }
}

#endif

#endif