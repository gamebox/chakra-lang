#include <stdlib.h>

#ifndef CHAKRA_SCHED_H
#define CHAKRA_SCHED_H

#if __linux__

#include <sys/sysinfo.h>

int set_cpu_affinity(pid_t pid, size_t size, cpu_set_t *mask) {
  int res = sched_setaffinity(pid, size, mask);
  return res;
}

#endif

#if __MACH__

#include <sys/sysctl.h>
#include <sys/types.h>

#define SYSCTL_CORE_COUNT "machdep.cpu.core_count"

typedef struct cpu_set {
  uint32_t count;
} cpu_set_t;

int set_cpu_affinity(pid_t pid, size_t size, cpu_set_t *mask) { return 0; }
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