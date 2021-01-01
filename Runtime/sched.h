#include <stdlib.h>

#if __linux__
#include <sys/sysinfo.h>
#endif

#if __MACH__

typedef struct cpu_set {
  uint32_t count;
} cpu_set_t;

#endif

int set_cpu_affinity(pid_t pid, size_t size, cpu_set_t *mask);
void CPU_ZERO(cpu_set_t *mask);
void CPU_SET(int cpu, cpu_set_t *mask);
int get_nprocs();