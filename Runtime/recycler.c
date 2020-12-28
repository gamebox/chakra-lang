#include <stdlib.h>

const int INTITAL_RC_SIZE = 1000;
const int INC_DEC_BUFFER_SIZE = 1000;

typedef struct RefCount {
    void* ptr;
    int count;
} RefCount;

struct RefCount **RC;
int RCIndex;

void **Inc;
void **Dec;
int IncIndex;
int DecIndex;


void Init();
void ChangeRefCount(void* P, int offset);
void Increment(void* S);
void Decrement(void* S);
void** children(void* S);
void Release(void* S);
void Update(void* R, void* S);
void* Allocate(int n);
void EnqueueIncrement(void* S);
void EnqueueDecrement(void* S);
void Collect();
void IncrementStack();
void ProcessIncrements();
void ProcessDecrements();
void DecrementStack();
void Release(void* S);

void** children(void* S) {
    return NULL;
}

void Init() {
    RC = (struct RefCount**) malloc(sizeof(RefCount) * INTITAL_RC_SIZE);
    Inc = (void**) malloc(sizeof(int) * INC_DEC_BUFFER_SIZE);
    Dec = (void**) malloc(sizeof(int) * INC_DEC_BUFFER_SIZE);
    if (RC == NULL || Inc == NULL || Dec == NULL) {
        exit(1);
    }

    IncIndex = 0;
    DecIndex = 0;
}

void ChangeRefCount(void* P, int offset) {
    int i = 0;
    struct RefCount *rc;
    while (RC[i] != NULL) {
        rc = RC[i];
        if (rc->ptr == P) {
            rc->count++;
            return;
        }
        i++;
    }
}

// Increment(S)
//     RC(S) = RC(S) + 1
void Increment(void* S) {
    ChangeRefCount(S, 1);
}

// Decrement(S)
//     RC(S) = RC(S) - 1
//     if (RC(S) == 0)
//         Release(S)
void Decrement(void* S) {
    ChangeRefCount(S, -1);
}

// Release(S)
//     for T in children(S)
//         Decrement(T)
//     SystemFree(S)
void Release(void* S) {
    void** cs = children(S);
    for (int i = 0; i < sizeof cs; i = i + 1) {
        Decrement(cs[i]);
    }
    free(S);
}

// Update(R, S)
//     T = Fetch&Store(*R, S)
//     if (S != null)
//         EnqueueIncrement(S)
//     if (T != null)
//         EnqueueDecrement(T)
void Update(void* R, void* S) {
    void *T = R;
    R = S;
    if (S != NULL) {
        EnqueueIncrement(S);
    }
    if (T != NULL) {
        EnqueueDecrement(T);
    }
}

// Allocate(n)
//     P = SystemAllocate(n)
//     if (P == null)
//         Collect()
//         P = SystemAllocate(n)
//         if (P == null)
//             throw OutOfMemoryError
//     RC(P) = 1
//     EnqueueDecrement(P)
//     return P
void* Allocate(int n) {
    void* P = malloc(n);
    if (P == NULL) {
        Collect();
        P = malloc(n);
        if (P == NULL) {
            exit(1);
        }
    }

    
    EnqueueDecrement(P);
    return P;
}

// EnqueueIncrement(S)
//     if (full(Inc))
//         Collect()
//     append S to Inc
void EnqueueIncrement(void* S) {
    if (IncIndex == sizeof Inc) {
        Collect();
    }
    Inc[IncIndex] = S;
    IncIndex++;
}

// EnqueueDecrement(S)
//     if (full(Dec))
//         Collect()
//     append S to Dec
void EnqueueDecrement(void* S) {
    if (DecIndex == sizeof Dec) {
        Collect();
    }
    Dec[DecIndex] = S;
    DecIndex++;
}

// Collect()
//     for T in Threads
//         Suspend(T)
//     IncrementStack()
//     ProcessIncrements()
//     ProcessDecrements()
//     DecrementStack()
//     CollectCycles()
//     for T in Threads
//         Resume(T)
void Collect() {
    IncrementStack();
    ProcessIncrements();
    ProcessDecrements();
    DecrementStack();
}

// IncrementStack()
//      for T in Threads
//          for N in Pointers(Stack(T))
//              Increment(N)
void IncrementStack() {}

// ProcessIncrements()
//     for M in Inc
//         Increment(M)
//     clear Inc
void ProcessIncrements() {
    for (int i = 0; i < IncIndex; i = i + 1) {
        Increment(Inc[i]);
        Inc[i] = NULL;
    }
    IncIndex = 0;
}

// ProcessDecrements()
//     for M n Dec
//         Decrement(M)
//     clear Dec
void ProcessDecrements() {
    for (int i = 0; i < DecIndex; i = i + 1) {
        Decrement(Dec[i]);
        Dec[i] = NULL;
    }
    DecIndex = 0;
}

// DecrementStack()
//     for T in Threads
//         for N in Pointers(Stack(T))
//             EnqueueDecrement(N)
void DecrementStack() {}
