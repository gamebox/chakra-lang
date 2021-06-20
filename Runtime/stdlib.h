#ifndef CHAKRA_STDLIB_H
#define CHAKRA_STDLIB_H

#include "actors.h"
#include "main.h"

typedef struct Tuple2_Str_ActorId {
  char *str;
  actor_id_t actor_id;
} tuple2_str_actorid_t;

typedef struct TimeoutCommand {
  int timeout_ms;
  envelope_t *env;
} timeout_command_t;

typedef struct Chakra_stdlib__io_t {
  envelope_t *(*print)(unsigned long long cap, char *text);
} stdlib__io_t;

typedef struct Chakra_stdlib__string_t {
  char *(*to_upper)(char *);
} stdlib__string_t;

typedef struct Chakra_stdlib__format_t {
  char *(*number)(double num);
} stdlib__format_t;

typedef struct Chakra_stdlib__math_t {
  double (*add)(double left, double right);
} stdlib__math_t;

envelope_t *Chakra_bootstrap(main_actor_t *actor, capabilities_t *caps);
envelope_t *Chakra_stdlib__print(unsigned long long cap, char *text);
envelope_t *Chakra_stdlib__kill(actor_id_t process_id);
envelope_t *Chakra_stdlib__spawn(actor_t *actor);
envelope_t *Chakra_stdlib__spawn1(actor_t *actor, void *arg0);
envelope_t *Chakra_stdlib__spawn2(actor_t *actor, void *arg0, void *arg1);
envelope_t *Chakra_stdlib__spawn3(actor_t *actor, void *arg0, void *arg1,
                                  void *arg2);
envelope_t *Chakra_stdlib__send(actor_id_t actor_id, msg_t msg);
envelope_t *Chakra_stdlib__timeout(int ms_timeout, void *msg);
actor_id_t Chakra_stdlib__self();
char *Chakra_stdlib__string__to_upper(char *str);

static inline char *Chakra_stdlib__format__number(double num);

double Chakra_stdlib__math__add(double left, double right);

extern const stdlib__io_t Chakra_stdlib__io;
extern const stdlib__string_t Chakra_stdlib__string;
extern const stdlib__format_t Chakra_stdlib__format;
extern const stdlib__math_t Chakra_stdlib__math;

#endif