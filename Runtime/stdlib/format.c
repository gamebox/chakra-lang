#ifndef CHAKRA_STDLIB_FORMAT_H
#define CHAKRA_STDLIB_FORMAT_H

#include <stdio.h>

#include "../stdlib.h"

char *Chakra_stdlib__format__number(double num) {
  int length = snprintf(NULL, 0, "%d", num);
  char *str = (char *)malloc(length + 1);
  snprintf(str, length, "%d", num);
  return str;
}

const stdlib__format_t Chakra_stdlib__format = {
    .number = Chakra_stdlib__format__number};

#endif