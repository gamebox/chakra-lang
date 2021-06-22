#ifndef CHAKRA_STDLIB_FORMAT_H
#define CHAKRA_STDLIB_FORMAT_H

#include <math.h>
#include <stdio.h>

#include "../stdlib.h"

char *Chakra_stdlib__format__number(double num) {
  int length = snprintf(NULL, 0, "%f", num);
  char *str = (char *)malloc(length + 1);
  snprintf(str, length + 1, "%f", num);
  return str;
}

char *Chakra_stdlib__format__integer(double num) {
  int truncated = (int)num;
  int length = snprintf(NULL, 0, "%d", truncated);
  char *str = (char *)malloc(length + 1);
  snprintf(str, length + 1, "%d", truncated);
  return str;
}

const stdlib__format_t Chakra_stdlib__format = {
    .number = Chakra_stdlib__format__number,
    .integer = Chakra_stdlib__format__integer};

#endif