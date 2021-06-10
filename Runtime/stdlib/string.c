#include <stdio.h>
#include <string.h>

#include "../stdlib.h"

const size_t LOWER_ALPHA_BOTTOM = 96;
const size_t LOWER_ALPHA_TOP = 123;
const size_t TO_UPPER_ALPHA_DELTA = 32;

char *Chakra_stdlib__string__to_upper(char *str) {
  size_t i = 0;
  int len = strlen(str);
  char *res = (char *)malloc(len);
  for (int i = 0; i < len; i = i + 1) {
    if (str[i] > LOWER_ALPHA_BOTTOM && str[i] < LOWER_ALPHA_TOP) {
      res[i] = (char)((int)str[i] - TO_UPPER_ALPHA_DELTA);
    } else {
      res[i] = str[i];
    }
  }
  return res;
}

const stdlib__string_t Chakra_stdlib__string = {
    .to_upper = Chakra_stdlib__string__to_upper};