/*
 *  This is a temporary implementation of the math stdlib
 *  using c `double` type or IEEE-754 double-precision
 *  floating point number.  Some of these functions live
 *  on the root of the standard library package, and are
 *  namespaced appropriately.
 */

#ifndef CHAKRA_STDLIB_MATH_H
#define CHAKRA_STDLIB_MATH_H

#include "../stdlib.h"

double Chakra_stdlib__math__add(double left, double right) {
  return left + right;
}

const stdlib__math_t Chakra_stdlib__math = {.add = Chakra_stdlib__math__add};

#endif