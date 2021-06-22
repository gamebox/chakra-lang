/*
 *  This is a temporary implementation of the math stdlib
 *  using c `double` type or IEEE-754 double-precision
 *  floating point number.  Some of these functions live
 *  on the root of the standard library package, and are
 *  namespaced appropriately.
 */

#ifndef CHAKRA_STDLIB_MATH_H
#define CHAKRA_STDLIB_MATH_H

#include <math.h>

#include "../stdlib.h"

static inline double Chakra_stdlib__math__add(double left, double right) {
  return left + right;
}

double Chakra_stdlib__math__sub(double left, double right) {
  return left - right;
}

double Chakra_stdlib__math__mul(double left, double right) {
  return left * right;
}

double Chakra_stdlib__math__div(double left, double right) {
  return left / right;
}

double Chakra_stdlib__math__pow(double num, double exponent) {
  return pow(num, exponent);
}

double Chakra_stdlib__math__floor(double num) { return floor(num); }

double Chakra_stdlib__math__ceil(double num) { return ceil(num); }

double Chakra_stdlib__math__round(double num) { return round(num); }

const stdlib__math_t Chakra_stdlib__math = {
    .add = Chakra_stdlib__math__add,
    .sub = Chakra_stdlib__math__sub,
    .mul = Chakra_stdlib__math__mul,
    .div = Chakra_stdlib__math__div,
    .pow = Chakra_stdlib__math__pow,
    .floor = Chakra_stdlib__math__floor,
    .ceil = Chakra_stdlib__math__ceil,
    .round = Chakra_stdlib__math__round,
};

#endif