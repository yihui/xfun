#include <R.h>
#include <Rinternals.h>
#include <stdint.h> // for uint64_t

SEXP rand_lcg(SEXP n, SEXP seed, SEXP a, SEXP c, SEXP m) {
  int len = asInteger(n);
  uint64_t s = (uint64_t) asReal(seed);
  uint64_t A = (uint64_t) asReal(a);
  uint64_t C = (uint64_t) asReal(c);
  uint64_t M = (uint64_t) asReal(m);

  SEXP result = PROTECT(allocVector(REALSXP, len));
  double *x = REAL(result);

  for (int i = 0; i < len; i++) {
    s = (A * s + C) % M;
    x[i] = (double) s;
  }

  UNPROTECT(1);
  return result;
}
