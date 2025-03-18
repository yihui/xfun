#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP base64_enc(SEXP);
extern SEXP base64_dec(SEXP);
extern SEXP rand_lcg(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"base64_enc", (DL_FUNC) &base64_enc, 1},
  {"base64_dec", (DL_FUNC) &base64_dec, 1},
  {"rand_lcg",   (DL_FUNC) &rand_lcg,   5},
  {NULL, NULL, 0}
};

void R_init_xfun(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
