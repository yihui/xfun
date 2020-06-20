#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP base64_encode(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"base64_encode", (DL_FUNC) &base64_encode, 1},
  {NULL, NULL, 0}
};

void R_init_xfun(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
