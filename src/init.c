#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP base64_enc(SEXP);
extern SEXP base64_dec(SEXP);
extern SEXP rand_lcg(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP httpd_start(SEXP, SEXP);
extern SEXP httpd_stop(void);
extern SEXP httpd_poll(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"base64_enc",  (DL_FUNC) &base64_enc,  1},
  {"base64_dec",  (DL_FUNC) &base64_dec,  1},
  {"rand_lcg",    (DL_FUNC) &rand_lcg,    5},
  {"httpd_start", (DL_FUNC) &httpd_start, 2},
  {"httpd_stop",  (DL_FUNC) &httpd_stop,  0},
  {"httpd_poll",  (DL_FUNC) &httpd_poll,  2},
  {NULL, NULL, 0}
};

void R_init_xfun(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
