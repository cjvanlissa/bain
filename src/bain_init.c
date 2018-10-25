#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
  Check these declarations against the C/Fortran source code.
*/

  /* .Fortran calls */
  extern void F77_NAME(about)(void *, void *, void *, void *, void *, void *);
extern void F77_NAME(forc)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(mrank)(void *, void *, void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
  {"about", (DL_FUNC) &F77_NAME(about),  6},
  {"forc",  (DL_FUNC) &F77_NAME(forc),  11},
  {"mrank", (DL_FUNC) &F77_NAME(mrank),  7},
  {NULL, NULL, 0}
};

void R_init_bain(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
