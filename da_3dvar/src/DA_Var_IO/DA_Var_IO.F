MODULE DA_Var_IO

!--Driver layer

   USE module_domain
   USE module_io_domain

!--Model layer

   USE module_configure

CONTAINS

#include "med_initialdata_input_3dvar.inc"
#include "med_initialdata_output_3dvar.inc"

END MODULE DA_Var_IO

