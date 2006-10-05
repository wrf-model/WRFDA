MODULE da_wrfvar_io

   !--Driver layer

   USE module_domain
   USE module_io_domain

   !--Model layer

   USE module_timing

   USE da_tracing
   use da_reporting

CONTAINS

#include "da_med_initialdata_input.inc"
#include "da_med_initialdata_output.inc"

END MODULE da_wrfvar_io
