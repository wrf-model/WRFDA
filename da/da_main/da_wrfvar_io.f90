module da_wrfvar_io

   !--Driver layer

   use module_domain
   use module_io_domain

   !--Model layer

   use module_timing

   use da_tracing
   use da_reporting

contains

#include "da_med_initialdata_input.inc"
#include "da_med_initialdata_output.inc"

end module da_wrfvar_io
