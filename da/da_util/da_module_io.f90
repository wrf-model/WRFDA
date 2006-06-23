module da_module_io

   use da_record_header
   use da_mm5_model_state

   implicit none

CONTAINS

#include "da_clean_rows.inc"
#include "da_crs_2_dot.inc"
#include "da_read_mm5_ic.inc"
#include "da_print_big_header.inc"
#include "da_print_sub_header.inc"

end module da_module_io
