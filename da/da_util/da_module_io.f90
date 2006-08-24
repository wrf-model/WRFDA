module da_module_io

   use da_record_header

   implicit none

CONTAINS

!#include "da_clean_rows.inc"
#include "da_crs_2_dot.inc"
#include "da_print_big_header.inc"
#include "da_print_sub_header.inc"

end module da_module_io
