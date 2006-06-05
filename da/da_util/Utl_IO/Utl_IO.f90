module module_io

   use record_header
   use mm5_model_state

   implicit none

CONTAINS

   include 'cleanRows.inc'
   include 'crs_2_dot.inc'
   include 'read_mm5_ic.inc'
   include 'print_big_header.inc'
   include 'print_sub_header.inc'

end module module_io
