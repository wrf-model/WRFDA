subroutine da_read_ssmi_integer_array(aa,nn,i0,i_step)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                 intent(in) :: nn, i0, i_step
   integer,  dimension(:), intent(out) :: aa

   integer     :: i, ii

   do i = 1, nn
      ii = i_step*(i-1) + i0
      read (unit=char_ned (ii:ii+8),fmt='((I7,2X))') aa(i)
   end do

end subroutine da_read_ssmi_integer_array


