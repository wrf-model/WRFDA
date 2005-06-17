subroutine da_print_be_stats_h_regional( outunit, variable, nk, scale_length )

   implicit none 

   integer, intent(inout)   :: outunit               ! Output file unit.
   character*10, intent(in) :: variable              ! Variable name.
   integer, intent(in)      :: nk                    ! Dimension of vertical index.
   real, intent(in)         :: scale_length(1:nk)    ! Correlation scale lengths.

   integer                  :: k                     ! Loop counter.

   write(6,'(3a,i5)')' Scale length for variable ', trim(variable), ' in unit ', outunit

   do k = 1, nk
      write(outunit,'(i4,1pe15.5)')k, scale_length(k)
   end do

   outunit = outunit + 1
   write(6,*)

end subroutine da_print_be_stats_h_regional

