subroutine da_print_be_stats_h_global( outunit, variable, k, max_wavenumber, total_power )

   implicit none 

   integer, intent(inout)   :: outunit               ! Output file unit.
   character*10, intent(in) :: variable              ! Variable name.
   integer, intent(in)      :: k                     ! Vertical index.
   integer, intent(in)      :: max_wavenumber        ! Smallest scale required (ni/2 - 1).
   real, intent(in) :: total_power(0:max_wavenumber) ! Total Power spectrum (averaged over time/members).

   integer             :: n                          ! Loop counter.
   real                :: accum_power, variance      ! Accumulated power, variance.

   accum_power = 0.0
   variance = sum( total_power(0:max_wavenumber) )

   write(6,'(3a,i5,a,i5)')' Power spectra for variable ', trim(variable), &
                          ' and level ', k, ' in unit ', outunit

   do n = 0, max_wavenumber
      accum_power = accum_power + total_power(n)
      write(outunit,'(2i4,2f18.5,f8.3)')k, n, total_power(n), accum_power, &
                                        accum_power / variance
   end do

   outunit = outunit + 1
   write(6,*)

end subroutine da_print_be_stats_h_global

