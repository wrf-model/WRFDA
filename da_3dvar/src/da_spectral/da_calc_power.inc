subroutine da_calc_power( max_wavenumber, cv_size, cv, power )

!-------------------------------------------------------------------------
!  Performs spectral to gridpoint transformation on a sphere.
!
!  History:
!  October 10th 2004: Coded by Dale Barker.
!-------------------------------------------------------------------------

   implicit none

   integer, intent(in) :: max_wavenumber             ! Smallest scale required (ni/2 - 1).
   integer, intent(in) :: cv_size                    ! Size of cv vector.
   complex, intent(in) :: cv(1:cv_size)              ! Spectral modes.
   real, intent(out)   :: power(0:max_wavenumber)    ! Power spectrum.

   integer             :: n, m                       ! Loop counters.
   integer             :: index                      ! Position markers in cv.
   real                :: accum_power                ! Power for given total wavenumber n.

   power(:) = 0.0

!  Calculate power spectrum from input 1D spectral mode vector:

   do n = 0, max_wavenumber

!     First consider m=0:
      m = 0
      index = m * ( max_wavenumber + 1 - m ) + m * ( m + 1 ) / 2 + 1 + n - m
      power(n) = real( cv(index) )**2

!     Now add m>0 terms:
      do m = 1, n
         index = m * ( max_wavenumber + 1 - m ) + m * ( m + 1 ) / 2 + 1 + n - m
         power(n) = power(n) + 2.0 * ( real( cv(index) )**2 + aimag( cv(index) )**2 )
      end do

   end do
      
end subroutine da_calc_power
