subroutine da_leg_tra_inv ( nj, max_wavenumber, alp_size, m, alp, v, r_leg )

   implicit none

   integer, intent(in)  :: nj                         ! Number of latitudes.
   integer, intent(in)  :: max_wavenumber             ! Maximum wavenumber.
   integer, intent(in)  :: alp_size                   ! Dimension of ALPs.
   integer, intent(in)  :: m                          ! Zonal wavenumber.
   real, intent(in)     :: alp(1:alp_size)            ! Associated Legendre Polynomials.

   complex, intent(in)  :: v(m:max_wavenumber)        ! Output spectral coefficient.
   complex, intent(out) :: r_leg(1:nj)                ! Field to transform.

   integer              :: l, j                       ! Loop counters.
   integer              :: index_m, index_j, index    ! Markers.
   integer              :: sign_switch                ! Make use of symmetry of ALPs.
   complex              :: sum_legtra1, sum_legtra2   ! Summation scalars.

   index_m = m * ( max_wavenumber + 1 - m ) + m * ( m + 1 ) / 2 + 1 - m

   do j = 1, nj / 2
      index_j = ( j - 1 ) * ( max_wavenumber + 1 ) * ( max_wavenumber + 2 ) / 2

      sum_legtra1 = ( 0.0, 0.0 )
      sum_legtra2 = ( 0.0, 0.0 )
      do l = m, max_wavenumber
         index = index_j + index_m + l
         sign_switch = (-1)**( l + m )

!        Calculate first quadrant values:
         sum_legtra1 = sum_legtra1 + v(l) * alp(index)

!        Calculate second quadrant values:
         sum_legtra2 = sum_legtra2 + v(l) * sign_switch * alp(index)
      end do
      r_leg(j) = sum_legtra1
      r_leg(nj+1-j) = sum_legtra2
   end do

!  Calculate equator value if odd number of latitudes:

   if ( (nj+1) / 2 == nj/2 + 1 ) then
      j = nj/2 + 1
      index_j = ( j - 1 ) * ( max_wavenumber + 1 ) * ( max_wavenumber + 2 ) / 2

      sum_legtra1 = ( 0.0, 0.0 )
      do l = m, max_wavenumber
         index = index_j + index_m + l
         sum_legtra1 = sum_legtra1 + v(l) * alp(index)
      end do
      r_leg(j) = sum_legtra1
   end if
end subroutine da_leg_tra_inv
