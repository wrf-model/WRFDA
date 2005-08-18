subroutine da_legtra_inv_adj( jds, jde, jts, jte, max_wavenumber, alp_size, m, alp, v, r_leg )

   implicit none

   integer, intent(in)  :: jds, jde                   ! Number of latitudes.
   integer, intent(in)  :: jts, jte                   ! Number of latitudes.
   integer, intent(in)  :: max_wavenumber             ! Maximum wavenumber.
   integer, intent(in)  :: alp_size                   ! Dimension of ALPs.
   integer, intent(in)  :: m                          ! Zonal wavenumber.
   real, intent(in)     :: alp(1:alp_size)            ! Associated Legendre Polynomials.

   complex, intent(out) :: v(m:max_wavenumber)        ! Output spectral coefficient.
   complex, intent(in) :: r_leg(jts:jte)              ! Field to transform.

   integer              :: l, j, js, je               ! Loop counters.
   integer              :: index_m, index_j           ! Markers.
   complex              :: sum_legtra                 ! Summation scalars.

   integer              :: jc, iequator

   index_m = m * ( max_wavenumber + 1 - m ) + m * ( m + 1 ) / 2 + 1 - m

   jc = (jde-jds+1)/2

   iequator = mod(jde-jds+1, 2)

   js = max(jts, jc+iequator+1)
   je = min(jc+iequator, jte)

   do l = m, max_wavenumber
      sum_legtra = ( 0.0, 0.0 )

      do j = js, jte
         index_j = ( jds+jde - j - 1 ) * ( max_wavenumber + 1 ) * ( max_wavenumber + 2 ) / 2
         if(mod(l+m,2) == 1) then
            sum_legtra = sum_legtra - r_leg(j) * alp(index_j + index_m + l)
         else
            sum_legtra = sum_legtra + r_leg(j) * alp(index_j + index_m + l)
         endif
      enddo

      do j = jts, je
         index_j = ( j - 1 ) * ( max_wavenumber + 1 ) * ( max_wavenumber + 2 ) / 2
         sum_legtra = sum_legtra + r_leg(j) * alp(index_j + index_m + l) 
      end do

      v(l) = sum_legtra
   end do

end subroutine da_legtra_inv_adj

