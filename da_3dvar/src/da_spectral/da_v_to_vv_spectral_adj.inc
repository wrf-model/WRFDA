subroutine da_v_to_vv_spectral_adj( ni, nj, max_wavenumber,inc, lenr,   &
                lensav,lenwrk, alp_size, r_cvsize, alp, wsave, &
                power, rcv,field)

!-------------------------------------------------------------------------
!  Performs Adjoint of spectral to grid transformation on a sphere.
!
!  History:   November 20th 2004: Coded by   Syed RH Rizvi
!-------------------------------------------------------------------------

   implicit none

   integer, intent(in) :: ni                         ! Number of longitudes.
   integer, intent(in) :: nj                         ! Number of latitudes.
   integer, intent(in) :: max_wavenumber             ! Smallest scale required (ni/2 - 1).
   integer, intent(in) :: inc                        ! Jump between elements of vector in array.
   integer, intent(in) :: lenr                       ! FFT array dimension (at least inc*(n-1)+1).
   integer, intent(in) :: lensav                     ! wsave dimension (n+int(log(real(ni)))+4).
   integer, intent(in) :: lenwrk                     ! Dimension of work array.
   integer, intent(in) :: alp_size                   ! Size of ALP vector.
   integer, intent(in) :: r_cvsize                   ! Size of real cv-array
   real, intent(in)    :: alp(1:alp_size)            ! Associated Legendre Polynomials.
   real, intent(in)    :: wsave(1:lensav)            ! Primes for FFT.
   real, intent(in)    :: power(0:max_wavenumber)  ! Power Spectrum
   real, intent(out)   :: rcv(1:r_cvsize)            ! Spectral modes.
   real, intent(in)    :: field(1:ni,1:nj)           ! Gridpoint field.
   integer             :: i, j, k, m, n, mm          ! Loop counters.
   integer             :: index_start, index_end     ! Position markers in cv.
   integer             :: index                      ! Position markers in rcv.
   integer             :: index_r, index_c           ! Array index for complex v_fou.
   integer             :: c_cvsize                   ! Size of complex cv vector.
   integer             :: ier                        ! Error return.

   complex, allocatable:: cv(:)                      ! Spectral modes.
   complex             :: v_fou(1:nj,0:max_wavenumber)! Intermediate Fourier state.
   complex             :: r_leg(1:nj)                ! Leg Tra. array.
   real                :: r_fou(1:lenr)              ! FFT array.
   real                :: work(1:lenwrk)             ! FFT work array. 
   logical             :: odd_longitudes             ! True if odd longitudes.
!
!-------------------------------------------------------------------------
    c_cvsize = 0.5 * r_cvsize   
   allocate (cv(1:c_cvsize))
!-------------------------------------------------------------------------
   if ( (ni+1) / 2 == ni/2 + 1 ) then  ! Odd number of longitudes:
      odd_longitudes = .true.
   else                                ! Even number of longitudes:
      odd_longitudes = .false.
   end if

!-------------------------------------------------------------------------
!  [1] Perform Adjoint of inverse Fourier decomposition in E-W direction:
!-------------------------------------------------------------------------
   v_fou = 0.
   do j = 1, nj
      r_fou(1:ni) = field(1:ni,j) 
      call rfft1f( ni, inc, r_fou, lenr, wsave, lensav, work, lenwrk, ier )
!-------------------------------------------------------------------------
!  Adjust the output for adjoint test
!-------------------------------------------------------------------------
       r_fou    =  real(ni/2.) * r_fou
       r_fou(1) =  r_fou(1)    * 2.0       
       if(.not. odd_longitudes)r_fou(ni) = 2.0*r_fou(ni)   
!

       if ( odd_longitudes ) then
          v_fou(j,0) = CMPLX( r_fou(1), 0.0 ) ! m = 0 is real.
       else
!         m = 0 is real, but pack R(NI/2) in imag m = 0:
          v_fou(j,0) = CMPLX( r_fou(1), r_fou(ni) )
       end if
       do m = 1, max_wavenumber
          index_r = 2 * m
          index_c = 2 * m + 1
          v_fou(j,m)  = v_fou(j,m) + cmplx(r_fou(index_r),r_fou(index_c))
       end do
   end do
!-------------------------------------------------------------------------
!  [2] Perform adjoint of inverse Legendre decomposition in N-S direction:
!-------------------------------------------------------------------------

   do m = 0, max_wavenumber
      index_start = m * ( max_wavenumber + 1 - m ) + m * ( m + 1 ) / 2 + 1
      index_end   = index_start + max_wavenumber - m

      r_leg(1:nj) =  v_fou(1:nj,m) 
      call da_leg_tra_inv_adj( nj, max_wavenumber, alp_size, m, alp, &
      cv(index_start:index_end), r_leg )
   end do

! Adjust Power spectrum
   do n =0, max_wavenumber
      cv(n+1) = cv(n+1) * sqrt(power(n)/(2.0*n+1))
    do m=1, n
     index = m*(max_wavenumber+1- m) + m*(m+1)/2+n-m + 1
     cv(index) =  cv(index) * sqrt (power(n)/(2*n+1) )
    end do
   end do

    do i=1,c_cvsize
     mm = 2*i - 1
     rcv(mm  ) = real ( cv(i) )
     rcv(mm+1) = aimag( cv(i) )
    end do 
deallocate (cv)
end subroutine da_v_to_vv_spectral_adj
