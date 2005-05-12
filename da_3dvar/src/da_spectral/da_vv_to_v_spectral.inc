subroutine da_vv_to_v_spectral( ni, nj, max_wavenumber, inc, lenr, lensav, lenwrk, &
                                alp_size, r_cvsize, alp, wsave, int_wgts, rcv, field)
!-------------------------------------------------------------------------
!  Performs gridpoint to spectral transformation on a sphere.
!  Note: Routine works for both regular and Gaussian latitude (latitude integration
!  weights contain necessary info).

!  History:
!  October  10th 2004: Coded by                           Dale Barker.
!  November 29th 2004: Updated for v_to_vv and other 
!                      with real control input array      Syed RH Rizvi
!-------------------------------------------------------------------------

   implicit none

   integer, intent(in) :: ni                         ! Number of longitudes.
   integer, intent(in) :: nj                         ! Number of latitudes.
   integer, intent(in) :: r_cvsize                   ! Size of real control cv-array.
   integer, intent(in) :: max_wavenumber             ! Smallest scale required (ni/2 - 1).
   integer, intent(in) :: inc                        ! Jump between elements of vector in array.
   integer, intent(in) :: lenr                       ! FFT array dimension (at least inc*(n-1)+1).
   real, intent(in)    :: field(1:ni,1:nj)           ! Gridpoint field.
   real, intent(out)   :: rcv(1:r_cvsize)            ! Spectral modes.
   integer, intent(in) :: lensav                     ! wsave dimension (n+int(log(real(ni)))+4).
   integer, intent(in) :: lenwrk                     ! Dimension of work array.
   integer, intent(in) :: alp_size                   ! Size of ALP vector.
   real, intent(in)    :: alp(1:alp_size)            ! Associated Legendre Polynomials.
   real, intent(in)    :: wsave(1:lensav)            ! Primes for FFT.
   real, intent(in)    :: int_wgts(1:nj)             ! Legendre integration weights.

   integer             :: i, j, k, m, mm             ! Loop counters.
   integer             :: ier                        ! Error return.
   integer             :: c_cvsize                   ! Size of complex cv-array
   integer             :: index_r, index_c           ! Array index for complex v_fou.
   integer             :: index_start, index_end     ! Position markers in cv.
   real                :: r_fou(1:lenr)              ! FFT array.
   real                :: work(1:lenwrk)             ! FFT work array. 
   logical             :: odd_longitudes
   complex             :: v_fou(1:nj,0:max_wavenumber)! Intermediate Fourier state.
   complex             :: r_leg(1:nj)                 ! Intermediate Fourier state.
   complex, allocatable:: cv(:)                      ! Spectral modes.
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
 
   if ( (ni+1) / 2 == ni/2 + 1 ) then  ! Odd number of longitudes:
      odd_longitudes = .true.
   else                                ! Even number of longitudes:
      odd_longitudes = .false.
   end if

!   [1] Perform Fourier decomposition in E-W direction:

     do j = 1, nj
       r_fou(1:ni) = field(1:ni,j)
       call rfft1f( ni, inc, r_fou, lenr, wsave, lensav, work, lenwrk, ier )

       if ( odd_longitudes ) then
          v_fou(j,0) = CMPLX( r_fou(1), 0.0 ) ! m = 0 is real.
       else
!         m = 0 is real, but pack R(NI/2) in imag m = 0:
          v_fou(j,0) = CMPLX( r_fou(1), r_fou(ni) )
       end if

       do m = 1, max_wavenumber
          index_r = 2 * m
          index_c = 2 * m + 1
          v_fou(j,m) = CMPLX( r_fou(index_r), r_fou(index_c) ) ! 2.0 * Fourier mode.
       end do
    end do

!  [2] Perform Legendre decomposition in N-S direction:

    do m = 0, max_wavenumber
       index_start = m * ( max_wavenumber + 1 - m ) + m * ( m + 1 ) / 2 + 1
       index_end   = index_start + max_wavenumber - m
       r_leg(1:nj) = v_fou(1:nj,m)
       call da_legtra ( nj, max_wavenumber, alp_size, m, int_wgts, alp, r_leg, &
                        cv(index_start:index_end) )
    end do

    do i=1,c_cvsize
     mm = 2*i - 1
     rcv(mm  ) = real ( cv(i) )
     rcv(mm+1) = aimag( cv(i) )
    end do
 deallocate (cv)
end subroutine da_vv_to_v_spectral
