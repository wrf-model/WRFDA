subroutine da_v_to_vv_spectral(ni, nj,max_wavenumber, inc,lenr, &
                lensav,lenwrk, alp_size, r_cvsize, alp, wsave,  &
                power, rcv,field)

!-------------------------------------------------------------------------
!  Performs spectral to gridpoint transformation on a sphere.
!
!  History:    November 20th 2004: Coded by    Syed RH Rizvi  
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
   real, intent(in)    :: rcv(1:r_cvsize)            ! Spectral modes.
   real, intent(in)    :: power(0:max_wavenumber)    ! Power spectrum 
   real, intent(out)   :: field(1:ni,1:nj)           ! Gridpoint field.
   integer             ::  i, j, k , m , n, mm       ! Loop counters.
   integer             :: index_start, index_end     ! Position markers in cv.
   integer             :: index                      ! Position markers in rcv.
   integer             :: index_r, index_c           ! Array index for complex v_fou.
   integer             :: ier                        ! Error return.

   complex, allocatable:: cv(:)                      ! Spectral modes.
   integer             :: c_cvsize                   ! Size of complex cv-array
   complex             :: v_fou(1:nj,0:max_wavenumber)! Intermediate Fourier state.
   complex             :: r_leg(1:nj)                ! Leg Tra. array.
   real                :: r_fou(1:lenr)              ! FFT array.
   real                :: work(1:lenwrk)             ! FFT work array. 
!---------------------------------------------------------------------------------
   real                :: xx

   c_cvsize = 0.5 * r_cvsize     
   allocate (cv(1:c_cvsize))
!---------------------------------------------------------------------------------
      v_fou = 0.0
      do n = 1, c_cvsize
      mm =  2*n - 1    
      cv(n)  = CMPLX( rcv(mm), rcv(mm+1) )
      end do
!---------------------------------------------------------------------------------
! [1]  Adjust Power spectrum
     if( .not. test_transforms ) &
     call da_apply_power(power, max_wavenumber, cv, c_cvsize)
!---------------------------------------------------------------------------------
!  [2] Perform inverse Legendre decomposition in N-S direction:

   do m = 0, max_wavenumber
      index_start = m * ( max_wavenumber + 1 - m ) + m * ( m + 1 ) / 2 + 1
      index_end   = index_start + max_wavenumber - m
      call da_leg_tra_inv ( nj, max_wavenumber, alp_size, m, alp, cv(index_start:index_end), r_leg )

      v_fou(1:nj,m) = r_leg(1:nj)
   end do
!---------------------------------------------------------------------------------

!  [3] Perform inverse Fourier decomposition in E-W direction:

   do j = 1, nj
      r_fou(1)  =  real(v_fou(j,0 ))  ! R(m=0) is real.
      r_fou(ni) = aimag(v_fou(j,0))   ! R(m=NI/2) is real, but packed in imag m = 0)
!rizvi make r_fou(ni) zero as there is no power computed corresponding to this wavenumber
      r_fou(ni) = 0.      

      do m = 1, max_wavenumber
         index_r = 2 * m
         index_c = 2 * m + 1
         r_fou(index_r) = real(v_fou(j,m))
         r_fou(index_c) = aimag(v_fou(j,m))
      end do

!---------------------------------------------------------------------------------
      call rfft1b( ni, inc, r_fou, lenr, wsave, lensav, work, lenwrk, ier )
      field(1:ni,j) = r_fou(1:ni)

   end do
   
   deallocate (cv)
end subroutine da_v_to_vv_spectral
