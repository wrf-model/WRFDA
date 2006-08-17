subroutine da_v_to_vv_spectral( max_wave, sizec, lenr, lenwrk, lensav, inc, &
                                alp_size, alp, wsave, power, rcv, field, &
                                ids, ide, jds, jde, &
                                ims, ime, jms, jme, &
                                its, ite, jts, jte )

   !-------------------------------------------------------------------------
   ! Performs spectral to gridpoint transformation on a sphere.
   !----------------------------------------------------------------------

   implicit none

   integer, intent(in) :: max_wave                 ! Max total wavenumber.
   integer, intent(in) :: sizec                    ! Size of packed spectral array.
   integer, intent(in) :: lenr                     ! FFT info.
   integer, intent(in) :: lenwrk                   ! FFT info.
   integer, intent(in) :: lensav                   ! FFT info.
   integer, intent(in) :: inc                      ! FFT info.
   integer, intent(in) :: alp_size                 ! Size of alp array.
   real, intent(in)    :: alp(1:alp_size)          ! Associated Legendre Polynomials.
   real, intent(in)    :: wsave(1:lensav)          ! Primes for FFT.
   real, intent(in)    :: power(0:max_wave)        ! Power spectrum 
   real, intent(in)    :: rcv(1:2*sizec)           ! Spectral modes.
   real, intent(out)   :: field(its:ite,jts:jte)   ! Gridpoint field.
   integer, intent(in) :: ids, ide, jds, jde, &
                          ims, ime, jms, jme, &
                          its, ite, jts, jte

   integer :: j, m, n                  ! Loop counters.
   integer :: index_start, index_end   ! Position markers in cv.
   integer :: index                    ! Position markers in rcv.
   integer :: index_r, index_c         ! Array index for complex v_fou.
   integer :: ier                      ! Error return.

   real    :: r_fou(1:lenr)            ! FFT array.
   real    :: work(1:lenwrk)           ! FFT work array. 
   complex :: v_fou(its:ite,0:max_wave)! Intermediate Fourier state.
   complex :: r_leg(jts:jte)           ! Leg Tra. array.
   complex :: ccv(1:sizec)             ! Spectral modes.

   !----------------------------------------------------------------------------
   ! [1] Create complex array from read array:
   !----------------------------------------------------------------------------

   v_fou = 0.0
   do n = 1, sizec
      ccv(n)  = CMPLX( rcv(2*n-1), rcv(2*n) )
   end do

   !----------------------------------------------------------------------------
   ! [2]  Apply power spectrum
   !----------------------------------------------------------------------------

   if ( .not. test_transforms) call da_apply_power(power, max_wave, ccv, sizec)

   !----------------------------------------------------------------------------
   ! [3] Perform inverse Legendre decomposition in N-S direction:
   !----------------------------------------------------------------------------

   do m = 0, max_wave
      index_start = m * ( max_wave + 1 - m ) + m * ( m + 1 ) / 2 + 1
      index_end   = index_start + max_wave - m

      call da_legtra_inv( jds, jde, jts, jte, max_wave, &
                          alp_size, m, alp, &
                          ccv(index_start:index_end),  v_fou(jts:jte,m) )
   end do

   !----------------------------------------------------------------------------
   ! [4] Perform inverse Fourier decomposition in E-W direction:
   !----------------------------------------------------------------------------

   do j = jts, jte
      r_fou(its) =  real(v_fou(j,0 ))  ! R(m=0) is real.
      ! r_fou(ite) = aimag(v_fou(j,0)) ! R(m=NI/2) is real, but packed in imag m = 0)
      ! make r_fou(ide) zero as there is no power computed corresponding to this wavenumber
      r_fou(ite) = 0.      

      do m = 1, max_wave
         index_r = 2 * m
         index_c = 2 * m + 1
         r_fou(index_r) = real(v_fou(j,m))
         r_fou(index_c) = aimag(v_fou(j,m))
      end do

      call rfft1b( ide, inc, r_fou, lenr, wsave, lensav, work, lenwrk, ier )
      field(its:ite,j) = r_fou(its:ite)
   end do
   
end subroutine da_v_to_vv_spectral

