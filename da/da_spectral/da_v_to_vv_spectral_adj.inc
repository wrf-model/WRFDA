subroutine da_v_to_vv_spectral_adj( max_wavenumber, sizec, lenr, lenwrk, lensav, inc, &
                                    alp_size, alp, wsave, power, rcv, field, &
                                    ids, ide, jds, jde, &
                                    ims, ime, jms, jme, &
                                    its, ite, jts, jte )

   !----------------------------------------------------------------------
   ! Performs Adjoint of spectral to grid transformation on a sphere.
   !----------------------------------------------------------------------

   implicit none

   integer, intent(in) :: max_wavenumber             ! Max total wavenumber.
   integer, intent(in) :: sizec                  ! Size of packed spectral array.
   integer, intent(in) :: lenr                       ! FFT info.
   integer, intent(in) :: lenwrk                     ! FFT info.
   integer, intent(in) :: lensav                     ! FFT info.
   integer, intent(in) :: inc                        ! FFT info.
   integer, intent(in) :: alp_size                   ! Size of alp array.
   real, intent(in)    :: alp(1:alp_size)            ! Associated Legendre Polynomials.
   real, intent(in)    :: wsave(1:lensav)            ! Primes for FFT.
   real, intent(in)    :: power(0:max_wavenumber)    ! Power spectrum
   real, intent(out)   :: rcv(1:2*sizec)             ! Spectral modes.
   real, intent(in)    :: field(its:ite,jts:jte)     ! Gridpoint field.
   integer, intent(in) :: ids, ide, jds, jde, &
                          ims, ime, jms, jme, &
                          its, ite, jts, jte

   integer             :: j, m, n                    ! Loop counters.
   integer             :: index_start, index_end     ! Position markers in cv.
   integer             :: index                      ! Position markers in rcv.
   integer             :: index_r, index_c           ! Array index for complex v_fou.
   integer             :: ier                        ! Error return.

   real                :: r_fou(1:lenr)              ! FFT array.
   real                :: work(1:lenwrk)             ! FFT work array.
   complex             :: v_fou(its:ite,0:max_wavenumber)! Intermediate Fourier state.
   complex             :: r_leg(jts:jte)             ! Leg Tra. array.
   complex             :: ccv(1:sizec)               ! Spectral modes.
   complex             :: ccv_local(1:sizec)         ! Spectral modes.

#ifdef DM_PARALLEL
#include <mpif.h>
#endif

   IF (trace_use) CALL da_trace_entry("da_v_to_vv_spectral_adj")

   !----------------------------------------------------------------------
   ! [1] Perform Adjoint of inverse Fourier decomposition in E-W direction:
   !----------------------------------------------------------------------

   v_fou = 0.
   do j = jts, jte
      r_fou(its:ite) = field(its:ite,j) 
      call rfft1f( ide, inc, r_fou, lenr, wsave, lensav, work, lenwrk, ier )

      !----------------------------------------------------------------------
      ! Adjust the output for adjoint test
      !----------------------------------------------------------------------
      r_fou      =  real(ite)/2. * r_fou
      r_fou(its) =  r_fou(its)   * 2.0       

      ! if(.not. odd_longitudes) r_fou(ite) = 2.0*r_fou(ite)   
      ! make r_fou(ide) zero as there is no power computed for this wavenumber
      r_fou(ite) = 0.

      v_fou(j,0) = CMPLX( r_fou(its), r_fou(ite) )

      do m = 1, max_wavenumber
         index_r = 2 * m
         index_c = 2 * m + 1
         v_fou(j,m)  = v_fou(j,m) + cmplx(r_fou(index_r),r_fou(index_c))
      end do
   end do

   !----------------------------------------------------------------------
   ! [2] Perform adjoint of inverse Legendre decomposition in N-S direction:
   !----------------------------------------------------------------------

   do m = 0, max_wavenumber
      index_start = m * ( max_wavenumber + 1 - m ) + m * ( m + 1 ) / 2 + 1
      index_end   = index_start + max_wavenumber - m

#ifdef DM_PARALLEL
      ccv_local(index_start:index_end) = 0.0
      call da_legtra_inv_adj( jds, jde, jts, jte, max_wavenumber, alp_size, m,alp, &
                              ccv_local(index_start:index_end), v_fou(jts:jte,m) )

      n = index_end - index_start + 1
      call MPI_ALLREDUCE( ccv_local(index_start:index_end), &
                          ccv(index_start:index_end), n, TRUE_MPI_COMPLEX, MPI_SUM, &
                          MPI_COMM_WORLD, IER )
#else
      call da_legtra_inv_adj( jds, jde, jts, jte, max_wavenumber, alp_size, m, alp, &
                              ccv(index_start:index_end), v_fou(jts:jte,m) )
#endif
   end do

   !----------------------------------------------------------------------
   ! [2] Apply Power spectrum
   !-------------------------------------------------------------------------

   if( .not. test_transforms ) call da_apply_power(power, max_wavenumber, ccv, sizec)

   do n = 1, sizec
      rcv(2*n - 1 ) = real ( ccv(n) )
      rcv(2*n     ) = aimag( ccv(n) )
   end do 

   IF (trace_use) CALL da_trace_exit("da_v_to_vv_spectral_adj")

end subroutine da_v_to_vv_spectral_adj

