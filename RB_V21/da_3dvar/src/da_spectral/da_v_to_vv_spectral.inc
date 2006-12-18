subroutine da_v_to_vv_spectral(xbx, power, rcv, field, &
                               ids, ide, jds, jde, &
                               ims, ime, jms, jme, &
                               its, ite, jts, jte )

!-------------------------------------------------------------------------
!  Performs spectral to gridpoint transformation on a sphere.
!
!  History:    November 20th 2004: Coded by    Syed RH Rizvi  
!-------------------------------------------------------------------------

   implicit none

   type (xbx_type),intent(in) :: xbx  ! For header & non-grid arrays.
   integer,        intent(in) :: ids, ide, jds, jde, &
                                 ims, ime, jms, jme, &
                                 its, ite, jts, jte

   real, intent(out)   :: field(ims:ime,jms:jme)     ! Gridpoint field.

   real, intent(in)    :: rcv(1:2*xbx%c_cv_size)            ! Spectral modes.
   real, intent(in)    :: power(0:xbx%max_wavenumber)    ! Power spectrum 

   integer             ::  i, j, k , m , n           ! Loop counters.
   integer             :: index_start, index_end     ! Position markers in cv.
   integer             :: index                      ! Position markers in rcv.
   integer             :: index_r, index_c           ! Array index for complex v_fou.
   integer             :: ier                        ! Error return.

   real                :: r_fou(1:xbx%lenr)              ! FFT array.
   real                :: work(1:xbx%lenwrk)             ! FFT work array. 

   complex             :: v_fou(its:ite,0:xbx%max_wavenumber)! Intermediate Fourier state.
   complex             :: r_leg(jts:jte)                ! Leg Tra. array.

   complex, dimension(xbx%c_cv_size) :: cv              ! Spectral modes.
   real                :: xx
!---------------------------------------------------------------------------------

   v_fou = 0.0
   do n = 1, xbx%c_cv_size
      cv(n)  = CMPLX( rcv(2*n-1), rcv(2*n) )
   end do
!---------------------------------------------------------------------------------
! [1]  Adjust Power spectrum
   if( .not. test_transforms ) &
     call da_apply_power(power, xbx%max_wavenumber, cv, xbx%c_cv_size)
!---------------------------------------------------------------------------------
!  [2] Perform inverse Legendre decomposition in N-S direction:

   do m = 0, xbx%max_wavenumber
      index_start = m * ( xbx%max_wavenumber + 1 - m ) + m * ( m + 1 ) / 2 + 1
      index_end   = index_start + xbx%max_wavenumber - m
      call da_legtra_inv(jds, jde, jts, jte, xbx%max_wavenumber, &
                         xbx%alp_size, m, xbx%alp, &
                         cv(index_start:index_end),  v_fou(jts:jte,m) )
   end do

!---------------------------------------------------------------------------------

!  [3] Perform inverse Fourier decomposition in E-W direction:

   do j = jts, jte
      r_fou(its) =  real(v_fou(j,0 ))  ! R(m=0) is real.
!     r_fou(ite) = aimag(v_fou(j,0))   ! R(m=NI/2) is real, but packed in imag m = 0)
!-----make r_fou(ni) zero as there is no power computed corresponding to this wavenumber

      r_fou(ite) = 0.      

      do m = 1, xbx%max_wavenumber
         index_r = 2 * m
         index_c = 2 * m + 1
         r_fou(index_r) = real(v_fou(j,m))
         r_fou(index_c) = aimag(v_fou(j,m))
      end do

!---------------------------------------------------------------------------------
      call rfft1b( xbx%ni, xbx%inc, r_fou, xbx%lenr, xbx%wsave, xbx%lensav, &
                   work, xbx%lenwrk, ier )
      field(its:ite,j) = r_fou(its:ite)
   end do
   
end subroutine da_v_to_vv_spectral

