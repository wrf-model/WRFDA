subroutine da_v_to_vv_spectral_adj(xbx, power, rcv, field, &
                                   ids, ide, jds, jde, &
                                   ims, ime, jms, jme, &
                                   its, ite, jts, jte )

!-------------------------------------------------------------------------
!  Performs Adjoint of spectral to grid transformation on a sphere.
!
!  History:   November 20th 2004: Coded by   Syed RH Rizvi
!-------------------------------------------------------------------------

   implicit none

   type (xbx_type),intent(in) :: xbx  ! For header & non-grid arrays.
   integer,        intent(in) :: ids, ide, jds, jde, &
                                 ims, ime, jms, jme, &
                                 its, ite, jts, jte
   real, intent(in)    :: field(ims:ime,jms:jme)     ! Gridpoint field.

   real, intent(out)   :: rcv(1:2*xbx%c_cv_size)            ! Spectral modes.
   real, intent(in)    :: power(0:xbx%max_wavenumber)    ! Power spectrum

   integer             :: i, j, k, m, n              ! Loop counters.
   integer             :: index_start, index_end     ! Position markers in cv.
   integer             :: index                      ! Position markers in rcv.
   integer             :: index_r, index_c           ! Array index for complex v_fou.
   integer             :: ier                        ! Error return.

   complex, dimension(xbx%c_cv_size) :: cv           ! Spectral modes.
   complex, dimension(xbx%c_cv_size) :: cv_local     ! Spectral modes.

   complex             :: v_fou(its:ite,0:xbx%max_wavenumber)! Intermediate Fourier state.

   real                :: r_fou(1:xbx%lenr)             ! FFT array.
   real                :: work(1:xbx%lenwrk)            ! FFT work array. 

#ifdef DM_PARALLEL
   include 'mpif.h'
#endif

!-------------------------------------------------------------------------
!  logical             :: odd_longitudes             ! True if odd longitudes.
!  if(mod(ide-ids+1, 2) == 1 ) then    ! Odd number of longitudes:
!     odd_longitudes = .true.
!  else                                ! Even number of longitudes:
!     odd_longitudes = .false.
!  end if

!-------------------------------------------------------------------------
!  [1] Perform Adjoint of inverse Fourier decomposition in E-W direction:
!-------------------------------------------------------------------------
   v_fou = 0.
   do j = jts, jte
      r_fou(its:ite) = field(its:ite,j) 
      call rfft1f( xbx%ni, xbx%inc, r_fou, xbx%lenr, xbx%wsave, xbx%lensav, work, xbx%lenwrk, ier )
!-------------------------------------------------------------------------
!  Adjust the output for adjoint test
!-------------------------------------------------------------------------
       r_fou      =  real(ite)/2. * r_fou
       r_fou(its) =  r_fou(its)   * 2.0       

!      if(.not. odd_longitudes) r_fou(ite) = 2.0*r_fou(ite)   
!------make r_fou(ni) zero as there is no power computed corresponding to this wavenumber
       r_fou(ite) = 0.

       v_fou(j,0) = CMPLX( r_fou(its), r_fou(ite) )

       do m = 1, xbx%max_wavenumber
          index_r = 2 * m
          index_c = 2 * m + 1
          v_fou(j,m)  = v_fou(j,m) + cmplx(r_fou(index_r),r_fou(index_c))
       end do
   end do
!-------------------------------------------------------------------------
!  [2] Perform adjoint of inverse Legendre decomposition in N-S direction:
!-------------------------------------------------------------------------

   do m = 0, xbx%max_wavenumber
      index_start = m * ( xbx%max_wavenumber + 1 - m ) + m * ( m + 1 ) / 2 + 1
      index_end   = index_start + xbx%max_wavenumber - m

#ifdef DM_PARALLEL
      cv_local(index_start:index_end) = 0.0
      call da_legtra_inv_adj( jds, jde, jts, jte, xbx%max_wavenumber, xbx%alp_size, m, xbx%alp, &
                              cv_local(index_start:index_end), v_fou(jts:jte,m) )

      n = index_end - index_start + 1
      call MPI_ALLREDUCE( cv_local(index_start:index_end), cv(index_start:index_end), n, TRUE_MPI_COMPLEX, MPI_SUM, MPI_COMM_WORLD, IER )
#else
      call da_legtra_inv_adj( jds, jde, jts, jte, xbx%max_wavenumber, xbx%alp_size, m, xbx%alp, &
                               cv(index_start:index_end), v_fou(jts:jte,m) )
#endif
   end do
!-------------------------------------------------------------------------
!
! [3] Adjust Power spectrum
!-------------------------------------------------------------------------
   if( .not. test_transforms ) &
      call da_apply_power(power, xbx%max_wavenumber, cv, xbx%c_cv_size)

    do i=1, xbx%c_cv_size
     rcv(2*i - 1 ) = real ( cv(i) )
     rcv(2*i     ) = aimag( cv(i) )
    end do 

end subroutine da_v_to_vv_spectral_adj

