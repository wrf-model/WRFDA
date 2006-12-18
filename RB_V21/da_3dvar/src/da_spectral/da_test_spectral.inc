subroutine da_test_spectral(xbx, field, &
                            ids, ide, jds, jde, &
                            ims, ime, jms, jme, &
                            its, ite, jts, jte )

   implicit none

   type (xbx_type),intent(in) :: xbx  ! For header & non-grid arrays.
   integer,        intent(in) :: ids, ide, jds, jde, &
                                 ims, ime, jms, jme, &
                                 its, ite, jts, jte

   real, dimension(ims:ime,jms:jme), intent(in) :: field    ! Gridpoint field.

   real, dimension(ims:ime,jms:jme)      :: field_out
   real, dimension(0:xbx%max_wavenumber) :: power   ! Power spectrum
   real, dimension(1:2*xbx%c_cv_size)    :: rcv     ! Spectral modes.
   real, dimension(1:2*xbx%c_cv_size)    :: rcv_out ! Spectral modes.

   integer             :: m,mm, i, j, k,c_cvsize, index_start, index_end

   complex             :: r_leg(1:xbx%nj)     
   complex, dimension(1:xbx%c_cv_size) :: cv, cv1    ! Spectral modes.
   real                :: den, num, xx
!------------------------------------------------------------------------------------------------------
   print*,' Test orthogonality of Associated Legendre Polynomials:'
!------------------------------------------------------------------------------------------------------
!  Initialise Power spectrum
   power  = 1.0
   call da_setlegpol_test( xbx%nj, xbx%max_wavenumber, xbx%alp_size, xbx%int_wgts, xbx%alp )

!------------------------------------------------------------------------------------------------------
   print*,' Test invertibility of spectral (Fourier, Legendre) transforms:'
!------------------------------------------------------------------------------------------------------

!  Gridpoint to spectral:
   rcv = 0. 
   call da_vv_to_v_spectral( xbx%ni, xbx%nj, xbx%max_wavenumber, xbx%inc, xbx%lenr, xbx%lensav, xbx%lenwrk, &
                             xbx%alp_size, 2*xbx%c_cv_size, xbx%alp, xbx%wsave, xbx%int_wgts, rcv, field)

   field_out = 0.
!  Spectral to gridpoint:
   call da_v_to_vv_spectral(xbx, power, rcv, field_out, &
                            ids, ide, jds, jde, &
                            ims, ime, jms, jme, &
                            its, ite, jts, jte )

   print 100,'Domain-Averaged (Grid->Spectral->Grid) Error = ', &
              sqrt( sum( ( field_out(1:xbx%ni,1:xbx%nj) - field(1:xbx%ni,1:xbx%nj) )**2 ) / &
                    sum( field(1:xbx%ni,1:xbx%nj)**2 ) )
100  format(1x,a,e30.10)
   rcv_out = 0.
   
!  Gridpoint to spectral (again):
   call da_vv_to_v_spectral( xbx%ni, xbx%nj, xbx%max_wavenumber, xbx%inc, xbx%lenr, xbx%lensav, xbx%lenwrk, &
                      xbx%alp_size, 2*xbx%c_cv_size, xbx%alp, xbx%wsave, xbx%int_wgts, rcv_out, field_out)

   rcv_out(1:2*xbx%c_cv_size) = &
   rcv_out(1:2*xbx%c_cv_size) - rcv(1:2*xbx%c_cv_size) ! Create difference for test diags.
    
   print 100 ,' Domain-Averaged (Spectral->Grid->Spectral) Error = ', &
                       sqrt( sum( rcv_out(1:2*xbx%c_cv_size)**2 ) ) / &
                       sqrt( sum( rcv    (1:2*xbx%c_cv_size)**2 ) )

!Adjoint test for Spectral Transform
   rcv_out = 0.0
   call da_v_to_vv_spectral_adj(xbx, power, rcv_out, field_out, &
                                ids, ide, jds, jde, &
                                ims, ime, jms, jme, &
                                its, ite, jts, jte )

   print*,' Adjoint test result for  Spectral -> Grid : '
   print 100, ' LHS  ( LX.LX)       = ',&
                    sum( field_out(1:xbx%ni,1:xbx%nj)*field_out(1:xbx%ni,1:xbx%nj) ) 
   print 100, ' RHS  (  X.L^TLX )   = ',&
                        sum( rcv(1:2*xbx%c_cv_size)*rcv_out(1:2*xbx%c_cv_size) ) 

! Inverse test for Legendre Transform


!------------------------------------------------------------------------------------------------------
   print*,'  Inverse and Adjoint Legendre test result:'
!------------------------------------------------------------------------------------------------------

  do m = 0, xbx%max_wavenumber
      index_start = m * ( xbx%max_wavenumber + 1 - m ) + m * ( m + 1 ) / 2 + 1
      index_end   = index_start + xbx%max_wavenumber - m

   do mm = index_start, index_end
   if( 2*mm > 2*xbx%c_cv_size) stop 'rcv_out index bounce'
   cv(mm) = cmplx (rcv_out(2*mm-1), rcv_out(2*mm))                    
   end do
   r_leg = 0.0
   call da_legtra_inv( jds, jde, jts, jte, xbx%max_wavenumber, xbx%alp_size, m, xbx%alp, &
                       cv(index_start:index_end), r_leg )

   cv1(index_start:index_end) = 0.
   call da_legtra ( xbx%nj, xbx%max_wavenumber, xbx%alp_size, m, xbx%int_wgts, xbx%alp, r_leg, &
                    cv1(index_start:index_end) )
   cv1(index_start:index_end) = cv1(index_start:index_end) - &
                                 cv(index_start:index_end) 
    num =  sum ( real(cv1(index_start:index_end))*real(cv1(index_start:index_end))+&
            aimag(cv1(index_start:index_end))* aimag(cv1(index_start:index_end)) )     
    den =  sum ( real(cv(index_start:index_end))*real(cv(index_start:index_end))+&
            aimag(cv(index_start:index_end))* aimag(cv(index_start:index_end)) )     
   print 12 ,m, sqrt(num)/sqrt(den)
12 format(1x,'For zonal wave number',i4,' difference ',e30.20)

   xx = sum( real(r_leg(1:xbx%nj))* real(r_leg(1:xbx%nj))+ &
            aimag(r_leg(1:xbx%nj))*aimag(r_leg(1:xbx%nj)) )
   print 10,'For Wave = ',m,' LX.LX    = ',xx
10  format(a,i5,a,e30.20)

   cv1(index_start:index_end) = 0.
   call da_legtra_inv_adj( jds, jde, jts, jte, xbx%max_wavenumber, xbx%alp_size, m, xbx%alp, &
                           cv1(index_start:index_end), r_leg )
   xx = sum( real(cv(index_start:index_end))*     &
             real(cv1(index_start:index_end))     +&
            aimag(cv(index_start:index_end))* &
            aimag(cv1(index_start:index_end)) )
   print 11,'For Wave = ',m,' X.L^TLX  = ',xx
11  format(a,i5,a,e30.20,/)

   end do

end subroutine da_test_spectral

