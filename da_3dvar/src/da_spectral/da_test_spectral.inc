subroutine da_test_spectral( max_wave, sizec, xbx, field, &
                             ids, ide, jds, jde, &
                             ims, ime, jms, jme, &
                             its, ite, jts, jte )

   implicit none

   integer, intent(in)        :: max_wave ! Maximum wavenumber.
   integer, intent(in)        :: sizec    ! Size of complex cv.
   type (xbx_type),intent(in) :: xbx  ! For header & non-grid arrays.
   integer,        intent(in) :: ids, ide, jds, jde, &
                                 ims, ime, jms, jme, &
                                 its, ite, jts, jte

   real, dimension(its:ite,jts:jte), intent(in) :: field    ! Gridpoint field.

   real, dimension(its:ite,jts:jte)      :: field_out
   real, dimension(0:max_wave)           :: power   ! Power spectrum
   real, dimension(1:2*sizec)    :: rcv     ! Spectral modes.
   real, dimension(1:2*sizec)    :: rcv_out ! Spectral modes.

   integer             :: m,mm, i, j, k, index_start, index_end

   complex             :: r_leg(1:jde)     
   complex, dimension(1:sizec) :: ccv, ccv1    ! Spectral modes.
   real                :: den, num, xx
!------------------------------------------------------------------------------------------------------
   print*,' Test orthogonality of Associated Legendre Polynomials:'
!------------------------------------------------------------------------------------------------------
!  Initialise Power spectrum
   power  = 1.0
   call da_setlegpol_test( jde, max_wave, xbx%alp_size, xbx%int_wgts, xbx%alp )

!------------------------------------------------------------------------------------------------------
   print*,' Test invertibility of spectral (Fourier, Legendre) transforms:'
!------------------------------------------------------------------------------------------------------

!  Gridpoint to spectral:
   rcv = 0. 
   call da_vv_to_v_spectral( xbx%ni, xbx%nj, max_wave, xbx%inc, xbx%lenr, xbx%lensav, xbx%lenwrk, &
                             xbx%alp_size, 2*sizec, xbx%alp, xbx%wsave, xbx%int_wgts, rcv, field)

   field_out = 0.
!  Spectral to gridpoint:
   call da_v_to_vv_spectral( max_wave, sizec, &
                             xbx % lenr, xbx % lenwrk, xbx % lensav, &
                             xbx % inc, xbx % alp_size, xbx % alp, &
                             xbx % wsave, power, rcv, field_out, &
                             ids, ide, jds, jde, &
                             ims, ime, jms, jme, &
                             its, ite, jts, jte )

   print 100,'Domain-Averaged (Grid->Spectral->Grid) Error = ', &
              sqrt( sum( ( field_out(1:xbx%ni,1:xbx%nj) - field(1:xbx%ni,1:xbx%nj) )**2 ) / &
                    sum( field(1:xbx%ni,1:xbx%nj)**2 ) )
100  format(1x,a,e30.10)
   rcv_out = 0.
   
!  Gridpoint to spectral (again):
   call da_vv_to_v_spectral( xbx%ni, xbx%nj, max_wave, xbx%inc, xbx%lenr, xbx%lensav, xbx%lenwrk, &
                      xbx%alp_size, 2*sizec, xbx%alp, xbx%wsave, xbx%int_wgts, rcv_out, field_out)

   rcv_out(1:2*sizec) = rcv_out(1:2*sizec) - rcv(1:2*sizec) ! Create difference for test diags.
    
   print 100 ,' Domain-Averaged (Spectral->Grid->Spectral) Error = ', &
                       sqrt( sum( rcv_out(1:2*sizec)**2 ) ) / sqrt( sum( rcv(1:2*sizec)**2 ) )

!Adjoint test for Spectral Transform
   rcv_out = 0.0
   call da_v_to_vv_spectral_adj( max_wave, sizec, &
                                 xbx % lenr, xbx % lenwrk, xbx % lensav, &
                                 xbx % inc, xbx % alp_size, xbx % alp, &
                                 xbx % wsave, power, rcv_out, field_out, &
                                 ids, ide, jds, jde, &
                                 ims, ime, jms, jme, &
                                 its, ite, jts, jte )

   print*,' Adjoint test result for  Spectral -> Grid : '
   print 100, ' LHS  ( LX.LX)       = ',&
                    sum( field_out(1:xbx%ni,1:xbx%nj)*field_out(1:xbx%ni,1:xbx%nj) ) 
   print 100, ' RHS  (  X.L^TLX )   = ', sum( rcv(1:2*sizec)*rcv_out(1:2*sizec) ) 

! Inverse test for Legendre Transform


!------------------------------------------------------------------------------------------------------
   print*,'  Inverse and Adjoint Legendre test result:'
!------------------------------------------------------------------------------------------------------

  do m = 0, max_wave
      index_start = m * ( max_wave + 1 - m ) + m * ( m + 1 ) / 2 + 1
      index_end   = index_start + max_wave - m

   do mm = index_start, index_end
      IF (2*mm > 2*sizec) THEN
         CALL wrf_error_fatal3(__FILE__,__LINE__, "rcv_out index bounce")
      END IF
      ccv(mm) = cmplx (rcv_out(2*mm-1), rcv_out(2*mm))                    
   end do
   r_leg = 0.0
   call da_legtra_inv( jds, jde, jts, jte, max_wave, xbx%alp_size, m, xbx%alp, &
                       ccv(index_start:index_end), r_leg )

   ccv1(index_start:index_end) = 0.
   call da_legtra ( xbx%nj, max_wave, xbx%alp_size, m, xbx%int_wgts, xbx%alp, r_leg, &
                    ccv1(index_start:index_end) )
   ccv1(index_start:index_end) = ccv1(index_start:index_end) - &
                                 ccv(index_start:index_end) 
    num =  sum ( real(ccv1(index_start:index_end))*real(ccv1(index_start:index_end))+&
            aimag(ccv1(index_start:index_end))* aimag(ccv1(index_start:index_end)) )     
    den =  sum ( real(ccv(index_start:index_end))*real(ccv(index_start:index_end))+&
            aimag(ccv(index_start:index_end))* aimag(ccv(index_start:index_end)) )     
   print 12 ,m, sqrt(num)/sqrt(den)
12 format(1x,'For zonal wave number',i4,' difference ',e30.20)

   xx = sum( real(r_leg(1:xbx%nj))* real(r_leg(1:xbx%nj))+ &
            aimag(r_leg(1:xbx%nj))*aimag(r_leg(1:xbx%nj)) )
   print 10,'For Wave = ',m,' LX.LX    = ',xx
10  format(a,i5,a,e30.20)

   ccv1(index_start:index_end) = 0.
   call da_legtra_inv_adj( jds, jde, jts, jte, max_wave, xbx%alp_size, m, xbx%alp, &
                           ccv1(index_start:index_end), r_leg )
   xx = sum( real(ccv(index_start:index_end))*     &
             real(ccv1(index_start:index_end))     +&
            aimag(ccv(index_start:index_end))* &
            aimag(ccv1(index_start:index_end)) )
   print 11,'For Wave = ',m,' X.L^TLX  = ',xx
11  format(a,i5,a,e30.20,/)

   end do

end subroutine da_test_spectral

