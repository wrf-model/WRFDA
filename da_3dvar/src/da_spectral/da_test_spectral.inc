subroutine da_test_spectral( ni, nj, max_wavenumber, inc, lenr, lensav, lenwrk, &
                             alp_size, r_cvsize, alp, wsave, int_wgts, field )
   implicit none
   integer, intent(in) :: ni                      ! Number of longitudes.
   integer, intent(in) :: nj                      ! Number of latitudes.
   integer, intent(in) :: max_wavenumber          ! Smallest scale required (ni/2 - 1).
   integer, intent(in) :: r_cvsize                ! Size of control variable array.
   integer, intent(in) :: inc                     ! Jump between elements of vector in array.
   integer, intent(in) :: lenr                    ! FFT array dimension (at least inc*(n-1)+1).
   integer, intent(in) :: lensav                  ! wsave dimension (n+int(log(real(ni)))+4).
   integer, intent(in) :: lenwrk                  ! Dimension of work array.
   integer, intent(in) :: alp_size                ! Size of ALP vector.
   real, intent(in)    :: alp(1:alp_size)         ! Associated Legendre Polynomials.
   real, intent(in)    :: wsave(1:lensav)         ! Primes for FFT.
   real, intent(in)    :: int_wgts(1:nj)          ! Legendre integration weights.
   real, intent(in)    :: field(1:ni,1:nj)        ! Test input field.
   real                :: field_out(1:ni,1:nj)    ! Test field.
   real                :: rcv(1:r_cvsize)         ! Test real control variable vector.
   real                :: rcv_out(1:r_cvsize)     ! Test real control variable vector.
   real                :: power(0:max_wavenumber)
   integer             :: m,mm, i, j, k,c_cvsize, index_start, index_end
   complex             :: r_leg(1:nj)     
   complex, allocatable:: cv(:)                      ! Spectral modes.
   complex, allocatable:: cv1(:)                     ! Spectral modes.
   real                :: den, num, xx
!------------------------------------------------------------------------------------------------------
   print*,' Test orthogonality of Associated Legendre Polynomials:'
!------------------------------------------------------------------------------------------------------
!  Initialise Power spectrum
   power  = 1.0
   call da_setlegpol_test( nj, max_wavenumber, alp_size, int_wgts, alp )

!------------------------------------------------------------------------------------------------------
   print*,' Test invertibility of spectral (Fourier, Legendre) transforms:'
!------------------------------------------------------------------------------------------------------

   c_cvsize  = 0.5*r_cvsize
   allocate (cv(1:c_cvsize))
   allocate (cv1(1:c_cvsize))
!  Gridpoint to spectral:
   rcv = 0. 
   call da_vv_to_v_spectral( ni, nj, max_wavenumber, inc, lenr, lensav, lenwrk, &
                             alp_size, r_cvsize, alp, wsave, int_wgts, rcv, field)
   field_out = 0.
!  Spectral to gridpoint:
  
   call da_v_to_vv_spectral( ni, nj, max_wavenumber, inc, lenr, lensav, lenwrk, &
                       alp_size, r_cvsize, alp, wsave, power, rcv, field_out )
   
   print 100,'Domain-Averaged (Grid->Spectral->Grid) Error = ', &
              sqrt( sum( ( field_out(1:ni,1:nj) - field(1:ni,1:nj) )**2 ) / &
                    sum( field(1:ni,1:nj)**2 ) )
100  format(1x,a,e30.10)
   rcv_out = 0.
   
!  Gridpoint to spectral (again):
   call da_vv_to_v_spectral( ni, nj, max_wavenumber, inc, lenr, lensav, lenwrk, &
                      alp_size, r_cvsize, alp, wsave, int_wgts, rcv_out, field_out)

   rcv_out(1:r_cvsize) = &
   rcv_out(1:r_cvsize) - rcv(1:r_cvsize) ! Create difference for test diags.
    
   print 100 ,' Domain-Averaged (Spectral->Grid->Spectral) Error = ', &
                       sqrt( sum( rcv_out(1:r_cvsize)**2 ) ) / &
                       sqrt( sum( rcv    (1:r_cvsize)**2 ) )

!Adjoint test for Spectral Transform
   rcv_out = 0
   call da_v_to_vv_spectral_adj( ni, nj, max_wavenumber, inc, lenr, lensav, lenwrk, &
                       alp_size, r_cvsize, alp, wsave, power, rcv_out, field_out)

   print*,' Adjoint test result for  Spectral -> Grid : '
   print 100, ' LHS  ( LX.LX)       = ',&
                    sum( field_out(1:ni,1:nj)*field_out(1:ni,1:nj) ) 
   print 100, ' RHS  (  X.L^TLX )   = ',&
                        sum( rcv(1:r_cvsize)*rcv_out(1:r_cvsize) ) 

! Inverse test for Legendre Transform


!------------------------------------------------------------------------------------------------------
   print*,'  Inverse and Adjoint Legendre test result:'
!------------------------------------------------------------------------------------------------------

  do m = 0, max_wavenumber
      index_start = m * ( max_wavenumber + 1 - m ) + m * ( m + 1 ) / 2 + 1
      index_end   = index_start + max_wavenumber - m

   do mm = index_start, index_end
   if( 2*mm > r_cvsize) stop 'rcv_out index bounce'
   cv(mm) = cmplx (rcv_out(2*mm-1), rcv_out(2*mm))                    
   end do
   r_leg = 0.0
   call da_leg_tra_inv ( nj, max_wavenumber, alp_size, m, alp, &
           cv(index_start:index_end), r_leg )

   cv1(index_start:index_end) = 0.
   call da_leg_tra ( nj, max_wavenumber, alp_size, m, int_wgts, alp, r_leg, &
                   cv1(index_start:index_end) )
   cv1(index_start:index_end) = cv1(index_start:index_end) - &
                                 cv(index_start:index_end) 
    num =  sum ( real(cv1(index_start:index_end))*real(cv1(index_start:index_end))+&
            aimag(cv1(index_start:index_end))* aimag(cv1(index_start:index_end)) )     
    den =  sum ( real(cv(index_start:index_end))*real(cv(index_start:index_end))+&
            aimag(cv(index_start:index_end))* aimag(cv(index_start:index_end)) )     
   print 12 ,m, sqrt(num)/sqrt(den)
12 format(1x,'For zonal wave number',i4,' difference ',e30.20)

   xx = sum( real(r_leg(1:nj))* real(r_leg(1:nj))+ &
            aimag(r_leg(1:nj))*aimag(r_leg(1:nj)) )
   print 10,'For Wave = ',m,' LX.LX    = ',xx
10  format(a,i5,a,e30.20)

   cv1(index_start:index_end) = 0.
   call da_leg_tra_inv_adj( nj, max_wavenumber, alp_size, m, alp, &
           cv1(index_start:index_end), r_leg )
   xx = sum( real(cv(index_start:index_end))*     &
             real(cv1(index_start:index_end))     +&
            aimag(cv(index_start:index_end))* &
            aimag(cv1(index_start:index_end)) )
   print 11,'For Wave = ',m,' X.L^TLX  = ',xx
11  format(a,i5,a,e30.20,/)

   end do

end subroutine da_test_spectral
