subroutine da_test_spectral( ni, nj, max_wavenumber, inc, lenr, lensav, lenwrk, &
                             alp_size, cv_size, alp, wsave, int_wgts, field )

   implicit none

   integer, intent(in) :: ni                         ! Number of longitudes.
   integer, intent(in) :: nj                         ! Number of latitudes.
   integer, intent(in) :: max_wavenumber             ! Smallest scale required (ni/2 - 1).
   integer, intent(in) :: inc                        ! Jump between elements of vector in array.
   integer, intent(in) :: lenr                       ! FFT array dimension (at least inc*(n-1)+1).
   integer, intent(in) :: lensav                     ! wsave dimension (n+int(log(real(ni)))+4).
   integer, intent(in) :: lenwrk                     ! Dimension of work array.
   integer, intent(in) :: alp_size                   ! Size of ALP vector.
   integer, intent(in) :: cv_size                    ! Size of control variable array.
   real, intent(in)    :: alp(1:alp_size)            ! Associated Legendre Polynomials.
   real, intent(in)    :: wsave(1:lensav)            ! Primes for FFT.
   real, intent(in)    :: int_wgts(1:nj)             ! Legendre integration weights.
   real, intent(in)    :: field(1:ni,1:nj)           ! Test input field.

   real                :: field_out(1:ni,1:nj)       ! Test field.
   complex             :: cv(1:cv_size)              ! Test control variable vector.
   complex             :: cv_out(1:cv_size)          ! Test control variable vector.

!------------------------------------------------------------------------------------------------------
   write(spec_unit,'(a)')' Test orthogonality of Associated Legendre Polynomials:'
!------------------------------------------------------------------------------------------------------
   call da_setlegpol_test( nj, max_wavenumber, alp_size, int_wgts, alp )

!------------------------------------------------------------------------------------------------------
   write(spec_unit,'(a)')' Test invertibility of spectral (Fourier, Legendre) transforms:'
!------------------------------------------------------------------------------------------------------

!  Gridpoint to spectral:
   call da_vv_to_v_spectral( ni, nj, max_wavenumber, inc, lenr, lensav, lenwrk, &
                             alp_size, cv_size, alp, wsave, int_wgts, field, cv )

!  Spectral to gridpoint:
   call da_v_to_vv_spectral( ni, nj, max_wavenumber, inc, lenr, lensav, lenwrk, &
                             alp_size, cv_size, alp, wsave, cv, field_out )

   write(spec_unit,'(a,f15.5)')' Domain-Averaged Grid->Spectral->Grid Error = ', &
                        sqrt( sum( ( field_out(1:ni,1:nj) - field(1:ni,1:nj) )**2 ) / &
                        sum( field(1:ni,1:nj)**2 ) )

!  Gridpoint to spectral (again):
   call da_vv_to_v_spectral( ni, nj, max_wavenumber, inc, lenr, lensav, lenwrk, &
                             alp_size, cv_size, alp, wsave, int_wgts, field_out, cv_out )

   cv_out(1:cv_size) = cv_out(1:cv_size) - cv(1:cv_size) ! Create difference for test diags.

   write(spec_unit,'(a,f15.5)')' Domain-Averaged Spectral->Grid Error->Spectral = ', &
                       sqrt( sum( real(cv_out(1:cv_size))**2 + aimag(cv_out(1:cv_size))**2 ) ) / &
                       sqrt( sum( real(    cv(1:cv_size))**2 + aimag(    cv(1:cv_size))**2 ) )

!------------------------------------------------------------------------------------------------------
   write(spec_unit,'(a)')' Test adjoint of spectral (Fourier, Legendre) transforms:'
!------------------------------------------------------------------------------------------------------

end subroutine da_test_spectral
