subroutine da_setup_flow_predictors( ix, jy, kz, ne, ep )

!------------------------------------------------------------------------------
!  PURPOSE: Setup structures for flow-dependent information and read it in. 
!
!  METHOD:  
!
!  HISTORY: 11/10/2005 - Creation.                          Dale Barker
!
!  PARENT_MODULE: da_setup_structures
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in)         :: ix, jy, kz            ! EP grid dimensions.
   integer, intent(in)         :: ne                    ! Ensemble size.
   type (ep_type), intent(inout):: ep                   ! Flow-dependent info.

   character*10                :: ce                    ! Member index -> character.
   character*200               :: filename              ! Input filename.
   character*10                :: var(1:5)              ! Variable name.
   integer                     :: ni, nj, nk            ! Grid dimensions.
   integer                     :: e, i, j               ! Loop counters.
   logical                     :: ldum1, ldum2,nkdum    ! Dummy.
   real                        :: temp3d(1:ix,1:jy,1:kz)! Temporary, real*4 array.
   real                        :: temp2d(1:ix,1:jy)     ! Temporary, real*4 array.

   real                        :: ens_scaling_inv       ! Ensemble scaling of perturbations.

   integer                     :: ep_unit

   IF (trace_use) call da_trace_entry("da_setup_flow_predictors")

   call da_get_unit(ep_unit)

   call da_message( &
      (/"Setup structures for flow-dependent information and read in"/))

   ep % ne = ne

   ens_scaling_inv = 1.0
   if (ne > 1) ens_scaling_inv = 1.0 / sqrt(real(ne-1))

!  Decide which space we are introducing flow-dependence:
   if ( alphacv_method == 2 ) then    ! xa space
      var(1) = 'u'
      var(2) = 'v'
      var(3) = 't'
      var(4) = 'q'
      var(5) = 'ps'
   else                               ! vp space (default)
      var(1) = 'psi'
      var(2) = 'chi_u'
      var(3) = 't_u'
      var(4) = 'rh'
      var(5) = 'ps_u'
   end if

!------------------------------------------------------------------------------
!  Input ensemble perturbations
!------------------------------------------------------------------------------

   do e = 1, ne

      write(UNIT=ce,FMT='(i3.3)')e

!     v1:
      filename = 'ep/'//trim(var(1))//'/'//trim(var(1))//'.e'//trim(ce)
      open(UNIT=ep_unit, file = filename, form = 'unformatted', status = 'old' )
      read(UNIT=ep_unit) ni, nj, nk

      if ( ni /= ix .or. nj /= jy .or. nk /= kz) then
         write(UNIT=stdout,FMT='(a)')' da_setup_flow_predictors: Inconsistent grid dimensions. Stop.'
         write(UNIT=stdout,FMT='(a,3i6)')' Grid dims for analysis grid: ', ix, jy
         write(UNIT=stdout,FMT='(a,3i6)')' Grid dims for flow predictors: ', ni, nj
      end if

      read(UNIT=ep_unit) temp3d(1:ix,1:jy,1:kz)
      close(UNIT=ep_unit)
      ep % v1(1:ix,1:jy,1:kz,e) = ens_scaling_inv * temp3d(1:ix,1:jy,1:kz)

!     v2:
      filename = 'ep/'//trim(var(2))//'/'//trim(var(2))//'.e'//trim(ce)
      open(UNIT=ep_unit, file = filename, form = 'unformatted', status = 'old' )
      read(UNIT=ep_unit) ni, nj, nk
      read(UNIT=ep_unit) temp3d(1:ix,1:jy,1:kz)
      ep % v2(1:ix,1:jy,1:kz,e) = ens_scaling_inv * temp3d(1:ix,1:jy,1:kz)
      close(UNIT=ep_unit)

!     v3:
      filename = 'ep/'//trim(var(3))//'/'//trim(var(3))//'.e'//trim(ce)
      open(UNIT=ep_unit, file = filename, form = 'unformatted', status = 'old' )
      read(UNIT=ep_unit) ni, nj, nk
      read(UNIT=ep_unit) temp3d(1:ix,1:jy,1:kz)
      ep % v3(1:ix,1:jy,1:kz,e) = ens_scaling_inv * temp3d(1:ix,1:jy,1:kz)
      close(UNIT=ep_unit)

!     v4:
      filename = 'ep/'//trim(var(4))//'/'//trim(var(4))//'.e'//trim(ce)
      open(UNIT=ep_unit, file = filename, form = 'unformatted', status = 'old' )
      read(UNIT=ep_unit) ni, nj, nk
      read(UNIT=ep_unit) temp3d(1:ix,1:jy,1:kz)
      ep % v4(1:ix,1:jy,1:kz,e) = ens_scaling_inv * temp3d(1:ix,1:jy,1:kz)
      close(UNIT=ep_unit)

!     v5:
      filename = 'ep/'//trim(var(5))//'/'//trim(var(5))//'.e'//trim(ce)
      open(UNIT=ep_unit, file = filename, form = 'unformatted', status = 'old' )
      read(UNIT=ep_unit) ni, nj, nkdum
      read(UNIT=ep_unit) ldum1, ldum2
      read(UNIT=ep_unit) temp2d(1:ix,1:jy)
      ep % v5(1:ix,1:jy,1,e) = ens_scaling_inv * temp2d(1:ix,1:jy)
      close(UNIT=ep_unit)

   end do 

   call da_free_unit(ep_unit)

   IF (trace_use) call da_trace_exit("da_setup_flow_predictors")

end subroutine da_setup_flow_predictors

