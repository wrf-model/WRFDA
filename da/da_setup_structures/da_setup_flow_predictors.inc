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
   character*10                :: var                   ! Variable name.
   integer                     :: ni, nj, nk            ! Grid dimensions.
   integer                     :: e, i, j               ! Loop counters.
   logical                     :: ldum1, ldum2,nkdum    ! Dummy.
   real                        :: temp3d(1:ix,1:jy,1:kz)! Temporary, real*4 array.
   real                        :: temp2d(1:ix,1:jy)     ! Temporary, real*4 array.

   IF (trace_use) call da_trace_entry("da_setup_flow_predictors")


   call wrf_message("Setup structures for flow-dependent information and read in")

!------------------------------------------------------------------------------
!  Input ensemble perturbations
!------------------------------------------------------------------------------

   ep % ne = ne

   do e = 1, ne

      write(UNIT=ce,FMT='(i3)')e
      if ( e < 10 ) ce = '00'//ce(3:3)
      if ( e >= 10 .and. e  < 100 ) ce = '0'//ce(2:3)

!     v1 = psi:
      var = 'psi'
      filename = 'ep/'//trim(var)//'.e'//trim(ce)
      open(UNIT=ep_unit, file = filename, form = 'unformatted', status = 'old' )
      read(UNIT=ep_unit) ni, nj, nk

      if ( ni /= ix .or. nj /= jy .or. nk /= kz) then
         write(UNIT=stdout,FMT='(a)')' da_setup_flow_predictors: Inconsistent grid dimensions. Stop.'
         write(UNIT=stdout,FMT='(a,3i6)')' Grid dims for analysis grid: ', ix, jy
         write(UNIT=stdout,FMT='(a,3i6)')' Grid dims for flow predictors: ', ni, nj
      end if

      read(UNIT=ep_unit) temp3d(1:ix,1:jy,1:kz)
      close(UNIT=ep_unit)
      ep % v1(1:ix,1:jy,1:kz,e) = temp3d(1:ix,1:jy,1:kz)

!     v2 = chi_u:
      var = 'chi_u'
      filename = 'ep/'//trim(var)//'.e'//trim(ce)
      open(UNIT=ep_unit, file = filename, form = 'unformatted', status = 'old' )
      read(UNIT=ep_unit) ni, nj, nk
      read(UNIT=ep_unit) temp3d(1:ix,1:jy,1:kz)
      ep % v2(1:ix,1:jy,1:kz,e) = temp3d(1:ix,1:jy,1:kz)
      close(UNIT=ep_unit)

!     v3 = t_u:
      var = 't_u'
      filename = 'ep/'//trim(var)//'.e'//trim(ce)
      open(UNIT=ep_unit, file = filename, form = 'unformatted', status = 'old' )
      read(UNIT=ep_unit) ni, nj, nk
      read(UNIT=ep_unit) temp3d(1:ix,1:jy,1:kz)
      ep % v3(1:ix,1:jy,1:kz,e) = temp3d(1:ix,1:jy,1:kz)
      close(UNIT=ep_unit)

!     v4 = pseudo rh:
      var = 'rh'
      filename = 'ep/'//trim(var)//'.e'//trim(ce)
      open(UNIT=ep_unit, file = filename, form = 'unformatted', status = 'old' )
      read(UNIT=ep_unit) ni, nj, nk
      read(UNIT=ep_unit) temp3d(1:ix,1:jy,1:kz)
      ep % v4(1:ix,1:jy,1:kz,e) = temp3d(1:ix,1:jy,1:kz)
      close(UNIT=ep_unit)

!     v5 = ps_u:
      var = 'ps_u'
      filename = 'ep/'//trim(var)//'.e'//trim(ce)
      open(UNIT=ep_unit, file = filename, form = 'unformatted', status = 'old' )
      read(UNIT=ep_unit) ni, nj, nkdum
      read(UNIT=ep_unit) ldum1, ldum2
      read(UNIT=ep_unit) temp2d(1:ix,1:jy)
      ep % v5(1:ix,1:jy,1,e) = temp2d(1:ix,1:jy)
      close(UNIT=ep_unit)

   end do 

   IF (trace_use) call da_trace_exit("da_setup_flow_predictors")


end subroutine da_setup_flow_predictors

