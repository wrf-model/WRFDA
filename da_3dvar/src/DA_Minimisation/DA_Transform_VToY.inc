subroutine DA_Transform_VToY(be, cv, iv, vp, vv, xa, xb, xbx, xp, y, &
                             ids, ide, jds, jde, kds, kde,  &
                             ims, ime, jms, jme, kms, kme,  &
                             its, ite, jts, jte, kts, kte,  &
                             grid, config_flags )

!-------------------------------------------------------------------------
!  History:
!
!    Purpose:  Does transform of control variable (V) to Obs-space (Y)
!
!   Creation:  03/14/2005                    Syed RH Rizvi
!
! PARENT_MODULE:  DA_VToX_Transforms 
!-------------------------------------------------------------------------
      implicit none

      type (be_type), intent(in)         :: be     ! background error structure.
      real,dimension(cv_size),intent(in) :: cv     ! control variable (local).
      type (ob_type), intent(inout)      :: iv     ! innovation vector (o-b).
      type (vp_type), intent(inout)      :: vp     ! Grdipt/level CV.
      type (vp_type), intent(inout)      :: vv     ! Grdipt/EOF CV.
      type (x_type) , intent(inout)      :: xa     ! gridded analy. incs. (local)
      type (xb_type), intent(in)         :: xb     ! first guess (local).
      type (xbx_type),intent(in)         :: xbx    ! For header & non-grid arrays.
      type (xpose_type), intent(inout)   :: xp     ! Domain decomposition vars.
      type (y_type) , intent(inout)      :: y      ! y = H(x_inc).

      integer, intent(in)           :: ids,ide, jds,jde, kds,kde ! domain dims.
      integer, intent(in)           :: ims,ime, jms,jme, kms,kme ! memory dims.
      integer, intent(in)           :: its,ite, jts,jte, kts,kte ! tile   dims.
      
      type(domain) ,   TARGET                   :: grid
      type(grid_config_rec_type), intent(inout) :: config_flags

      integer :: nobwin

      character(len=4) :: filnam

!
      call da_transform_vtox( xb, xbx, be, cv, vv, vp, xp, xa,  &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              its, ite, jts, jte, kts, kte )

      if ( lvar4d ) then

#ifdef WRFVAR
         filnam = 'tl00'
         call DA_Transfer_XatoWRFTL( grid, config_flags, filnam, &
#include <em_actual_args.inc>
                                 )

         call system("runvar4dtl.csh")

         do nobwin=1, num_fgat_time
            iv%current_ob_time = nobwin
            write(filnam,'(a2,i2.2)') 'tl',nobwin-1
            call DA_Transfer_WRFTLtoXa( grid, config_flags, filnam, &
#include <em_actual_args.inc>
                                 )
            call da_transform_xtoy( xb, iv, xa, xp, y )
         enddo
#else
         call da_transform_xtoy( xb, iv, xa, xp, y )
#endif


      else  ! lvar4d

         call da_transform_xtoy( xb, iv, xa, xp, y )

      endif ! lvar4d

end subroutine DA_Transform_VToY

