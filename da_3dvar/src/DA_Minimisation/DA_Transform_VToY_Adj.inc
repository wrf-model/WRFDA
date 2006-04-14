subroutine DA_Transform_VToY_Adj( cv_size, be, ep, cv, iv, vp, vv, xa, xb, xbx, xp, y, &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  its, ite, jts, jte, kts, kte,     &
                                  grid, config_flags )

!-------------------------------------------------------------------------
!    Purpose:  Does Adjoint of control variable (V) transform to Obs-space(Y)
!
! PARENT_MODULE:  DA_VToX_Transforms
!-------------------------------------------------------------------------

     implicit none
      integer, intent(in)                :: cv_size ! Size of cv array.
      type (be_type), intent(in)         :: be     ! background error structure.
      type (ep_type), intent(in)         :: ep     ! ensemble perturbation structure.
      real, intent(out)                  :: cv(1:cv_size) ! control variables.
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

      integer :: nobwin,ndynopt

      character(len=4) :: filnam
!-------------------------------------------------------------------------

      if ( lvar4d ) then

         do nobwin=num_fgat_time,1,-1
            iv%current_ob_time = nobwin
            call DA_zero_x(xa)
            call da_transform_xtoy_adj( xb, iv, xp, y, xa )
            write(filnam,'(a2,i2.2)') 'af',nobwin-1
            write(unit=*, fmt='(2a,2x,a,i6)') 'file:', __FILE__, 'line:', __LINE__
            call DA_Transfer_WRFTLtoXa_Adj( grid, config_flags, filnam, &
#include <em_actual_args.inc>
                                 )
         enddo

         call system("runvar4dad.csh")
         filnam = 'gr00'
         call DA_Transfer_XatoWRFTL_Adj( grid, config_flags, filnam, &
#include <em_actual_args.inc>
                                 )
      else  ! lvar4d

         call DA_zero_x(xa)
         call da_transform_xtoy_adj( xb, iv, xp, y, xa )

      endif ! lvar4d

!-------------------------------------------------------------------------
      cv = 0.0
      call da_transform_vtox_adj( cv_size, xb, xbx, be, ep, xa, xp, vp, vv, cv, &
                                  ids, ide, jds, jde, kds, kde,       &
                                  ims, ime, jms, jme, kms, kme,       &
                                  its, ite, jts, jte, kts, kte )

end subroutine DA_Transform_VToY_Adj

