subroutine da_minimise( grid, config_flags, &
                        it, ob, xb, xbx, be, iv, xhat, &
                        cv, xa, vv, vp, xp, re, y, j,  &
                        ids, ide, jds, jde, kds, kde,  &
                        ims, ime, jms, jme, kms, kme,  &
                        its, ite, jts, jte, kts, kte )

!-------------------------------------------------------------------------
!  Purpose: Main minimisation wrapper
!-------------------------------------------------------------------------

   implicit none

   type(domain) ,   TARGET                   :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   integer, intent(in)               :: it    ! external iteration.
   type (y_type), intent(in)         :: ob    ! Observation structure.
   type (xb_type), intent(in)        :: xb    ! first guess (local).
   type (xbx_type),intent(in)        :: xbx   ! Header & non-gridded vars.
   type (be_type), intent(in)        :: be    ! background error structure.
   type (ob_type), intent(inout)     :: iv    ! ob. increment vector.
   real,intent(out),dimension(cv_size)::xhat! Increament control variable
   type (cv_type), intent(inout)     :: cv    ! control variable (local).
   type (x_type), intent(out)        :: xa    ! analysis increments (local).
   type (vp_type), intent(inout)     :: vv    ! Grdipt/EOF CV.
   type (vp_type), intent(inout)     :: vp    ! Grdipt/level CV.
   type (xpose_type), intent(inout)  :: xp    ! Dimensions and xpose buffers. 
   type (y_type), intent(inout)      :: re    ! residual (o-a) structure.
   type (y_type), intent(inout)      :: y     ! y = H(x_inc) structure.

   type (j_type), intent(out)        :: j     ! cost function
   integer, intent(in)               :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)               :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)               :: its,ite, jts,jte, kts,kte ! tile   dims.

   character(len=120)                :: message

   sm31             = grid%sm31
   em31             = grid%em31
   sm32             = grid%sm32
   em32             = grid%em32
   sm33             = grid%sm33
   em33             = grid%em33

   sm31x            = grid%sm31x
   em31x            = grid%em31x
   sm32x            = grid%sm32x
   em32x            = grid%em32x
   sm33x            = grid%sm33x
   em33x            = grid%em33x

   sm31y            = grid%sm31y
   em31y            = grid%em31y
   sm32y            = grid%sm32y
   em32y            = grid%em32y
   sm33y            = grid%sm33y
   em33y            = grid%em33y

!--Minimize cost function:

   if(minimisation_option == 1) then
      CALL DA_Minimise_QN( grid, config_flags, &
                           it, ob, xb, xbx, be, iv, xhat, &
                           cv, xa, vv, vp, xp, re, y, j,  &
                           ids, ide, jds, jde, kds, kde,  &
                           ims, ime, jms, jme, kms, kme,  &
                           its, ite, jts, jte, kts, kte )
   else if(minimisation_option == 2) then
      CALL DA_Minimise_CG( grid, config_flags, &
                           it, ob, xb, xbx, be, iv, xhat, &
                           cv, xa, vv, vp, xp, re, y, j,  &
                           ids, ide, jds, jde, kds, kde,  &
                           ims, ime, jms, jme, kms, kme,  &
                           its, ite, jts, jte, kts, kte )
   else
      WRITE( message , * ) &
           'da_3dvar/src/DA_Minimisation/da_minimise.inc: ', &
           'Invalid minimisation_option =', minimisation_option
      CALL WRF_ERROR_FATAL ( message )
   endif

end subroutine da_minimise

