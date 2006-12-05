subroutine da_transform_vtovv( cv_size, xb, be, cv, vv, xp, &
                               ids,ide, jds,jde, kds,kde, &
                               ims,ime, jms,jme, kms,kme, &
                               its,ite, jts,jte, kts,kte )

   implicit none

   integer, intent(in)              :: cv_size ! Size of cv array.
   type (xb_type), intent(in)       :: xb   ! First guess structure.
   type (be_type), intent(in)       :: be   ! Background error structure.
   real, intent(in)                 :: cv(1:cv_size)   ! control variables.
   type (vp_type), intent(inout)    :: vv   ! Grid point/EOF equivalent.
   type (xpose_type), intent(inout) :: xp   ! Dimensions and xpose buffers. 
   integer, intent(in)              :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)              :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)              :: its,ite, jts,jte, kts,kte ! tile   dims.

   integer                          :: mz   ! Vertical truncation.

   if (trace_use) call da_trace_entry("da_transform_vtovv")

   !-------------------------------------------------------------------------
   ! [1.0] Fill vv arrays from 1-dimensional cv array.
   !-------------------------------------------------------------------------

   call da_cv_to_vv( cv_size, cv, xp, &
      (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%ne /), vv)

   !-------------------------------------------------------------------------
   ! [2.0] Perform VToVV Transform:
   !-------------------------------------------------------------------------

   ! [2.1] Transform 1st control variable:

   mz = be % v1 % mz
   if ( mz > 0 ) then
      call da_transform_through_rf( mz, xb % grid_box_area, be % v1 % rf_alpha,&
                                    be % v1 % val, xp, vv % v1, &
                                    ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte )
   end if

   ! [2.2] Transform 2nd control variable:

   mz = be % v2 % mz
   if ( mz > 0 ) then
      call da_transform_through_rf( mz, xb % grid_box_area, be % v2 % rf_alpha,&
                                    be % v2 % val, xp, vv % v2, &
                                    ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte )
   end if

   ! [2.3] Transform 3rd control variable

   mz = be % v3 % mz
   if ( mz > 0 ) then
      call da_transform_through_rf( mz, xb % grid_box_area, be % v3 % rf_alpha,&
                                    be % v3 % val, xp, vv % v3, &
                                    ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte )
   end if

   ! [2.4] Transform 4th control variable
      
   mz = be % v4 % mz
   if ( mz > 0 ) then
      call da_transform_through_rf( mz, xb % grid_box_area, be % v4 % rf_alpha,&
                                    be % v4 % val, xp, vv % v4, &
                                    ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte )
   end if

   ! [2.5] Transform 5th control variable

   mz = be % v5 % mz
   if ( mz > 0 ) then
      call da_transform_through_rf( mz, xb % grid_box_area, be % v5 % rf_alpha,&
                                    be % v5 % val, xp, vv % v5, &
                                    ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte )
   end if

   ! [2.6] Transform alpha control variable

   mz = be % alpha % mz
   if ( mz > 0 ) then
      call da_transform_through_rf( mz, xb % grid_box_area, be % alpha % rf_alpha,&
                                    be % alpha % val, xp, vv % alpha, &
                                    ids,ide, jds,jde, kds,kde,  &
                                    ims,ime, jms,jme, kms,kme,  &
                                    its,ite, jts,jte, kts,kte )
   end if

   if (trace_use) call da_trace_exit("da_transform_vtovv")


end subroutine da_transform_vtovv

