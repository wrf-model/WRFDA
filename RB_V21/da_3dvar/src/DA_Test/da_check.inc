subroutine da_check( xb, xbx, be, iv, cv, &
                     xa, vv, vp, xp, ob, y, &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte )

   implicit none

   type (xb_type),    intent(in)    :: xb    ! first guess (local).
   type (xbx_type),   intent(in)    :: xbx   ! Header & non-gridded vars.
   type (be_type),    intent(in)    :: be    ! background error structure.
   type (ob_type),    intent(inout) :: iv    ! ob. increment vector.
   type (cv_type),    intent(inout) :: cv    ! control variable (local).
   type (x_type),     intent(out)   :: xa    ! analysis increments (local).
   type (vp_type),    intent(inout) :: vv    ! Grdipt/EOF CV.
   type (vp_type),    intent(inout) :: vp    ! Grdipt/level CV.
   type (xpose_type), intent(inout) :: xp    ! Dimensions and xpose buffers. 
   TYPE (y_type),     intent(inout) :: ob            ! yo
   TYPE (y_type),     intent(inout) :: y             ! y = h (xa)

   integer, intent(in)            :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)            :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)            :: its,ite, jts,jte, kts,kte ! tile   dims.
   
!------------------------------------------------------------------------------------

   write(unit=*, fmt='(a)') &
        '----------------------------------', &
        ' Transform test and adjoint check ', &
        '----------------------------------'
   write(unit=*, fmt='(/)')

!--test transforms:

   if ( test_transforms ) then
      write(unit=*, fmt='(/a/)') &
          'Test vxtransform'
      
      call da_test_vxtransform( xb, xbx, xp, be, &
                                xa, vv, vp, &
                               ids,ide, jds,jde, kds,kde, &
                               ims,ime, jms,jme, kms,kme, &
                               its,ite, jts,jte, kts,kte )

      write(unit=*, fmt='(/a/)') &
           'Test vxtransform Finished.'
   endif
    
!--test adjoint of y=hx transform:

   if ( Testing_3DVAR ) then

      write(unit=*, fmt='(/a/)') &
           'check_xtoy_adjoint'

      call da_zero_x(xa)

#if 1
      call da_setup_testfield( xb, xa, xp, &
                              ids,ide, jds,jde, kds,kde, &
                              ims,ime, jms,jme, kms,kme, &
                              its,ite, jts,jte, kts,kte )
#else

!-----Make cv_array random.

      call random_number(cv%array(:))
      cv%array(:) = cv%array(:) - 0.5

      call da_transform_vtox( xb, xbx, be, cv%array, &
                              vv, vp, xp, xa, &
                              ids, ide, jds, jde, kds, kde,           &
                              ims, ime, jms, jme, kms, kme,           &
                              its, ite, jts, jte, kts, kte )
#endif

!     call check_sfc_assi( xb, xa, iv, xp, y, &
!                          ids,ide, jds,jde, kds,kde, &
!                          ims,ime, jms,jme, kms,kme, &
!                          its,ite, jts,jte, kts,kte )

      call da_check_xtoy_adjoint( xb, xa, iv, xp, y, &
                                 ids,ide, jds,jde, kds,kde, &
                                 ims,ime, jms,jme, kms,kme, &
                                 its,ite, jts,jte, kts,kte )

      write(unit=*, fmt='(/a/)') &
           'check_xtoy_adjoint  Finished.'
   endif

   write(unit=*, fmt='(/)')
   write(unit=*, fmt='(a)') &
        '-------------------------------------------', &
        ' Finished transform test and adjoint check ', &
        '-------------------------------------------'
   write(unit=*, fmt='(/)')

   CALL wrf_shutdown
   stop 'in da_check'

end subroutine da_check

