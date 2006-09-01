subroutine da_transform_vtoy_adj( iter,cv_size, be, ep, cv, iv, vp, vv, xa, xb, &
                                  xbx, xp, y, &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  its, ite, jts, jte, kts, kte,     &
                                  grid, config_flags )

   !-------------------------------------------------------------------------
   ! Purpose:  Does Adjoint of control variable (V) transform to Obs-space(Y)
   !-------------------------------------------------------------------------

   implicit none

   integer, intent(in)                :: iter
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
   integer ierr, comm

   character(len=4) :: filnam

   if (lvar4d) then
      do nobwin=num_fgat_time,1,-1
         iv%current_ob_time = nobwin
         call da_zero_x(xa)
         call da_transform_xtoy_adj( xb, iv, xp, y, xa )
         write(UNIT=filnam,FMT='(a2,i2.2)') 'af',nobwin-1
         write(unit=stdout, fmt='(2a,2x,a,i6)') &
            'file:', __FILE__, 'line:', __LINE__

         call da_transfer_wrftltoxa_adj( grid, config_flags, filnam, &
#include "em_actual_args.inc"
                              )
      enddo

      ndynopt      = grid%dyn_opt
      grid%dyn_opt = DYN_EM_TL
      call nl_set_dyn_opt ( 1 , DYN_EM_TL )

      if ( jcdfi_use .AND. iter > 0 ) then
         filnam ='tldf'
         call da_med_initialdata_input( grid , config_flags, in_filename=filnam )
      else
         grid%em_g_u_2 = 0.0
         grid%em_g_v_2 = 0.0
         grid%em_g_w_2 = 0.0
         grid%em_g_t_2 = 0.0
         grid%em_g_ph_2 = 0.0
         grid%em_g_mu_2 = 0.0
         grid%g_moist = 0.0
      endif

      filnam ='afdf'
      call da_med_initialdata_output( grid , config_flags, filnam )

      grid%dyn_opt = ndynopt
      call nl_set_dyn_opt ( 1 , DYN_EM )

      call da_trace("da_transform_vtoy_adj","Starting da_run_wrfplus_ad.ksh")

#ifdef DM_PARALLEL
      call system_4dv("da_run_wrfplus_ad.ksh pre ")

      IF ( wrf_dm_on_monitor() ) THEN
        call system("rm -rf ad/wrf_done")
        call system("touch ad/wrf_go_ahead")
        DO WHILE ( .true. )
          OPEN(99,file="ad/wrf_done",status="old",err=303)
          CLOSE(99)
          EXIT
303          CONTINUE
          CALL system("sleep 1")
        ENDDO
      ENDIF
      CALL wrf_get_dm_communicator ( comm )
      CALL mpi_barrier( comm, ierr )

      call system_4dv("da_run_wrfplus_ad.ksh post ")
#else
      call system("da_run_wrfplus_ad.ksh")
#endif
      call da_trace("da_transform_vtoy_adj","Finished da_run_wrfplus_ad.ksh")

      filnam = 'gr00'
      call da_transfer_xatowrftl_adj( grid, config_flags, filnam, &
#include "em_actual_args.inc"
                              )
   else  ! lvar4d

      call da_zero_x(xa)
      call da_transform_xtoy_adj( xb, iv, xp, y, xa )

   endif ! lvar4d

   cv = 0.0
   call da_transform_vtox_adj( cv_size, xb, xbx, be, ep, xa, xp, vp, vv, cv, &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               its, ite, jts, jte, kts, kte )

end subroutine da_transform_vtoy_adj

