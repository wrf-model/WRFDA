subroutine DA_Transform_VToY_Adj(be, cv, iv, vp, vv, xa, xb, xbx, xp, y, &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 its, ite, jts, jte, kts, kte,     &
                                 grid, config_flags )

!-------------------------------------------------------------------------
!  History:
!
!    Purpose:  Does Adjoint of control variable (V) transform to Obs-space(Y)
!
!   Creation:  03/14/2005                    Syed RH Rizvi
!
! PARENT_MODULE:  DA_VToX_Transforms
!-------------------------------------------------------------------------
     implicit none

      type (be_type), intent(in)         :: be     ! background error structure.
      real,dimension(cv_size),intent(out):: cv     ! control variable
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
!------------------------------------------------------------------------------
      LOGICAL, EXTERNAL :: wrf_dm_on_monitor     ! For statistics output.
!------------------------------------------------------------------------------

!-------------------------------------------------------------------------

      CALL wrf_debug(100,"entering DA_Transform_VToY_Adj")

      if ( lvar4d ) then

#ifdef WRFVAR
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

#ifdef DM_PARALLEL
         call system_4dv("runvar4dad.csh pre ")

         IF ( wrf_dm_on_monitor() ) THEN
           call system("rm wrf_done")
           call system("touch wrf_go_ahead")
           DO WHILE ( .true. )
             OPEN(99,file="wrf_done",status="old",err=303)
             CLOSE(99)
             EXIT
303          CONTINUE
             CALL system("sleep 1")
           ENDDO
         ENDIF
         CALL wrf_get_dm_communicator ( comm )
         CALL mpi_barrier( comm, ierr )

         call system_4dv("runvar4dad.csh post ")
#else
         call system("runvar4dad.csh")
#endif

         filnam = 'gr00'
         call DA_Transfer_XatoWRFTL_Adj( grid, config_flags, filnam, &
#include <em_actual_args.inc>
                                 )
#else
         call DA_zero_x(xa)
         call da_transform_xtoy( xb, iv, xa, xp, y )
#endif
      else  ! lvar4d

         call DA_zero_x(xa)
         call da_transform_xtoy_adj( xb, iv, xp, y, xa )

      endif ! lvar4d

!-------------------------------------------------------------------------
      cv = 0.
      call da_transform_vtox_adj( xb, xbx, be, xa, xp, vp, vv, cv, &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               its, ite, jts, jte, kts, kte )

      CALL wrf_debug(100,"leaving DA_Transform_VToY_Adj")
!
end subroutine DA_Transform_VToY_Adj

