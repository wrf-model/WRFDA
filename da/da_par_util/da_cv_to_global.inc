
      SUBROUTINE da_cv_to_global( cv_size, cv_size_global, x, xp, mzs, xg )
!
!     Gathers local cv-array x into domain cv-array xg(:).  
!     Global cv-array xg will only be valid on the "monitor" task.  
!
!     Must be called by all MPI tasks.  
!
        IMPLICIT NONE

        INTEGER,           INTENT(IN) :: cv_size        ! Size of local cv-array
        INTEGER,           INTENT(IN) :: cv_size_global ! Size of domain cv-array
        REAL,              INTENT(IN) :: x(1:cv_size)   ! local cv-array
        TYPE (xpose_type), INTENT(IN) :: xp   ! decomposed dimensions
        INTEGER,           INTENT(IN) :: mzs(:)  ! mz for each variable
                                              ! (to identify empty or 2D arrays)
        REAL,           INTENT(INOUT) :: xg(1:cv_size_global) ! global cv-array

#ifdef DM_PARALLEL

        ! Local declarations
        TYPE (vp_type) :: vv_x    ! Grdipt/EOF cv_array (local)
        TYPE (vp_type) :: vv_xg   ! Grdipt/EOF cv_array (global)
        TYPE (xpose_type) :: xpg  ! global dimensions
        INTEGER :: ids, ide, jds, jde, kds, kde, &
                   ims, ime, jms, jme, kms, kme, &
                   ips, ipe, jps, jpe, kps, kpe
        LOGICAL, EXTERNAL :: wrf_dm_on_monitor

        !
        ! Gather to mimic serial summation order.  
        !

        ! k?e varies with variable v1 - v5
        ids = xp%ids; ide = xp%ide; jds = xp%jds; jde = xp%jde; kds = xp%kds
        ims = xp%ims; ime = xp%ime; jms = xp%jms; jme = xp%jme; kms = xp%kms
        ips = xp%ips; ipe = xp%ipe; jps = xp%jps; jpe = xp%jpe; kps = xp%kps

        ! TODO:  encapsulate this crap!  
        ! allocate vv_x
        ALLOCATE( vv_x%v1(xp%ims:xp%ime,xp%jms:xp%jme,mzs(1)) )
        ALLOCATE( vv_x%v2(xp%ims:xp%ime,xp%jms:xp%jme,mzs(2)) )
        ALLOCATE( vv_x%v3(xp%ims:xp%ime,xp%jms:xp%jme,mzs(3)) )
        ALLOCATE( vv_x%v4(xp%ims:xp%ime,xp%jms:xp%jme,mzs(4)) )
        ALLOCATE( vv_x%v5(xp%ims:xp%ime,xp%jms:xp%jme,mzs(5)) )
        ALLOCATE( vv_x%alpha(xp%ims:xp%ime,xp%jms:xp%jme,mzs(6)) )

        CALL da_cv_to_vv ( cv_size, x, xp, mzs, vv_x )

        IF ( wrf_dm_on_monitor() ) THEN
          ! allocate vv_xg
          ALLOCATE( vv_xg%v1(xp%ids:xp%ide,xp%jds:xp%jde,mzs(1)) )
          ALLOCATE( vv_xg%v2(xp%ids:xp%ide,xp%jds:xp%jde,mzs(2)) )
          ALLOCATE( vv_xg%v3(xp%ids:xp%ide,xp%jds:xp%jde,mzs(3)) )
          ALLOCATE( vv_xg%v4(xp%ids:xp%ide,xp%jds:xp%jde,mzs(4)) )
          ALLOCATE( vv_xg%v5(xp%ids:xp%ide,xp%jds:xp%jde,mzs(5)) )
          ALLOCATE( vv_xg%alpha(xp%ids:xp%ide,xp%jds:xp%jde,mzs(6)) )
        ELSE
          ! Allocate dummy array for non-monitor process to keep Fortran
          ! CICO happy...
          ALLOCATE( vv_xg%v1(1,1,1) )
          ALLOCATE( vv_xg%v2(1,1,1) )
          ALLOCATE( vv_xg%v3(1,1,1) )
          ALLOCATE( vv_xg%v4(1,1,1) )
          ALLOCATE( vv_xg%v5(1,1,1) )
          ALLOCATE( vv_xg%alpha(1,1,1) )
        ENDIF

        ! TODO:  encapsulate this crap!  
        ! gather to global data structures
        ! it is possible to gather straight into a globally-sized cv-array
        ! TODO:  add this optimization later
        CALL da_wv_patch_to_global( vv_x%v1, vv_xg%v1,       &
                                 xp%domdesc, mzs(1),    &
                                 ids, ide, jds, jde, kds, &
                                 ims, ime, jms, jme, kms, &
                                 ips, ipe, jps, jpe, kps )
        CALL da_wv_patch_to_global( vv_x%v2, vv_xg%v2,       &
                                 xp%domdesc, mzs(2),    &
                                 ids, ide, jds, jde, kds, &
                                 ims, ime, jms, jme, kms, &
                                 ips, ipe, jps, jpe, kps )
        CALL da_wv_patch_to_global( vv_x%v3, vv_xg%v3,       &
                                 xp%domdesc, mzs(3),    &
                                 ids, ide, jds, jde, kds, &
                                 ims, ime, jms, jme, kms, &
                                 ips, ipe, jps, jpe, kps )
        CALL da_wv_patch_to_global( vv_x%v4, vv_xg%v4,       &
                                 xp%domdesc, mzs(4),    &
                                 ids, ide, jds, jde, kds, &
                                 ims, ime, jms, jme, kms, &
                                 ips, ipe, jps, jpe, kps )
        CALL da_wv_patch_to_global( vv_x%v5, vv_xg%v5,       &
                                 xp%domdesc, mzs(5), &
                                 ids, ide, jds, jde, kds, &
                                 ims, ime, jms, jme, kms, &
                                 ips, ipe, jps, jpe, kps )
        CALL da_wv_patch_to_global( vv_x%alpha, vv_xg%alpha,       &
                                 xp%domdesc, mzs(6),    &
                                 ids, ide, jds, jde, kds, &
                                 ims, ime, jms, jme, kms, &
                                 ips, ipe, jps, jpe, kps )

        ! deallocate vv_x
        DEALLOCATE (vv_x%v1, vv_x%v2, vv_x%v3, vv_x%v4, vv_x%v5 )

        IF ( wrf_dm_on_monitor() ) THEN
          ! finally, collapse data back into a globally-sized cv-array
          xpg%ids = xp%ids; xpg%ide = xp%ide
          xpg%ims = xp%ids; xpg%ime = xp%ide
          xpg%its = xp%ids; xpg%ite = xp%ide
          xpg%jds = xp%jds; xpg%jde = xp%jde
          xpg%jms = xp%jds; xpg%jme = xp%jde
          xpg%jts = xp%jds; xpg%jte = xp%jde
          xpg%kds = xp%kds; xpg%kde = xp%kde
          xpg%kms = xp%kds; xpg%kme = xp%kde
          xpg%kts = xp%kds; xpg%kte = xp%kde
          CALL da_vv_to_cv( vv_xg, xpg, mzs, cv_size_global, xg )
        ENDIF

        ! deallocate vv_xg
        DEALLOCATE( vv_xg%v1, vv_xg%v2, vv_xg%v3, vv_xg%v4, vv_xg%v5, vv_xg%alpha )

#endif

      END SUBROUTINE da_cv_to_global

