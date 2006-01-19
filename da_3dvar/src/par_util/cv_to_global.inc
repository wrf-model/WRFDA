
      SUBROUTINE cv_to_global( x, n, xp, mzs, cv_size_global, xg )
!
!     Gathers local cv-array x into global cv-array xg(:).  
!     Global cv-array xg will only be valid on the "monitor" task.  
!
!     Must be called by all MPI tasks.  
!
        IMPLICIT NONE

        INTEGER,           INTENT(IN) :: n    ! size of local cv-array
        REAL,              INTENT(IN) :: x(n) ! local cv-array
        TYPE (xpose_type), INTENT(IN) :: xp   ! decomposed dimensions
        INTEGER,           INTENT(IN) :: mzs(:)  ! mz for each variable
                                              ! (to identify empty or 2D arrays)
        INTEGER,          INTENT( IN) :: cv_size_global !size of global cv-array
        REAL,           INTENT(INOUT) :: xg(:) ! global cv-array

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
        ! This is a necessary but not sufficient condition!
        IF ( n /= cv_size ) THEN
          CALL wrf_error_fatal( 'ERROR in cv_to_global:  incorrect vector size' )
        ENDIF

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

        CALL cv_to_vv ( cv_size, x, xp, mzs, vv_x )

        IF ( wrf_dm_on_monitor() ) THEN
          ! allocate vv_xg
          ALLOCATE( vv_xg%v1(xp%ids:xp%ide,xp%jds:xp%jde,mzs(1)) )
          ALLOCATE( vv_xg%v2(xp%ids:xp%ide,xp%jds:xp%jde,mzs(2)) )
          ALLOCATE( vv_xg%v3(xp%ids:xp%ide,xp%jds:xp%jde,mzs(3)) )
          ALLOCATE( vv_xg%v4(xp%ids:xp%ide,xp%jds:xp%jde,mzs(4)) )
          ALLOCATE( vv_xg%v5(xp%ids:xp%ide,xp%jds:xp%jde,mzs(5)) )
        ELSE
          ! Allocate dummy array for non-monitor process to keep Fortran
          ! CICO happy...
          ALLOCATE( vv_xg%v1(1,1,1) )
          ALLOCATE( vv_xg%v2(1,1,1) )
          ALLOCATE( vv_xg%v3(1,1,1) )
          ALLOCATE( vv_xg%v4(1,1,1) )
          ALLOCATE( vv_xg%v5(1,1,1) )
        ENDIF

        ! TODO:  encapsulate this crap!  
        ! gather to global data structures
        ! it is possible to gather straight into a globally-sized cv-array
        ! TODO:  add this optimization later
        CALL wv_patch_to_global( vv_x%v1, vv_xg%v1,       &
                                 xp%domdesc, mzs(1),    &
                                 ids, ide, jds, jde, kds, &
                                 ims, ime, jms, jme, kms, &
                                 ips, ipe, jps, jpe, kps )
        CALL wv_patch_to_global( vv_x%v2, vv_xg%v2,       &
                                 xp%domdesc, mzs(2),    &
                                 ids, ide, jds, jde, kds, &
                                 ims, ime, jms, jme, kms, &
                                 ips, ipe, jps, jpe, kps )
        CALL wv_patch_to_global( vv_x%v3, vv_xg%v3,       &
                                 xp%domdesc, mzs(3),    &
                                 ids, ide, jds, jde, kds, &
                                 ims, ime, jms, jme, kms, &
                                 ips, ipe, jps, jpe, kps )
        CALL wv_patch_to_global( vv_x%v4, vv_xg%v4,       &
                                 xp%domdesc, mzs(4),    &
                                 ids, ide, jds, jde, kds, &
                                 ims, ime, jms, jme, kms, &
                                 ips, ipe, jps, jpe, kps )
        CALL wv_patch_to_global( vv_x%v5, vv_xg%v5,       &
                                 xp%domdesc, mzs(5),    &
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
          CALL vv_to_cv( vv_xg, xpg, mzs, cv_size_global, xg )
        ENDIF

        ! deallocate vv_xg
        DEALLOCATE( vv_xg%v1, vv_xg%v2, vv_xg%v3, vv_xg%v4, vv_xg%v5 )

#endif

      END SUBROUTINE cv_to_global



SUBROUTINE wv_patch_to_global_3d( vlocal, vglobal,         &
                                  domdesc, mz,             &
                                  ids, ide, jds, jde, kds, &
                                  ims, ime, jms, jme, kms, &
                                  ips, ipe, jps, jpe, kps )
!     Gathers local 3D array vlocal into global array vglobal.  
!     Assumes that "k" is not 
!     decomposed.  End indicies in the "k" dimension are inferred from 
!     mz, which can be less than kde.  
!
!     Must be called by all MPI tasks.  

  IMPLICIT NONE

  REAL,              INTENT(IN   ) :: vlocal(:,:,:)
  REAL,              INTENT(  OUT) :: vglobal(:,:,:)
  INTEGER,           INTENT(IN   ) :: domdesc
  INTEGER,           INTENT(IN   ) :: mz
  INTEGER,           INTENT(IN   ) :: ids, ide, jds, jde, kds, &
                                      ims, ime, jms, jme, kms, &
                                      ips, ipe, jps, jpe, kps
#ifdef DM_PARALLEL
  ! Local declarations
  CHARACTER(LEN=3) :: ordering, stagger
  INTEGER :: kde, kme, kpe
  stagger = 'xyz'  ! This hack will keep wrf_patch_to_global_*() from
                   ! increasing DE[123].  Recall that wrfvar arrays are 
                   ! all on A-grid.  
  ordering = 'xyz'
  IF ( mz > 0 ) THEN
    kde = kds + mz - 1
    kme = kde
    kpe = kde
    CALL wrf_patch_to_global_real ( vlocal, vglobal, domdesc,      &
                                    TRIM(stagger), TRIM(ordering), &
                                    ids, ide, jds, jde, kds, kde,  &
                                    ims, ime, jms, jme, kms, kme,  &
                                    ips, ipe, jps, jpe, kps, kpe )
  ENDIF
#endif

END SUBROUTINE wv_patch_to_global_3d



SUBROUTINE wv_patch_to_global_2d( vlocal, vglobal,    &
                                  domdesc,            &
                                  ids, ide, jds, jde, &
                                  ims, ime, jms, jme, &
                                  ips, ipe, jps, jpe )
!     Gathers local 2D array vlocal into global array vglobal.  
!     Assumes that "k" is not 
!     decomposed.  End indicies in the "k" dimension are inferred from 
!     mz, which can be less than kde.  
!
!     Must be called by all MPI tasks.  

  IMPLICIT NONE

  REAL,              INTENT(IN   ) :: vlocal(:,:)
  REAL,              INTENT(  OUT) :: vglobal(:,:)
  INTEGER,           INTENT(IN   ) :: domdesc
  INTEGER,           INTENT(IN   ) :: ids, ide, jds, jde, &
                                      ims, ime, jms, jme, &
                                      ips, ipe, jps, jpe
#ifdef DM_PARALLEL
  ! Local declarations
  INTEGER :: kds, kms, kps, mz
  REAL, ALLOCATABLE :: vlocal3d(:,:,:), vglobal3d(:,:,:)
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor

  kds=1; kms=1; kps=1; mz=1
  ALLOCATE( vlocal3d ( ims:ime, jms:jme, 1 ), &
            vglobal3d( ids:ide, jds:jde, 1 ) )
  ! get rid of copies later
  vlocal3d(:,:,1) = vlocal(:,:)
  CALL wv_patch_to_global_3d( vlocal3d, vglobal3d,     &
                              domdesc, mz,             &
                              ids, ide, jds, jde, kds, &
                              ims, ime, jms, jme, kms, &
                              ips, ipe, jps, jpe, kps )
  IF ( wrf_dm_on_monitor() ) THEN
    vglobal(:,:) = vglobal3d(:,:,1)
  ENDIF
  DEALLOCATE( vlocal3d, vglobal3d )
#endif

END SUBROUTINE wv_patch_to_global_2d



SUBROUTINE write_3d_debug( localpatch, domdesc, mz, fnameheader, &
                           ids, ide, jds, jde, kds,              &
                           ims, ime, jms, jme, kms,              &
                           its, ite, jts, jte, kts, fnameID )
!
!     Gathers local 3D array localpatch into a global array and writes 
!     to disk in various debug formats.  Assumes that "k" is not 
!     decomposed.  End indicies in the "k" dimension are inferred from 
!     mz, which can be less than kde.  
!
!     Must be called by all MPI tasks.  
!
!     Writes nothing without complaint if mz<=0 .  
!

  IMPLICIT NONE

  REAL,              INTENT(IN   ) :: localpatch(:,:,:)
  INTEGER,           INTENT(IN   ) :: domdesc
  INTEGER,           INTENT(IN   ) :: mz
  CHARACTER(LEN=*),  INTENT(IN   ) :: fnameheader
  INTEGER,           INTENT(IN   ) :: ids,ide, jds,jde, kds ! domain dims.
  INTEGER,           INTENT(IN   ) :: ims,ime, jms,jme, kms ! memory dims.
  INTEGER,           INTENT(IN   ) :: its,ite, jts,jte, kts ! tile   dims.
  INTEGER, OPTIONAL, INTENT(IN   ) :: fnameID       ! optional file ID tag
#ifdef DM_PARALLEL
  ! Local declarations
  REAL, ALLOCATABLE :: arrayglobal(:,:,:)
  INTEGER :: i, j, k
  LOGICAL, EXTERNAL :: wrf_dm_on_monitor
  CHARACTER(LEN=256) :: debugfname

  IF ( mz > 0 ) THEN
    ALLOCATE( arrayglobal( ids:ide, jds:jde, 1:mz ) )
    CALL wv_patch_to_global( localpatch, arrayglobal, &
                             domdesc, mz,             &
                             ids, ide, jds, jde, kds, &
                             ims, ime, jms, jme, kms, &
                             its, ite, jts, jte, kts )
    IF ( wrf_dm_on_monitor() ) THEN
      ! binary
      IF ( PRESENT( fnameID ) ) THEN
        WRITE (debugfname,'(a,i0.2)') TRIM(fnameheader),fnameID
      ELSE
        WRITE (debugfname,'(a)') TRIM(fnameheader)
      ENDIF
      OPEN( UNIT=983, FILE=TRIM(debugfname), FORM='unformatted' )
      WRITE (983) arrayglobal
      CLOSE (983)
      ! ASCII
      IF ( PRESENT( fnameID ) ) THEN
        WRITE (debugfname,'(a,a,i0.2)') TRIM(fnameheader),'_ASCII_',fnameID
      ELSE
        WRITE (debugfname,'(a,a)') TRIM(fnameheader),'_ASCII'
      ENDIF
      OPEN( UNIT=983, FILE=TRIM(debugfname), FORM='formatted' )
      DO k = 1, mz
        DO j = jds, jde
          DO i = ids, ide
            WRITE (983,*) arrayglobal(i,j,k)
          ENDDO
        ENDDO
      ENDDO
      CLOSE (983)
    ENDIF
    DEALLOCATE( arrayglobal )
  ENDIF
#endif
END SUBROUTINE write_3d_debug




SUBROUTINE write_2d_debug( localpatch, domdesc, fnameheader, &
                           ids, ide, jds, jde,               &
                           ims, ime, jms, jme,               &
                           its, ite, jts, jte, fnameID )
!
!     Gathers local 2D array localpatch into a global array and writes 
!     to disk in various debug formats.  
!
!     Must be called by all MPI tasks.  
!

  IMPLICIT NONE

  REAL,              INTENT(IN   ) :: localpatch(ims:ime,jms:jme)
  INTEGER,           INTENT(IN   ) :: domdesc
  CHARACTER(LEN=*),  INTENT(IN   ) :: fnameheader
  INTEGER,           INTENT(IN   ) :: ids,ide, jds,jde      ! domain dims.
  INTEGER,           INTENT(IN   ) :: ims,ime, jms,jme      ! memory dims.
  INTEGER,           INTENT(IN   ) :: its,ite, jts,jte      ! tile   dims.
  INTEGER, OPTIONAL, INTENT(IN   ) :: fnameID       ! optional file ID tag
#ifdef DM_PARALLEL
  ! Local declarations
  REAL :: array3d(ims:ime,jms:jme,1)

  array3d(its:ite,jts:jte,1) = localpatch(its:ite,jts:jte)
  CALL write_3d_debug( array3d, domdesc, 1, fnameheader, &
                       ids, ide, jds, jde, 1,            &
                       ims, ime, jms, jme, 1,            &
                       its, ite, jts, jte, 1, fnameID )
#endif
END SUBROUTINE write_2d_debug




SUBROUTINE write_vp_debug( vv, be, xp, fnameheader, &
                           ids, ide, jds, jde, kds, &
                           ims, ime, jms, jme, kms, &
                           its, ite, jts, jte, kts )

  IMPLICIT NONE

  TYPE (vp_type),    INTENT(INOUT) :: vv   ! Grid point/EOF control var.
  TYPE (be_type),    INTENT(IN   ) :: be   ! Background error structure.
  TYPE (xpose_type), INTENT(INOUT) :: xp   ! Dimensions and xpose buffers. 
  CHARACTER(LEN=*),  INTENT(IN   ) :: fnameheader
  INTEGER,           INTENT(IN   ) :: ids,ide, jds,jde, kds ! domain dims.
  INTEGER,           INTENT(IN   ) :: ims,ime, jms,jme, kms ! memory dims.
  INTEGER,           INTENT(IN   ) :: its,ite, jts,jte, kts ! tile   dims.
  ! Local declarations
  ! List of 3D vectors
  TYPE vec3d
    REAL, POINTER :: ptr(:,:,:)
    INTEGER :: mz
  END TYPE vec3d
  TYPE (vec3d) :: arraylist(5) ! list of arrays
  INTEGER :: arrayID

  arraylist(1)%ptr => vv%v1
  arraylist(1)%mz  =  be%v1%mz
  arraylist(2)%ptr => vv%v2
  arraylist(2)%mz  =  be%v2%mz
  arraylist(3)%ptr => vv%v3
  arraylist(3)%mz  =  be%v3%mz
  arraylist(4)%ptr => vv%v4
  arraylist(4)%mz  =  be%v4%mz
  arraylist(5)%ptr => vv%v5
  arraylist(5)%mz  =  be%v5%mz
  DO arrayID=1,5
    CALL write_3d_debug( arraylist(arrayID)%ptr, xp%domdesc, &
                         arraylist(arrayID)%mz, fnameheader, &
                         ids, ide, jds, jde, kds,            &
                         ims, ime, jms, jme, kms,            &
                         its, ite, jts, jte, kts, arrayID )
  ENDDO
END SUBROUTINE write_vp_debug

