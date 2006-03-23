



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
        WRITE (UNIT=debugfname,FMT='(a,i0.2)') TRIM(fnameheader),fnameID
      ELSE
        WRITE (UNIT=debugfname,FMT='(a)') TRIM(fnameheader)
      ENDIF
      OPEN( UNIT=unit_983, FILE=TRIM(debugfname), FORM='unformatted' )
      WRITE (UNIT=unit_983) arrayglobal
      CLOSE (UNIT=unit_983)
      ! ASCII
      IF ( PRESENT( fnameID ) ) THEN
        WRITE (debugfname,'(a,a,i0.2)') TRIM(fnameheader),'_ASCII_',fnameID
      ELSE
        WRITE (debugfname,'(a,a)') TRIM(fnameheader),'_ASCII'
      ENDIF
      OPEN( UNIT=unit_983, FILE=TRIM(debugfname), FORM='formatted' )
      DO k = 1, mz
        DO j = jds, jde
          DO i = ids, ide
            WRITE (UNIT=unit_983,FMT=*) arrayglobal(i,j,k)
          ENDDO
        ENDDO
      ENDDO
      CLOSE (UNIT=unit_983)
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




