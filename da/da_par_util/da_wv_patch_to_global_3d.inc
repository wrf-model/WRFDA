SUBROUTINE da_wv_patch_to_global_3d( vlocal, vglobal,         &
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

END SUBROUTINE da_wv_patch_to_global_3d
