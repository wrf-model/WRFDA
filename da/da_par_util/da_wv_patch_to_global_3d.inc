subroutine da_wv_patch_to_global_3d( vlocal, vglobal,         &
                                  domdesc, mz,             &
                                  ids, ide, jds, jde, kds, &
                                  ims, ime, jms, jme, kms, &
                                  ips, ipe, jps, jpe, kps )

   ! Gathers local 3D array vlocal into global array vglobal.  
   ! Assumes that "k" is not 
   ! decomposed.  End indicies in the "k" dimension are inferred from 
   ! mz, which can be less than kde.  
   !
   ! Must be called by all MPI tasks.  

   implicit none

   real,              intent(in   ) :: vlocal(:,:,:)
   real,              intent(  out) :: vglobal(:,:,:)
   integer,           intent(in   ) :: domdesc
   integer,           intent(in   ) :: mz
   integer,           intent(in   ) :: ids, ide, jds, jde, kds, &
                                       ims, ime, jms, jme, kms, &
                                       ips, ipe, jps, jpe, kps
#ifdef DM_PARALLEL
   ! Local declarations
   character(LEN=3) :: ordering, stagger
   integer :: kde, kme, kpe
   stagger = 'xyz'  ! This hack will keep wrf_patch_to_global_*() from
                    ! increasing DE[123].  Recall that wrfvar arrays are 
                    ! all on A-grid.  
   ordering = 'xyz'
   if ( mz > 0 ) then
      kde = kds + mz - 1
      kme = kde
      kpe = kde
      call wrf_patch_to_global_real ( vlocal, vglobal, domdesc,      &
                                    trim(stagger), trim(ordering), &
                                    ids, ide, jds, jde, kds, kde,  &
                                    ims, ime, jms, jme, kms, kme,  &
                                    ips, ipe, jps, jpe, kps, kpe )
   end if
#endif

end subroutine da_wv_patch_to_global_3d
