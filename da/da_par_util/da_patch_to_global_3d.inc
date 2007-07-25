subroutine da_patch_to_global_3d(xp, vlocal, vglobal, mz)

   !----------------------------------------------------------------------
   ! Purpose: Gathers local 3D array vlocal into global array vglobal.  
   ! Assumes that "k" is not 
   ! decomposed.  End indicies in the "k" dimension are inferred from 
   ! mz, which can be less than kde.  
   !
   ! Must be called by all MPI tasks.  
   !----------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(in)  :: xp
   real,              intent(in)  :: vlocal(ims:ime,jms:jme,kms:kme)
   real,              intent(out) :: vglobal(ids:ide,jds:jde,kds:kde)
   integer, optional, intent(in)  :: mz

   integer :: local_kde
   integer :: local_kme
   integer :: local_kpe

#ifdef DM_PARALLEL

   if (present(mz)) then
      local_kde = kds + mz - 1
      local_kme = local_kde
      local_kpe = local_kde
   else
      local_kde = kde
      local_kme = kme
      local_kpe = kpe
   end if

   if (local_kde > 0) then
      call wrf_patch_to_global_real (vlocal, vglobal, xp%domdesc, &
         trim(grid_stagger), trim(grid_ordering), &
         ids, ide, jds, jde, kds, local_kde,  &
         ims, ime, jms, jme, kms, local_kme,  &
         ips, ipe, jps, jpe, kps, local_kpe)
   end if
#else
   vglobal(ids:ide,jds:jde,kds:kde) = vlocal(ids:ide,jds:jde,kds:kde)
#endif

end subroutine da_patch_to_global_3d


