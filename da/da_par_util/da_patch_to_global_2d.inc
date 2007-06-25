subroutine da_patch_to_global_2d (xp, vlocal, vglobal)

   !---------------------------------------------------------------------
   ! Purpose: Gathers local 2D array vlocal into global array vglobal. 
   !
   ! Must be called by all MPI tasks.
   !---------------------------------------------------------------------  

   implicit none

   type (xpose_type), intent(in)  :: xp
   real,              intent(in)  :: vlocal(:,:)
   real,              intent(out) :: vglobal(:,:)

   ! Local declarations
   real, allocatable :: vlocal3d(:,:,:), vglobal3d(:,:,:)
write(0,*) __FILE__,__LINE__,"ims,ime,jms,jme,ids,ide,jds,jde",ims,ime,jms,jme,ids,ide,jds,jde
   allocate(vlocal3d (ims:ime, jms:jme, 1:1))
write (0,*) __FILE__,__LINE__
   allocate(vglobal3d(ids:ide, jds:jde, 1:1))
write (0,*) __FILE__,__LINE__

   vlocal3d(:,:,1) = vlocal(:,:)
   call da_patch_to_global_3d(xp, vlocal3d, vglobal3d, 1)
   if (rootproc) then
      vglobal(:,:) = vglobal3d(:,:,1)
   end if

   deallocate(vlocal3d)
   deallocate(vglobal3d)

end subroutine da_patch_to_global_2d


