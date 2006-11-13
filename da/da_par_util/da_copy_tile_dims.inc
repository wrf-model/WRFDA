subroutine da_copy_tile_dims( grid, xp, its, ite, jts, jte, kts, kte )

   !---------------------------------------------------------------------------
   ! PURPOSE: Copy tile dimensions from grid structure.
   !---------------------------------------------------------------------------

   type(domain), intent(in)         :: grid
   type (xpose_type),intent(inout)  :: xp      ! Transpose variables.
   integer,      intent(out)        :: its,ite, jts,jte, kts,kte ! tile dims.

   integer                  :: ij   ! Loop counter

   if (trace_use) call da_trace_entry("da_copy_tile_dims")

   ! De-reference tile indices stored in the grid data structure.

   do ij = 1 , grid%num_tiles
      its = grid%i_start(ij)
      ite = grid%i_end(ij)
      jts = grid%j_start(ij)
      jte = grid%j_end(ij)
      kts = xp%kds
      kte = xp%kde

      xp%its = its
      xp%ite = ite
      xp%jts = jts
      xp%jte = jte
      xp%kts = kts
      xp%kte = kte

      if(xp%ite > xp%ide) xp%ite = xp%ide
      if(xp%jte > xp%jde) xp%jte = xp%jde
      if(xp%kte > xp%kde) xp%kte = xp%kde

      if(ite > xp%ide) ite = xp%ide
      if(jte > xp%jde) jte = xp%jde
      if(kte > xp%kde) kte = xp%kde

      if (print_detail_parallel) then
         write(unit=stdout, fmt='(/)')
         write(unit=stdout, fmt='(A,i3,A,5x,3(i3,A,i3,5x))') 'Tile ',ij, &
                 ' size:', its,':',ite, jts,':',jte, kts,':',kte
      end if
   end do

   if (trace_use) call da_trace_exit("da_copy_tile_dims")

end subroutine da_copy_tile_dims

