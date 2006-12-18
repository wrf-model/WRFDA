SUBROUTINE copy_tile_dims( grid, xp, its, ite, jts, jte, kts, kte )

!------------------------------------------------------------------------------
!  PURPOSE: Copy tile dimensions from grid structure.
!
!------------------------------------------------------------------------------

   TYPE(domain), INTENT(IN)         :: grid
   TYPE (xpose_type),INTENT(INOUT)  :: xp      ! Transpose variables.
   INTEGER,      INTENT(OUT)        :: its,ite, jts,jte, kts,kte ! tile dims.

   INTEGER                  :: ij   ! Loop counter

!  De-reference tile indices stored in the grid data structure.

   DO ij = 1 , grid%num_tiles
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

     if ( print_detail > 0) then
        write(unit=*, fmt='(/)')
        write(unit=*, fmt='(A,i3,A,5x,3(i3,A,i3,5x))') 'Tile ',ij, &
                ' size:', its,':',ite, jts,':',jte, kts,':',kte
     end if
   END DO

END SUBROUTINE copy_tile_dims

