
SUBROUTINE be_local_copy( be_g, be, jts, jte, kts,kte )

!------------------------------------------------------------------------------
!  PURPOSE: Make local copy of be_type gridded arrays from global copy. Note
!           that no message passing is required since each processor already
!           has be_g.
!------------------------------------------------------------------------------

  TYPE (be_type), INTENT(INOUT):: be_g      ! Background errors (global grid)
  TYPE (be_type), INTENT(OUT)  :: be        ! Background errors (local grid)
  integer, intent(in)          :: jts,jte,kts,kte  ! Tile dimension.
 
   ALLOCATE ( be % pb_vert_reg(jts:jte,kts:kte,kts:kte) )
   be % pb_vert_reg(jts:jte,kts:kte,kts:kte) = be_g % pb_vert_reg(jts:jte,kts:kte,kts:kte)
   DEALLOCATE ( be_g % pb_vert_reg )

   call alloc_and_copy_be_arrays( be_g % v1 , be % v1, jts,jte, kts,kte )
   call alloc_and_copy_be_arrays( be_g % v2 , be % v2, jts,jte, kts,kte )
   call alloc_and_copy_be_arrays( be_g % v3 , be % v3, jts,jte, kts,kte )
   call alloc_and_copy_be_arrays( be_g % v4 , be % v4, jts,jte, kts,kte )
   call alloc_and_copy_be_arrays( be_g % v5 , be % v5, jts,jte, kts,kte )

END SUBROUTINE be_local_copy

