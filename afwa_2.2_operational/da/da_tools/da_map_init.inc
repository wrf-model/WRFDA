subroutine da_map_init(proj)

   !-----------------------------------------------------------------------
   ! Purpose: Initializes the map projection structure to missing values
   !-----------------------------------------------------------------------

   implicit none

   type(proj_info), intent(inout)  :: proj

   proj%lat1 =    -999.9
   proj%lon1 =    -999.9
   proj%dx    =    -999.9
   proj%stdlon =   -999.9
   proj%truelat1 = -999.9
   proj%truelat2 = -999.9
   proj%hemi     = 0.0
   proj%cone     = -999.9
   proj%polei    = -999.9
   proj%polej    = -999.9
   proj%rsw      = -999.9
   proj%knowni   = -999.9
   proj%knownj   = -999.9
   proj%init     = .false.

end subroutine da_map_init


