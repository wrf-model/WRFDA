subroutine da_latlon_to_ij(proj, lat, lon, i, j)

   !-----------------------------------------------------------------------
   ! Purpose: Converts input lat/lon values to the cartesian (i,j) value
   ! for the given projection. 
   !-----------------------------------------------------------------------

   implicit none

   type(proj_info), intent(in)  :: proj
   real,            intent(in)  :: lat
   real,            intent(in)  :: lon
   real,            intent(out) :: i
   real,            intent(out) :: j

   if (trace_use_frequent) call da_trace_entry("da_latlon_to_ij")

   if (.NOT.proj%init) then
      call da_error(__FILE__,__LINE__, &
        (/"You have not called map_set for this projection!"/))
   end if

   select case(proj%code)
 
      case(PROJ_LATLON)
         call da_llij_latlon(lat,lon,proj,i,j)

      case(PROJ_MERC)
         call da_llij_merc(lat,lon,proj,i,j)
         i = i + proj%knowni - 1.0
         j = j + proj%knownj - 1.0

      case(PROJ_PS)
         call da_llij_ps(lat,lon,proj,i,j)
      
      case(PROJ_LC)
         call da_llij_lc(lat,lon,proj,i,j)
         i = i + proj%knowni - 1.0
         j = j + proj%knownj - 1.0

      case default
         write(unit=message(1),fmt='(A,I2)') &
            'Unrecognized map projection code: ', proj%code
         call da_error(__FILE__,__LINE__,message(1:1))
   end select

   if (trace_use_frequent) call da_trace_exit("da_latlon_to_ij")

end subroutine da_latlon_to_ij


