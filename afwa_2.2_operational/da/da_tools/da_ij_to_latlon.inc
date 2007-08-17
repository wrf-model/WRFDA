subroutine da_ij_to_latlon(proj, ii, jj, lat, lon)

   !-----------------------------------------------------------------------
   ! Purpose: Computes geographical latitude and longitude for a given (i,j) 
   ! point in a grid with a projection of proj
   !-----------------------------------------------------------------------

   implicit none

   type(proj_info),intent(in)          :: proj
   real, intent(in)                    :: ii
   real, intent(in)                    :: jj
   real, intent(out)                   :: lat
   real, intent(out)                   :: lon
   real                                :: i, j

   if (.NOT.proj%init) then
      call da_error(__FILE__,__LINE__, &
         (/"You have not called map_set for this projection!"/))
   end if

   i = ii
   j = jj

   select case (proj%code)
      case (PROJ_LATLON)
         call da_ijll_latlon(i, j, proj, lat, lon)

      case (PROJ_MERC)
         i = ii - proj%knowni + 1.0
         j = jj - proj%knownj + 1.0
         call da_ijll_merc(i, j, proj, lat, lon)

      case (PROJ_PS)
         call da_ijll_ps(i, j, proj, lat, lon)

      case (PROJ_LC)

         i = ii - proj%knowni + 1.0
         j = jj - proj%knownj + 1.0
         call da_ijll_lc(i, j, proj, lat, lon)

      case default
         write(unit=message(1),fmt='(A,I2)') &
            "Unrecognized map projection code: ", proj%code
         call da_error(__FILE__,__LINE__,message(1:1))

   end select

end subroutine da_ij_to_latlon


