subroutine da_ll_to_xy (info, loc, xp, &
                        outside, outside_all)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   ! This routine converts (lat, lon) into (x,y) coordinates

   implicit none

   type(info_type),       intent(in)    :: info
   type(model_loc_type),  intent(inout) :: loc
   type(xpose_type),      intent(in)    :: xp           !Domain decomposition vars.
   logical      ,         intent(out)   :: outside      !wrt local domain
   logical, optional,     intent(out)   :: outside_all  !wrt all domains

   outside = .false.
   loc % x   = -1.0
   loc % y   = -1.0
   
   ! get the (x, y) coordinates

   if (fg_format == fg_format_wrf) then
      call da_latlon_to_ij(map_info, info%lat, info%lon, loc%x, loc%y)
   else if (fg_format == fg_format_kma) then
      call da_global_ll_to_xy (info%lat, info%lon, loc%x, loc%y)
   else
      call da_llxy (info%lat, info%lon, loc%x, loc%y)
   end if

   call da_togrid (loc%x, xp%its-2, xp%ite+2, loc%i, loc%dx, loc%dxm)!

   call da_togrid (loc%y, xp%jts-2, xp%jte+2, loc%j, loc%dy, loc%dym)

   ! refactor to remove this ugly duplication later
   if (present(outside_all)) then
      outside_all = .false.
      ! Do not check for global options 
      if (.not. global) then 
         if ((int(loc%x) < xp%ids) .or. (int(loc%x) >= xp%ide) .or. &
            (int(loc%y) < xp%jds) .or. (int(loc%y) >= xp%jde)) then
            outside_all = .true. 
         end if
         if (def_sub_domain) then
            if (x_start_sub_domain > loc%x .or. y_start_sub_domain > loc%y .or. &
                x_end_sub_domain   < loc%x .or. y_end_sub_domain   < loc%y) then
               outside_all = .true.
            end if
         end if
      end if
   end if

   if (fg_format == fg_format_kma) then
      if ((loc%j < xp%jts-1) .or. (loc%j > xp%jte)) then
         outside = .true.
         return
      end if

      if (loc%j == xp%jde) then
         loc%j = loc%j - 1
         loc%dy  = 1.0
         loc%dym = 0.0
      end if

      return
   end if

   ! Check for edge of domain:

   if ((loc%i < xp%ids) .or. (loc%i >= xp%ide) .or. &
      (loc%j < xp%jds) .or. (loc%j >= xp%jde)) then
      outside     = .true. 
      return
   end if

   ! JRB hack
   if ((loc%i < xp%its-1) .or. (loc%i > xp%ite) .or. &
      (loc%j < xp%jts-1) .or. (loc%j > xp%jte)) then
   ! if ((loc%i < xp%its-1) .or. (loc%i >= xp%ite) .or. &
   !     (loc%j < xp%jts-1) .or. (loc%j >= xp%jte)) then
      outside = .true.
      return

      if (def_sub_domain) then
         if (x_start_sub_domain > loc%x .or. y_start_sub_domain > loc%y .or. &
             x_end_sub_domain   < loc%x .or. y_end_sub_domain   < loc%y) then
             outside = .true.
         end if
      end if
   end if

end subroutine da_ll_to_xy


