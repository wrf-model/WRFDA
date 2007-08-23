subroutine da_ll_to_xy_new (info, loc, xp, outside, outside_all)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   ! This routine converts (lat, lon) into (x,y) coordinates

   implicit none

   type(info_type_new),       intent(in)    :: info
   type(model_loc_type_new),  intent(inout) :: loc
   type(xpose_type),      intent(in)    :: xp              ! Domain decomposition vars.
   logical,               intent(inout) :: outside(:,:)      ! wrt local domain
   logical, optional,     intent(out)   :: outside_all(:,:)  ! wrt all domains

   if (trace_use) call da_trace_entry("da_ll_to_xy_new")

   outside(:,:) = .false.
   loc%x(:,:)   = -1.0
   loc%y(:,:)   = -1.0
   
   ! get the (x, y) coordinates

   if (fg_format == fg_format_wrf) then
      call da_latlon_to_ij_new(map_info, info%lat, info%lon, loc%x, loc%y)
   else if (fg_format == fg_format_kma_global) then
      call da_global_ll_to_xy_new (info%lat, info%lon, loc%x, loc%y)
   else
      call da_llxy_new (info%lat, info%lon, loc%x, loc%y)
   end if

   call da_togrid_new (loc%x, xp%its-2, xp%ite+2, loc%i, loc%dx, loc%dxm)

   call da_togrid_new (loc%y, xp%jts-2, xp%jte+2, loc%j, loc%dy, loc%dym)

   ! refactor to remove this ugly duplication later
   if (present(outside_all)) then
      outside_all(:,:) = .false.
      ! Do not check for global options 
      if (.not. global) then 
         where ((int(loc%x(:,:)) < xp%ids) .or. (int(loc%x(:,:)) >= xp%ide) .or. &
            (int(loc%y(:,:)) < xp%jds) .or. (int(loc%y(:,:)) >= xp%jde))
            outside_all(:,:) = .true. 
            outside(:,:) = .true. 
         end where
         if (def_sub_domain) then
            where (x_start_sub_domain > loc%x(:,:) .or. y_start_sub_domain > loc%y(:,:) .or. &
                x_end_sub_domain   < loc%x(:,:) .or. y_end_sub_domain   < loc%y(:,:))
               outside_all(:,:) = .true.
               outside(:,:) = .true. 
            end where
         end if
      end if
   end if

   if (fg_format == fg_format_kma_global) then
      where ((loc%j(:,:) < xp%jts-1) .or. (loc%j(:,:)  > xp%jte))
         outside(:,:) = .true.
      end where

      where (loc%j(:,:) == xp%jde)
         loc%j(:,:) = loc%j(:,:) - 1
         loc%dy(:,:)  = 1.0
         loc%dym(:,:) = 0.0
      end where

      return
   end if

   ! Check for edge of domain:

   where ((loc%i(:,:) < xp%ids) .or. (loc%i(:,:) >= xp%ide) .or. &
      (loc%j(:,:) < xp%jds) .or. (loc%j(:,:) >= xp%jde))
      outside     = .true. 
   end where

   ! FIX? hack
   where ((loc%i(:,:) < xp%its-1) .or. (loc%i(:,:) > xp%ite) .or. &
      (loc%j(:,:) < xp%jts-1) .or. (loc%j(:,:) > xp%jte))
      outside(:,:) = .true.
   end where

   if (def_sub_domain) then
      where (x_start_sub_domain > loc%x(:,:) .or. y_start_sub_domain > loc%y(:,:) .or. &
             x_end_sub_domain   < loc%x(:,:) .or. y_end_sub_domain   < loc%y(:,:))
         outside = .true.
      end where 
   end if

   if (trace_use) call da_trace_exit("da_ll_to_xy_new")

end subroutine da_ll_to_xy_new


