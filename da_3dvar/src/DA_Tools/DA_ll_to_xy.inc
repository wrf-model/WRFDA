SUBROUTINE DA_ll_to_xy (info, loc, xp, &
                        outside, inside_halo)

!  This routine convert the (lat, lon) into (x,y) coordinates

   IMPLICIT NONE

   !  Argument

   TYPE(info_type),       intent(in)    :: info
   TYPE(model_loc_type),  intent(inout) :: loc
   TYPE(xpose_type),      intent(in)    :: xp           !Domain decomposition vars.
   LOGICAL      ,         intent(out)   :: outside      !wrt local domain
   LOGICAL      ,         intent(out)   :: inside_halo

   outside = .false.
   inside_halo = .true.

   loc % x   = -1.0
   loc % y   = -1.0

!  1.0 First, get the (x, y) coordinates

   if(fg_format == 1) then
      CALL latlon_to_ij(map_info, info%lat, info%lon, loc%x, loc%y)
   else
      CALL llxy (info%lat, info%lon, loc%x, loc%y)
   endif

   call toGrid (loc%x, xp%its-2, xp%ite+2, loc%i, loc%dx, loc%dxm)
   call toGrid (loc%y, xp%jts-2, xp%jte+2, loc%j, loc%dy, loc%dym)

!-----Computation to check for edge of domain:

   if((loc%i < xp%ids) .or. (loc%i >= xp%ide) .or. &
      (loc%j < xp%jds) .or. (loc%j >= xp%jde)) then
      outside = .true.
      inside_halo = .false.
      return
   endif

   if((loc%i < xp%its-1) .or. (loc%i > xp%ite) .or. &
      (loc%j < xp%jts-1) .or. (loc%j > xp%jte)) then
      outside = .true.
      inside_halo = .false.

      if(def_sub_domain ) then
         if( x_start_sub_domain > loc%x .or. y_start_sub_domain > loc%y .or. &
             x_end_sub_domain   < loc%x .or. y_end_sub_domain   < loc%y) then
             outside = .true.
             inside_halo = .false.
         endif
      endif
   endif

END SUBROUTINE DA_ll_to_xy

