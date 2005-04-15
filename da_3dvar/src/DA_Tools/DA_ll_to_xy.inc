SUBROUTINE DA_ll_to_xy (info, loc, xp, &
                        outside, inside_halo)

!  This routine convert the (lat, lon) into (x,y) coordinates
!  Additions:      10/26/2004    Syed RH Rizvi
!                  Global domain co-ordinates
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
   
!  get the (x, y) coordinates

   if(fg_format == 1) then
      CALL latlon_to_ij(map_info, info%lat, info%lon, loc%x, loc%y)
!  else if(fg_format == 2) then
!     CALL llxy (info%lat, info%lon, loc%x, loc%y)
   else if(fg_format == 3) then
      CALL da_global_ll_to_xy (info%lat, info%lon, loc%x, loc%y)
   else
      CALL llxy (info%lat, info%lon, loc%x, loc%y)
   endif

   call toGrid (loc%x, xp%its-2, xp%ite+2, loc%i, loc%dx, loc%dxm)
   call toGrid (loc%y, xp%jts-2, xp%jte+2, loc%j, loc%dy, loc%dym)

   if(fg_format == 3)then
      if((loc%j < xp%jts-1) .or. (loc%j > xp%jte)) then
         outside = .true.
         inside_halo = .false.
         return
      endif

      if(loc%j == xp%jde) then
         loc%j = loc%j - 1
         loc%dy  = 1.0
         loc%dym = 0.0
      end if

      return
   endif

!--Computation to check for edge of domain:
   if((loc%i < xp%ids) .or. (loc%i >= xp%ide) .or. &
      (loc%j < xp%jds) .or. (loc%j >= xp%jde)) then
      outside     = .true. 
      inside_halo = .false.
      return
   endif

   if((loc%i < xp%its-1) .or. (loc%i > xp%ite) .or. &
      (loc%j < xp%jts-1) .or. (loc%j > xp%jte)) then
      outside = .true.
      inside_halo = .false.
      return

      if(def_sub_domain ) then
         if( x_start_sub_domain > loc%x .or. y_start_sub_domain > loc%y .or. &
             x_end_sub_domain   < loc%x .or. y_end_sub_domain   < loc%y) then
             outside = .true.
             inside_halo = .false.
         endif
      endif
   endif

END SUBROUTINE DA_ll_to_xy

