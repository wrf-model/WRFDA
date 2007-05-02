MODULE da_verif_init
!----------------------------------------------------------------------------   
! History:
!
!  Abstract:  
!   Main module for 
!   initializing various arrays                   
!
!  Author:   Syed RH Rizvi     NCAR/MMM         05/30/2006
!----------------------------------------------------------------------------   
   USE da_verif_control

CONTAINS

   subroutine da_advance_cymdh( start_date, dh, end_date )

   implicit none

   character (len=10), intent(in)  :: start_date ! In date (ccyymmddhh).
   integer, intent(in)             :: dh         ! Period to advance (-ve for past).
   character (len=10), intent(out) :: end_date   ! Out date (ccyymmddhh).

   integer :: ccyy, mm, dd, hh

   read(start_date(1:10), fmt='(i4, 3i2)')  ccyy, mm, dd, hh

   hh = hh + dh

   do while (hh < 0)
      hh = hh + 24
      call da_change_date ( ccyy, mm, dd, -1 )
   end do

   do while (hh > 23)
      hh = hh - 24
      call da_change_date ( ccyy, mm, dd, 1 )
   end do

   write(UNIT=end_date(1:10), fmt='(i4, 3i2.2)')  ccyy, mm, dd, hh

end subroutine da_advance_cymdh
subroutine da_change_date( ccyy, mm, dd, delta )

   implicit none

   integer, intent(inout) :: ccyy, mm, dd
   integer, intent(in)    :: delta

   integer, dimension(12) :: mmday

   mmday = (/31,28,31,30,31,30,31,31,30,31,30,31/)

   mmday(2) = 28
   if (mod(ccyy,4) == 0) then
      mmday(2) = 29

      if ( mod(ccyy,100) == 0) then
         mmday(2) = 28
      endif

      if(mod(ccyy,400) == 0) then
         mmday(2) = 29
      end if
   endif

   dd = dd + delta

   if(dd == 0) then
      mm = mm - 1

      if(mm == 0) then
         mm = 12
         ccyy = ccyy - 1
      endif

      dd = mmday(mm)
   elseif ( dd .gt. mmday(mm) ) then
      dd = 1
      mm = mm + 1
      if(mm > 12 ) then
         mm = 1
         ccyy = ccyy + 1
      end if
   end if
end subroutine da_change_date
   subroutine initialize_surface_type(surface)
   type(surface_type), intent(inout)    :: surface
!
   call initialize_stats_type(surface%uomb,surface%uoma)
   call initialize_stats_type(surface%vomb,surface%voma)
   call initialize_stats_type(surface%tomb,surface%toma)
   call initialize_stats_type(surface%pomb,surface%poma)
   call initialize_stats_type(surface%qomb,surface%qoma)

   end subroutine initialize_surface_type
!
   subroutine initialize_upr_type(upr)
   type(upr_type), intent(inout)    :: upr
!
   integer    :: k
!
   do k = 1, nstd
   call initialize_stats_type(upr%uomb(k),upr%uoma(k))
   call initialize_stats_type(upr%vomb(k),upr%voma(k))
   call initialize_stats_type(upr%tomb(k),upr%toma(k))
   call initialize_stats_type(upr%qomb(k),upr%qoma(k))
   enddo

   end subroutine initialize_upr_type
!
   subroutine initialize_gpspw_type(gpspw)
   type(gpspw_type), intent(inout)    ::  gpspw
!
      call initialize_stats_type(gpspw%tpwomb,gpspw%tpwoma)
   end subroutine initialize_gpspw_type
!  
   subroutine initialize_gpsref_type(gpsref)
   type(gpsref_type), intent(inout)    ::  gpsref
!  
   call initialize_stats_type(gpsref%refomb,gpsref%refoma)
   end subroutine initialize_gpsref_type
!  
   subroutine initialize_stats_type(omb, oma)
   type(stats_value), intent(inout)    :: omb, oma
   omb%num   = 0 ; oma%num   = 0
   omb%bias  = 0 ; oma%bias  = 0
   omb%abias = 0 ; oma%abias = 0
   omb%rmse  = 0 ; oma%rmse  = 0
   end subroutine initialize_stats_type
!
!-------------------------------------------------
end MODULE da_verif_init
