SUBROUTINE Setup_ad_date_string ( grid, ad_date_string )
   USE module_domain
   USE module_configure
   USE WRF_ESMF_MOD
   IMPLICIT NONE
   TYPE(domain), POINTER :: grid
   character(len=19), dimension(25), intent(out) :: ad_date_string

   character(len=19)                             :: tmp_string, begin_date_string, end_date_string

! Local
   INTEGER :: start_year,start_month,start_day,start_hour,start_minute,start_second
   INTEGER :: end_year,end_month,end_day,end_hour,end_minute,end_second

   INTEGER :: auxinput3_interval  ,  &
              auxinput3_interval_mo, &
              auxinput3_interval_d,  &
              auxinput3_interval_h,  &
              auxinput3_interval_m,  &
              auxinput3_interval_s

   INTEGER :: auxinput3_begin  ,  &
              auxinput3_begin_y,  &
              auxinput3_begin_mo, &
              auxinput3_begin_d,  &
              auxinput3_begin_h,  &
              auxinput3_begin_m,  &
              auxinput3_begin_s

   INTEGER :: auxinput3_end  , &
              auxinput3_end_y, &
              auxinput3_end_mo,&
              auxinput3_end_d, &
              auxinput3_end_h, &
              auxinput3_end_m, &
              auxinput3_end_s

   INTEGER :: current_y, &
              current_mo,&
              current_d, &
              current_h, &
              current_m, &
              current_s

   INTEGER :: index, i, n 

   logical, external :: string_a_less_than_b

   CALL nl_get_start_year(1,start_year)
   CALL nl_get_start_month(1,start_month)
   CALL nl_get_start_day(1,start_day)
   CALL nl_get_start_hour(1,start_hour)
   CALL nl_get_start_minute(1,start_minute)
   CALL nl_get_start_second(1,start_second)

   CALL nl_get_end_year(1,end_year)
   CALL nl_get_end_month(1,end_month)
   CALL nl_get_end_day(1,end_day)
   CALL nl_get_end_hour(1,end_hour)
   CALL nl_get_end_minute(1,end_minute)
   CALL nl_get_end_second(1,end_second)

   print *, 'start_year,start_month,start_day,start_hour,start_minute,start_second=', &
             start_year,start_month,start_day,start_hour,start_minute,start_second

   print *, 'end_year,end_month,end_day,end_hour,end_minute,end_second=', &
             end_year,end_month,end_day,end_hour,end_minute,end_second

! AUXINPUT3_ INTERVAL
! auxinput3_interval is left there (and means minutes) for consistency, but
! auxinput3_interval_m will take precedence if specified

   CALL nl_get_auxinput3_interval( 1, auxinput3_interval )   ! same as minutes

   CALL nl_get_auxinput3_interval_mo( 1, auxinput3_interval_mo )
   CALL nl_get_auxinput3_interval_d( 1, auxinput3_interval_d )
   CALL nl_get_auxinput3_interval_h( 1, auxinput3_interval_h )
   CALL nl_get_auxinput3_interval_m( 1, auxinput3_interval_m )
   CALL nl_get_auxinput3_interval_s( 1, auxinput3_interval_s )

   if(auxinput3_interval_mo < 1 .and. &
      auxinput3_interval_d < 1 .and. &
      auxinput3_interval_h < 1 .and. &
      auxinput3_interval_m < 1 .and. &
      auxinput3_interval_s < 1) then
      auxinput3_interval_m = auxinput3_interval
   endif

   CALL nl_get_auxinput3_begin_y( 1, auxinput3_begin_y )
   CALL nl_get_auxinput3_begin_mo( 1, auxinput3_begin_mo )
   CALL nl_get_auxinput3_begin_d( 1, auxinput3_begin_d )
   CALL nl_get_auxinput3_begin_h( 1, auxinput3_begin_h )
   CALL nl_get_auxinput3_begin_m( 1, auxinput3_begin_m )
   CALL nl_get_auxinput3_begin_s( 1, auxinput3_begin_s )

   CALL nl_get_auxinput3_end_y( 1, auxinput3_end_y )
   CALL nl_get_auxinput3_end_mo( 1, auxinput3_end_mo )
   CALL nl_get_auxinput3_end_d( 1, auxinput3_end_d )
   CALL nl_get_auxinput3_end_h( 1, auxinput3_end_h )
   CALL nl_get_auxinput3_end_m( 1, auxinput3_end_m )
   CALL nl_get_auxinput3_end_s( 1, auxinput3_end_s )

!--ad_date_string(0) = 2000-01-24_12:00:00

   write(end_date_string(1:19), fmt='(i4.4,5(a1,i2.2))') &
         end_year, &
         '-', end_month, &
         '-', end_day, &
         '_', end_hour, &
         ':', end_minute, &
         ':', end_second

   write(begin_date_string(1:19), fmt='(i4.4,5(a1,i2.2))') &
         start_year, &
         '-', start_month, &
         '-', start_day, &
         '_', start_hour, &
         ':', start_minute, &
         ':', start_second

   current_y  = start_year
   current_mo = start_month
   current_d  = start_day
   current_h  = start_hour
   current_m  = start_minute
   current_s  = start_second

   index = 0

   write(unit=*, fmt='(2(2x, 2a))') &
        'begin_date_string =', begin_date_string, 'end_date_string =', end_date_string

   do while(string_a_less_than_b(begin_date_string, end_date_string))
      write(unit=*, fmt='(2(2x, 2a))') &
           'begin_date_string =', begin_date_string, 'end_date_string =', end_date_string

      index = index + 1

      write(ad_date_string(index)(1:19), fmt='(i4.4,5(a1,i2.2))') &
         current_y, &
         '-', current_mo, &
         '-', current_d, &
         '_', current_h, &
         ':', current_m, &
         ':', current_s

      write(unit=*, fmt='(a,i2,2a)') &
        'ad_date_string(', index, ')=', ad_date_string(index)

      call advance_ccyymmddhhmmss(current_y, current_mo, current_d, current_h, current_m, current_s, &
                                   0, auxinput3_interval_mo, auxinput3_interval_d, auxinput3_interval_h, auxinput3_interval_m, auxinput3_interval_s )


      write(begin_date_string(1:19), fmt='(i4.4,5(a1,i2.2))') &
         current_y, &
         '-', current_mo, &
         '-', current_d, &
         '_', current_h, &
         ':', current_m, &
         ':', current_s

      if(index > 20) exit
   end do

   n = index

   do i=1, index/2
      tmp_string = ad_date_string(i)
      ad_date_string(i) = ad_date_string(n)
      ad_date_string(n) = tmp_string
      n = n - 1
   enddo

   CALL wrf_debug ( 100 , 'Setup_ad_date_string:  returning...' )

END SUBROUTINE Setup_ad_date_string

subroutine advance_ccyymmddhhmmss(current_y, current_mo, current_d, current_h, current_m, current_s, &
                                  interval_y, interval_mo, interval_d, interval_h, interval_m, interval_s )

   implicit none

   integer, intent(inout) :: current_y, current_mo, current_d, current_h, current_m, current_s
   integer, intent(in)    :: interval_y, interval_mo, interval_d, interval_h, interval_m, interval_s

   if(interval_y /= 0 .or. interval_mo /= 0 .or. interval_d /= 0) then
      print *, 'interval_y = ', interval_y
      print *, 'interval_mo= ', interval_mo
      print *, 'interval_d = ', interval_d
      print *, 'Can not handle this yet.'
      stop
   endif

   current_y  = current_y  + interval_y
   current_mo = current_mo + interval_mo
   current_d  = current_d  + interval_d
   current_h  = current_h  + interval_h
   current_m  = current_m  + interval_m
   current_s  = current_s  + interval_s

   do while (current_s < 0)
      current_s = current_s + 60
      current_m = current_m - 1
   end do

   do while (current_s >= 60)
      current_s = current_s - 60
      current_m = current_m + 1
   end do

   do while (current_m < 0)
      current_m = current_m + 60
      current_h = current_h - 1
   end do

   do while (current_m >= 60)
      current_m = current_m - 60
      current_h = current_h + 1
   end do

   do while (current_h < 0)
      current_h = current_h + 24
      call change_date ( current_y, current_mo, current_d, -1 )
   end do

   do while (current_h > 23)
      current_h = current_h - 24
      call change_date ( current_y, current_mo, current_d, 1 )
   end do

end subroutine advance_ccyymmddhhmmss

   subroutine change_date( ccyy, mm, dd, delta )

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
   end subroutine change_date

function string_a_less_than_b(begin_date_string, end_date_string)
   IMPLICIT NONE
   character(len=19), intent(in) :: begin_date_string, end_date_string

   character(len=1) :: char

! Local
   INTEGER :: begin_year,begin_month,begin_day,begin_hour,begin_minute,begin_second
   INTEGER :: end_year,end_month,end_day,end_hour,end_minute,end_second

   logical :: string_a_less_than_b

   string_a_less_than_b = .true.

   read(begin_date_string(1:19), fmt='(i4.4,5(a1,i2.2))') &
         begin_year, &
         char, begin_month, &
         char, begin_day, &
         char, begin_hour, &
         char, begin_minute, &
         char, begin_second

   read(end_date_string(1:19), fmt='(i4.4,5(a1,i2.2))') &
         end_year, &
         char, end_month, &
         char, end_day, &
         char, end_hour, &
         char, end_minute, &
         char, end_second

   if(end_year   < begin_year  ) then
      string_a_less_than_b = .false.
      return
   endif

   if(end_month  < begin_month ) then
      string_a_less_than_b = .false.
      return
   endif

   if(end_day    < begin_day   ) then
      string_a_less_than_b = .false.
      return
   endif

   if(end_hour   < begin_hour  ) then
      string_a_less_than_b = .false.
      return
   endif

   if(end_minute < begin_minute) then
      string_a_less_than_b = .false.
      return
   endif

   if(end_second < begin_second) then
      string_a_less_than_b = .false.
   endif

end function string_a_less_than_b

