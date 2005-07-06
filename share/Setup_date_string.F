SUBROUTINE Setup_date_string ( grid, date_string, adantn )
   USE module_domain
   USE module_configure
   USE WRF_ESMF_MOD
   IMPLICIT NONE
   TYPE(domain), POINTER :: grid
   character(len=19), dimension(1000), intent(out) :: date_string
   character(len=2),                   intent(in ) :: adantn

   character(len=19)                             :: tmp_string, begin_date_string, end_date_string

! Local
   INTEGER :: start_year,start_month,start_day,start_hour,start_minute,start_second
   INTEGER :: end_year,end_month,end_day,end_hour,end_minute,end_second

   INTEGER :: local_interval  ,  &
              local_interval_mo, &
              local_interval_d,  &
              local_interval_h,  &
              local_interval_m,  &
              local_interval_s

   INTEGER :: local_begin  ,  &
              local_begin_y,  &
              local_begin_mo, &
              local_begin_d,  &
              local_begin_h,  &
              local_begin_m,  &
              local_begin_s

   INTEGER :: local_end  , &
              local_end_y, &
              local_end_mo,&
              local_end_d, &
              local_end_h, &
              local_end_m, &
              local_end_s

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

   if( adantn == 'ad') then

      CALL nl_get_auxinput3_begin_y( 1, local_begin_y )
      CALL nl_get_auxinput3_begin_mo( 1, local_begin_mo )
      CALL nl_get_auxinput3_begin_d( 1, local_begin_d )
      CALL nl_get_auxinput3_begin_h( 1, local_begin_h )
      CALL nl_get_auxinput3_begin_m( 1, local_begin_m )
      CALL nl_get_auxinput3_begin_s( 1, local_begin_s )

      CALL nl_get_auxinput3_end_y( 1, local_end_y )
      CALL nl_get_auxinput3_end_mo( 1, local_end_mo )
      CALL nl_get_auxinput3_end_d( 1, local_end_d )
      CALL nl_get_auxinput3_end_h( 1, local_end_h )
      CALL nl_get_auxinput3_end_m( 1, local_end_m )
      CALL nl_get_auxinput3_end_s( 1, local_end_s )

      CALL nl_get_auxinput3_interval( 1, local_interval )   ! same as minutes

      CALL nl_get_auxinput3_interval_mo( 1, local_interval_mo )
      CALL nl_get_auxinput3_interval_d( 1, local_interval_d )
      CALL nl_get_auxinput3_interval_h( 1, local_interval_h )
      CALL nl_get_auxinput3_interval_m( 1, local_interval_m )
      CALL nl_get_auxinput3_interval_s( 1, local_interval_s )

   else 

      CALL nl_get_auxinput2_begin_y( 1, local_begin_y )
      CALL nl_get_auxinput2_begin_mo( 1, local_begin_mo )
      CALL nl_get_auxinput2_begin_d( 1, local_begin_d )
      CALL nl_get_auxinput2_begin_h( 1, local_begin_h )
      CALL nl_get_auxinput2_begin_m( 1, local_begin_m )
      CALL nl_get_auxinput2_begin_s( 1, local_begin_s )

      CALL nl_get_auxinput2_end_y( 1, local_end_y )
      CALL nl_get_auxinput2_end_mo( 1, local_end_mo )
      CALL nl_get_auxinput2_end_d( 1, local_end_d )
      CALL nl_get_auxinput2_end_h( 1, local_end_h )
      CALL nl_get_auxinput2_end_m( 1, local_end_m )
      CALL nl_get_auxinput2_end_s( 1, local_end_s )

      CALL nl_get_auxinput2_interval( 1, local_interval )   ! same as minutes

      CALL nl_get_auxinput2_interval_mo( 1, local_interval_mo )
      CALL nl_get_auxinput2_interval_d( 1, local_interval_d )
      CALL nl_get_auxinput2_interval_h( 1, local_interval_h )
      CALL nl_get_auxinput2_interval_m( 1, local_interval_m )
      CALL nl_get_auxinput2_interval_s( 1, local_interval_s )

   endif

   if(local_interval_mo < 1 .and. &
      local_interval_d < 1 .and. &
      local_interval_h < 1 .and. &
      local_interval_m < 1 .and. &
      local_interval_s < 1) then
      local_interval_m = local_interval
   endif

      write(6,*) 'xyh intervals =', local_interval_mo, local_interval_d, local_interval_h, local_interval_m, local_interval_s


!--date_string(0) = 2000-01-24_12:00:00

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

      write(date_string(index)(1:19), fmt='(i4.4,5(a1,i2.2))') &
         current_y, &
         '-', current_mo, &
         '-', current_d, &
         '_', current_h, &
         ':', current_m, &
         ':', current_s

      write(unit=*, fmt='(a,i4,2a)') &
        'date_string(', index, ')=', date_string(index)

      write(6,*) 'xyh intervals =', local_interval_mo, local_interval_d, local_interval_h, local_interval_m, local_interval_s

      write(6,*) 'xyh intervals b4 =', current_y, current_mo, current_d, current_h, current_m, current_s

      call advance_ccyymmddhhmmss(current_y, current_mo, current_d, current_h, current_m, current_s, &
                                   0, local_interval_mo, local_interval_d, local_interval_h, local_interval_m, local_interval_s )

      write(6,*) 'xyh intervals =', current_y, current_mo, current_d, current_h, current_m, current_s

      write(begin_date_string(1:19), fmt='(i4.4,5(a1,i2.2))') &
         current_y, &
         '-', current_mo, &
         '-', current_d, &
         '_', current_h, &
         ':', current_m, &
         ':', current_s

      if(index > 1000) exit
   end do

   if( adantn /= 'tn' ) then ! reverse order

      n = index
      do i=1, index/2
         tmp_string = date_string(i)
         date_string(i) = date_string(n)
         date_string(n) = tmp_string
         n = n - 1
      enddo

   endif

!xyh
   do i=1,index
      write(6,*)'xyh:',adantn,i,date_string(i)
   enddo

   CALL wrf_debug ( 100 , 'Setup_date_string:  returning...' )

END SUBROUTINE Setup_date_string

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

   integer(kind=8) :: begin_date, end_date

   logical :: string_a_less_than_b

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

   begin_date = begin_second + 100*(begin_minute + 100*(begin_hour &
                             +  100*(begin_day + 100*(begin_month + 100*begin_year))))
   end_date = end_second + 100*(end_minute + 100*(end_hour &
                             +  100*(end_day + 100*(end_month + 100*end_year))))

   if(end_date < begin_date  ) then
      string_a_less_than_b = .false.
   else
      string_a_less_than_b = .true.
   endif

end function string_a_less_than_b

