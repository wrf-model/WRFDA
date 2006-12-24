subroutine da_read_radar (ob, xp, filename)

   !-----------------------------------------------------------------------
   ! Purpose: Read the Radar observation file
   !----------------------------------------------------------------------------------------!

   implicit none

   type (xpose_type), intent(in)   :: xp    ! Domain decomposition vars.
   type (ob_type), intent (inout)  :: ob

   character(*), intent(in)        :: filename

   character (LEN = 120)         :: char_total_Radar
   character (LEN = 120)         :: char_ned

   integer                       :: i, j, n, iost, nlevels, fm

   type (Radar_multi_level_type) :: platform

   logical                       :: outside

   integer                       :: total_obs, num_Radar, total_Radar
   integer                        ::n_dup, ndup, iunit

   if (trace_use) call da_trace_entry("da_read_radar")


! -------------------------------------------------------------------

   total_obs = ob%total_obs
   num_Radar = ob%num_Radar

!----------------------------------------------------------------------------!
! 1. open FILE
! ============
   call da_get_unit(iunit)
   open(unit   = iunit,     &
        FILE   = trim(filename), &
        FORM   = 'FORMATTED',  &
        ACCESS = 'SEQUENTIAL', &
        iostat =  iost,     &
        STATUS = 'OLD')

   if (iost /= 0) then
      write(unit=stderr, fmt='(/A,I3,3(2X,A)/)') &
           'ERROR in OBS inPUT FILE unit ',iunit, &
           'OBS FILENAME:', trim(filename), &
           'FOR RADAR OBSERVATIONS CANNOT BE FOUND OR CANNOT BE openED'
      return
   end if

!----------------------------------------------------------------------------!
! 2. read TOTAL RADAR
! ===================

!  2.1 read FIRST LINE
!      ---------------

   read (unit=iunit, fmt = '(A)', iostat = iost) char_total_Radar

!  2.2 PROCESS ERROR
!      -------------

   if (iost /= 0) then
     call da_error(__FILE__,__LINE__, &
        (/"Cannot read radar file"/))
   end if

!  2.3 TOTAL RADAR NUMBER
!      ------------------

   read (unit=char_total_Radar (15:17),fmt='(I3)', iostat = iost) total_Radar

   if (print_detail_radar) write (unit=stderr,fmt='(/,A,I3,/)') ' TOTAL RADAR: ', total_Radar

!  2.4 SKIP ONE LINE
!      -------------

   read (unit=iunit, fmt = '(A)', iostat = iost)

!----------------------------------------------------------------------------!
! 3. read RADAR DATA
! ==================

  loop_total_Radar: do n = 1, total_Radar

!  3.1 SKIP ONE BLANK LINE
!  -----------------------

   read (unit=iunit, fmt = '(A)', iostat = iost)

! 3.2 read HEADER
!     -----------

   read (unit=iunit, fmt = '(A)', iostat = iost) char_ned

! 3.3 read HEADER inFORMATION
!     -----------------------

   read (unit=char_ned (1:5),fmt='(A5)', iostat = iost) platform % stn % platform

   if (print_detail_radar) write (unit=stderr,fmt='(A)') 'RADAR Observations information'

   read (unit=char_ned (8:19),fmt='(A12)', iostat = iost) platform % stn % name

   if (print_detail_radar) write (unit=stderr,fmt='(A,A5,A,A12)')  &
                        ' Expect: ',platform % stn % platform, &
                        'data at station:', platform % stn % name

   read (unit=char_ned(20:27),fmt='(F8.3)', iostat = iost) platform % stn % lon

   read (unit=char_ned (30:37),fmt='(F8.3)', iostat = iost) platform % stn % lat

   read (unit=char_ned (40:47),fmt='(F8.1)', iostat = iost) platform % stn % elv

   if (print_detail_radar) write (unit=stderr,fmt='(A,2(F8.3,2X),F8.1)')  &
                        'The station longitude, latutude, and altitude are: ', &
                         platform % stn % lon, &
                         platform % stn % lat, platform % stn % elv

   read (unit=char_ned (50:68),fmt='(A19)', iostat = iost) platform % stn % date_char

   if (print_detail_radar) write (unit=stderr,fmt='(A,A19)')   &
                        'The observation time for this data is ',     &
                        platform % stn % date_char

   read (unit=char_ned (69:74),fmt='(I6)', iostat = iost) platform % stn % numObs

   if (print_detail_radar) write (unit=stderr,fmt='(A,I6)')   &
                        'Total number of Super-observations is ', &
                        platform % stn % numObs

   read (unit=char_ned (75:80),fmt='(I6)', iostat = iost) platform % stn % levels

   if (print_detail_radar) write (unit=stderr,fmt='(A,I6)')   &
                        'Vertical layers for each Super-observation is ', &
                        platform % stn % levels

   write (unit=stderr,fmt='(A)')  &
   '--------------------------------------------------------------------------'

!----------------------------------------------------------------------------!
!  3.4 SKIP TWO LINES
!      --------------

   read (unit=iunit, fmt = '(A)', iostat = iost)
   read (unit=iunit, fmt = '(A)', iostat = iost)

!----------------------------------------------------------------------------!
!  3.5 LOOP OVER RECORDS
!  --------------------

   reports: do j = 1, platform % stn % numObs

!    3.5.1 read STATION GENERAL inFO
!           -------------------------

      read (unit = iunit, iostat = iost, &
                   fmt = '(A12,3X,A19,2X,2(F12.3,2X),F8.1,2X,I6)') &
                   platform % info % platform,  &
                   platform % info % date_char, &
                   platform % info % lat,       &
                   platform % info % lon,       &
                   platform % info % elv,       &
                   platform % info % levels

      read(platform % info % platform (4:6), '(I3)') fm

      total_obs = total_obs + 1


!    3.5.2 read EACH LEVEL
!          ---------------

    loop_level: do i = 1, platform % info % levels

      platform%each(i) = Radar_each_level_type(missing_r, missing, -1.0,       & ! height
                                    field_type(missing_r, missing, missing_r), & ! rv
                                    field_type(missing_r, missing, missing_r))   ! rf

      read (unit = iunit, fmt = '(3X, F12.1, 2(F12.3,I4,F12.3,2X))') &
                          platform % each (i) % height,           &
                          platform % each (i) % rv % inv,         &
                          platform % each (i) % rv % qc,          &
                          platform % each (i) % rv % error,       &
                          platform % each (i) % rf % inv,         &
                          platform % each (i) % rf % qc,          &
                          platform % each (i) % rf % error

         if (platform % each (i) % rv % error == 0.0) then
              platform % each (i) % rv % error  = 1.0
         end if

         if (platform % each (i) % rf % error == 0.0) then
              platform % each (i) % rf % error  = 1.0
         end if

         if (platform % each (i) % rv % inv   == missing_r .or. &
             platform % each (i) % rv % error == missing_r) then
             platform % each (i) % rv % qc     = missing_data
         end if

         if (platform % each (i) % rf % inv   == missing_r .or. &
             platform % each (i) % rf % error == missing_r) then
             platform % each (i) % rf % qc     = missing_data
         end if

    end do loop_level

    call da_ll_to_xy (platform%info, platform%loc, xp, outside)

    if (outside) then
      cycle reports
    end if

    nlevels = platform%info%levels

    if (nlevels > max_ob_levels) then

       write(unit=stderr,fmt='(A,2I8)') &
       ' Radar=> nlevels > max_ob_levels:',nlevels, max_ob_levels
    
       nlevels = max_ob_levels
       platform%info%levels = nlevels

    else if (nlevels < 1) then

       cycle reports

    end if

!    Loop over duplicating obs for global
   n_dup = 1
   if (global .and. &
     (platform%loc%i == xp%ids .or. platform%loc%i == xp%ide))n_dup= 2
   do ndup = 1, n_dup

       select case (fm)
 
          case (128)

          num_Radar = num_Radar + 1
   
          if (num_Radar > max_Radar) then
            write(unit=message(1),fmt='(A,I6,A,I6)') &
              "Radar #= ",ob % num_Radar, " > max_Radar = ", max_Radar
            call da_error(__FILE__,__LINE__,message(1:1))
          end if

          ob % Radar (num_Radar) % stn_loc % lon = platform % stn % lon
          ob % Radar (num_Radar) % stn_loc % lat = platform % stn % lat
          ob % Radar (num_Radar) % stn_loc % elv = platform % stn % elv

          ob % Radar (num_Radar) % info = platform % info
          ob % Radar (num_Radar) % loc  = platform % loc
 
          allocate (ob % Radar (num_Radar) % height   (1:nlevels))
          allocate (ob % Radar (num_Radar) % height_qc(1:nlevels))
          allocate (ob % Radar (num_Radar) % zk       (1:nlevels))
          allocate (ob % Radar (num_Radar) % rv       (1:nlevels))
          allocate (ob % Radar (num_Radar) % rf       (1:nlevels))

          do i = 1, nlevels
    
            ob % Radar (num_Radar) % height(i)    = platform % each(i) % height
            ob % Radar (num_Radar) % height_qc(i) = platform % each(i) % height_qc
            ob % Radar (num_Radar) % zk(i)        = platform % each(i) % zk
            ob % Radar (num_Radar) % rv(i)        = platform % each(i) % rv
            ob % Radar (num_Radar) % rf(i)        = platform % each(i) % rf

          end do

          case default;

          write(unit=stdout, fmt='(a)') 'Warining: unsaved obs found:'

          write(unit=stdout, fmt='(2a)') &
               'platform % info % platform=', platform % info % platform

          write(unit=stdout, fmt='(a, i3)') &
               'platform % info % levels=', platform % info % levels

       end select

      if (global .and. ndup == 1) then
      if (platform%loc % i >= xp % ide) then
      platform%loc%i = xp % ids
      platform%loc%proc_domain = .false.
      else if (platform%loc % i <= xp % ids) then
      platform%loc%i = xp % ide
      platform%loc%proc_domain = .false.
      end if
     end if

    end do        !  loop over duplicate



   end do reports

  end do loop_total_Radar

!----------------------------------------------------------------------------!

   ob % num_Radar = num_Radar

!----------------------------------------------------------------------------!
! 4. PRinT out
! ============
 
  if (print_detail_radar) then

    write(unit=stdout, fmt='(a)')  ' '

    write(unit=stdout, fmt='(5x,a,i6,a)') &
         ' Read:  ', ob % num_Radar, ' Radar reports '

  end if

    write (unit=stderr,fmt='(/,A,I3,/)') ' end OF unit: ',iunit

    if (ob%ob_numb(ob%current_ob_time)%Radar /= ob%num_Radar) then
      write(unit=message(1),fmt='(a, i6, 2x, a, i6)') &
        'Radar obs mismatch ob%ob_numb(ob%current_ob_time)%Radar=', &
        ob%ob_numb(ob%current_ob_time)%Radar, &
        'ob%num_Radar=', ob%num_Radar
      call da_error(__FILE__,__LINE__,message(1:1))
    end if
!----------------------------------------------------------------------------!

   close(iunit)
   call da_free_unit(iunit)

   if (trace_use) call da_trace_exit("da_read_radar")


end subroutine da_read_radar


