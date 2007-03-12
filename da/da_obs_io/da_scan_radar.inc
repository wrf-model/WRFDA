subroutine da_scan_radar (ob, xp, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Scan the Radar observation file
   !---------------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(in)  :: xp       ! Domain decomposition vars.
   type (ob_type), intent (inout) :: ob

   character(len=*), intent(in)   :: filename

   integer                       :: i, j, n, iost, nlevels, num_Radar, fm
   integer                       :: total_Radar
   integer                       :: iunit

   type (Radar_multi_level_type) :: platform

   character (LEN = 120)         :: char_total_Radar
   character (LEN = 120)         :: char_ned

   logical                       :: outside
   integer                        ::n_dup, ndup

   if (trace_use) call da_trace_entry("da_scan_radar")

   ! default value.
   ob%ob_numb(ob%current_ob_time)%Radar = ob%num_Radar

   ! 1. open file
   ! ============

   call da_get_unit(iunit)
   open(unit   = iunit,     &
        FILE   = trim(filename), &
        FORM   = 'FORMATTED',  &
        ACCESS = 'SEQUENTIAL', &
        iostat =  iost,     &
        STATUS = 'OLD')

   if (iost /= 0) then
      ! Does not matter of radar file missing
      call da_warning(__FILE__,__LINE__, &
         (/"Cannot open radar file "//trim(filename)/))
      call da_free_unit(iunit) 
      return
   end if

   num_Radar = 0

   ! 2. read total radar
   ! ===================

   ! 2.1 read first line
   !     ---------------

   read (unit=iunit, fmt = '(A)', iostat = iost) char_total_Radar
   if (iost /= 0) then
      ! Does matter if present and unreadable
      call da_error(__FILE__,__LINE__, &
         (/"Cannot read radar file"/))
   end if

   ! 2.3 total radar number

   read (unit=char_total_Radar (15:17),fmt='(I3)', iostat = iost) total_Radar

   ! 2.4 skip one lines

   read (unit=iunit, fmt = '(A)', iostat = iost)

   ! 3. read radar data

   do n = 1, total_Radar

      ! 3.1 skip one blank line

      read (unit=iunit, fmt = '(A)', iostat = iost)

      ! 3.2 read header

      read (unit=iunit, fmt = '(A)', iostat = iost) char_ned

      ! 3.3 read header information

      read (unit=char_ned (69:74), fmt='(I6)', iostat = iost) platform % stn % numObs

      ! 3.4 skip two lines

      read (unit=iunit, fmt = '(A)', iostat = iost)
      read (unit=iunit, fmt = '(A)', iostat = iost)

      ! 3.5 loop over records

      reports: do j = 1, platform % stn % numObs

         ! 3.5.1 read station general info

         read (unit = iunit, iostat = iost, &
                      fmt = '(A12,3X,A19,2X,2(F12.3,2X),F8.1,2X,I6)') &
                      platform % info % platform,  &
                      platform % info % date_char, &
                      platform % info % lat,       &
                      platform % info % lon,       &
                      platform % info % elv,       &
                      platform % info % levels

         read(unit=platform % info % platform (4:6), fmt='(I3)') fm

         ob % total_obs = ob % total_obs + 1

         !     3.5.2 read each level

         do i = 1, platform % info % levels
            ! height
            platform%each (i) = Radar_each_level_type(missing_r, missing, -1.0,&
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
         end do

         call da_ll_to_xy (platform%info, platform%loc, xp, outside)

         if (outside) then
            cycle reports
         end if

         nlevels = platform%info%levels

         if (nlevels > max_ob_levels) then
             write(unit=message(1),fmt='(A,2I8)') &
                ' Radar=> nlevels > max_ob_levels:',nlevels, max_ob_levels
             call da_warning(__FILE__,__LINE__,message(1:1))

             nlevels = max_ob_levels
             platform%info%levels = nlevels
         else if (nlevels < 1) then
            cycle reports
         end if

         ! Loop over duplicating obs for global
         n_dup = 1
         if (global .and. &
            (platform%loc%i == xp%ids .or. platform%loc%i == xp%ide)) n_dup= 2
   
         do ndup = 1, n_dup
            select case (fm)

            case (128)
               num_Radar = num_Radar + 1

               if (num_Radar > max_Radar) then
                  write(unit=message(1),fmt='(A,I6,A,I6)') &
                     ' Radar #= ',ob % num_Radar, ' > max_Radar = ', max_Radar
                  call da_error(__FILE__,__LINE__,message(1:1))
               end if

            case default;
               write(unit=stdout, fmt='(a)') 'Warining: unsaved obs found:'

               write(unit=stdout, fmt='(2a)') &
                  'platform % info % platform=', platform % info % platform

               write(unit=stdout, fmt='(a, i3)') &
                  'platform % info % levels=', platform % info % levels
            end select
         end do        !  loop over duplicate
      end do reports
   end do

   close (iunit)
   call da_free_unit(iunit)
 
   if (print_detail_radar) write(unit=stdout, fmt='(/5x,a,i6,a)') &
      ' Read:  ', num_Radar, ' Radar reports '

   ob%num_Radar = ob%num_Radar + num_Radar

   ob%ob_numb(ob%current_ob_time)%Radar = ob%num_Radar

   if (trace_use) call da_trace_exit("da_scan_radar")


end subroutine da_scan_radar


