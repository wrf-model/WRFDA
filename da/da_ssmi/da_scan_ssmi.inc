subroutine da_scan_ssmi (ob, xp, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Read the header of a GTS observation file
   !---------------------------------------------------------------------------

   implicit none

   type(xpose_type), intent(in)    :: xp    ! Domain decomposition vars.
   type(ob_type),    intent(inout) :: ob
   character(*), intent(in)        :: filename

   character (LEN =  10)        :: fmt_name

   character (LEN = 160)        :: fmt_info,    &
                                   fmt_loc
   character (LEN = 40)         :: obs_platform
   character (LEN = 120)        :: char_ned

   integer                      :: i, j, k, i1, j1, k1, iost, fm,iunit
   real                         :: dx, dxm, dy, dym, dz, dzm

   type (model_loc_type)        :: loc
   type (info_type)             :: info
   type (field_type)            :: speed, tpw

   type (field_type)            :: tb19v, tb19h, tb22v
   type (field_type)            :: tb37v, tb37h, tb85v, tb85h

   logical                      :: isfilter,ipass 
   logical                      :: outside
   integer                      :: irain, iprecip

   isfilter = .true. ! filter out rain points
   irain = 0

   ob%ob_numb(ob%current_ob_time)%ssmi_retrieval = ob%num_ssmi_retrieval
   ob%ob_numb(ob%current_ob_time)%ssmi_tb        = ob%num_ssmi_tb

   ! open FILE
   call da_get_unit(iunit)
   open(unit   = iunit,     &
        FILE   = trim(filename), &
        FORM   = 'FORMATTED',  &
        ACCESS = 'SEQUENTIAL', &
        iostat =  iost,     &
        STATUS = 'OLD')

   if (iost /= 0) then
      write(unit=stdout, fmt='(/A,I3,3(2X,A)/)') &
           'ERROR in SSMI inPUT FILE unit ',iunit, &
           'SSMI FILENAME:', trim(filename), &
           'FOR SSMI CANNOT BE FOUND OR CANNOT BE openED'
      return
   end if

   rewind (unit = iunit)

   ! 2.  read HEADER
   ! ===============

   ! 2.1 read FIRST LINE
   !     ---------------

   read (unit = iunit, fmt = '(A)', iostat = iost) char_ned

   ! 2.2 PROCESS ERROR
   !     -------------

   if (iost /= 0) then
      write(unit=stderr, fmt='(/a,i3/)') &
           'ERROR readinG FILE unit:', iunit, &
           'ERROR CODE:', iost

      return
   else
      write(unit=stdout, fmt='(/2a/)') &
           'in da_scan_ssmi.inc, char_ned=', trim(char_ned)
   end if

   ! 2.3 read NUMBER OF REPORTS
   !     ---------------------

   do
      read (unit = iunit, fmt = '(A)', iostat = iost) char_ned

      if (iost /= 0) then
         write(unit=stderr, fmt='(/a,i3/)') &
              'ERROR readinG FILE unit:', iunit, &
              'ERROR CODE:', iost

         return
      end if

      if (char_ned(1:6) == 'NESTIX') exit
   end do

   do
      read (unit = iunit, fmt = '(A)', iostat = iost) char_ned
      if (char_ned(1:6) == 'inFO  ') exit
   end do

   read (unit = iunit, fmt = '(A)', iostat = iost) char_ned

   ! read FORMATS
   ! ------------

   read (unit=iunit, fmt = '(A,1X,A)') &
        fmt_name, fmt_info, &
        fmt_name, fmt_loc

   ! SKIP 1 line
   ! -----------

   read (unit=iunit, fmt = '(A)') fmt_name

   !  write (unit = stdout, fmt = '(/,2(A,F3.0,/))')  &
   !     'Error max test ratio for SWS = ',max_error_uv,  &
   !     'Error max test ratio for PW  = ',max_error_pw,  &
   !     'Error max test ratio for PW  = ',max_error_tb


   ! LOOP OVER RECORDS
   ! -----------------

   reports: do

      ! read STATION GENERAL inFO
      ! =========================

      read (unit=iunit, fmt = fmt_info, iostat = iost) &
                   info % platform,    &
                   info % date_char,   &
                   info % name,        &
                   info % levels,      &
                   info % lat,         &
                   info % lon,         &
                   info % elv,         &
                   info % id

      read(unit=info % platform (4:6),fmt='(I3)') fm

      if (iost /= 0) then
         exit reports
      end if

      ob % total_obs = ob % total_obs + 1

      select case(fm)
      case (125)    ;
         ! read SURFACE WinD SPEED AND PRECIPITABLE WATER
         read (unit=iunit, fmt = fmt_loc) speed, tpw
      case (126)    ;
         read (unit=iunit, fmt = fmt_loc) &
            tb19v, tb19h, tb22v, tb37v, tb37h, tb85v, tb85h
      case default;
         write(unit=stdout, fmt='(/a/)') &
              'warning   warning   warning   warning   warning :'
         write(unit=stdout, fmt='(a, i6)') 'unsaved ssmi obs found, fm=', fm
         write(unit=stdout, fmt='(a, 2f12.6)') &
              'info%(lon,lat)=', info%lon, info%lat
      end select

      ! check if obs is in horizontal domain
      ! ====================================

      ! Compute the model horizontal coordinate x, y
      ! Check if obs is wihin horizontal domain

      call da_ll_to_xy (info, loc, xp, outside)

      if (outside) cycle reports

      select case(fm)
      case (125) ;
         if (.not. Use_SsmiRetrievalObs) cycle reports

         ! Check if at least one field is present
         if ((tpw % qc == missing) .AND. (speed % qc == missing)) then
            cycle reports
         end if

         ob % num_ssmi_retrieval = ob % num_ssmi_retrieval + 1

      case (126) ;
         if (.not. Use_SsmiTbObs) cycle reports

         ! Check if at least one field is present

         if ((tb19v % qc == missing) .AND. (tb19h % qc == missing)  .AND. &
             (tb22v % qc == missing)                                .AND. &
             (tb37v % qc == missing) .AND. (tb37h % qc == missing)  .AND. &
             (tb85v % qc == missing) .AND. (tb85h % qc == missing)) then
              cycle reports
         end if

        ! FILTER RAin PIXELS
        ! ====================================

          if (isfilter) then

            ipass = .false.
            iprecip = 0
             call filter(tb19v%inv, tb19h%inv, tb22v%inv, tb37v%inv, &
                         tb37h%inv, tb85v%inv, tb85h%inv, ipass, iprecip, &
                         info%lat)
            if (iprecip == 1) then
                irain = irain + 1
                cycle reports
             end if
          end if

         ob % num_ssmi_tb = ob % num_ssmi_tb + 1

      case default;
         ! Do nothing.
      end select

   end do reports

   close(unit=iunit)
   call da_free_unit(iunit)
 
   write(unit=stdout, fmt='(5x,a,i6,a)') &
      'Read:  ', ob % num_ssmi_retrieval , ' SSMI_RETRIEVAL reports,', &
      'Read:  ', ob % num_ssmi_tb ,        ' SSMI_Tb reports,', &
      'Read:  ', ob % total_obs, ' Total Observations.'

   write(unit=stdout, fmt='(/,5x,a,i6/)') &
      '** Rain contaminated SSMI_Tb =', irain

   ob%ob_numb(ob%current_ob_time)%ssmi_retrieval = ob%num_ssmi_retrieval
   ob%ob_numb(ob%current_ob_time)%ssmi_tb        = ob%num_ssmi_tb

end subroutine da_scan_ssmi


