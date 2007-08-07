subroutine da_scan_ssmi (ob, xp, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Read the header of a SSMI 2.0 GTS observation file
   !---------------------------------------------------------------------------

   implicit none

   type(xpose_type), intent(in)    :: xp    ! Domain decomposition vars.
   type(ob_type),    intent(inout) :: ob
   character(len=*), intent(in)    :: filename

   character (LEN =  10)        :: fmt_name
   character (LEN = 160)        :: fmt_info, fmt_loc
   character (LEN = 120)        :: char_ned

   integer                      :: iost, fm,iunit

   type (model_loc_type)        :: loc
   type (info_type)             :: info
   type (field_type)            :: speed, tpw

   type (field_type)            :: tb19v, tb19h, tb22v
   type (field_type)            :: tb37v, tb37h, tb85v, tb85h

   logical                      :: isfilter,ipass 
   logical                      :: outside
   integer                      :: irain, iprecip

   if (trace_use) call da_trace_entry("da_scan_ssmi")

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
      call da_warning(__FILE__,__LINE__, &
         (/"Cannot open SSMI file "//trim(filename)/))
      call da_free_unit(iunit)
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
      call da_error(__FILE__,__LINE__, &
         (/"Cannot read SSMI file "//trim(filename)/))
   else
      write(unit=stdout, fmt='(/2a/)') &
           'in da_scan_ssmi.inc, char_ned=', trim(char_ned)
   end if

   ! 2.3 read NUMBER OF REPORTS
   !     ---------------------

   do
      read (unit = iunit, fmt = '(A)', iostat = iost) char_ned
      if (iost /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot read SSMI file "//trim(filename)/))
      end if
      if (char_ned(1:6) == 'NESTIX') exit
   end do

   do
      read (unit = iunit, fmt = '(A)', iostat = iost) char_ned
      if (char_ned(1:6) == 'INFO  ') exit
   end do

   read (unit = iunit, fmt = '(A)', iostat = iost) char_ned

   ! read formats
   ! ------------

   read (unit=iunit, fmt = '(A,1X,A)') &
      fmt_name, fmt_info, &
      fmt_name, fmt_loc

   ! skip 1 line
   ! -----------

   read (unit=iunit, fmt = '(A)') fmt_name

   !  write (unit = stdout, fmt = '(/,2(A,F3.0,/))')  &
   !     'Error max test ratio for SWS = ',max_error_uv,  &
   !     'Error max test ratio for PW  = ',max_error_pw,  &
   !     'Error max test ratio for PW  = ',max_error_tb


   ! loop over records
   ! -----------------

   reports: do

      ! read station general info
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
      if (iost /= 0) exit reports

      ob % total_obs = ob % total_obs + 1

      select case(fm)
      case (125)    ;
         ! read surface wind speed and precipitable water
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
         if (.not. use_ssmiretrievalobs) cycle reports

         ! Check if at least one field is present
         if ((tpw % qc == missing) .AND. (speed % qc == missing)) then
            cycle reports
         end if

         ob % num_ssmi_retrieval = ob % num_ssmi_retrieval + 1

      case (126) ;
         if (.not. use_ssmitbobs) cycle reports

         ! Check if at least one field is present

         if ((tb19v % qc == missing) .AND. (tb19h % qc == missing)  .AND. &
             (tb22v % qc == missing)                                .AND. &
             (tb37v % qc == missing) .AND. (tb37h % qc == missing)  .AND. &
             (tb85v % qc == missing) .AND. (tb85h % qc == missing)) then
            cycle reports
         end if

         ! filter rain pixels
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

   if (trace_use) call da_trace_exit("da_scan_ssmi")

end subroutine da_scan_ssmi


