subroutine da_read_ssmi (ob, xp, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Read the header of a SSMI 2.0 GTS observation file
   !---------------------------------------------------------------------------

   implicit none

   type(xpose_type), intent(in)    :: xp    ! Domain decomposition vars.
   type(ob_type),    intent(inout) :: ob
   character(*), intent(in)        :: filename

   character (LEN =  10)        :: fmt_name

   character (LEN = 160)        :: fmt_info,    &
                                   fmt_loc
   character (LEN = 120)        :: char_ned

   integer                      :: iost, fm,iunit

   type (model_loc_type)        :: loc
   type (info_type)             :: info
   type (field_type)            :: speed, tpw

   type (field_type)            :: tb19v, tb19h, tb22v
   type (field_type)            :: tb37v, tb37h, tb85v, tb85h

   type (count_obs_number_type) :: count_obs_ssmir
   type (count_obs_number_type) :: count_obs_ssmit

   logical                      :: isfilter,ipass 
   logical                      :: outside, outside_all
   integer                      :: irain, iprecip
   integer                      :: n, ndup

   count_obs_ssmir = count_obs_number_type(0, 0, 0, 0)
   count_obs_ssmit = count_obs_number_type(0, 0, 0, 0)

   isfilter = .true. ! filter out rain points
   irain = 0

   ! open file

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
      return
   end if

   rewind (unit = iunit)

   ! 2.  read header
   ! ===============

   ! 2.1 read first line
   !     ---------------

   read (unit = iunit, fmt = '(A)', iostat = iost) char_ned
   if (iost /= 0) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot read SSMI file"//trim(filename)/))
   else
      write(unit=stdout, fmt='(/2a/)') &
           'in da_read_ssmi_info.inc, char_ned=', trim(char_ned)
   end if

   ! 2.3 read number of reports
   !     ---------------------

   do
      read (unit = iunit, fmt = '(A)', iostat = iost) char_ned

      if (iost /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Cannot read SSMI file"//trim(filename)/))
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

   !  SKIP 1 line
   !  -----------

   read (unit=iunit, fmt = '(A)') fmt_name

   !  loop over records
   !  -----------------

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

      if (iost /= 0) then
         exit reports
      end if

      ob % total_obs = ob % total_obs + 1

      select case(fm)
         case (125)    ;
            ! read surface wind speed and precipitable water
            read (unit=iunit, fmt = fmt_loc) speed, tpw
         case (126)    ;
            read (unit=iunit, fmt = fmt_loc) &
               tb19v, tb19h, tb22v, tb37v, tb37h, tb85v, tb85h

               tb19v % error = tb19v % error + 2.
               tb19h % error = tb19h % error + 2.
               tb22v % error = tb22v % error + 2.
               tb37h % error = tb37h % error + 2.
               tb37v % error = tb37v % error + 2.
               tb85h % error = tb85h % error + 2.
               tb85v % error = tb85v % error + 2.

         case default;
            write(unit=message(1), fmt='(a, i6)') 'unsaved ssmi obs found, fm=', fm
            write(unit=message(2), fmt='(a, 2f12.6)') &
               'info%(lon,lat)=', info%lon, info%lat
            call da_warning(__FILE__,__LINE__,message(1:2))
      end select

      ! check if obs is in horizontal domain
      ! ====================================

      ! Compute the model horizontal coordinate x, y
      ! Check if obs is wihin horizontal domain

      call da_ll_to_xy (info, loc, xp, outside, outside_all)

      if (outside_all) cycle reports

      loc % pw  % inv = missing_r
      loc % pw  % qc  = missing_data
      loc % slp = loc % pw

      ! Loop over duplicating obs for global
      ndup = 1
      if (global .and. &
         (loc%i < xp%ids .or. loc%i >= xp%ide)) ndup= 2

      ! It is possible that logic for counting obs is incorrect for the
      ! global case with >1 MPI tasks due to obs duplication, halo, etc.
      ! TBH:  20050913

      if (.not.outside) then
         if (print_detail_obs .and. ndup > 1) then
            write(unit=stdout, fmt = fmt_info) &
               info%platform,    &
               info%date_char,   &
               info%name,        &
               info%levels,      &
               info%lat,         &
               info%lon,         &
               info%elv,         &
               info%id

            write(unit=stdout, fmt = '(a,2i5,4e20.10)') &
               ' duplicating obs since loc% i,j,dx,dxm,dy & dym ', &
               loc%i,  loc%j,   &
               loc%dx, loc%dxm, &
               loc%dy, loc%dym
         end if
      end if

      dup_loop: do n = 1, ndup

      select case(fm)
         case (125) ;

            if (n==1) ob%num_ssmi_retrieval_glo = ob%num_ssmi_retrieval_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not. Use_SsmiRetrievalObs) cycle reports

            ! Check if at least one field is present
            if ((tpw % qc == missing_data) .AND. (speed % qc == missing_data)) then
               count_obs_ssmir % num_missing = &
               count_obs_ssmir % num_missing + 1
               cycle reports
            end if

            ! fill permanent structure
            ! ========================

            ob % num_ssmi_retrieval = ob % num_ssmi_retrieval + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit
            ! tests with different numbers of MPI tasks.  
            loc%obs_global_index = ob%num_ssmi_retrieval_glo

            !  One more data used

            count_obs_ssmir % num_used = count_obs_ssmir % num_used + 1
      
            !  Initialize other non read fields

            ob % ssmi_retrieval (ob%num_ssmi_retrieval) % loc = loc

            !  Initialize with read fields

            ob % ssmi_retrieval (ob%num_ssmi_retrieval) % info = info

            ob % ssmi_retrieval (ob%num_ssmi_retrieval) % speed = speed
            ob % ssmi_retrieval (ob%num_ssmi_retrieval) % tpw   = tpw

         case (126) ;

            if (n==1) ob%num_ssmi_tb_glo = ob%num_ssmi_tb_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not. Use_SsmiTbObs) cycle reports

            ! Check if at least one field is present

            if ((tb19v % qc == missing_data) .AND. (tb19h % qc == missing_data)  .AND. &
                (tb22v % qc == missing_data)                                .AND. &
                (tb37v % qc == missing_data) .AND. (tb37h % qc == missing_data)  .AND. &
                (tb85v % qc == missing_data) .AND. (tb85h % qc == missing_data)) then
               count_obs_ssmit % num_missing = &
               count_obs_ssmit % num_missing + 1
               ! write (unit=stdout,fmt=*) 'missing data'
               cycle reports
            end if

            ! filter rain pixels
            !  ====================================

            if (isfilter) then
               ipass = .false.
               iprecip = 0
               call filter(tb19v%inv, tb19h%inv, tb22v%inv, tb37v%inv, &
                          tb37h%inv, tb85v%inv, tb85h%inv, ipass, iprecip, &
                          info%lat)
               if (iprecip .eq. 1) then
                  tb19v % qc    = -88.
                  tb19h % qc    = -88.
                  tb22v % qc    = -88.
                  tb37h % qc    = -88.
                  tb37v % qc    = -88.
                  tb85h % qc    = -88.
                  tb85v % qc    = -88.
                  irain = irain + 1
                  cycle reports
               end if
            end if

            ! fill permanent structure
            ! ========================

            ! One more data read in

            ob % num_ssmi_tb = ob % num_ssmi_tb + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit
            ! tests with different numbers of MPI tasks.  
            loc%obs_global_index = ob%num_ssmi_tb_glo

            !  One more data used

            count_obs_ssmit % num_used = count_obs_ssmit % num_used + 1

            !  Initialize other non read fields

            ob % ssmi_tb (ob%num_ssmi_tb) % loc = loc

            !  Initialize with read fields

            ob % ssmi_tb (ob % num_ssmi_tb) % info = info

            ob % ssmi_tb (ob % num_ssmi_tb) % tb19v = tb19v
            ob % ssmi_tb (ob % num_ssmi_tb) % tb19h = tb19h
            ob % ssmi_tb (ob % num_ssmi_tb) % tb22v = tb22v
            ob % ssmi_tb (ob % num_ssmi_tb) % tb37v = tb37v
            ob % ssmi_tb (ob % num_ssmi_tb) % tb37h = tb37h
            ob % ssmi_tb (ob % num_ssmi_tb) % tb85v = tb85v
            ob % ssmi_tb (ob % num_ssmi_tb) % tb85h = tb85h

         case default;
            ! Do nothing.
      end select
      end do dup_loop
   end do reports

   close(iunit)
   call da_free_unit(iunit)
 
   write(unit=stdout, fmt='(5x,a,i6,a)') &
      'Read:  ', ob % num_ssmi_retrieval , ' SSMI_RETRIEVAL reports,', &
      'Read:  ', ob % num_ssmi_tb ,        ' SSMI_Tb reports,', &
      'Read:  ', ob % total_obs, ' Total reports.', &
      'There are ', ob % total_obs                  &
                  - ob % num_ssmi_retrieval         &
                  - ob % num_ssmi_tb,               &
                '  reports unsaved.'

   write(unit=stdout, fmt='(/,25x,a)') &
                        '     used   outdomain  max_er_chk   missing' 
   write(unit=stdout, fmt='(4x,a,4i11)') &
                   'SSMI_RETRIEVAL_diag: ', count_obs_ssmir
   write(unit=stdout, fmt='(4x,a,4i11)') &
                   'SSMI_Tb_diag:        ', count_obs_ssmit
   write(unit=stdout, fmt='(/,a)') &
 "------------------------------------------------------------------------------"

   write(unit=stdout, fmt='(/,5x,a,i6/)') '** Rain contaminated SSMI_Tb =', irain

end subroutine da_read_ssmi


