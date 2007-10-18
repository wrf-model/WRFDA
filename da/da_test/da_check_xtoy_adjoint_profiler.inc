subroutine da_check_xtoy_adjoint_profiler(iv, y, adjtest_lhs, pertile_lhs)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)     :: iv            ! obs. inc. vector (o-b).
   type (y_type) , intent(inout)  :: y             ! y = h (xa)
   real          , intent(inout)  :: adjtest_lhs, pertile_lhs  

   integer :: n, k          ! Loop counter.
   
   if (trace_use_dull) call da_trace_entry("da_check_xtoy_adjoint_profiler")

   do n=1, iv%info(profiler)%nlocal
      if (iv%info(profiler)%proc_domain(1,n)) then
         do k=1, iv%info(profiler)%levels(n)
            adjtest_lhs = adjtest_lhs &
              + (y%profiler(n)%u(k)/typical_u_rms)**2 + (y%profiler(n)%v(k)/typical_v_rms)**2

         end do
      end if

      do k=1, iv%info(profiler)%levels(n)
         pertile_lhs = pertile_lhs &
            + (y%profiler(n)%u(k)/typical_u_rms)**2 + (y%profiler(n)%v(k)/typical_v_rms)**2

         y%profiler(n)%u(k)= y%profiler(n)%u(k) / typical_u_rms ** 2
         y%profiler(n)%v(k)= y%profiler(n)%v(k) / typical_v_rms ** 2
      end do
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_xtoy_adjoint_profiler")

end subroutine da_check_xtoy_adjoint_profiler


__, (/"Cannot open SSMI file "//trim(filename)/))
      call da_free_unit(iunit)
      return
   end if

   rewind (unit = iunit)

   ! 2.  read header
   ! ===============

   ! 2.1 read first line
   !     ---------------

   read (unit = iunit, fmt = '(A)', iostat = iost) char_ned
   if (iost /= 0) then
      call da_error(__FILE__,__LINE__, (/"Cannot read SSMI file "//trim(filename)/))
   end if

   ! 2.3 read number of reports
   do
      read (unit = iunit, fmt = '(A)', iostat = iost) char_ned
      if (iost /= 0) then
         call da_error(__FILE__,__LINE__, (/"Cannot read SSMI file "//trim(filename)/))
      end if
      if (char_ned(1:6) == 'NESTIX') exit
   end do

   do
      read (unit = iunit, fmt = '(A)', iostat = iost) char_ned
      if (char_ned(1:6) == 'INFO  ') exit
   end do

   read (unit = iunit, fmt = '(A)', iostat = iost) char_ned

   ! read formats
   read (unit=iunit, fmt = '(A,1X,A)') &
      fmt_name, fmt_info, &
      fmt_name, fmt_loc

   ! skip 1 line
   read (unit=iunit, fmt = '(A)') fmt_name

   ! loop over records
   reports: do
      ! read station general info
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

      select case(fm)
      case (125)    ;
         ! read surface wind speed and precipitable water
         read (unit=iunit, fmt = fmt_loc) speed, tpw
      case (126)    ;
         read (unit=iunit, fmt = fmt_loc) tb19v, tb19h, tb22v, tb37v, tb37h, tb85v, tb85h
      case default;
         write(unit=message(1), fmt='(a, i6)') 'unsaved ssmi obs found, fm=', fm
         write(unit=message(2), fmt='(a, 2f12.6)') 'info%(lon,lat)=', info%lon, info%lat
         call da_warning(__FILE__,__LINE__,message(1:2))
      end select

      ! check if obs is in horizontal domain

      ! Compute the model horizontal coordinate x, y
      ! Check if obs is wihin horizontal domain

      call da_llxy (info, loc, outside)

      select case(fm)
      case (125) ;
         if (.not. use_ssmiretrievalobs .or. iv%ntotal(ssmi_rv) == max_ssmi_rv_input) cycle reports

         ! Check if at least one field is present
         if ((tpw % qc == missing) .AND. (speed % qc == missing)) then
            cycle reports
         end if
         iv % ntotal(ssmi_rv) = iv % ntotal(ssmi_rv) + 1
         if (outside) cycle reports

         iv % nlocal(ssmi_rv) = iv % nlocal(ssmi_rv) + 1

      case (126) ;
         if (.not. use_ssmitbobs .or. iv%ntotal(ssmi_tb) == max_ssmi_tb_input) cycle reports

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

         iv % ntotal(ssmi_tb) = iv % ntotal(ssmi_tb) + 1
         if (outside) cycle reports
         iv % nlocal(ssmi_tb) = iv % nlocal(ssmi_tb) + 1

      case default;
         ! Do nothing.
      end select

   end do reports

   close(unit=iunit)
   call da_free_unit(iunit)

   if (irain > 0) then
      write(unit=stdout, fmt='(/,5x,a,i6/)') 'Rejected rain contaminated ssmi_tb =', irain
      write(unit=stdout, fmt='(A)') ' '
   end if

   iv%plocal(iv%time,:) = iv%nlocal(:)
   iv%ptotal(iv%time,:) = iv%ntotal(:)

   if (trace_use) call da_trace_exit("da_scan_obs_ssmi")

end subroutine da_scan_obs_ssmi


