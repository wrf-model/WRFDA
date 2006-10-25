subroutine da_final_write_filtered_obs( ounit )
!-----------------------------------------------------------------------------!
!  Purpose: Scans intermediate Filtered Obs files  
!           and writes the data part on ounit
!
! Original code:      10/21/2006            Syed RH Rizvi
! Update History:     
!                     
!-----------------------------------------------------------------------------!
   implicit none
   INTEGER,           intent(in) :: ounit          
!
   INTEGER                    :: i,k, iost,iunit, files, total_obs     
   TYPE (multi_level_type)    :: platform
   REAL                       :: height_error
   character*20               :: filename
!----------------------------------------------------------------------------!
   if (trace_use) CALL da_trace_entry("da_final_write_filtered_obs")
!----------------------------------------------------------------------------!
   call da_get_unit(iunit)
!  Loop over all data files
!--------------------------
file_number: DO files = 0, numb_procs-1

   write(UNIT=filename, FMT='(a,i3.3)') 'filtered_obs.',files      
   open(unit=iunit, file= trim(filename),form='formatted',iostat=iost)
   if(iost /= 0) &
   call da_error(__FILE__,__LINE__, (/"Canot open "//trim(filename)/))

!  LOOP OVER RECORDS
!  -----------------
   reports: DO
!     READ STATION GENERAL INFO
!     =============================
      READ (iunit, FMT = fmt_info, IOSTAT = iost) &
                   platform%info%platform,    &
                   platform%info%date_char,   &
                   platform%info%name,        &
                   platform%info%levels,      &
                   platform%info%lat,         &
                   platform%info%lon,         &
                   platform%info%elv,         &
                   platform%info%id
      IF (iost /= 0) THEN
          WRITE (0,'(/A,I9)') ' END OF OBS UNIT: ',iunit
          WRITE (0,'(/A,I9)') ' IOSTAT:          ',iost
         EXIT reports
      ENDIF
       write(ounit, FMT = fmt_info)           &
                   platform%info%platform,    &
                   platform%info%date_char,   &
                   platform%info%name,        &
                   platform%info%levels,      &
                   platform%info%lat,         &
                   platform%info%lon,         &
                   platform%info%elv,         &
                   platform%info%id

!  Read surface Info
!  -------------------
      READ (iunit, FMT = fmt_srfc) platform%loc%slp, platform%loc%pw
      write(ounit, FMT = fmt_srfc) platform%loc%slp, platform%loc%pw

! == levels < 1 and not GPSPW, go back to reports

      IF ((platform%info%levels < 1) .AND.            &
          (index(platform%info%platform, 'GPSPW') <= 0)) then
           cycle reports
      ENDIF

!     READ EACH LEVELS
!     ----------------
loop_level: DO i = 1, platform%info%levels

      platform%each (i) = each_level_type(missing_r, missing, -1.0, & ! height
                       field_type(missing_r, missing, missing_r), & ! u
                       field_type(missing_r, missing, missing_r), & ! v
                       field_type(missing_r, missing, missing_r), & ! p
                       field_type(missing_r, missing, missing_r), & ! t
                       field_type(missing_r, missing, missing_r), & ! q
                       field_type(missing_r, missing, missing_r), & ! rh
                       field_type(missing_r, missing, missing_r), & ! td
                       field_type(missing_r, missing, missing_r))  ! speed 

       READ (UNIT = iunit, FMT = TRIM (fmt_each)) &
              platform%each (i)%p,            &
              platform%each (i)%speed,        &
              platform%each (i)%v,            &
              platform%each (i)%height,       &
              platform%each (i)%height_qc,    &
              height_error,                   &
              platform%each (i)%t,            &
              platform%each (i)%td,           &
              platform%each (i)%rh

       write (UNIT = ounit, FMT = TRIM (fmt_each)) &
              platform%each (i)%p,            &
              platform%each (i)%speed,        &
              platform%each (i)%v,            &
              platform%each (i)%height,       &
              platform%each (i)%height_qc,    &
              height_error,                   &
              platform%each (i)%t,            &
              platform%each (i)%td,           &
              platform%each (i)%rh

      ENDDO loop_level

   ENDDO reports                  !  Loop over reports              
   close ( iunit )
   ENDDO file_number              !  Loop over all data files

   call da_free_unit (iunit)
   if (trace_use) CALL da_trace_exit("da_final_write_filtered_obs")
end subroutine da_final_write_filtered_obs 
