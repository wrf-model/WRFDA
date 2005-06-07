SUBROUTINE DA_Read_Radar (iunit, ob, xp, filename)

!----------------------------------------------------------------------------------------!
! Read the Radar observation file
! Written by Qingnong Xiao, September 2002
!   History of the radial velocity with respect to 
!              the WRF space, April 2004, Jianfeng Gu
!   History of the reflectivity with respect to 
!              the WRF space, July 2004, Jianfeng Gu
!   History of multi Radar Obs., 29 September 2004, Jianfeng Gu
!              Finalized on February 03, 2005, Qingnong Xiao
!----------------------------------------------------------------------------------------!

   implicit none

   INTEGER,          INTENT (IN)   :: iunit
   TYPE (xpose_type), INTENT(IN)   :: xp    ! Domain decomposition vars.
   TYPE (ob_type), INTENT (inout)  :: ob

   CHARACTER(*), INTENT(in), OPTIONAL :: filename

   CHARACTER (LEN = 120)         :: char_total_Radar
   CHARACTER (LEN = 120)         :: char_ned

   INTEGER                       :: i, j, n, iost, nlevels, fm

   TYPE (Radar_multi_level_type) :: platform

   LOGICAL                       :: inside_halo
   LOGICAL                       :: outside

   INTEGER                       :: total_obs, num_Radar, total_Radar
   INTEGER                        ::n_dup, ndup

! -------------------------------------------------------------------

   total_obs = ob%total_obs
   num_Radar = ob%num_Radar

!----------------------------------------------------------------------------!
! 1. OPEN FILE
! ============

   if(present(filename)) then
      OPEN(UNIT   = iunit,     &
           FILE   = trim(filename), &
           FORM   = 'FORMATTED',  &
           ACCESS = 'SEQUENTIAL', &
           IOSTAT =  iost,     &
           STATUS = 'OLD')

      IF (iost /= 0) THEN
         write(unit=*, fmt='(/A,I3,3(2X,A)/)') &
              'ERROR IN OBS INPUT FILE UNIT ',iunit, &
              'OBS FILENAME:', trim(filename), &
              'FOR RADAR OBSERVATIONS CANNOT BE FOUND OR CANNOT BE OPENED'
         RETURN
      ENDIF
   else
      OPEN(UNIT   = iunit,     &
           FORM   = 'FORMATTED',  &
           ACCESS = 'SEQUENTIAL', &
           IOSTAT =  iost,     &
           STATUS = 'OLD')

      IF (iost /= 0) THEN
         write(unit=*, fmt='(/A,I3,2X,A/)') &
              'ERROR IN INPUT FILE UNIT ',iunit, &
              'FOR RADAR OBSERVATIONS CANNOT BE FOUND OR CANNOT BE OPENED'
         RETURN
      ENDIF
   endif

!----------------------------------------------------------------------------!
! 2. READ TOTAL RADAR
! ===================

!  2.1 READ FIRST LINE
!      ---------------

   READ (iunit, FMT = '(A)', IOSTAT = iost) char_total_Radar

!  2.2 PROCESS ERROR
!      -------------

   IF (iost /= 0) THEN
       WRITE (0,'(/A,i3/)') ' ERROR READING FILE UNIT:', iunit
       STOP         ' in read_Radar_header.F'
   ENDIF

!  2.3 TOTAL RADAR NUMBER
!      ------------------

   READ (char_total_Radar (15:17), '(I3)', IOSTAT = iost) total_Radar

   if(print_detail > 0) WRITE (0,'(/,A,I3,/)') ' TOTAL RADAR: ', total_Radar

!  2.4 SKIP ONE LINE
!      -------------

   READ (iunit, FMT = '(A)', IOSTAT = iost)

!----------------------------------------------------------------------------!
! 3. READ RADAR DATA
! ==================

  loop_total_Radar: DO n = 1, total_Radar

!  3.1 SKIP ONE BLANK LINE
!  -----------------------

   READ (iunit, FMT = '(A)', IOSTAT = iost)

! 3.2 READ HEADER
!     -----------

   READ (iunit, FMT = '(A)', IOSTAT = iost) char_ned

! 3.3 READ HEADER INFORMATION
!     -----------------------

   READ (char_ned (1:5),'(A5)', IOSTAT = iost) platform % stn % platform

   if(print_detail > 0) WRITE (0, '(A)') 'RADAR Observations information'

   READ (char_ned (8:19), '(A12)', IOSTAT = iost) platform % stn % name

   if(print_detail > 0) WRITE (0,'(A,A5,A,A12)')  &
                        ' Expect: ',platform % stn % platform, &
                        'data at station:', platform % stn % name

   READ (char_ned (20:27), '(F8.3)', IOSTAT = iost) platform % stn % lon

   READ (char_ned (30:37), '(F8.3)', IOSTAT = iost) platform % stn % lat

   READ (char_ned (40:47), '(F8.1)', IOSTAT = iost) platform % stn % elv

   if(print_detail > 0) WRITE (0, '(A,2(F8.3,2X),F8.1)')  &
                        'The station longitude, latutude, and altitude are: ', &
                         platform % stn % lon, &
                         platform % stn % lat, platform % stn % elv

   READ (char_ned (50:68), '(A19)', IOSTAT = iost) platform % stn % date_char

   if(print_detail > 0) WRITE (0, '(A,A19)')   &
                        'The observation time for this data is ',     &
                        platform % stn % date_char

   READ (char_ned (69:74), '(I6)', IOSTAT = iost) platform % stn % numObs

   if(print_detail > 0) WRITE (0, '(A,I6)')   &
                        'Total number of Super-observations is ', &
                        platform % stn % numObs

   READ (char_ned (75:80), '(I6)', IOSTAT = iost) platform % stn % levels

   if(print_detail > 0) WRITE (0, '(A,I6)')   &
                        'Vertical layers for each Super-observation is ', &
                        platform % stn % levels

   WRITE (0,'(A)')  &
   '--------------------------------------------------------------------------'

!----------------------------------------------------------------------------!
!  3.4 SKIP TWO LINES
!      --------------

   READ (iunit, FMT = '(A)', IOSTAT = iost)
   READ (iunit, FMT = '(A)', IOSTAT = iost)

!----------------------------------------------------------------------------!
!  3.5 LOOP OVER RECORDS
!  --------------------

   reports: DO j = 1, platform % stn % numObs

!    3.5.1 READ STATION GENERAL INFO
!           -------------------------

      READ (UNIT = iunit, IOSTAT = iost, &
                   FMT = '(A12,3X,A19,2X,2(F12.3,2X),F8.1,2X,I6)' ) &
                   platform % info % platform,  &
                   platform % info % date_char, &
                   platform % info % lat,       &
                   platform % info % lon,       &
                   platform % info % elv,       &
                   platform % info % levels

      read(platform % info % platform (4:6), '(I3)') fm

      total_obs = total_obs + 1


!    3.5.2 READ EACH LEVEL
!          ---------------

    loop_level: DO i = 1, platform % info % levels

      platform%each(i) = Radar_each_level_type(missing_r, missing, -1.0,       & ! height
                                    field_type(missing_r, missing, missing_r), & ! rv
                                    field_type(missing_r, missing, missing_r))   ! rf

      READ (UNIT = iunit, FMT = '( 3X, F12.1, 2(F12.3,I4,F12.3,2X) )' ) &
                          platform % each (i) % height,           &
                          platform % each (i) % rv % inv,         &
                          platform % each (i) % rv % qc,          &
                          platform % each (i) % rv % error,       &
                          platform % each (i) % rf % inv,         &
                          platform % each (i) % rf % qc,          &
                          platform % each (i) % rf % error

         if ( platform % each (i) % rv % error == 0.0 ) then
              platform % each (i) % rv % error  = 1.0
         endif

         if ( platform % each (i) % rf % error == 0.0 ) then
              platform % each (i) % rf % error  = 1.0
         endif

         if (platform % each (i) % rv % inv   == missing_r .or. &
             platform % each (i) % rv % error == missing_r) then
             platform % each (i) % rv % qc     = missing_data
         end if

         if (platform % each (i) % rf % inv   == missing_r .or. &
             platform % each (i) % rf % error == missing_r) then
             platform % each (i) % rf % qc     = missing_data
         end if

    ENDDO loop_level

    call DA_ll_to_xy (platform%info, platform%loc, xp, outside, inside_halo )

    IF(outside) THEN
      cycle reports
    ENDIF

    nlevels = platform%info%levels

    if( nlevels > max_ob_levels) then

       write(0,'(A,2I8)') &
       ' Radar=> nlevels > max_ob_levels:',nlevels, max_ob_levels
    
       nlevels = max_ob_levels
       platform%info%levels = nlevels

    else if( nlevels < 1) then

       cycle reports

    endif

!    Loop over duplicating obs for global
   n_dup = 1
   if(global .and. &
     (platform%loc%i == xp%ids .or. platform%loc%i == xp%ide))n_dup= 2
   DO ndup = 1, n_dup

       SELECT CASE ( fm )
 
          CASE (128)

          num_Radar = num_Radar + 1
   
          if( num_Radar > max_Radar ) then
                write(0,'(A,I6,A,I6)') 'Radar #= ',ob % num_Radar, &
                          ' > max_Radar = ', max_Radar
                STOP
          endif

          ob % Radar ( num_Radar ) % stn_loc % lon = platform % stn % lon
          ob % Radar ( num_Radar ) % stn_loc % lat = platform % stn % lat
          ob % Radar ( num_Radar ) % stn_loc % elv = platform % stn % elv

          ob % Radar ( num_Radar ) % info = platform % info
          ob % Radar ( num_Radar ) % loc  = platform % loc
 
          allocate (ob % Radar ( num_Radar ) % height   (1:nlevels))
          allocate (ob % Radar ( num_Radar ) % height_qc(1:nlevels))
          allocate (ob % Radar ( num_Radar ) % zk       (1:nlevels))
          allocate (ob % Radar ( num_Radar ) % rv       (1:nlevels))
          allocate (ob % Radar ( num_Radar ) % rf       (1:nlevels))

          do i = 1, nlevels
    
            ob % Radar ( num_Radar ) % height(i)    = platform % each(i) % height
            ob % Radar ( num_Radar ) % height_qc(i) = platform % each(i) % height_qc
            ob % Radar ( num_Radar ) % zk(i)        = platform % each(i) % zk
            ob % Radar ( num_Radar ) % rv(i)        = platform % each(i) % rv
            ob % Radar ( num_Radar ) % rf(i)        = platform % each(i) % rf

          enddo

          CASE DEFAULT;

          write(unit=*, fmt='(a)') 'Warining: unsaved obs found:'

          write(unit=*, fmt='(2a)') &
               'platform % info % platform=', platform % info % platform

          write(unit=*, fmt='(a, i3)') &
               'platform % info % levels=', platform % info % levels

       END SELECT

      if( global .and. ndup == 1 ) then
      if( platform%loc % i >= xp % ide) then
      platform%loc%i = xp % ids
      platform%loc%proc_domain = .FALSE.
      else if( platform%loc % i <= xp % ids) then
      platform%loc%i = xp % ide
      platform%loc%proc_domain = .FALSE.
      end if
     end if

    ENDDO        !  loop over duplicate



   ENDDO reports

  ENDDO loop_total_Radar

!----------------------------------------------------------------------------!

   ob % num_Radar = num_Radar

!----------------------------------------------------------------------------!
! 4. PRINT OUT
! ============
 
  if(print_detail > 0) then

    write(unit=*, fmt='(a)')  ' '

    write(unit=*, fmt='(5x,a,i6,a)') &
         ' Read:  ', ob % num_Radar, ' Radar reports '

  endif

    WRITE (0,'(/,A,I3,/)') ' END OF UNIT: ',iunit

    if(ob%ob_numb(ob%current_ob_time)%Radar /= ob%num_Radar) then
      write(unit=*, fmt='(a, i6, 2x, a, i6)') &
           'ob%ob_numb(ob%current_ob_time)%Radar=', ob%ob_numb(ob%current_ob_time)%Radar, &
           'ob%num_Radar=', ob%num_Radar
      stop 'Radar Obs Mismatch.'
    endif
!----------------------------------------------------------------------------!

END SUBROUTINE DA_Read_Radar

