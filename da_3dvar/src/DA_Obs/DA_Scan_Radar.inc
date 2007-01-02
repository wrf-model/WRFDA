SUBROUTINE DA_Scan_Radar (iunit, ob, xp, filename)

!-----------------------------------------------------------------------------!
!  Scan the Radar observation file
!  History: 13 Sept. 2004, Update,   Jianfeng Gu
!  History: Multi Radar Obs., 29 Sept. 2004,  Jianfeng Gu
!           Finalized on February 03, 2005, Qingnong Xiao
!
!-----------------------------------------------------------------------------!

   implicit none

   INTEGER,          INTENT (IN)  :: iunit
   TYPE (xpose_type), INTENT(IN)  :: xp       ! Domain decomposition vars.
   TYPE (ob_type), INTENT (inout) :: ob

   CHARACTER(*), INTENT(in), OPTIONAL :: filename

   INTEGER                       :: i, j, n, iost, nlevels, num_Radar, fm
   INTEGER                       :: total_Radar

   TYPE (Radar_multi_level_type) :: platform

   CHARACTER (LEN = 120)         :: char_total_Radar
   CHARACTER (LEN = 120)         :: char_ned

   LOGICAL                       :: outside, inside_halo
   INTEGER                        ::n_dup, ndup


! -------------------------------------------------------------------
!--default value.

   ob%ob_numb(ob%current_ob_time)%Radar = ob%num_Radar

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

  num_Radar = 0

!----------------------------------------------------------------------------!
! 2. READ TOTAL RADAR
! ===================

! 2.1 READ FIRST LINE
!     ---------------

   READ (iunit, FMT = '(A)', IOSTAT = iost) char_total_Radar

! 2.2 PROCESS ERROR
!     -------------

   IF (iost /= 0) THEN
       WRITE (0,'(/A,i3/)') ' ERROR READING FILE UNIT:', iunit
       STOP         ' in read_Radar_header.F'
   ENDIF

! 2.3 TOTAL RADAR NUMBER
!     ------------------

   READ (char_total_Radar (15:17), '(I3)', IOSTAT = iost) total_Radar

!  2.4 SKIP ONE LINES
!      --------------

   READ (iunit, FMT = '(A)', IOSTAT = iost)

!----------------------------------------------------------------------------!
! 3. READ RADAR DATA
! ==================

  loop_total_Radar: DO n = 1, total_Radar

!  3.1 SKIP ONE BLANK LINES
!  ------------------------

   READ (iunit, FMT = '(A)', IOSTAT = iost)

!  3.2 READ HEADER
!  ---------------

   READ (iunit, FMT = '(A)', IOSTAT = iost) char_ned

!  3.3 READ HEADER INFORMATION
!      -----------------------

   READ (char_ned (69:74), '(I6)', IOSTAT = iost) platform % stn % numObs

!----------------------------------------------------------------------------!
!  3.4 SKIP TWO LINES
!  ------------------

   READ (iunit, FMT = '(A)', IOSTAT = iost)
   READ (iunit, FMT = '(A)', IOSTAT = iost)

!----------------------------------------------------------------------------!
!  3.5 LOOP OVER RECORDS
!  ---------------------

   reports: DO j = 1, platform % stn % numObs

!     3.5.1 READ STATION GENERAL INFO
!     -------------------------------

      READ (UNIT = iunit, IOSTAT = iost, &
                   FMT = '(A12,3X,A19,2X,2(F12.3,2X),F8.1,2X,I6)' ) &
                   platform % info % platform,  &
                   platform % info % date_char, &
                   platform % info % lat,       &
                   platform % info % lon,       &
                   platform % info % elv,       &
                   platform % info % levels

      read(platform % info % platform (4:6), '(I3)') fm

      ob % total_obs = ob % total_obs + 1

!     3.5.2 READ EACH LEVELS
!     ----------------------

    loop_level: DO i = 1, platform % info % levels

      platform%each (i) = Radar_each_level_type(missing_r, missing, -1.0,       & ! height
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
!
   DO ndup = 1, n_dup


      SELECT CASE ( fm )
 
        CASE (128)
   
          num_Radar = num_Radar + 1

          if( num_Radar > max_Radar ) then
                write(0,'(A,I6,A,I6)') ' Radar #= ',ob % num_Radar, &
                          ' > max_Radar = ', max_Radar
                STOP
          endif

        CASE DEFAULT;

          write(unit=*, fmt='(a)') 'Warining: unsaved obs found:'

          write(unit=*, fmt='(2a)') &
               'platform % info % platform=', platform % info % platform

          write(unit=*, fmt='(a, i3)') &
               'platform % info % levels=', platform % info % levels

      END SELECT
    ENDDO        !  loop over duplicate

    ENDDO reports

  ENDDO loop_total_Radar

   close (iunit)

!----------------------------------------------------------------------------!
! 4. PRINT OUT
! ============
 
   if(print_detail > 0) write(unit=*, fmt='(/5x,a,i6,a)') &
         ' Read:  ', num_Radar, ' Radar reports '

   WRITE (0,'(/,A,I3,/)') ' END OF UNIT: ',iunit

   ob%num_Radar = ob%num_Radar + num_Radar

   ob%ob_numb(ob%current_ob_time)%Radar = ob%num_Radar

END SUBROUTINE DA_Scan_Radar

