SUBROUTINE DA_Scan_Radar (iunit, ob, xp, filename)

!-----------------------------------------------------------------------------!
!  Scan the Radar observation file
!  History: 13 Sept. 2004, Update,   Jianfeng Gu
!  History: Multi Radar Obs., 29 Sept. 2004,  Jianfeng Gu
!           Finalized on February 03, 2005, Qingnong Xiao
!  History: KMA multi Radar Obs., 28 Marchr 2006, Duk-Jin WON
!-----------------------------------------------------------------------------!

   implicit none

   INTEGER,        INTENT (in)        :: iunit
   TYPE (xpose_type), INTENT (in)     :: xp
   TYPE (ob_type), INTENT (out)       :: ob

   CHARACTER(*), INTENT(in), OPTIONAL :: filename
   TYPE (Radar_multi_level_type)      :: platform

   CHARACTER (LEN = 120)              :: char_total_Radar
   CHARACTER (LEN = 120)              :: char_ned
   LOGICAL                            :: outside, inside_halo

   INTEGER                            :: iost,i,n,num_Radar,fm, nlevels
   INTEGER                            :: n_dup, ndup
   INTEGER                            :: total_Radar,tRad, prf       !by djwon
   INTEGER,DIMENSION(:),ALLOCATABLE   :: prf_scn                     !by djwon

!-----------------------------------------------------------------------------!

   ob%ob_numb(ob%current_ob_time)%Radar = ob%num_Radar

!-----------------------------------------------------------------------------!
   WRITE (0,'(A)')  &
   'Scan Radar----------------------------------------------------------------'

! 1. OPEN FILE
!    ---------
   if(present(filename)) then
!
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
#ifndef crayx1
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
#endif
   endif
   
!----------------------------------------------------------------------------!

   num_Radar = 0

!----------------------------------------------------------------------------!
! 2.  READ HEADER
!----------------------------------------------------------------------------!

! 2.0 READ HEAD LINE   
!     --------------- 
                     
   READ (iunit, FMT = '(A)', IOSTAT = iost) char_total_Radar
                                                         
! 2.1 PROCESS ERROR                                     
!     -------------                                   
                                                       
   IF (iost /= 0) THEN                               
       WRITE (0,'(/A,i3/)') ' ERROR READING FILE UNIT:', iunit      
       STOP         ' in read_Radar_header.F'                      
   ENDIF                                                          
                                                                 
                                                                
   READ (char_total_Radar (29:31),'(I3)', IOSTAT = iost) total_Radar 
   WRITE (0, '(A,I10)') char_total_Radar (1:21),total_Radar
   ALLOCATE ( prf_scn(1:total_Radar) )
                                                             
   loop_radars: DO n = 1, total_Radar                            

! 2.2 READ FIRST LINE
!     ---------------
     READ (iunit, FMT = '(A)', IOSTAT = iost) char_ned
!
! 2.3 RED NUMBER OF REPORTS
!     ---------------------
     READ (char_ned (50:68), '(A19)', IOSTAT = iost) platform % stn % date_char
     WRITE (0, '(A,A19)') 'The observation time for this data is ',     &
                        platform % stn % date_char
     READ (char_ned (69:74), '(I6)', IOSTAT = iost) platform % stn % numObs
     WRITE (0, '(A,I6)') 'Total read-in number of Super-observations is ', &
                        platform % stn % numObs
            prf_scn(n) = platform % stn % numObs
     READ (char_ned (75:80), '(I6)', IOSTAT = iost) platform % stn % levels
     WRITE (0, '(A,I6)') 'Vertical layers for each Super-observation is ', &
                        platform % stn % levels
   ENDDO loop_radars                                      
!----------------------------------------------------------------------------!

   READ (iunit, FMT = '(A)', IOSTAT = iost)           
   READ (iunit, FMT = '(A)', IOSTAT = iost)          

!----------------------------------------------------------------------------!
!  READ FORMATS
!  ------------

!  LOOP OVER RECORDS
!  -----------------
   
  loop_totrad: DO tRad = 1, total_Radar
     loop_profile: DO prf=1, prf_scn(tRad)
!     =============================
!     READ RADAR STATION GENERAL INFO
!     =============================
       READ (UNIT = iunit, IOSTAT = iost, &
                   FMT = '(A12,3X,A19,2X,2(F12.3,2X),F8.1,2X,I6)' ) &
                   platform % info % platform,  &
                   platform % info % date_char, &
                   platform % info % lat,       &
                   platform % info % lon,       &
                   platform % info % elv,       &
                   platform % info % levels

       IF (iost /= 0) THEN
         WRITE (0,'(/,A,I5,/)') ' END OF UNIT: ',iunit
         WRITE (0,'(A,I5)')     ' IOSTAT == ',iost
         EXIT loop_profile
       ENDIF
!
       read(platform % info % platform (4:6), '(I3)') fm
!
       ob%total_obs = ob%total_obs + 1
       WRITE (0,'(A,4I5,I10)') 'total_obs: ',tRad,prf,ob%total_obs,fm,prf_scn(tRad)
!----------------------------------------------------------------------------!
! 1.0 READ EACH LEVEL OF RADAR DATA
!----------------------------------------------------------------------------!

       loop_level: DO i = 1, platform % info % levels

         platform%each(i) = Radar_each_level_type(missing_r,missing,-1.0, & ! height
                          field_type(missing_r, missing, missing_r),    & ! rv
                          field_type(missing_r, missing, missing_r))      ! rf

         READ (UNIT = iunit, FMT = '(3X,F12.1,F12.3,I4,F12.3,2X,F12.3,I4,F12.3)') &
                          platform % each (i) % height,           &
                          platform % each (i) % rv % inv,         &
                          platform % each (i) % rv % qc,          &
                          platform % each (i) % rv % error,       &
                          platform % each (i) % rf % inv,         &
                          platform % each (i) % rf % qc,          &
                          platform % each (i) % rf % error

       ENDDO loop_level
!----------------------------------------------------------------------------!
!------------------------------------
!Check Area of Radar profile
!------------------------------------
      call DA_ll_to_xy (platform%info, platform%loc, xp, outside, inside_halo )
!dj      IF(outside) THEN
!dj        WRITE(0,'(a,2I5)') 'outside: ',tRad, prf
!dj        cycle loop_profile
!dj      ENDIF
!
       nlevels = platform%info%levels

       if( nlevels > max_ob_levels) then
          write(0,'(A,2I8)') &
          ' Radar=> nlevels > max_ob_levels:',nlevels, max_ob_levels
          nlevels = max_ob_levels
          platform%info%levels = nlevels
       else if( nlevels < 1) then
          cycle loop_profile
       endif

!    Loop over duplicating obs for global  ??
       n_dup = 1
       if(global .and. &
         (platform%loc%i == xp%ids .or. platform%loc%i == xp%ide))n_dup= 2

       DO ndup = 1, n_dup

         SELECT CASE ( fm )
           CASE (128)
!
           num_Radar = num_Radar + 1
!
           if( num_Radar > max_Radar ) then
              write(0,'(A,I6,A,I6)') 'Radar #= ',num_Radar, &
                      ' > max_Radar = ', max_Radar
              STOP
           endif
 
           CASE DEFAULT ;
           write(unit=*, fmt='(a)') 'Warining: unsaved obs found:'
           write(unit=*, fmt='(2a)') &
                   'platform % info % platform=', platform % info % platform
           write(unit=*, fmt='(a, i3)') &
                   'platform % info % levels=', platform % info % levels
         END SELECT

        ENDDO    ! loop over duplicate  ??

       ENDDO loop_profile

     ENDDO loop_totrad

     CLOSE (iunit)

!----------------------------------------------------------------------------!
! 4. PRINT OUT
! ============

   if(print_detail > 0) write(unit=*, fmt='(/5x,a,i6,a)') &
         ' Scan:  ', num_Radar, ' Radar reports '

   ob%num_Radar = ob%num_Radar + num_Radar
   ob%ob_numb(ob%current_ob_time)%Radar = ob%num_Radar

   WRITE (0,'(A,I3)') ' NUM OF RADAR: ',ob%num_Radar

   WRITE (0,'(/,A,I3,/)') ' END OF UNIT: ',iunit
END SUBROUTINE DA_Scan_Radar
