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
!   History of KMA multi Radar Obs., 28 Marchr 2006, Duk-Jin Won
!----------------------------------------------------------------------------------------!

   implicit none

   INTEGER,           INTENT (in)     :: iunit
   TYPE (xpose_type), INTENT(IN)      :: xp      ! Domain Decomposition vars.
   TYPE (ob_type),    INTENT (inout)  :: ob

   CHARACTER(*), INTENT(in), OPTIONAL :: filename
   CHARACTER (LEN = 120)              :: char_ned
   CHARACTER (LEN = 120)              :: char_total_Radar

   TYPE (Radar_multi_level_type)      :: platform


   INTEGER                            :: i, iost, nlevels, fm,n
   INTEGER                            :: num_Radar, total_Radar
   INTEGER                            :: n_dup, ndup
   INTEGER                            :: prf,max_Radar_input,tRad        !by djwon
   INTEGER, DIMENSION(:),allocatable  :: prf_num,stn_lon,stn_lat,stn_elv !by djwon

   LOGICAL                            :: inside_halo
   LOGICAL                            :: outside
! -------------------------------------------------------------------

   num_Radar = ob%num_Radar 

!----------------------------------------------------------------------------!
! 1. OPEN FILE
! ============
   PRINT*, 'READ Radar Data ------------------------------------------------'
   PRINT*, 'ob%Num_Radar Beg on Read Rad:',ob%num_Radar
!
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


   READ (char_total_Radar (22:31),'(I10)', IOSTAT = iost) total_Radar
   READ (char_total_Radar (32:41),'(I10)', IOSTAT = iost) max_Radar_input
   READ (char_total_Radar (42:51),'(F10.1)', IOSTAT = iost) ob%missing
   WRITE (0, '(A,2I10)') char_total_Radar (1:21),total_Radar,max_Radar_input
!-----------------------------------
! Defile Dimension
   ALLOCATE (prf_num(1:total_Radar))
   ALLOCATE (stn_lon(1:total_Radar))
   ALLOCATE (stn_lat(1:total_Radar))
   ALLOCATE (stn_elv(1:total_Radar))
!------------------------------------
   loop_radars: DO n = 1, total_Radar
!
! 2.2 READ FIRST LINE
!     ---------------
     READ (iunit, FMT = '(A)', IOSTAT = iost) char_ned
!
! 2.3 RED NUMBER OF REPORTS
!     ---------------------
     READ (char_ned (8:19), '(A12)', IOSTAT = iost) platform % stn % name
     READ (char_ned (20:27), '(F8.3)', IOSTAT = iost) platform % stn % lon
     READ (char_ned (30:37), '(F8.3)', IOSTAT = iost) platform % stn % lat
     READ (char_ned (40:47), '(F8.1)', IOSTAT = iost) platform % stn % elv
     READ (char_ned (50:68), '(A19)', IOSTAT = iost) platform % stn % date_char
     READ (char_ned (69:74), '(I6)', IOSTAT = iost) platform % stn % numObs
     READ (char_ned (75:80), '(I6)', IOSTAT = iost) platform % stn % levels
!
     prf_num(n) = platform % stn % numObs
     stn_lon(n) = platform % stn % lon
     stn_lat(n) = platform % stn % lat
     stn_elv(n) = platform % stn % elv
!
     WRITE (0,'(A,A12)') ' data at station:', platform % stn % name
     WRITE (0, '(A,2(F8.3,2X),F8.1)') &
                'The station longitude, latutude, and altitude are: ', &
                 platform % stn % lon, &
                 platform % stn % lat, platform % stn % elv
     WRITE (0, '(A,A19)') 'The observation time for this data is ',     &
                 platform % stn % date_char
     WRITE(0,'(A,2I6)') 'each prof num: ', n, prf_num(n)
     WRITE (0, '(A,I6)') 'Total read-in number of Super-observations is ', &
                 platform % stn % numObs
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
!
   loop_totrad: Do tRad = 1, total_Radar
     loop_profile: DO prf=1, prf_num(tRad)
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
!
      IF (iost /= 0) THEN
         WRITE (0,'(/,A,I5,/)') ' END OF UNIT: ',iunit
         WRITE (0,'(A,I5)')     ' IOSTAT == ',iost
         EXIT loop_profile
      ENDIF
!
      read(platform % info % platform (4:6), '(I3)') fm
      
!----------------------------------------------------------------------------!
! 1.0 READ EACH LEVEL OF RADAR DATA
!----------------------------------------------------------------------------!

       loop_level: DO i = 1, platform % info % levels

         platform%each(i)=Radar_each_level_type(missing_r,missing,-1.0, & ! height
                       field_type(missing_r, missing, missing_r),    & ! rv
                       field_type(missing_r, missing, missing_r))      ! rf

         READ(UNIT=iunit,FMT='(3X,F12.1,F12.3,I4,F12.3,2X,F12.3,I4,F12.3)') &
                          platform % each (i) % height,           &
                          platform % each (i) % rv % inv,         &
                          platform % each (i) % rv % qc,          &
                          platform % each (i) % rv % error,       &
                          platform % each (i) % rf % inv,         &
                          platform % each (i) % rf % qc,          &
                          platform % each (i) % rf % error
!
         if (platform % each(i) % rv % error == 0.0 ) then
             platform % each(i) % rv % error = 1.0  
         endif
         if (platform % each(i) % rf % error == 0.0 ) then
             platform % each(i) % rf % error = 1.0  
         endif
         if (platform % each(i) % rv % inv   == missing_r .or. &
             platform % each(i) % rv % error == missing_r ) then
             platform % each(i) % rv % qc    = missing_data
         endif
         if (platform % each(i) % rf % inv   == missing_r .or. &
             platform % each(i) % rf % error == missing_r ) then
             platform % each(i) % rf % qc    = missing_data
         endif

       ENDDO loop_level
!----------------------------------------------------------------------------!
!------------------------------------
!Check Area of Radar profile
!------------------------------------
      call DA_ll_to_xy (platform%info, platform%loc, xp, outside, inside_halo )
      IF(outside) THEN
        PRINT*, 'READ outside: ',tRad, prf
        cycle loop_profile
      ENDIF
!
       nlevels = platform % info % levels
!
       if( nlevels > max_ob_levels) then
         write(0,'(A,2I8)') &
         ' Radar=> nlevels > max_ob_levels:',nlevels, max_ob_levels
         nlevels = max_ob_levels
         platform%info%levels = nlevels
       else if( nlevels < 1) then
         cycle loop_profile
       endif
!
           num_Radar = num_Radar + 1
           PRINT*,'DJWON num_Radar:',num_Radar
! 
           ob % Radar ( num_Radar ) % stn_loc % lon = stn_lon(tRad) 
           ob % Radar ( num_Radar ) % stn_loc % lat = stn_lat(tRad)
           ob % Radar ( num_Radar ) % stn_loc % elv = stn_elv(tRad)
!
!           ob % Radar ( num_Radar ) % stn_loc % lon = platform % stn % lon
!           ob % Radar ( num_Radar ) % stn_loc % lat = platform % stn % lat
!           ob % Radar ( num_Radar ) % stn_loc % elv = platform % stn % elv
!
           ob % Radar ( num_Radar ) % info = platform % info
           ob % Radar ( num_Radar ) % loc  = platform % loc
!
           allocate (ob % Radar ( num_Radar ) % height   (1:nlevels))
           allocate (ob % Radar ( num_Radar ) % height_qc(1:nlevels))
           allocate (ob % Radar ( num_Radar ) % zk       (1:nlevels))
           allocate (ob % Radar ( num_Radar ) % rv       (1:nlevels))
           allocate (ob % Radar ( num_Radar ) % rf       (1:nlevels))
!
           do i = 1, nlevels
             ob % Radar(num_Radar) % height(i)    =  platform % each(i) % height
             ob % Radar(num_Radar) % height_qc(i) =  platform % each(i) % height_qc
             ob % Radar (num_Radar) % zk(i)        = platform % each(i) % zk
             ob % Radar (num_Radar) % rv(i)        = platform % each(i) % rv
             ob % Radar (num_Radar) % rf(i)        = platform % each(i) % rf
           enddo
!
!-------------------------------------------------------------------------------------
     ENDDO loop_profile

   ENDDO loop_totrad
    
   CLOSE(iunit)

   ob % num_Radar = num_Radar
!  =============
!  PRINT OUT
!  =============
   write(unit=*, fmt='(a)')  ' '
   write(unit=*, fmt='(5x,a,i10,a)') &
         'Checked in:  ', num_Radar, ' Radar SuperObs '

   PRINT*, 'ob%Num_Radar END OF Read Rad:',ob%num_Radar
!
END SUBROUTINE DA_Read_Radar
