SUBROUTINE  FM_DECODER (fm, platform, synop, ship,  metar, pilot, sound,   &
                                      satem, satob, airep, gpspw, gpsref,  &
                                      ssmi_retrieval, ssmi_tb, ssmt1, ssmt2, &
                                      qscat,profiler, buoy, other)
!---------------------------------------------------------------------------   
!  History:
!
!  Additions:
!             07/08/2003      Profiler and Buoy Obs            S. R. H. Rizvi
!             05/27/2004      GPSZD and GPSRF added.           Y.-R. Guo
!
!---------------------------------------------------------------------------   
! Given the WMO code fm, return the observation platform type and increment
! the corresponding counter if present.
!
! Returned platforms are reduced to 10 output classes:
!
!   Name    WMO Codes     WMO Code names
!   synop    12,14       'SYNOP','SYNOP MOBIL'
!   ship     13, 17      'SHIP'
!   metar    15,16       'METAR','SPECI'
!   pilot    32,33,34    'PILOT','PILOT SHIP','PILOT MOBIL'
!   sound    35,36,37,38 'TEMP','TEMP SHIP, 'TEMP DROP','TEMP MOBIL'
!   satem    86          'SATEM'
!   satob    88          'SATOB'
!   airep    42, 96,97   'AIREP', 'AMDAR'
!   gpspw    111         'GPSPW'
!   gpsztd   114         'GPSZD'
!   gpsref   116         'GPSRF'
!   ssmi_retrieval 125         'SSMI_RETRIEVAL'
!   ssmi_tb  126         'SSMI_TB'
!   SSM/T1   121         'SSMT1'
!   SSM/T2   122         'SSMT2'
!   QSCAT    281         'QSCAT'
!   Profiler 132         'PROFI'
!   Buoy     18,19       'BUOY '
!   other      Any other code
!------------------------------------------------------------------------------!

    IMPLICIT NONE
    INTEGER,              INTENT (in)    :: fm
    CHARACTER (LEN = 40), INTENT (out)   :: platform
    INTEGER,              INTENT (inout), OPTIONAL :: synop, ship,  metar, &
                                                      pilot, sound, satem, &
                                                      satob, airep, gpspw, &
                                                      ssmi_retrieval, ssmi_tb,&
                                                      ssmt1, ssmt2, qscat, &
                                                      profiler, buoy, gpsref, &
                                                      other

!------------------------------------------------------------------------------!

       SELECT CASE(fm)

          !  Report of surface observations from a fixed land station

          CASE (12) ; platform = 'SYNOP'
                      IF (PRESENT (synop)) synop = synop + 1

          !  Report of surface observations from a sea station

          CASE (13) ; platform = 'SHIP'
                      IF (PRESENT (ship)) ship  = ship + 1

          !  Report of surface observations from a mobile land station

          CASE (14) ; platform = 'SYNOP MOBIL'
                      IF (PRESENT (synop)) synop  = synop + 1

          !  Aviation routine weather report (with/without trend forecast)

          CASE (15) ; platform = 'METAR'
                      IF (PRESENT (metar)) metar  = metar + 1

          !  Aviation selected special weather report 
          !  (with/without trend forecast)

          CASE (16) ; platform = 'SPECI'
                      IF (PRESENT (metar)) metar  = metar + 1

          !  Report of buoy observation

          CASE (18,19) ; platform = 'BUOY'
                      IF (PRESENT (buoy)) buoy  = buoy + 1

          !  Report of ground radar weather observation

          CASE (20) ; platform = 'RADOB'
                      IF (PRESENT (other)) other  = other + 1

          !  Radiological data report (monitored on a routine basis and/or 
          !  in case of accident

          CASE (22) ; platform = 'RADREP'
                      IF (PRESENT (other)) other  = other + 1

          !  Upper-wind report from a fixed land station

          CASE (32) ; platform = 'PILOT'
                      IF (PRESENT (pilot)) pilot = pilot + 1

          !  Upper-wind report from a sea station

          CASE (33) ; platform = 'PILOT SHIP'
                      IF (PRESENT (pilot)) pilot = pilot + 1

          !  Upper-wind report from a mobile land station

          CASE (34) ; platform = 'PILOT MOBIL'
                      IF (PRESENT (pilot)) pilot = pilot + 1

          !  Upper-level pressure, temperature, humidity and wind report 
          !  from a fixed land station

          CASE (35) ; platform = 'TEMP'
                      IF (PRESENT (sound)) sound = sound + 1

          !  Upper-level pressure, temperature, humidity and wind report 
          !  from a sea station

          CASE (36) ; platform = 'TEMP SHIP'
                      IF (PRESENT (sound)) sound = sound + 1

          !  Upper-level pressure, temperature, humidity and wind report 
          !  from a sonde released by carrier balloons and aircraft

          CASE (37) ; platform = 'TEMP DROP'
                      IF (PRESENT (sound)) sound = sound + 1

          !  Upper-level pressure, temperature, humidity and wind report 
          !  from a mobile land station

          CASE (38) ; platform = 'TEMP MOBIL'
                      IF (PRESENT (sound)) sound = sound + 1

          !  Upper-level temperature, wind and air density from a land 
          !  rocketsonde station

          CASE (39) ; platform = 'ROCOB'
                      IF (PRESENT (other)) other = other + 1

          !  Upper-level temperature, wind and air density from a 
          !  rocketsonde station on a ship

          CASE (40) ; platform = 'ROCOB SHIP'
                      IF (PRESENT (other)) other = other + 1

          !  Upper-air report from an aircraft 
          !  (other than weather reconnaissance aircraft)

          CASE (41) ; platform = 'CODAR'
                      IF (PRESENT (other)) other = other + 1

          !  Aircraft report (aircraft meteorological data relay)

          CASE (42) ; platform = 'AMDAR'
                      IF (PRESENT (airep)) airep = airep + 1

          !  Ice analysis

          CASE (43) ; platform = 'ICEAN'
                      IF (PRESENT (other)) other = other + 1

          !  Analysis in full form

          CASE (45) ; platform = 'IAC'
                      IF (PRESENT (other)) other = other + 1

          !  Analysis in abbreviated form

          CASE (46) ; platform = 'IAC FLEET'
                      IF (PRESENT (other)) other = other + 1

          !  Processed data in form of grid-point values

          CASE (47) ; platform = 'GRID'
                      IF (PRESENT (other)) other = other + 1

          !  Processed data in form of grid-point values (abbreviated code form)

          CASE (49) ; platform = 'GRAF'
                      IF (PRESENT (other)) other = other + 1

          !  Forecast upper wind and temperature for aviation

          CASE (50) ; platform = 'WINTEM'
                      IF (PRESENT (other)) other = other + 1

          !  Aerodrome forecast

          CASE (51) ; platform = 'TAF'
                      IF (PRESENT (other)) other = other + 1

          !  Area forecast for aviation

          CASE (53) ; platform = 'ARFOR'
                      IF (PRESENT (other)) other = other + 1

          !  Route forecast for aviation

          CASE (54) ; platform = 'ROFOR'
                      IF (PRESENT (other)) other = other + 1

          !  Radiological trajectory dose forecast 
          !  (defined time of arrival and location)

          CASE (57) ; platform = 'RADOF'
                      IF (PRESENT (other)) other = other + 1

          !  Forecast for shipping

          CASE (61) ; platform = 'MAFOR'
                      IF (PRESENT (other)) other = other + 1

          !  Report of marine surface observations along a ship's track

          CASE (62) ; platform = 'TRACKOB'
                      IF (PRESENT (other)) other = other + 1

          !  Report of bathymetrical observation

          CASE (63) ; platform = 'BATHY'
                      IF (PRESENT (other)) other = other + 1

          !  Temperature salinity and current report from a sea station

          CASE (64) ; platform = 'TRESAC'
                      IF (PRESENT (other)) other = other + 1

          !  Report of spectral wave information from sea station or from
          !  a remote platform (airecraft or satellite)

          CASE (65) ; platform = 'WAVEOB'
                      IF (PRESENT (other)) other = other + 1

          !  Report of hydrological observations from a hydrological station

          CASE (66) ; platform = 'HYDRA'
                      IF (PRESENT (other)) other = other + 1

          !  Hydrological forecast

          CASE (67) ; platform = 'HYFOR'
                      IF (PRESENT (other)) other = other + 1

          !  Report of monthly values from a land station

          CASE (71) ; platform = 'CLIMAT'
                      IF (PRESENT (other)) other = other + 1

          !  Report of monthly means and total from an ocean weather station

          CASE (72) ; platform = 'CLIMAT SHIP'
                      IF (PRESENT (other)) other = other + 1

          !  Report of monthly means for an oceanic area

          CASE (73) ; platform = 'NACLI CLINP SPLCI CLISA INCLI'
                      IF (PRESENT (other)) other = other + 1

          !  Report of monthly aerological means from a land station

          CASE (75) ; platform = 'CLIMAT TEMP'
                      IF (PRESENT (other)) other = other + 1

          !  Report of monthly aerological means from an ocean weather station

          CASE (76) ; platform = 'CLIMAT TEMP SHIP'
                      IF (PRESENT (other)) other = other + 1

          !  Synoptic report of bearings of sources of atmospherics

          CASE (81) ; platform = 'SFAZI'
                      IF (PRESENT (other)) other = other + 1

          !  Synoptic report of the geographical location of sources of 
          !  atmopsherics

          CASE (82) ; platform = 'SFLOC'
                      IF (PRESENT (other)) other = other + 1

          !  Detailed report of the distribution of sources of atmospherics
          !  by bearings for any period up to including 24 hours

          CASE (83) ; platform = 'SFAZU'
                      IF (PRESENT (other)) other = other + 1

          !  Report of synoptic interpretation of cloud data obtained by a
          !  meteorlogical satellite

          CASE (85) ; platform = 'SAREP'
                      IF (PRESENT (other)) other = other + 1

          !  Report of satellite remote upper-air soundings of 
          !  pressure, temperature and humidity

          CASE (86) ; platform = 'SATEM'
                      IF (PRESENT (satem)) satem = satem + 1

          !  Report of satellite clear radiance observations

          CASE (87) ; platform = 'SARAD'
                      IF (PRESENT (other)) other = other + 1

          !  Report of satellite remote upper-air soundings of 
          !  pressure, temperature and humidity

          CASE (88) ; platform = 'SATOB'
                      IF (PRESENT (satob)) satob = satob + 1

          !  Airep reports (not a WMO report)

          CASE (96:97) ; platform = 'AIREP'
                         IF (PRESENT (airep)) airep = airep + 1

          !  GPS Precipitable Water Vapor or GPS Zineth Total Delay
          !  (not a WMO report)

          CASE (111) ; platform = 'GPSPW'
                      IF (PRESENT (gpspw)) gpspw = gpspw + 1
          CASE (114) ; platform = 'GPSZD'
                      IF (PRESENT (gpspw)) gpspw = gpspw + 1

          !  GPS Refractivity (not a WMO report)

          CASE (116) ; platform = 'GPSRF'
                      IF (PRESENT (gpsref)) gpsref = gpsref + 1

          CASE (121) ; platform = 'SSMT1'
                      IF (PRESENT (ssmt1)) ssmt1 = ssmt1 + 1

          CASE (122) ; platform = 'SSMT2'
                      IF (PRESENT (ssmt2)) ssmt2 = ssmt2 + 1

          CASE (125) ; platform = 'SSMI_RETRIEVAL'
                      IF (PRESENT (ssmi_retrieval)) ssmi_retrieval = ssmi_retrieval + 1

          CASE (126) ; platform = 'SSMI_TB'
                      IF (PRESENT (ssmi_tb)) ssmi_tb = ssmi_tb + 1

          CASE (281) ; platform = 'QSCAT'
                      IF (PRESENT (qscat)) qscat = qscat + 1

          CASE (132) ; platform = 'PROFI'
                      IF (PRESENT (profiler)) profiler = profiler + 1
          !  Others

          CASE DEFAULT;
                       platform = 'UNKNOWN'
                       IF (PRESENT (other)) other = other + 1

       END SELECT

       !  Reduce the platform name to one of the 10 classes

       SELECT CASE (TRIM (platform))

       CASE ('SYNOP','SYNOP MOBIL');                       platform = "SYNOP";
       CASE ('SHIP');                                      platform = "SHIP";
       CASE ('METAR','SPECI');                             platform = "METAR";
       CASE ('PILOT','PILOT SHIP','PILOT MOBIL');          platform = "PILOT";
       CASE ('TEMP','TEMP SHIP','TEMP DROP','TEMP MOBIL'); platform = "SOUND";
       CASE ('SATEM');                                     platform = "SATEM";
       CASE ('SATOB');                                     platform = "SATOB";
       CASE ('AIREP');                                     platform = "AIREP";
       CASE ('OTHER');                                     platform = "OTHER";
       CASE ('BUOY');                                      platform = "BUOY";
       CASE ('PROFI');                                     platform = "PROFI";
       CASE DEFAULT;                                       platform = "OTHER";

       END SELECT
     
END SUBROUTINE fm_decoder 
