!
Module rttov_const
  ! Description:
  ! Definition of all parameters (constants) for RTTOV
  !
  ! Copyright:
  !    This software was developed within the context of
  !    the EUMETSAT Satellite Application Facility on
  !    Numerical Weather Prediction (NWP SAF), under the
  !    Cooperation Agreement dated 25 November 1998, between
  !    EUMETSAT and the Met Office, UK, by one or more partners
  !    within the NWP SAF. The partners in the NWP SAF are
  !    the Met Office, ECMWF, KNMI and MeteoFrance.
  !
  !    Copyright 2002, EUMETSAT, All Rights Reserved.
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0   01/12/2002  New F90 code with structures (P Brunel A Smith)
  !  1.1   29/01/2003  New platforms and instruments (P Brunel)
  !                    Hard limits for input profiles
  !  1.2   19/02/2003  Some changes to limits and comments (R Saunders)
  !  1.3   06/05/2003  Change version number to 7.3.1
  !                    and add references for physical constants (P Brunel)
  !  1.4      08/2003  Added variables for MW scattering (F Chevallier)
  !  1.5   18/09/2003  Added coefficients for cloud absorption properties (P Francis)
  !  1.6   15/10/2003  Added new sections in parameter files for scatt   (F Chevallier)
  !  1.7   23/11/2003  Added new definitions of polarisations 2.1 (S English)
  !

  !1.1 general
  !-----------
  ! Version number of the current code
  Use parkind1, Only : jpim     ,jprb
  Implicit None
  Integer(Kind=jpim), Parameter :: version = 8
  Integer(Kind=jpim), Parameter :: release = 5
  Integer(Kind=jpim), Parameter :: minor_version = 0

  Integer(Kind=jpim), Parameter :: version_compatible_min = 7 ! minimum version number
  Integer(Kind=jpim), Parameter :: version_compatible_max = 8 ! maximum version number
          ! compatible for coefficients.
          ! coef files with "id_comp_lvl" outside range will be rejected

  Character (len=16), Parameter :: rttov_magic_string = '%RTTOV_COEFF    '
  Real(Kind=jprb),               Parameter :: rttov_magic_number = 1.2345E+12_JPRB

  Integer(Kind=jpim), Parameter :: default_err_unit = 0  ! standard error unit number
                              ! standard error unit number is 7 for HPUX


  !1.2 physical constants
  !----------------------
  ! Molecular weights  (g/mole) are calculated by adding NIST Standard Atomic Weights
  ! Molecular weight of dry air refers to US standard atmosphere 1976
  ! NIST  Standard Atomic Weight are:
  ! H    1.00794   (7)
  ! C   12.0107    (8)
  ! N   14.0067    (2)
  ! O   15.9994    (3)
  Real(Kind=jprb), Parameter :: mair = 28.9644_JPRB
  Real(Kind=jprb), Parameter :: mh2o = 18.01528_JPRB
  Real(Kind=jprb), Parameter :: mo3  = 47.9982_JPRB
  Real(Kind=jprb), Parameter :: mco2 = 44.0095_JPRB
  Real(Kind=jprb), Parameter :: mch4 = 16.04246_JPRB
  Real(Kind=jprb), Parameter :: mn2o = 44.0128_JPRB
  Real(Kind=jprb), Parameter :: mco  = 28.0101_JPRB

  ! Gravity from NIST 9.80665 ms-1 (exact)
  Real(Kind=jprb), Parameter :: gravity = 9.80665_JPRB

  !
  ! Kaye & Laby latest library edition is 16e 1995, and gives
  ! * standard value  g = 9.80665 ms-1 exactly (p.191)
  ! * earth mean radius r= 6371.00 km (p191)
  !    [defined as [(r_equator)^2 (r_pole)]^1/3]
  Real(Kind=jprb), Parameter :: pi      = 3.1415926535_JPRB
  Real(Kind=jprb), Parameter :: deg2rad = pi/180.0_JPRB
  Real(Kind=jprb), Parameter :: earthradius = 6371.00_JPRB

  ! The Cosmic Microwave Background Spectrum from the Full COBE FIRAS Data Set
  ! Fixsen D.J. et all
  ! Astrophysical Journal v.473, p.576 December 1996
  ! CMBR = 2.728 +- 0.004K
  Real(Kind=jprb), Parameter :: tcosmic     = 2.728_JPRB
  !  Real(Kind=jprb), Parameter :: tcosmic     = 0.1_JPRB !used for ECMWF tests

  ! Universal gas constant R = 8.314510 J/mol/K
  Real(Kind=jprb), Parameter :: rgp = 8.314510_JPRB

  ! mean molar mass of dry air rm = 0.0289644 kg.mol^-1
  Real(Kind=jprb), Parameter :: rm = 0.0289644_JPRB

  !1.3 satellite and instrument information
  !----------------------------------------

  !platform id codes
  Integer(Kind=jpim), Parameter :: nplatforms = 20
  Integer(Kind=jpim), Parameter :: &
       platform_id_noaa     = 1, &
       platform_id_dmsp     = 2, &
       platform_id_meteosat = 3, &
       platform_id_goes     = 4, &
       platform_id_gms      = 5, &
       platform_id_fy2      = 6, &
       platform_id_trmm     = 7, &
       platform_id_ers      = 8, &
       platform_id_eos      = 9, &
       platform_id_metop    = 10, &
       platform_id_envisat  = 11, &
       platform_id_msg      = 12, &
       platform_id_fy1      = 13, &
       platform_id_adeos    = 14, &
       platform_id_mtsat    = 15, &
       platform_id_coriolis = 16, &
       platform_id_npoess   = 17, &
       platform_id_gifts    = 18

  !platform names
  Character (len=8), Parameter :: platform_name(nplatforms) = &
       (/ 'noaa    ', 'dmsp    ', 'meteosat', 'goes    ', 'gms     ', &
          'fy2     ', 'trmm    ', 'ers     ', 'eos     ', 'metop   ', &
          'envisat ', 'msg     ', 'fy1     ', 'adeos   ', 'mtsat   ', &
          'coriolis', 'npoess  ', 'gifts   ', 'xxxxxxxx', 'xxxxxxxx'/)

  !instrument id codes
  Integer(Kind=jpim), Parameter :: &
       inst_id_hirs   =  0, &
       inst_id_msu    =  1, &
       inst_id_ssu    =  2, &
       inst_id_amsua  =  3, &
       inst_id_amsub  =  4, &
       inst_id_avhrr  =  5, &
       inst_id_ssmi   =  6, &
       inst_id_vtpr1  =  7, &
       inst_id_vtpr2  =  8, &
       inst_id_tmi    =  9, &
       inst_id_ssmis  = 10, &
       inst_id_airs   = 11, &
       inst_id_hsb    = 12, &
       inst_id_modis  = 13, &
       inst_id_atsr   = 14, &
       inst_id_mhs    = 15, &
       inst_id_iasi   = 16, &
       inst_id_amsr   = 17, &
       inst_id_mtsatim= 18, &
       inst_id_atms   = 19, &
       inst_id_mviri  = 20, &
       inst_id_seviri = 21, &
       inst_id_goesim = 22, &
       inst_id_goessd = 23, &
       inst_id_gmsim  = 24, &
       inst_id_vissr  = 25, &
       inst_id_mvisr  = 26, &
       inst_id_cris   = 27, &
       inst_id_cmis   = 28, &
       inst_id_viirs  = 29, &
       inst_id_windsat= 30, &
       inst_id_gifts  = 31


  Integer(Kind=jpim), Parameter :: ninst = 35
  ! List of instruments  !!!! HIRS is number 0
  Character (len=8), Dimension(0:ninst-1) :: inst_name  =                &
       & (/ 'hirs    ', 'msu     ', 'ssu     ', 'amsua   ', 'amsub   ',  &
       &    'avhrr   ', 'ssmi    ', 'vtpr1   ', 'vtpr2   ', 'tmi     ',  &
       &    'ssmis   ', 'airs    ', 'hsb     ', 'modis   ', 'atsr    ',  &
       &    'mhs     ', 'iasi    ', 'amsr    ', 'imager  ', 'atms    ',  &
       &    'mviri   ', 'seviri  ', 'imager  ', 'sounder ', 'imager  ',  &
       &    'vissr   ', 'mvisr   ', 'cris    ', 'cmis    ', 'viirs   ',  &
       &    'windsat ', 'gifts   ', 'xxxxxxxx', 'xxxxxxxx', 'xxxxxxxx'   /)


  !1.4 Coefficient file Section names
  !----------------------------------
  Integer(Kind=jpim), Parameter :: nsections = 19
  Character(len=21), Parameter :: section_types(nsections) = &
    (/ 'IDENTIFICATION       ', 'LINE-BY-LINE         ', &
       'FAST_MODEL_VARIABLES ', 'FILTER_FUNCTIONS     ', &
       'FUNDAMENTAL_CONSTANTS', 'SSIREM               ', &
       'FASTEM               ', 'REFERENCE_PROFILE    ', &
       'PROFILE_LIMITS       ', 'FAST_COEFFICIENTS    ', &
       'COEF_SUB_FILES       ', 'GAZ_UNITS            ', &
       'DIMENSIONS           ', 'FREQUENCIES          ', &
       'HYDROMETEOR          ', 'CONVERSIONS          ', &
       'EXTINCTION           ', 'ALBEDO               ', &
       'ASYMMETRY            ' /)

  !sensors id codes
  Integer(Kind=jpim), Parameter :: nsensors = 3
  Integer(Kind=jpim), Parameter :: &
       sensor_id_ir     = 1, &
       sensor_id_mw     = 2, &
       sensor_id_hi     = 3

  !sensors names
  Character (len=2), Parameter :: sensor_name(nsensors) = &
       (/ 'ir', 'mw', 'hi' /)

  !gas id codes
  Integer(Kind=jpim), Parameter :: ngases_max = 8
  Integer(Kind=jpim), Parameter :: &
       & gas_id_mixed       = 1, &
       & gas_id_watervapour = 2, &
       & gas_id_ozone       = 3, &
       & gas_id_wvcont      = 4, &
       & gas_id_co2         = 5, &
       & gas_id_n2o         = 6, &
       & gas_id_co          = 7, &
       & gas_id_ch4         = 8

  !gas names
  Character (len=12), Parameter :: gas_name(ngases_max) = &
       & (/ 'Mixed_gases ', &
       &    'Water_vapour', &
       &    'Ozone       ', &
       &    'WV_Continuum', &
       &    'CO2         ', &
       &    'N2O         ', &
       &    'CO          ', &
       &    'CH4         ' /)

  !gas units
  Integer(Kind=jpim), Parameter :: ngases_unit = 2
  Integer(Kind=jpim), Parameter :: &
       & gas_unit_specconc  = 1, &
       & gas_unit_ppmv      = 2
  Character (len=12), Parameter :: gas_unit_name(ngases_unit) = &
       & (/ 'spec. concen', &
       &    'ppmv        '  /)


  !1.5 error reporting
  !-------------------
  !error status values
  Integer(Kind=jpim), Parameter :: nerrorstatus = 3
  Integer(Kind=jpim), Parameter :: errorstatus_success = 0
  Integer(Kind=jpim), Parameter :: errorstatus_warning = 1
  Integer(Kind=jpim), Parameter :: errorstatus_fatal   = 2
  Integer(Kind=jpim), Parameter :: errorstatus_info    = 3
  Character(len=*), Parameter :: errorstatus_text(0:nerrorstatus) = &
       (/ 'success', &
       'warning', &
       'fatal  ', &
       'info   '  /)


  !1.6 surface types
  !-----------------
  Integer(Kind=jpim), Parameter :: nsurftype = 2
  Integer(Kind=jpim), Parameter :: surftype_land = 0
  Integer(Kind=jpim), Parameter :: surftype_sea = 1
  Integer(Kind=jpim), Parameter :: surftype_seaice = 2


  !1.7 cloud emissivity
  !---------------------
  Integer(Kind=jpim), Parameter :: overlap_scheme = 2    ! overlap scheme
  ! 1 => Geleyn and Hollingsworth (1979)
  ! 2 => Raisanen (1998)

  !
  ! Water cloud coefficients
  ! from Hu and Stamnes, 1993, J. Climate, Vol. 6, pp. 728-742
  !
  Integer(Kind=jpim), Parameter  :: nvalhusta = 53   ! No. of wavelengths for tabulated Hu & Stamnes (droplet) data
  !
  Real(Kind=jprb),    Parameter  :: zhustaom(nvalhusta) = &
       &  (/ 3819.71_JPRB, 3179.65_JPRB, 2710.03_JPRB, 2564.10_JPRB, 2439.02_JPRB, &
       &     2325.58_JPRB, 2222.22_JPRB, 2127.66_JPRB, 2040.82_JPRB, 1960.78_JPRB, &
       &     1886.79_JPRB, 1851.85_JPRB, 1818.18_JPRB, 1754.39_JPRB, 1694.92_JPRB, &
       &     1666.67_JPRB, 1639.34_JPRB, 1612.90_JPRB, 1587.30_JPRB, 1538.46_JPRB, &
       &     1492.54_JPRB, 1428.57_JPRB, 1408.45_JPRB, 1369.86_JPRB, 1315.79_JPRB, &
       &     1250.00_JPRB, 1162.79_JPRB, 1111.11_JPRB, 1041.67_JPRB, 1000.00_JPRB, &
       &     952.381_JPRB, 909.091_JPRB, 869.565_JPRB, 800.000_JPRB, 740.741_JPRB, &
       &     714.286_JPRB, 689.655_JPRB, 666.667_JPRB, 645.161_JPRB, 606.061_JPRB, &
       &     588.235_JPRB, 571.429_JPRB, 555.556_JPRB, 526.316_JPRB, 500.000_JPRB, &
       &     400.000_JPRB, 312.500_JPRB, 250.000_JPRB, 200.000_JPRB, 166.667_JPRB, &
       &     125.000_JPRB, 100.000_JPRB, 66.6667_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: zhustaa1(nvalhusta) = &
       &  (/  4.56E+03_JPRB,  2.71E+03_JPRB,  5.29E+03_JPRB,  6.40E+03_JPRB,  5.42E+03_JPRB, &
       &      4.30E+03_JPRB,  3.32E+03_JPRB,  2.69E+03_JPRB,  2.29E+03_JPRB,  2.03E+03_JPRB, &
       &      2.52E+03_JPRB, -4.31E+04_JPRB, -1.10E+03_JPRB, -2.61E+02_JPRB, -1.84E+02_JPRB, &
       &     -4.93E+02_JPRB, -3.15E+04_JPRB,  1.95E+03_JPRB,  2.41E+03_JPRB, -1.14E+03_JPRB, &
       &     -1.87E+02_JPRB, -4.36E+01_JPRB, -1.76E+01_JPRB, -7.13E+00_JPRB, -1.97E+00_JPRB, &
       &     -2.89E-01_JPRB, -1.29E-02_JPRB, -2.60E-04_JPRB, -7.62E-02_JPRB, -9.91E-06_JPRB, &
       &     -5.91E+04_JPRB, -3.88E-05_JPRB, -1.79E+00_JPRB, -8.34E+01_JPRB, -4.90E+02_JPRB, &
       &     -7.78E+02_JPRB, -7.47E+02_JPRB, -6.18E+02_JPRB, -4.56E+02_JPRB, -2.83E+02_JPRB, &
       &     -1.82E+02_JPRB, -1.23E+02_JPRB, -7.98E+01_JPRB, -3.52E+01_JPRB, -9.86E+00_JPRB, &
       &     -1.22E-01_JPRB, -7.27E-06_JPRB, -2.93E+04_JPRB, -3.93E+03_JPRB, -4.00E+02_JPRB, &
       &      8.63E+01_JPRB,  1.71E+00_JPRB,  3.93E-02_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: zhustab1(nvalhusta) = &
       &  (/ -1.61E+00_JPRB, -1.27E+00_JPRB, -1.73E+00_JPRB, -1.79E+00_JPRB, -1.63E+00_JPRB, &
       &     -1.42E+00_JPRB, -1.19E+00_JPRB, -9.84E-01_JPRB, -7.86E-01_JPRB, -5.46E-01_JPRB, &
       &     -2.26E-01_JPRB,  8.00E-03_JPRB,  2.04E-01_JPRB,  4.58E-01_JPRB,  5.00E-01_JPRB, &
       &      2.70E-01_JPRB,  8.00E-03_JPRB, -2.50E-01_JPRB, -1.86E-01_JPRB,  1.86E-01_JPRB, &
       &      5.32E-01_JPRB,  9.24E-01_JPRB,  1.20E+00_JPRB,  1.49E+00_JPRB,  1.91E+00_JPRB, &
       &      2.57E+00_JPRB,  3.65E+00_JPRB,  5.06E+00_JPRB,  3.00E+00_JPRB,  6.00E+00_JPRB, &
       &     -6.00E+00_JPRB,  5.24E+00_JPRB,  1.36E+00_JPRB,  4.12E-01_JPRB,  1.66E-01_JPRB, &
       &      1.28E-01_JPRB,  1.38E-01_JPRB,  1.64E-01_JPRB,  2.08E-01_JPRB,  2.90E-01_JPRB, &
       &      3.78E-01_JPRB,  4.66E-01_JPRB,  5.70E-01_JPRB,  7.86E-01_JPRB,  1.16E+00_JPRB, &
       &      2.61E+00_JPRB,  6.00E+00_JPRB, -5.18E+00_JPRB, -3.69E+00_JPRB, -1.60E+00_JPRB, &
       &      2.92E-01_JPRB,  1.36E+00_JPRB,  2.35E+00_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: zhustac1(nvalhusta) = &
       &  (/  5.74E+01_JPRB,  2.35E+01_JPRB,  7.34E+01_JPRB,  7.03E+01_JPRB,  4.78E+01_JPRB, &
       &      1.52E+01_JPRB, -3.31E+01_JPRB, -9.66E+01_JPRB, -1.89E+02_JPRB, -3.88E+02_JPRB, &
       &     -1.30E+03_JPRB,  4.41E+04_JPRB,  1.96E+03_JPRB,  9.55E+02_JPRB,  7.83E+02_JPRB, &
       &      1.11E+03_JPRB,  3.23E+04_JPRB, -9.02E+02_JPRB, -1.38E+03_JPRB,  1.95E+03_JPRB, &
       &      8.45E+02_JPRB,  5.82E+02_JPRB,  4.99E+02_JPRB,  4.43E+02_JPRB,  3.87E+02_JPRB, &
       &      3.35E+02_JPRB,  2.84E+02_JPRB,  2.48E+02_JPRB,  3.02E+02_JPRB,  1.95E+02_JPRB, &
       &      1.55E+02_JPRB,  1.40E+02_JPRB,  1.66E+02_JPRB,  3.51E+02_JPRB,  8.69E+02_JPRB, &
       &      1.20E+03_JPRB,  1.19E+03_JPRB,  1.07E+03_JPRB,  9.07E+02_JPRB,  7.28E+02_JPRB, &
       &      6.15E+02_JPRB,  5.41E+02_JPRB,  4.81E+02_JPRB,  4.02E+02_JPRB,  3.30E+02_JPRB, &
       &      2.37E+02_JPRB,  1.76E+02_JPRB,  1.44E+02_JPRB,  1.35E+02_JPRB,  1.39E+02_JPRB, &
       &     -6.76E+01_JPRB,  2.87E+01_JPRB,  1.90E+01_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: zhustaa2(nvalhusta) = &
       &  (/  2.05E+03_JPRB,  2.02E+03_JPRB,  2.17E+03_JPRB,  2.24E+03_JPRB,  2.26E+03_JPRB, &
       &      2.28E+03_JPRB,  2.28E+03_JPRB,  2.28E+03_JPRB,  2.28E+03_JPRB,  2.28E+03_JPRB, &
       &      2.30E+03_JPRB,  2.34E+03_JPRB,  2.41E+03_JPRB,  2.76E+03_JPRB,  3.12E+03_JPRB, &
       &      2.92E+03_JPRB,  2.74E+03_JPRB,  2.61E+03_JPRB,  2.62E+03_JPRB,  2.76E+03_JPRB, &
       &      3.03E+03_JPRB,  3.56E+03_JPRB,  4.06E+03_JPRB,  4.74E+03_JPRB,  5.98E+03_JPRB, &
       &      8.00E+03_JPRB,  9.63E+03_JPRB,  8.53E+03_JPRB,  7.52E+03_JPRB,  2.37E+03_JPRB, &
       &      8.17E+02_JPRB,  7.94E+02_JPRB,  5.38E+02_JPRB,  7.11E+02_JPRB,  1.07E+03_JPRB, &
       &      1.28E+03_JPRB,  1.40E+03_JPRB,  1.50E+03_JPRB,  1.59E+03_JPRB,  1.73E+03_JPRB, &
       &      1.82E+03_JPRB,  1.89E+03_JPRB,  1.95E+03_JPRB,  2.02E+03_JPRB,  2.03E+03_JPRB, &
       &      1.69E+03_JPRB,  9.38E+02_JPRB,  1.19E+03_JPRB, -7.38E+02_JPRB, -9.44E+01_JPRB, &
       &     -4.07E-01_JPRB, -1.90E-07_JPRB, -4.49E+03_JPRB /)
  !
  Real(Kind=jprb),    Parameter :: zhustab2(nvalhusta) = &
       &  (/ -1.09E+00_JPRB, -1.08E+00_JPRB, -1.10E+00_JPRB, -1.11E+00_JPRB, -1.12E+00_JPRB, &
       &     -1.12E+00_JPRB, -1.11E+00_JPRB, -1.11E+00_JPRB, -1.11E+00_JPRB, -1.11E+00_JPRB, &
       &     -1.11E+00_JPRB, -1.12E+00_JPRB, -1.13E+00_JPRB, -1.19E+00_JPRB, -1.24E+00_JPRB, &
       &     -1.22E+00_JPRB, -1.19E+00_JPRB, -1.16E+00_JPRB, -1.16E+00_JPRB, -1.18E+00_JPRB, &
       &     -1.22E+00_JPRB, -1.29E+00_JPRB, -1.34E+00_JPRB, -1.40E+00_JPRB, -1.49E+00_JPRB, &
       &     -1.60E+00_JPRB, -1.66E+00_JPRB, -1.59E+00_JPRB, -1.55E+00_JPRB, -1.02E+00_JPRB, &
       &     -4.24E-01_JPRB, -1.48E-01_JPRB, -2.88E-01_JPRB, -6.06E-01_JPRB, -7.94E-01_JPRB, &
       &     -8.62E-01_JPRB, -8.94E-01_JPRB, -9.18E-01_JPRB, -9.38E-01_JPRB, -9.66E-01_JPRB, &
       &     -9.82E-01_JPRB, -9.94E-01_JPRB, -1.00E+00_JPRB, -1.01E+00_JPRB, -1.00E+00_JPRB, &
       &     -8.92E-01_JPRB, -5.44E-01_JPRB, -9.20E-02_JPRB,  8.00E-02_JPRB,  3.06E-01_JPRB, &
       &      1.48E+00_JPRB,  5.41E+00_JPRB, -1.84E+00_JPRB /)
  !
  Real(Kind=jprb),    Parameter :: zhustac2(nvalhusta) = &
       &  (/  2.66E+00_JPRB,  2.24E+00_JPRB,  3.01E+00_JPRB,  3.32E+00_JPRB,  3.29E+00_JPRB, &
       &      3.16E+00_JPRB,  2.91E+00_JPRB,  2.66E+00_JPRB,  2.42E+00_JPRB,  2.19E+00_JPRB, &
       &      2.25E+00_JPRB,  2.60E+00_JPRB,  3.47E+00_JPRB,  6.82E+00_JPRB,  9.51E+00_JPRB, &
       &      8.24E+00_JPRB,  6.86E+00_JPRB,  5.49E+00_JPRB,  5.36E+00_JPRB,  6.34E+00_JPRB, &
       &      8.35E+00_JPRB,  1.16E+01_JPRB,  1.40E+01_JPRB,  1.65E+01_JPRB,  1.96E+01_JPRB, &
       &      2.25E+01_JPRB,  2.26E+01_JPRB,  1.83E+01_JPRB,  1.91E+01_JPRB, -1.69E+01_JPRB, &
       &     -1.35E+02_JPRB, -4.23E+02_JPRB, -1.48E+02_JPRB, -3.72E+01_JPRB, -1.73E+01_JPRB, &
       &     -1.27E+01_JPRB, -1.08E+01_JPRB, -9.58E+00_JPRB, -8.61E+00_JPRB, -7.26E+00_JPRB, &
       &     -6.67E+00_JPRB, -6.18E+00_JPRB, -6.07E+00_JPRB, -6.14E+00_JPRB, -7.63E+00_JPRB, &
       &     -2.02E+01_JPRB, -8.44E+01_JPRB, -8.08E+02_JPRB,  1.03E+03_JPRB,  3.34E+02_JPRB, &
       &      1.32E+02_JPRB,  8.89E+01_JPRB,  7.12E+01_JPRB /)
  !
  Real(Kind=jprb),    Parameter :: zhustaa3(nvalhusta) = &
       &  (/  1.12E+03_JPRB,  1.12E+03_JPRB,  1.17E+03_JPRB,  1.20E+03_JPRB,  1.22E+03_JPRB, &
       &      1.23E+03_JPRB,  1.24E+03_JPRB,  1.26E+03_JPRB,  1.27E+03_JPRB,  1.28E+03_JPRB, &
       &      1.30E+03_JPRB,  1.31E+03_JPRB,  1.31E+03_JPRB,  1.32E+03_JPRB,  1.30E+03_JPRB, &
       &      1.27E+03_JPRB,  1.28E+03_JPRB,  1.30E+03_JPRB,  1.32E+03_JPRB,  1.35E+03_JPRB, &
       &      1.37E+03_JPRB,  1.39E+03_JPRB,  1.40E+03_JPRB,  1.41E+03_JPRB,  1.42E+03_JPRB, &
       &      1.44E+03_JPRB,  1.46E+03_JPRB,  1.50E+03_JPRB,  1.52E+03_JPRB,  1.96E+03_JPRB, &
       &      2.45E+03_JPRB,  1.68E+03_JPRB,  9.77E+02_JPRB,  8.92E+02_JPRB,  1.03E+03_JPRB, &
       &      1.13E+03_JPRB,  1.19E+03_JPRB,  1.24E+03_JPRB,  1.28E+03_JPRB,  1.35E+03_JPRB, &
       &      1.40E+03_JPRB,  1.44E+03_JPRB,  1.47E+03_JPRB,  1.53E+03_JPRB,  1.61E+03_JPRB, &
       &      1.81E+03_JPRB,  2.13E+03_JPRB,  1.86E+03_JPRB,  1.52E+03_JPRB,  1.54E+03_JPRB, &
       &      1.36E+03_JPRB,  6.85E+02_JPRB, -2.26E-01_JPRB /)
  !
  Real(Kind=jprb),    Parameter :: zhustab3(nvalhusta) = &
       &  (/ -8.52E-01_JPRB, -8.52E-01_JPRB, -8.64E-01_JPRB, -8.70E-01_JPRB, -8.74E-01_JPRB, &
       &     -8.76E-01_JPRB, -8.78E-01_JPRB, -8.82E-01_JPRB, -8.84E-01_JPRB, -8.88E-01_JPRB, &
       &     -8.92E-01_JPRB, -8.94E-01_JPRB, -8.94E-01_JPRB, -8.96E-01_JPRB, -8.92E-01_JPRB, &
       &     -8.84E-01_JPRB, -8.86E-01_JPRB, -8.90E-01_JPRB, -8.94E-01_JPRB, -9.00E-01_JPRB, &
       &     -9.04E-01_JPRB, -9.08E-01_JPRB, -9.10E-01_JPRB, -9.12E-01_JPRB, -9.14E-01_JPRB, &
       &     -9.16E-01_JPRB, -9.20E-01_JPRB, -9.28E-01_JPRB, -9.30E-01_JPRB, -1.02E+00_JPRB, &
       &     -1.09E+00_JPRB, -9.66E-01_JPRB, -7.88E-01_JPRB, -7.62E-01_JPRB, -8.08E-01_JPRB, &
       &     -8.36E-01_JPRB, -8.50E-01_JPRB, -8.62E-01_JPRB, -8.72E-01_JPRB, -8.86E-01_JPRB, &
       &     -8.96E-01_JPRB, -9.04E-01_JPRB, -9.10E-01_JPRB, -9.20E-01_JPRB, -9.34E-01_JPRB, &
       &     -9.66E-01_JPRB, -1.01E+00_JPRB, -9.52E-01_JPRB, -8.72E-01_JPRB, -8.62E-01_JPRB, &
       &     -7.90E-01_JPRB, -4.68E-01_JPRB,  1.29E+00_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: zhustac3(nvalhusta) = &
       &  (/ -8.94E+00_JPRB, -8.99E+00_JPRB, -8.67E+00_JPRB, -8.51E+00_JPRB, -8.38E+00_JPRB, &
       &     -8.35E+00_JPRB, -8.32E+00_JPRB, -8.19E+00_JPRB, -8.16E+00_JPRB, -8.03E+00_JPRB, &
       &     -7.90E+00_JPRB, -7.86E+00_JPRB, -7.90E+00_JPRB, -7.82E+00_JPRB, -7.94E+00_JPRB, &
       &     -8.20E+00_JPRB, -8.14E+00_JPRB, -8.04E+00_JPRB, -7.92E+00_JPRB, -7.76E+00_JPRB, &
       &     -7.66E+00_JPRB, -7.56E+00_JPRB, -7.52E+00_JPRB, -7.48E+00_JPRB, -7.47E+00_JPRB, &
       &     -7.48E+00_JPRB, -7.42E+00_JPRB, -7.15E+00_JPRB, -7.13E+00_JPRB, -3.73E+00_JPRB, &
       &     -1.57E+00_JPRB, -5.84E+00_JPRB, -1.28E+01_JPRB, -1.35E+01_JPRB, -1.15E+01_JPRB, &
       &     -1.05E+01_JPRB, -1.01E+01_JPRB, -9.66E+00_JPRB, -9.37E+00_JPRB, -8.97E+00_JPRB, &
       &     -8.67E+00_JPRB, -8.42E+00_JPRB, -8.28E+00_JPRB, -8.01E+00_JPRB, -7.58E+00_JPRB, &
       &     -6.71E+00_JPRB, -5.72E+00_JPRB, -8.68E+00_JPRB, -1.30E+01_JPRB, -1.45E+01_JPRB, &
       &     -2.17E+01_JPRB, -6.75E+01_JPRB,  8.10E+01_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: zhustad1(nvalhusta) = &
       &  (/  2.00E+00_JPRB, -1.22E+00_JPRB, -5.94E-01_JPRB,  1.69E-01_JPRB,  1.40E-01_JPRB, &
       &      1.50E-01_JPRB,  1.40E-01_JPRB,  7.89E-02_JPRB,  3.83E-02_JPRB,  1.77E-02_JPRB, &
       &      6.96E-03_JPRB,  3.68E-03_JPRB,  2.29E-03_JPRB,  1.92E-03_JPRB,  9.32E-04_JPRB, &
       &      1.32E-04_JPRB,  2.13E-03_JPRB,  3.83E-02_JPRB,  3.56E-02_JPRB,  7.68E-03_JPRB, &
       &      2.28E-03_JPRB,  6.59E-04_JPRB,  2.80E-04_JPRB,  1.13E-04_JPRB,  2.67E-05_JPRB, &
       &      1.96E-06_JPRB,  1.91E-08_JPRB,  1.35E+02_JPRB,  2.69E-08_JPRB,  7.67E+00_JPRB, &
       &      1.70E+00_JPRB,  1.07E+00_JPRB,  9.80E-01_JPRB,  9.43E-01_JPRB,  8.57E-01_JPRB, &
       &      8.38E-01_JPRB,  8.36E-01_JPRB,  8.41E-01_JPRB,  8.53E-01_JPRB,  8.82E-01_JPRB, &
       &      9.18E-01_JPRB,  9.54E-01_JPRB,  1.00E+00_JPRB,  1.10E+00_JPRB,  1.22E+00_JPRB, &
       &      1.39E+00_JPRB,  1.38E+00_JPRB,  1.27E+00_JPRB,  1.92E+00_JPRB, -1.05E+01_JPRB, &
       &     -3.03E-01_JPRB, -3.95E-02_JPRB, -1.37E-03_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: zhustae1(nvalhusta) = &
       &  (/  4.60E-02_JPRB, -1.90E+00_JPRB, -5.24E-01_JPRB,  2.84E-01_JPRB,  3.60E-01_JPRB, &
       &      4.04E-01_JPRB,  4.70E-01_JPRB,  6.28E-01_JPRB,  8.28E-01_JPRB,  1.06E+00_JPRB, &
       &      1.36E+00_JPRB,  1.57E+00_JPRB,  1.73E+00_JPRB,  1.87E+00_JPRB,  2.07E+00_JPRB, &
       &      2.61E+00_JPRB,  1.62E+00_JPRB,  7.74E-01_JPRB,  8.60E-01_JPRB,  1.41E+00_JPRB, &
       &      1.83E+00_JPRB,  2.26E+00_JPRB,  2.56E+00_JPRB,  2.87E+00_JPRB,  3.38E+00_JPRB, &
       &      4.33E+00_JPRB,  6.00E+00_JPRB, -6.00E+00_JPRB,  5.93E+00_JPRB, -3.08E+00_JPRB, &
       &     -1.50E+00_JPRB, -8.10E-01_JPRB, -4.36E-01_JPRB, -3.90E-01_JPRB, -5.22E-01_JPRB, &
       &     -6.18E-01_JPRB, -6.76E-01_JPRB, -7.32E-01_JPRB, -7.86E-01_JPRB, -8.72E-01_JPRB, &
       &     -9.46E-01_JPRB, -1.01E+00_JPRB, -1.07E+00_JPRB, -1.17E+00_JPRB, -1.25E+00_JPRB, &
       &     -1.23E+00_JPRB, -9.68E-01_JPRB, -5.24E-01_JPRB, -1.90E-01_JPRB,  2.40E-02_JPRB, &
       &      4.12E-01_JPRB,  9.70E-01_JPRB,  2.03E+00_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: zhustaf1(nvalhusta) = &
       &  (/ -2.06E+00_JPRB,  4.93E-01_JPRB,  4.22E-01_JPRB, -2.17E-01_JPRB, -1.90E-01_JPRB, &
       &     -2.07E-01_JPRB, -1.93E-01_JPRB, -1.08E-01_JPRB, -4.42E-02_JPRB, -3.46E-03_JPRB, &
       &      2.61E-02_JPRB,  3.91E-02_JPRB,  4.96E-02_JPRB,  1.16E-01_JPRB,  2.70E-01_JPRB, &
       &      3.96E-01_JPRB,  3.72E-01_JPRB,  2.31E-01_JPRB,  1.74E-01_JPRB,  1.63E-01_JPRB, &
       &      1.59E-01_JPRB,  1.67E-01_JPRB,  1.69E-01_JPRB,  1.72E-01_JPRB,  1.82E-01_JPRB, &
       &      1.96E-01_JPRB,  2.21E-01_JPRB,  2.47E-01_JPRB,  2.38E-01_JPRB,  2.96E-01_JPRB, &
       &      3.23E-01_JPRB,  3.27E-01_JPRB,  2.31E-01_JPRB,  2.32E-01_JPRB,  3.42E-01_JPRB, &
       &      3.88E-01_JPRB,  4.08E-01_JPRB,  4.25E-01_JPRB,  4.38E-01_JPRB,  4.55E-01_JPRB, &
       &      4.66E-01_JPRB,  4.73E-01_JPRB,  4.80E-01_JPRB,  4.86E-01_JPRB,  4.88E-01_JPRB, &
       &      4.71E-01_JPRB,  4.13E-01_JPRB,  2.35E-01_JPRB, -5.67E-01_JPRB,  1.18E+01_JPRB, &
       &      1.49E+00_JPRB,  1.12E+00_JPRB,  1.01E+00_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: zhustad2(nvalhusta) = &
       &  (/  5.92E-01_JPRB, -9.00E-05_JPRB, -2.76E+00_JPRB,  6.57E-02_JPRB,  1.44E-01_JPRB, &
       &      1.29E+00_JPRB, -1.27E+00_JPRB, -1.19E+00_JPRB, -1.27E+00_JPRB, -1.39E+00_JPRB, &
       &     -1.48E+00_JPRB, -1.50E+00_JPRB, -1.35E+00_JPRB, -3.34E+00_JPRB, -1.59E+03_JPRB, &
       &     -6.31E+04_JPRB, -1.05E-11_JPRB, -6.06E-10_JPRB, -6.27E+04_JPRB, -9.39E+01_JPRB, &
       &     -2.36E+01_JPRB, -1.95E+01_JPRB, -1.79E+01_JPRB, -1.78E+01_JPRB, -2.00E+01_JPRB, &
       &     -1.97E+01_JPRB, -1.40E+01_JPRB, -6.68E+00_JPRB, -1.55E+01_JPRB, -1.68E+00_JPRB, &
       &      8.00E-03_JPRB,  3.01E-08_JPRB,  4.73E+01_JPRB,  1.96E+00_JPRB,  1.09E+00_JPRB, &
       &      8.47E-01_JPRB,  7.25E-01_JPRB,  6.30E-01_JPRB,  5.55E-01_JPRB,  4.70E-01_JPRB, &
       &      4.34E-01_JPRB,  4.48E-01_JPRB,  6.34E-01_JPRB, -2.97E-01_JPRB, -2.33E-02_JPRB, &
       &     -8.04E-04_JPRB,  6.00E+04_JPRB,  1.18E+02_JPRB,  9.23E+00_JPRB,  9.92E+00_JPRB, &
       &      4.12E+01_JPRB,  3.91E+01_JPRB,  1.06E+01_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: zhustae2(nvalhusta) = &
       &  (/  1.06E-01_JPRB,  1.40E+00_JPRB, -4.00E-02_JPRB,  4.66E-01_JPRB,  3.40E-01_JPRB, &
       &      8.80E-02_JPRB, -2.24E-01_JPRB, -2.94E-01_JPRB, -2.34E-01_JPRB, -1.92E-01_JPRB, &
       &     -1.74E-01_JPRB, -1.74E-01_JPRB, -2.30E-01_JPRB, -1.14E+00_JPRB, -3.99E+00_JPRB, &
       &     -6.00E+00_JPRB,  6.00E+00_JPRB,  4.87E+00_JPRB, -6.00E+00_JPRB, -2.84E+00_JPRB, &
       &     -2.12E+00_JPRB, -1.96E+00_JPRB, -1.87E+00_JPRB, -1.83E+00_JPRB, -1.84E+00_JPRB, &
       &     -1.79E+00_JPRB, -1.59E+00_JPRB, -1.23E+00_JPRB, -1.69E+00_JPRB, -2.14E-01_JPRB, &
       &      9.36E-01_JPRB,  4.04E+00_JPRB, -2.67E+00_JPRB, -1.16E+00_JPRB, -8.90E-01_JPRB, &
       &     -7.82E-01_JPRB, -7.08E-01_JPRB, -6.34E-01_JPRB, -5.60E-01_JPRB, -4.34E-01_JPRB, &
       &     -3.24E-01_JPRB, -2.24E-01_JPRB, -1.06E-01_JPRB,  1.08E-01_JPRB,  4.34E-01_JPRB, &
       &      1.05E+00_JPRB, -6.00E+00_JPRB, -3.02E+00_JPRB, -1.75E+00_JPRB, -1.74E+00_JPRB, &
       &     -2.28E+00_JPRB, -2.12E+00_JPRB, -1.31E+00_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: zhustaf2(nvalhusta) = &
       &  (/ -5.89E-01_JPRB,  4.85E-01_JPRB,  2.76E+00_JPRB, -8.47E-02_JPRB, -1.86E-01_JPRB, &
       &     -1.41E+00_JPRB,  9.81E-01_JPRB,  8.33E-01_JPRB,  9.60E-01_JPRB,  1.10E+00_JPRB, &
       &      1.19E+00_JPRB,  1.19E+00_JPRB,  9.83E-01_JPRB,  5.09E-01_JPRB,  4.93E-01_JPRB, &
       &      5.02E-01_JPRB,  5.02E-01_JPRB,  5.00E-01_JPRB,  4.93E-01_JPRB,  4.87E-01_JPRB, &
       &      4.90E-01_JPRB,  4.91E-01_JPRB,  4.92E-01_JPRB,  4.93E-01_JPRB,  4.95E-01_JPRB, &
       &      5.01E-01_JPRB,  5.23E-01_JPRB,  5.66E-01_JPRB,  5.19E-01_JPRB,  1.28E+00_JPRB, &
       &      2.74E-01_JPRB,  4.57E-01_JPRB,  5.07E-01_JPRB,  4.82E-01_JPRB,  4.58E-01_JPRB, &
       &      4.47E-01_JPRB,  4.39E-01_JPRB,  4.31E-01_JPRB,  4.21E-01_JPRB,  3.95E-01_JPRB, &
       &      3.59E-01_JPRB,  2.94E-01_JPRB,  6.19E-02_JPRB,  9.35E-01_JPRB,  6.10E-01_JPRB, &
       &      5.44E-01_JPRB,  5.21E-01_JPRB,  5.23E-01_JPRB,  5.15E-01_JPRB,  5.15E-01_JPRB, &
       &      5.18E-01_JPRB,  5.02E-01_JPRB,  4.12E-01_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: zhustad3(nvalhusta) = &
       &  (/ -1.13E+00_JPRB,  2.17E-01_JPRB, -1.17E+00_JPRB, -1.68E+00_JPRB, -1.55E+00_JPRB, &
       &     -2.84E+00_JPRB, -8.65E+00_JPRB, -1.15E+01_JPRB, -8.50E+00_JPRB, -6.49E+00_JPRB, &
       &     -5.09E+00_JPRB, -4.41E+00_JPRB, -4.50E+00_JPRB, -4.19E+01_JPRB, -1.15E-04_JPRB, &
       &      3.19E-01_JPRB,  4.34E-01_JPRB,  4.03E-01_JPRB,  1.82E-01_JPRB, -1.93E-12_JPRB, &
       &     -6.01E+06_JPRB, -2.54E+04_JPRB, -2.77E+03_JPRB, -9.06E+02_JPRB, -5.94E+02_JPRB, &
       &     -5.23E+02_JPRB, -1.45E+03_JPRB, -5.13E+03_JPRB, -4.70E+04_JPRB, -1.79E+05_JPRB, &
       &     -6.31E+04_JPRB, -1.82E+04_JPRB, -3.52E-04_JPRB,  8.68E-01_JPRB,  1.05E+00_JPRB, &
       &      9.69E-01_JPRB,  9.26E-01_JPRB,  8.83E-01_JPRB,  8.43E-01_JPRB,  7.97E-01_JPRB, &
       &      7.58E-01_JPRB,  7.25E-01_JPRB,  6.95E-01_JPRB,  6.35E-01_JPRB,  5.41E-01_JPRB, &
       &      5.33E-01_JPRB, -1.42E-03_JPRB, -2.62E-04_JPRB, -9.94E-02_JPRB, -1.26E-01_JPRB, &
       &     -4.56E-04_JPRB,  1.16E-01_JPRB,  1.62E+07_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: zhustae3(nvalhusta) = &
       &  (/ -1.34E-01_JPRB, -7.48E-01_JPRB, -5.40E-01_JPRB, -1.64E-01_JPRB, -3.58E-01_JPRB, &
       &     -7.66E-01_JPRB, -1.29E+00_JPRB, -1.41E+00_JPRB, -1.28E+00_JPRB, -1.16E+00_JPRB, &
       &     -1.04E+00_JPRB, -9.76E-01_JPRB, -9.88E-01_JPRB, -2.02E+00_JPRB,  1.15E+00_JPRB, &
       &     -6.20E-01_JPRB, -7.44E-01_JPRB, -7.22E-01_JPRB, -3.40E-01_JPRB,  5.20E+00_JPRB, &
       &     -6.00E+00_JPRB, -4.21E+00_JPRB, -3.47E+00_JPRB, -3.09E+00_JPRB, -2.93E+00_JPRB, &
       &     -2.87E+00_JPRB, -3.18E+00_JPRB, -3.55E+00_JPRB, -4.33E+00_JPRB, -4.55E+00_JPRB, &
       &     -4.19E+00_JPRB, -3.99E+00_JPRB,  9.38E-01_JPRB, -8.10E-01_JPRB, -8.48E-01_JPRB, &
       &     -8.14E-01_JPRB, -7.92E-01_JPRB, -7.68E-01_JPRB, -7.44E-01_JPRB, -7.12E-01_JPRB, &
       &     -6.84E-01_JPRB, -6.60E-01_JPRB, -6.36E-01_JPRB, -5.86E-01_JPRB, -4.94E-01_JPRB, &
       &     -1.28E-01_JPRB,  8.88E-01_JPRB,  1.22E+00_JPRB,  2.04E-01_JPRB,  1.86E-01_JPRB, &
       &      1.09E+00_JPRB, -3.16E-01_JPRB, -6.00E+00_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: zhustaf3(nvalhusta) = &
       &  (/  9.75E-01_JPRB,  4.57E-01_JPRB,  5.37E-01_JPRB,  1.20E+00_JPRB,  7.30E-01_JPRB, &
       &      5.42E-01_JPRB,  4.93E-01_JPRB,  4.90E-01_JPRB,  4.96E-01_JPRB,  5.03E-01_JPRB, &
       &      5.13E-01_JPRB,  5.20E-01_JPRB,  5.19E-01_JPRB,  4.82E-01_JPRB,  4.97E-01_JPRB, &
       &      4.59E-01_JPRB,  4.61E-01_JPRB,  4.57E-01_JPRB,  4.33E-01_JPRB,  4.82E-01_JPRB, &
       &      4.80E-01_JPRB,  4.80E-01_JPRB,  4.81E-01_JPRB,  4.82E-01_JPRB,  4.83E-01_JPRB, &
       &      4.85E-01_JPRB,  4.87E-01_JPRB,  4.89E-01_JPRB,  4.85E-01_JPRB,  4.96E-01_JPRB, &
       &      5.03E-01_JPRB,  5.05E-01_JPRB,  5.21E-01_JPRB,  4.64E-01_JPRB,  4.52E-01_JPRB, &
       &      4.45E-01_JPRB,  4.42E-01_JPRB,  4.39E-01_JPRB,  4.36E-01_JPRB,  4.32E-01_JPRB, &
       &      4.29E-01_JPRB,  4.27E-01_JPRB,  4.24E-01_JPRB,  4.19E-01_JPRB,  4.08E-01_JPRB, &
       &      1.71E-01_JPRB,  5.51E-01_JPRB,  5.44E-01_JPRB,  7.38E-01_JPRB,  7.78E-01_JPRB, &
       &      5.54E-01_JPRB,  4.91E-01_JPRB,  5.13E-01_JPRB /)
  !
  ! Lower and upper r_e limits (in microns) for the Hu & Stamnes water cloud scheme
  !
  Real(Kind=jprb),    Parameter  :: low_re(3) = &
       &  (/  2.5_JPRB, 12.5_JPRB, 30.0_JPRB /)
  Real(Kind=jprb),    Parameter  :: upp_re(3) = &
       &  (/ 12.5_JPRB, 30.0_JPRB, 60.0_JPRB /)
  !
  ! Ice cloud coefficients for hexagonal columns and aggregates (Baran et al. references, etc.)
  !
  Integer(Kind=jpim), Parameter  :: nvalice = 52   ! No. of wavelengths for tabulated ice cloud data (columns & aggregates)
  Real(Kind=jprb),    Parameter  :: ziceom(nvalice) = &
       &  (/ 3030.30_JPRB, 2941.18_JPRB, 2857.14_JPRB, 2739.73_JPRB, 2631.58_JPRB, 2500.00_JPRB, &
       &     2352.94_JPRB, 2222.22_JPRB, 2105.26_JPRB, 2000.00_JPRB, 1904.76_JPRB, 1818.18_JPRB, &
       &     1739.13_JPRB, 1666.67_JPRB, 1600.00_JPRB, 1538.46_JPRB, 1481.48_JPRB, 1428.57_JPRB, &
       &     1379.31_JPRB, 1333.33_JPRB, 1290.32_JPRB, 1250.00_JPRB, 1212.12_JPRB, 1176.47_JPRB, &
       &     1142.86_JPRB, 1111.11_JPRB, 1081.08_JPRB, 1052.63_JPRB, 1025.64_JPRB, 1000.00_JPRB, &
       &     975.610_JPRB, 952.381_JPRB, 930.233_JPRB, 909.091_JPRB, 888.889_JPRB, 869.565_JPRB, &
       &     851.064_JPRB, 833.333_JPRB, 816.327_JPRB, 800.000_JPRB, 784.314_JPRB, 769.231_JPRB, &
       &     754.717_JPRB, 740.741_JPRB, 727.273_JPRB, 714.286_JPRB, 701.754_JPRB, 689.655_JPRB, &
       &     677.966_JPRB, 666.667_JPRB, 645.161_JPRB, 625.000_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: ziceclmna(nvalice) = &
       &  (/ -2.730E-04_JPRB,  2.955E-03_JPRB,  8.551E-03_JPRB,  1.104E-02_JPRB,  1.142E-02_JPRB, &
       &      1.063E-02_JPRB,  6.645E-03_JPRB,  4.585E-03_JPRB,  7.438E-03_JPRB,  1.153E-02_JPRB, &
       &      1.118E-02_JPRB,  8.874E-03_JPRB,  4.290E-03_JPRB,  7.193E-05_JPRB, -2.919E-04_JPRB, &
       &      1.430E-03_JPRB,  1.499E-03_JPRB,  2.203E-03_JPRB,  2.854E-03_JPRB,  3.659E-03_JPRB, &
       &      4.395E-03_JPRB,  5.954E-03_JPRB,  8.233E-03_JPRB,  8.613E-03_JPRB,  8.671E-03_JPRB, &
       &      8.635E-03_JPRB,  8.579E-03_JPRB,  8.933E-03_JPRB,  9.294E-03_JPRB,  9.194E-03_JPRB, &
       &      6.983E-03_JPRB,  4.237E-03_JPRB,  8.328E-04_JPRB, -1.586E-03_JPRB, -3.119E-03_JPRB, &
       &     -4.084E-03_JPRB, -4.778E-03_JPRB, -5.424E-03_JPRB, -5.775E-03_JPRB, -6.258E-03_JPRB, &
       &     -6.650E-03_JPRB, -7.006E-03_JPRB, -7.293E-03_JPRB, -7.489E-03_JPRB, -7.606E-03_JPRB, &
       &     -7.604E-03_JPRB, -7.290E-03_JPRB, -6.789E-03_JPRB, -6.010E-03_JPRB, -5.113E-03_JPRB, &
       &     -3.079E-03_JPRB, -1.319E-03_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: ziceclmnb(nvalice) = &
       &  (/ 1.15507_JPRB, 0.998864_JPRB, 0.648575_JPRB, 0.413856_JPRB, 0.357719_JPRB, 0.477263_JPRB, &
       &    0.801942_JPRB, 0.929286_JPRB, 0.748354_JPRB, 0.484662_JPRB, 0.532899_JPRB, 0.756074_JPRB, &
       &     1.06101_JPRB,  1.29158_JPRB,  1.31124_JPRB,  1.22431_JPRB,  1.22406_JPRB,  1.18912_JPRB, &
       &     1.15642_JPRB,  1.11504_JPRB,  1.06638_JPRB, 0.975032_JPRB, 0.839719_JPRB, 0.820845_JPRB, &
       &    0.817312_JPRB, 0.822945_JPRB, 0.824715_JPRB, 0.803417_JPRB, 0.776200_JPRB, 0.779677_JPRB, &
       &    0.905234_JPRB,  1.05443_JPRB,  1.22364_JPRB,  1.33783_JPRB,  1.41295_JPRB,  1.46198_JPRB, &
       &     1.49971_JPRB,  1.53374_JPRB,  1.56394_JPRB,  1.59190_JPRB,  1.61783_JPRB,  1.64190_JPRB, &
       &     1.66343_JPRB,  1.68113_JPRB,  1.69468_JPRB,  1.70311_JPRB,  1.70024_JPRB,  1.68537_JPRB, &
       &     1.65613_JPRB,  1.61908_JPRB,  1.52735_JPRB,  1.44473_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: ziceclmnc(nvalice) = &
       &  (/ -1.26793_JPRB, -2.27565_JPRB, -1.96593_JPRB, -1.35472_JPRB, -1.18284_JPRB, -1.54074_JPRB, &
       &     -2.25679_JPRB, -2.36525_JPRB, -2.17367_JPRB, -1.82730_JPRB, -2.00055_JPRB, -2.66083_JPRB, &
       &     -3.22855_JPRB, -3.10677_JPRB, -3.16384_JPRB, -3.33039_JPRB, -3.34724_JPRB, -3.37792_JPRB, &
       &     -3.39756_JPRB, -3.38943_JPRB, -3.22239_JPRB, -3.14114_JPRB, -2.88840_JPRB, -2.85444_JPRB, &
       &     -2.85180_JPRB, -2.85761_JPRB, -2.86914_JPRB, -2.80742_JPRB, -2.72661_JPRB, -2.69076_JPRB, &
       &     -2.82381_JPRB, -2.82896_JPRB, -2.48929_JPRB, -1.97028_JPRB, -1.58561_JPRB, -1.34155_JPRB, &
       &     -1.19335_JPRB, -1.12488_JPRB, -1.14122_JPRB, -1.19303_JPRB, -1.34383_JPRB, -1.52861_JPRB, &
       &     -1.76771_JPRB, -2.04851_JPRB, -2.34234_JPRB, -2.67892_JPRB, -3.03952_JPRB, -3.42093_JPRB, &
       &     -3.78222_JPRB, -4.04325_JPRB, -4.34446_JPRB, -4.42520_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: ziceclmnd(nvalice) = &
       &  (/  1.999E-06_JPRB, -1.308E-05_JPRB, -3.710E-05_JPRB, -4.492E-05_JPRB, -4.540E-05_JPRB, &
       &     -4.410E-05_JPRB, -2.943E-05_JPRB, -2.046E-05_JPRB, -3.272E-05_JPRB, -4.888E-05_JPRB, &
       &     -4.826E-05_JPRB, -4.139E-05_JPRB, -2.206E-05_JPRB, -2.334E-06_JPRB, -7.321E-07_JPRB, &
       &     -9.059E-06_JPRB, -9.474E-06_JPRB, -1.289E-05_JPRB, -1.603E-05_JPRB, -1.986E-05_JPRB, &
       &     -2.281E-05_JPRB, -2.979E-05_JPRB, -3.992E-05_JPRB, -4.159E-05_JPRB, -4.172E-05_JPRB, &
       &     -4.155E-05_JPRB, -4.107E-05_JPRB, -4.245E-05_JPRB, -4.362E-05_JPRB, -4.273E-05_JPRB, &
       &     -3.247E-05_JPRB, -1.948E-05_JPRB, -3.936E-06_JPRB,  6.769E-06_JPRB,  1.342E-05_JPRB, &
       &      1.751E-05_JPRB,  2.041E-05_JPRB,  2.323E-05_JPRB,  2.422E-05_JPRB,  2.637E-05_JPRB, &
       &      2.814E-05_JPRB,  2.976E-05_JPRB,  3.107E-05_JPRB,  3.195E-05_JPRB,  3.244E-05_JPRB, &
       &      3.234E-05_JPRB,  3.050E-05_JPRB,  2.792E-05_JPRB,  2.396E-05_JPRB,  1.944E-05_JPRB, &
       &      9.359E-06_JPRB,  6.752E-07_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: ziceaggra(nvalice) = &
       &  (/ -5.185e-05_JPRB,  4.493e-03_JPRB,  9.876e-03_JPRB,  1.049e-02_JPRB,  1.053e-02_JPRB, &
       &      1.069e-02_JPRB,  1.015e-02_JPRB,  7.414e-03_JPRB,  1.021e-02_JPRB,  1.086e-02_JPRB, &
       &      1.067e-02_JPRB,  9.625e-03_JPRB,  6.808e-03_JPRB,  2.553e-03_JPRB,  2.433e-03_JPRB, &
       &      4.034e-03_JPRB,  4.089e-03_JPRB,  4.599e-03_JPRB,  5.161e-03_JPRB,  5.677e-03_JPRB, &
       &      6.128e-03_JPRB,  7.258e-03_JPRB,  7.541e-03_JPRB,  7.768e-03_JPRB,  7.849e-03_JPRB, &
       &      7.810e-03_JPRB,  7.874e-03_JPRB,  8.063e-03_JPRB,  8.300e-03_JPRB,  8.178e-03_JPRB, &
       &      6.610e-03_JPRB,  4.199e-03_JPRB,  5.587e-05_JPRB, -3.759e-03_JPRB, -5.227e-03_JPRB, &
       &     -6.710e-03_JPRB, -7.729e-03_JPRB, -8.106e-03_JPRB, -8.477e-03_JPRB, -8.424e-03_JPRB, &
       &     -8.372e-03_JPRB, -8.584e-03_JPRB, -8.162e-03_JPRB, -7.566e-03_JPRB, -6.899e-03_JPRB, &
       &     -6.173e-03_JPRB, -5.201e-03_JPRB, -3.949e-03_JPRB, -2.594e-03_JPRB, -1.514e-03_JPRB, &
       &      7.812e-04_JPRB,  2.239e-03_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: ziceaggrb(nvalice) = &
       &  (/  1.11934_JPRB, 0.847526_JPRB, 0.463047_JPRB, 0.259500_JPRB, 0.203728_JPRB, 0.279832_JPRB, &
       &     0.403984_JPRB, 0.650849_JPRB, 0.397133_JPRB, 0.264136_JPRB, 0.288991_JPRB, 0.422823_JPRB, &
       &     0.640145_JPRB, 0.877570_JPRB, 0.884022_JPRB, 0.789077_JPRB, 0.787059_JPRB, 0.755748_JPRB, &
       &     0.724099_JPRB, 0.689130_JPRB, 0.663811_JPRB, 0.587901_JPRB, 0.509921_JPRB, 0.498516_JPRB, &
       &     0.492339_JPRB, 0.498351_JPRB, 0.496686_JPRB, 0.488979_JPRB, 0.472445_JPRB, 0.482186_JPRB, &
       &     0.595324_JPRB, 0.757605_JPRB,  1.00574_JPRB,  1.21263_JPRB,  1.29811_JPRB,  1.38007_JPRB, &
       &      1.44194_JPRB,  1.46135_JPRB,  1.47735_JPRB,  1.48062_JPRB,  1.48395_JPRB,  1.49712_JPRB, &
       &      1.47944_JPRB,  1.45448_JPRB,  1.40992_JPRB,  1.37143_JPRB,  1.32063_JPRB,  1.25652_JPRB, &
       &      1.17969_JPRB,  1.11374_JPRB, 0.986506_JPRB, 0.896953_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: ziceaggrc(nvalice) = &
       &  (/ -0.402978_JPRB, -0.993241_JPRB, -0.761126_JPRB, -0.510089_JPRB, -0.398976_JPRB, -0.560947_JPRB, &
       &     -0.801117_JPRB,  -1.17222_JPRB, -0.799439_JPRB, -0.543211_JPRB, -0.598025_JPRB, -0.851190_JPRB, &
       &      -1.16924_JPRB,  -1.39290_JPRB,  -1.40250_JPRB,  -1.38837_JPRB,  -1.39815_JPRB,  -1.38553_JPRB, &
       &      -1.35820_JPRB,  -1.32358_JPRB,  -1.29793_JPRB,  -1.19598_JPRB,  -1.10910_JPRB,  -1.09096_JPRB, &
       &      -1.07515_JPRB,  -1.08374_JPRB,  -1.07629_JPRB,  -1.06212_JPRB,  -1.02335_JPRB,  -1.02745_JPRB, &
       &      -1.19042_JPRB,  -1.38410_JPRB,  -1.55130_JPRB,  -1.55680_JPRB,  -1.40604_JPRB,  -1.38190_JPRB, &
       &      -1.40931_JPRB,  -1.33680_JPRB,  -1.31955_JPRB,  -1.38799_JPRB,  -1.45670_JPRB,  -1.58177_JPRB, &
       &      -1.67483_JPRB,  -1.78235_JPRB,  -1.80958_JPRB,  -1.90786_JPRB,  -1.99872_JPRB,  -2.06038_JPRB, &
       &      -2.06921_JPRB,  -2.04467_JPRB,  -1.96709_JPRB,  -1.87323_JPRB /)
  !
  Real(Kind=jprb),    Parameter  :: ziceaggrd(nvalice) = &
       &  (/ 1.268e-06_JPRB, -1.567e-05_JPRB, -3.467e-05_JPRB, -3.147e-05_JPRB, -3.023e-05_JPRB, &
       &    -3.283e-05_JPRB, -3.331e-05_JPRB, -2.502e-05_JPRB, -3.346e-05_JPRB, -3.312e-05_JPRB, &
       &    -3.254e-05_JPRB, -3.033e-05_JPRB, -2.041e-05_JPRB, -2.780e-06_JPRB, -2.405e-06_JPRB, &
       &    -8.877e-06_JPRB, -9.126e-06_JPRB, -1.115e-05_JPRB, -1.350e-05_JPRB, -1.542e-05_JPRB, &
       &    -1.731e-05_JPRB, -2.160e-05_JPRB, -2.077e-05_JPRB, -2.169e-05_JPRB, -2.191e-05_JPRB, &
       &    -2.180e-05_JPRB, -2.207e-05_JPRB, -2.286e-05_JPRB, -2.362e-05_JPRB, -2.300e-05_JPRB, &
       &    -1.709e-05_JPRB, -7.907e-06_JPRB,  8.274e-06_JPRB,  2.344e-05_JPRB,  2.856e-05_JPRB, &
       &     3.424e-05_JPRB,  3.794e-05_JPRB,  3.926e-05_JPRB,  4.080e-05_JPRB,  4.030e-05_JPRB, &
       &     3.980e-05_JPRB,  4.075e-05_JPRB,  3.888e-05_JPRB,  3.623e-05_JPRB,  3.382e-05_JPRB, &
       &     3.086e-05_JPRB,  2.682e-05_JPRB,  2.155e-05_JPRB,  1.606e-05_JPRB,  1.188e-05_JPRB, &
       &     2.402e-06_JPRB, -3.311e-06_JPRB /)

  !
  !1.8 Hard limits for control of input profile
  !--------------------------------------------
  ! Temperature
  Real(Kind=jprb), Parameter :: tmax   = 400.0_JPRB       ! degK
  Real(Kind=jprb), Parameter :: tmin   = 90.0_JPRB        ! degK
  ! Water Vapour
  Real(Kind=jprb), Parameter :: qmax   = 0.60E+06_JPRB    ! ppmv 0.373_JPRB kg/kg
  Real(Kind=jprb), Parameter :: qmin   = 0.00_JPRB        ! ppmv
  ! Ozone
  Real(Kind=jprb), Parameter :: o3max  = 1000.0_JPRB      ! ppmv  1.657E-3_JPRB kg/kg
  Real(Kind=jprb), Parameter :: o3min  = 0.0_JPRB         ! ppmv
  ! CO2
  Real(Kind=jprb), Parameter :: co2max = 1000.0_JPRB      ! ppmv
  Real(Kind=jprb), Parameter :: co2min = 0.0_JPRB         ! ppmv
  ! Cloud Liquid Water
  Real(Kind=jprb), Parameter :: clwmax = 1.0_JPRB         ! kg/kg
  Real(Kind=jprb), Parameter :: clwmin = 0.0_JPRB         ! kg/kg
  ! Surface Pressure
  Real(Kind=jprb), Parameter :: pmax   = 1100.0_JPRB      ! surface pressure hPa
  Real(Kind=jprb), Parameter :: pmin   = 400.0_JPRB       ! hPa
  ! Surface Wind
  Real(Kind=jprb), Parameter :: wmax   =  100.0_JPRB      ! surface wind speed (m/s)
  ! Zenith Angle
  Real(Kind=jprb), Parameter :: zenmax = 75.0_JPRB        ! zenith angle (Deg) = secant 3.86_JPRB
  ! Cloud Top Pressure
  Real(Kind=jprb), Parameter :: ctpmax = 1100.0_JPRB      ! (hPa)
  Real(Kind=jprb), Parameter :: ctpmin =   50.0_JPRB      ! (hPa)


  !1.9  Maximum Optical Depth
  !--------------------------
  ! maximum value of optical depth for transmittance calculation
  ! e(-30) -> 10**-14
  ! e(-50) -> 10**-22
  Real(Kind=jprb), Parameter  :: max_optical_depth = 50._JPRB

  !2 RTTOV7 aux parameters
  !-------------------------
  Integer(Kind=jpim), Parameter :: fastem_sp = 5  ! max. number of fastem surface parameters
  Integer(Kind=jpim), Parameter :: mwcldtop = 25  ! Upper level for lwp calcs
  Real(Kind=jprb), Parameter    :: pressure_top = 0.004985_JPRB ! Pressure of top level for
                                                ! Line/Line calculations (hPa)
  Real(Kind=jprb) , Dimension(8), Parameter :: dcoeff =      &  ! Debye coefs
       & (/ 17.1252_JPRB, 134.2450_JPRB, 310.2125_JPRB,  5.667_JPRB,   &
       &   188.7979_JPRB,  80.5419_JPRB,   0.1157_JPRB,  4.8417_JPRB/)

  !2.1 Polarisation definitions
  !----------------------------
  ! 1 = 0.5 (V+H)
  ! 2 = QV
  ! 3 = QH
  ! 4 = V
  ! 5 = H
  ! 6 = V , H
  ! 7 = Stokes (i.e. V , H , U, RHC)
  Integer(Kind=jpim), Dimension(7), Parameter :: npolar_compute = &
  & (/ 2, 2, 2, 1, 1, 2, 4/)
  Integer(Kind=jpim), Dimension(7), Parameter :: npolar_return = &
  & (/ 1, 1, 1, 1, 1, 2, 4/)
  Real(Kind=jprb), Parameter :: pol_v(3,5) = Reshape( &
  &  (/ 0.5_JPRB, 0.0_JPRB, 0.0_JPRB, &
  &     0.0_JPRB, 0.0_JPRB, 1.0_JPRB, &
  &     0.0_JPRB, 1.0_JPRB, 0.0_JPRB, &
  &     1.0_JPRB, 0.0_JPRB, 0.0_JPRB, &
  &     0.0_JPRB, 0.0_JPRB, 0.0_JPRB  /), (/3,5/) )
  Real(Kind=jprb), Parameter :: pol_h(3,5) = Reshape( &
  &  (/ 0.5_JPRB, 0.0_JPRB, 0.0_JPRB, &
  &     0.0_JPRB, 1.0_JPRB, 0.0_JPRB, &
  &     0.0_JPRB, 0.0_JPRB, 1.0_JPRB, &
  &     0.0_JPRB, 0.0_JPRB, 0.0_JPRB, &
  &     1.0_JPRB, 0.0_JPRB, 0.0_JPRB  /), (/3,5/) )

  !3 RTTOVSCATT aux parameters
  !---------------------------
  ! Minimum cloud cover processed by rttov_scatt
  Real(Kind=jprb),    Parameter  :: ccthres = 0.05_JPRB
  ! Variables for hemispheric integration
  Integer(Kind=jpim), Parameter :: nangle_scatt = 9*2                           ! Number of angles
  Real(Kind=jprb) , Dimension(nangle_scatt), Parameter :: mue_scatt     = &     ! cos of angles
         (/ 1.0000000000_JPRB, 0.9761055574_JPRB, 0.9206491853_JPRB, &
            0.8355935352_JPRB, 0.7236793292_JPRB, 0.5885048343_JPRB, &
            0.4344150369_JPRB, 0.2663626528_JPRB, 0.0897490935_JPRB, &
            1.0000000000_JPRB, 0.9761055574_JPRB, 0.9206491853_JPRB, &
            0.8355935352_JPRB, 0.7236793292_JPRB, 0.5885048343_JPRB, &
            0.4344150369_JPRB, 0.2663626528_JPRB, 0.0897490935_JPRB /)
  Real(Kind=jprb) , Dimension(nangle_scatt), Parameter :: weight_scatt  = &     ! weights of angles
         (/ 0.0065359477_JPRB, 0.0399706288_JPRB, 0.0706371669_JPRB, &
            0.0990162717_JPRB, 0.1242105331_JPRB, 0.1454119615_JPRB, &
            0.1619395172_JPRB, 0.1732621094_JPRB, 0.1790158634_JPRB, &
            0.0065359477_JPRB, 0.0399706288_JPRB, 0.0706371669_JPRB, &
            0.0990162717_JPRB, 0.1242105331_JPRB, 0.1454119615_JPRB, &
            0.1619395172_JPRB, 0.1732621094_JPRB, 0.1790158634_JPRB /)
  ! Rain density (g.cm-1)
  Real(Kind=jprb), Parameter :: rho_rain = 1.0_JPRB
  ! Snow density (g.cm-1)
  Real(Kind=jprb), Parameter :: rho_snow = 0.1_JPRB


End Module rttov_const
