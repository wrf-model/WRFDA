!+ Parameters used in RTTOV-7 suite
!
MODULE MOD_CPARAM
  !
  !    This software was developed within the context of
  !    the EUMETSAT Satellite Application Facility on
  !    Numerical Weather Prediction (NWP SAF), under the
  !    Cooperation Agreement dated 25 November 1998, between
  !    EUMETSAT and the Met Office, UK, by one or more partners
  !    within the NWP SAF. The partners in the NWP SAF are
  !    the Met Office, ECMWF, KNMI and MeteoFrance.
  !
  !    Copyright 2002, EUMETSAT, All Rights Reserved.
  ! Description:
  ! Set up parameters that define the maximum dimension of some arrays
  !  used in the RTTOV suite.
  !
  ! Compatible with RTTOV8 library but only able to
  ! run with coefficients created on RTTOV7 43 pressure levels
  !
  ! Owner:
  ! SAF NWP
  !
  ! History:
  ! Version      Date        Comment
  ! 1            01/05/2000  Original code. .
  ! 2            21/08/2000  jpnssv increased from 1 to 6. Stephen English.
  ! 3            12/02/2001  fundamental constants added Roger Saunders
  ! 4            18/04/2001  new coefficient file Pascal Brunel
  ! 5            08/08/2001  updated to include AIRS Roger Saunders
  ! 6            20/09/2001  remove fileunit variable.  Andrew Collard.
  ! 7            01/12/2002  New F90 code with structures (P Brunel A Smith)
  !                               add pointer on coefficient structure
  !
  ! Code description:
  ! Language:              Fortran 90.
  ! Software Standards:    "European Standards for Writing and Documenting
  !                              Exchangeable Fortran 90 code".
  !
  ! Imported Type Definitions:
  use rttov_types, only : &  ! for coefficient structure
    rttov_coef

  Use parkind1, Only : jpim     ,jprb
  IMPLICIT NONE
  !----------------------------------------------------------------------
  ! Global parameters which users may want to edit to optimise
  ! for their application
  Integer(Kind=jpim), PARAMETER :: jppf   =  3       ! Max no. profiles per RTTOV call
  Integer(Kind=jpim), PARAMETER :: jpch   = 2378     ! Max. no. of channels
  Integer(Kind=jpim), PARAMETER :: jpchus = 324      ! Max. no. of channels computed/call
  Integer(Kind=jpim), PARAMETER :: jpnsat = 30       ! Max no sensors to be used
  !----------------------------------------------------------------------
  !Global parameters normally not changed by users
  Integer(Kind=jpim), PARAMETER :: jplev  = 43       ! No. of pressure levels
  Integer(Kind=jpim), PARAMETER :: jpnav  =  4       ! No. of profile variables
  Integer(Kind=jpim), PARAMETER :: jpnsav =  5       ! No. of surface air variables
  Integer(Kind=jpim), PARAMETER :: jpnssv =  6       ! No. of skin variables
  Integer(Kind=jpim), PARAMETER :: jpncv  =  2       ! No. of cloud variables
  Integer(Kind=jpim), PARAMETER :: jpchpf = jppf*jpchus ! Max no. of profs * chans used
  Integer(Kind=jpim), PARAMETER :: jpcofm = 15       ! Mixed gas coeffs (max
  Integer(Kind=jpim), PARAMETER :: jpcofw = 15       ! Water vapour coeffs (max
  Integer(Kind=jpim), PARAMETER :: jpcofo = 15       ! Ozone coeffs
  Integer(Kind=jpim), PARAMETER :: jpst   = 10       ! Max no. of surface types
  Integer(Kind=jpim), PARAMETER :: iu1    = 10       ! Default Unit for rt coeff files
  Integer(Kind=jpim), PARAMETER :: nulout = 0        ! Unit for error messages
  Integer(Kind=jpim), PARAMETER :: jmwcldtop = 25    ! Upper level for lwp calcs

  Integer(Kind=jpim), parameter :: jpplat=15         ! No of platforms
  Integer(Kind=jpim), parameter :: jpinst=30         ! No of instruments (starting at 0)
  Integer(Kind=jpim), Parameter :: jpgas=3           ! No of different gases
  !
  !
  ! fundamental constants
  Real(Kind=jprb), PARAMETER :: pi   = 3.1415926535_JPRB
  Real(Kind=jprb), PARAMETER :: deg2rad = PI/180.0_JPRB        ! Degrees to radians conversion factor
  Real(Kind=jprb), PARAMETER :: rcnv = 6.03504E+5_JPRB          ! kg/kg--> ppmv ozone
  Real(Kind=jprb), PARAMETER :: rcnw = 1.60771704E+6_JPRB       ! kg/kg--> ppmv water vapour
  Real(Kind=jprb), PARAMETER :: gravity = 9.81_JPRB            ! m/s^2
  Real(Kind=jprb), PARAMETER :: speedl = 29979245800.0_JPRB    ! speed of light cm/sec
  Real(Kind=jprb), PARAMETER :: plcon1 = 1.1910659E-05_JPRB    ! first plank constant mW/(m^2._JPRBster.cm^-4)
  Real(Kind=jprb), PARAMETER :: plcon2 = 1.438833_JPRB         ! second plank constant K/cm^-1
  !
  Real(Kind=jprb), Parameter :: q_mixratio_to_ppmv  = 1.60771704e+6_JPRB
  Real(Kind=jprb), Parameter :: q_ppmv_to_mixratio  = 1.0_JPRB/1.60771704e+6_JPRB
  Real(Kind=jprb), Parameter :: o3_mixratio_to_ppmv = 6.03504e+5_JPRB
  Real(Kind=jprb), Parameter :: o3_ppmv_to_mixratio = 1.0_JPRB/6.03504e+5_JPRB
  !
  ! Module arguments:

  ! Global scalars:F
  Integer(Kind=jpim) :: njpnsat     ! Total max sats to be used
  Integer(Kind=jpim) :: njplev      ! No. of pressure levels
  Integer(Kind=jpim) :: njpnav      ! No. of profile variables
  Integer(Kind=jpim) :: njpnsav     ! No. of surface air variables
  Integer(Kind=jpim) :: njpnssv     ! No. of skin variables
  Integer(Kind=jpim) :: njpncv      ! No. of cloud variables
  Integer(Kind=jpim) :: njppf       ! Max no. profiles
  Integer(Kind=jpim) :: njpch       ! Max. no. of tovs channels
  Integer(Kind=jpim) :: njpchus     ! Max. no. of channels used tovs
  Integer(Kind=jpim) :: njpchpf     ! Max no. of profs * chans used
  Integer(Kind=jpim) :: njpcofm     ! Mixed gas coeffs (max)
  Integer(Kind=jpim) :: njpcofw     ! Water vapour coeffs (max)
  Integer(Kind=jpim) :: njpcofo     ! Ozone coeffs (max)
  Integer(Kind=jpim) :: njpst       ! Max no. of surface types
  Integer(Kind=jpim) :: nmwcldtop   ! Upper level for lwp calcs

  ! list of allowed platforms
  Character (len=8), Dimension(jpplat) :: platform_name =                &
       & (/ 'noaa    ', 'dmsp    ', 'meteosat', 'goes    ', 'gms     ',  &
       &    'fy2     ', 'trmm    ', 'ers     ', 'eos     ', 'metop   ',  &
       &    'envisat ', 'msg     ', 'fy1     ', 'xxxxxxxx', 'xxxxxxxx'   /)

  ! List of instruments  !!!! HIRS is number 0
  Character (len=8), Dimension(0:jpinst-1) :: inst_name  =                &
       & (/ 'hirs    ', 'msu     ', 'ssu     ', 'amsua   ', 'amsub   ',  &
       &    'avhrr   ', 'ssmi    ', 'vtpr1   ', 'vtpr2   ', 'tmi     ',  &
       &    'ssmis   ', 'airs    ', 'hsb     ', 'modis   ', 'atsr    ',  &
       &    'mhs     ', 'iasi    ', 'xxxxxxxx', 'xxxxxxxx', 'xxxxxxxx',  &
       &    'mviri   ', 'seviri  ', 'imager  ', 'sounder ', 'imager  ',  &
       &    'vissr   ', 'mvisr   ', 'xxxxxxxx', 'xxxxxxxx', 'xxxxxxxx'   /)

  ! List of gases
  Character (len=16), Dimension(jpgas) :: gas_name =    &
       & (/ 'mixed_gases     ',&
       &    'water_vapour    ',&
       &    'ozone           ' /)
  ! End of module arguments:

  type( rttov_coef ), target :: coef(jpnsat)         ! coefficients
  !-----End of header----------------------------------------------------


END MODULE MOD_CPARAM
