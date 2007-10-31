Interface
!
Subroutine rttov_setup (&
     & errorstatus,     & ! out
     & Err_unit,        & ! in
     & verbosity_level, & ! in
     & ninst,           & ! in
     & coef,            & ! out
     & instrument,      & ! in
     & channels         ) ! in Optional
  Use rttov_const, Only :   &
       errorstatus_fatal

  Use rttov_types, Only : &
       rttov_coef

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  Integer(Kind=jpim), Intent (in) :: Err_Unit        ! Logical error unit (<0 for default)
  Integer(Kind=jpim), Intent (in) :: verbosity_level ! (<0 for default)
  Integer(Kind=jpim), Intent (in) :: ninst           ! number of RTTOV ids / instruments  requested
  Integer(Kind=jpim), Intent (in) :: instrument(:,:) ! Instrument triplet
         ! first dimension  : (platform, satellite identification, instrument) number
         ! second dimension : nsat
  Integer(Kind=jpim), Optional, Intent (in) :: channels(:,:)   ! list of channels to extract (channels,msat)

  Integer(Kind=jpim), Intent (out) :: errorstatus(ninst) ! return code
  Type( rttov_coef ), Intent (out) :: coef(ninst)        ! coefficients



End Subroutine rttov_setup
End Interface
