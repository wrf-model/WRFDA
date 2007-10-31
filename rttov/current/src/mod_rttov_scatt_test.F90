  module mod_rttov_scatt_test  
  
  Use parkind1, only: jpim  ,jprb
  
  Use rttov_types, only : rttov_coef, rttov_scatt_coef 

  IMPLICIT NONE

  integer (kind=jpim), parameter :: nulout = 5
  integer (kind=jpim), parameter :: nulerr = 6
  integer (kind=jpim), parameter :: kproma = 3
  integer (kind=jpim), parameter :: kflevg = 60
  
  integer (kind=jpim), parameter :: ioin  = 10
  integer (kind=jpim), parameter :: ioout = 20
  
  integer (kind=jpim), parameter :: inproc  = 1
  integer (kind=jpim), parameter :: imyproc = 1
  integer (kind=jpim), parameter :: iioproc = 1
  
  
!* from module /satrad/module/onedvar_const.F90
  real    (kind=jprb), parameter :: fastem_land_coeff (5) = (/ 3.0_JPRB, 5.0_JPRB, 15.0_JPRB, 0.1_JPRB, 0.3_JPRB /)
  real    (kind=jprb), parameter :: fastem_ocean          = 0.0_JPRB
  
  real    (kind=jprb) :: zenangle
  
  end module mod_rttov_scatt_test
