  program rttovscatt_test
  
  use parkind1, only: jpim  ,jprb
  
  Use rttov_types, only : rttov_coef, rttov_scatt_coef 
  Use rttov_const, only : inst_id_ssmi, inst_id_amsua, inst_id_amsub
       
  use mod_rttov_scatt_test, only: kproma, ioin, inproc, imyproc, iioproc, zenangle
  
  implicit none  
   
#include "rttov_readcoeffs.interface"	
#include "rttov_readscattcoeffs.interface"
#include "rttov_setupchan.interface"
#include "rttov_setupindex.interface"
#include "rttov_initcoeffs.interface"
#include "rttov_scatt_setupindex.interface"
#include "rttov_scatt_test.interface"

  integer (kind=jpim) :: nfrequencies, nchannels, nbtout, n_chan (kproma)
  real    (kind=jprb), allocatable :: emissivity    (:), surfem (:)
  integer (kind=jpim), allocatable :: polarisations (:,:)
  integer (kind=jpim), allocatable :: channels      (:)
  integer (kind=jpim), allocatable :: frequencies   (:)
  integer (kind=jpim), allocatable :: lprofiles     (:)
  integer (kind=jpim), allocatable :: lsprofiles    (:)   
  integer (kind=jpim), allocatable :: lsprofiles2    (:)   

  type (rttov_coef      ) :: coef_rttov        
  type (rttov_scatt_coef) :: coef_scatt
  
  integer (kind=jpim) :: errorstatus, instrument (3), i

!- End of header ------------------------------------------------------

!* Read satellite/instrument ID's
  read (*,*) instrument (1)
  read (*,*) instrument (2)
  read (*,*) instrument (3)
  read (*,*) zenangle
  
!* Read coefficients
  call rttov_readcoeffs      (errorstatus, coef_rttov, instrument, file_id = ioin)
  call rttov_initcoeffs      (errorstatus, coef_rttov, inproc, imyproc, iioproc)
  call rttov_readscattcoeffs (errorstatus, coef_rttov, coef_scatt)
  
  n_chan=coef_rttov%fmv_chn

 
!* in IFS called from /satrad/rttov/rttov_parm.F90 (called from /ifs/pp_obs/radtr.F90)
  call rttov_setupchan (&
     & kproma,          & ! in
     & n_chan,          & ! in
     & coef_rttov,      & ! in
     & nfrequencies,    & ! out
     & nchannels,       & ! out
     & nbtout)            ! out     

!* as in /satrad/rttov/rttov.F90     
  allocate (lprofiles     (nfrequencies))
  allocate (lsprofiles    (nchannels))
  allocate (lsprofiles2   (nbtout))
  allocate (channels      (nfrequencies))
  allocate (frequencies   (nchannels))
  allocate (polarisations (nchannels,3)) 
  allocate (surfem        (nchannels ) )
  allocate (emissivity    (nchannels ) )
  
  surfem (:) = 0.0_JPRB
 
   call rttov_setupindex (&
     & n_chan,           & ! in
     & kproma,           & ! in
     & nfrequencies,     & ! in
     & nchannels,        & ! in
     & nbtout,           & ! in
     & coef_rttov,       & ! in
     & surfem,           & ! in
     & lprofiles,        & ! out
     & channels,         & ! out
     & polarisations,    & ! out
     & emissivity)         ! out
     
!* Set up remaining indices
  call rttov_scatt_setupindex (kproma, n_chan, coef_rttov, nchannels, &
                             & lsprofiles, lsprofiles2              , & 
                             & frequencies, nbtout)       
     
  call rttov_scatt_test (nfrequencies, nchannels, nbtout, &       
                       & coef_rttov, coef_scatt         , &
                       & lprofiles                      , & 
                       & lsprofiles                     , & 
                        & lsprofiles2                     , & 
                      & channels                       , & 
                       & frequencies                    , & 
                       & polarisations                  , & 
                       & emissivity)       
 
  end program rttovscatt_test


