    subroutine rttov_scatt_test (nfrequencies, nchannels, nbtout, coef_rttov, coef_scatt, &
                               & lprofiles                      , & 
                               & lsprofiles                     , & 
                               & lsprofiles2                    , & 
                               & channels                       , & 
                               & frequencies                    , & 
                               & polarisations                  , & 
                               & emissivity)       

    Use mod_rttov_scatt_test

    Use rttov_const, only :   &
     &  errorstatus_fatal,    &
     &  errorstatus_success,  &
     &  default_err_unit,     &
     &  sensor_id_mw,         &
     &  npolar_return,        &
     &  npolar_compute,       &
     &  fastem_sp             ,&
     &  inst_id_ssmi         ,&
     &  inst_id_amsua        ,&
     &  inst_id_amsub         
       
    Use mod_cparam, only :   &
     &  q_mixratio_to_ppmv

    Use rttov_types, only :   &
     &  geometry_type        ,&
     &  rttov_coef           ,&
     &  rttov_scatt_coef     ,&
     &  profile_type         ,&
     &  profile_cloud_type   ,&
     &  transmission_type    ,&
     &  radiance_cloud_type  ,&
     &  profile_scatt_aux   
     
     
    Use parkind1, only: jpim  ,jprb

    IMPLICIT NONE
  
#include "rttov_intex.interface"
#include "rttov_setpressure.interface"
#include "rttov_scatt.interface"
#include "rttov_scatt_tl.interface"
#include "rttov_scatt_ad.interface"
#include "rttov_scatt_k.interface"

    integer (kind=jpim), intent (in) :: nfrequencies, nchannels, nbtout
    real    (kind=jprb), intent (in)   , dimension (nchannels)    :: emissivity 
    integer (kind=jpim), intent (in)   , dimension (nchannels,3)  :: polarisations
    integer (kind=jpim), intent (in)   , dimension (nfrequencies) :: channels    
    integer (kind=jpim), intent (in)   , dimension (nchannels)    :: frequencies    
    integer (kind=jpim), intent (in)   , dimension (nfrequencies) :: lprofiles    
    integer (kind=jpim), intent (in)   , dimension (nchannels)    :: lsprofiles    
    integer (kind=jpim), intent (in)   , dimension (nbtout)    :: lsprofiles2    

    type (rttov_coef      ), intent (inout) :: coef_rttov        
    type (rttov_scatt_coef), intent (inout) :: coef_scatt  

!* FORWARD   
    type (profile_type)         :: profiles_d1     (kproma)
    type (profile_cloud_type)   :: cld_profiles_d1 (kproma)
    type (radiance_cloud_type)  :: radiance_d1  
               
    real (kind=jprb), dimension (nchannels) :: emissivity_d1 

    real (kind=jprb), target :: p1__p   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: p1__t   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: p1__q   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: p1__o3  (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: p1__clw (coef_rttov%nlevels,kproma)

    real (kind=jprb), target :: cp1__p    (kflevg,kproma)
    real (kind=jprb), target :: cp1__ph   (kflevg+1,kproma)
    real (kind=jprb), target :: cp1__t    (kflevg,kproma)
    real (kind=jprb), target :: cp1__q    (kflevg,kproma)
    real (kind=jprb), target :: cp1__cc   (kflevg,kproma)
    real (kind=jprb), target :: cp1__clw  (kflevg,kproma)
    real (kind=jprb), target :: cp1__ciw  (kflevg,kproma)
    real (kind=jprb), target :: cp1__rain (kflevg,kproma)
    real (kind=jprb), target :: cp1__sp   (kflevg,kproma)

    real (kind=jprb), target :: r1__clear_out (nbtout)
    real (kind=jprb), target :: r1__out       (nbtout)
    real (kind=jprb), target :: r1__out_clear (nbtout)
    real (kind=jprb), target :: r1__total_out (nbtout)
    real (kind=jprb), target :: r1__clear     (nchannels)
    real (kind=jprb), target :: r1__cloudy    (nchannels)
    real (kind=jprb), target :: r1__total     (nchannels)
    real (kind=jprb), target :: r1__bt        (nchannels)
    real (kind=jprb), target :: r1__bt_clear  (nchannels)
    real (kind=jprb), target :: r1__upclear   (nchannels)
    real (kind=jprb), target :: r1__dnclear   (nchannels)
    real (kind=jprb), target :: r1__reflclear (nchannels)
    real (kind=jprb), target :: r1__freq_used (nchannels)
    real (kind=jprb), target :: r1__overcast  (kflevg,nchannels)
    real (Kind=jprb), target :: r1__downcld   (kflevg,nchannels)
    
!* TL   
    type (profile_type)              :: profiles_d2     (kproma)
    type (profile_type)              :: profiles_tl     (kproma)
    type (profile_type)              :: profiles_tl2     (kproma)
    type (profile_cloud_type)        :: cld_profiles_d2 (kproma)
    type (profile_cloud_type)        :: cld_profiles_tl (kproma)
    type (profile_cloud_type)        :: cld_profiles_tl2 (kproma)
    type (radiance_cloud_type)       :: radiance_d2             
    type (radiance_cloud_type)       :: radiance_d3             
    type (radiance_cloud_type)       :: radiance_tl  
    type (radiance_cloud_type)       :: radiance_tl2  
           
    real (kind=jprb), dimension (nchannels) :: emissivity_d2 
    real (kind=jprb), dimension (nchannels) :: emissivity_tl 
    real (kind=jprb), dimension (nchannels) :: emissivity_tl2 

    real (kind=jprb), target :: p2__p   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: p2__t   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: p2__q   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: p2__o3  (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: p2__clw (coef_rttov%nlevels,kproma)

    real (kind=jprb), target :: cp2__p    (kflevg,kproma)
    real (kind=jprb), target :: cp2__ph   (kflevg+1,kproma)
    real (kind=jprb), target :: cp2__t    (kflevg,kproma)
    real (kind=jprb), target :: cp2__q    (kflevg,kproma)
    real (kind=jprb), target :: cp2__cc   (kflevg,kproma)
    real (kind=jprb), target :: cp2__clw  (kflevg,kproma)
    real (kind=jprb), target :: cp2__ciw  (kflevg,kproma)
    real (kind=jprb), target :: cp2__rain (kflevg,kproma)
    real (kind=jprb), target :: cp2__sp   (kflevg,kproma)

    real (kind=jprb), target :: ptl__p   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: ptl__t   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: ptl__q   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: ptl__o3  (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: ptl__clw (coef_rttov%nlevels,kproma)

    real (kind=jprb), target :: cptl__p    (kflevg,kproma)
    real (kind=jprb), target :: cptl__ph   (kflevg+1,kproma)
    real (kind=jprb), target :: cptl__t    (kflevg,kproma)
    real (kind=jprb), target :: cptl__q    (kflevg,kproma)
    real (kind=jprb), target :: cptl__cc   (kflevg,kproma)
    real (kind=jprb), target :: cptl__clw  (kflevg,kproma)
    real (kind=jprb), target :: cptl__ciw  (kflevg,kproma)
    real (kind=jprb), target :: cptl__rain (kflevg,kproma)
    real (kind=jprb), target :: cptl__sp   (kflevg,kproma)
    
    real (kind=jprb), target :: ptl2__p   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: ptl2__t   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: ptl2__q   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: ptl2__o3  (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: ptl2__clw (coef_rttov%nlevels,kproma)

    real (kind=jprb), target :: cptl2__p    (kflevg,kproma)
    real (kind=jprb), target :: cptl2__ph   (kflevg+1,kproma)
    real (kind=jprb), target :: cptl2__t    (kflevg,kproma)
    real (kind=jprb), target :: cptl2__q    (kflevg,kproma)
    real (kind=jprb), target :: cptl2__cc   (kflevg,kproma)
    real (kind=jprb), target :: cptl2__clw  (kflevg,kproma)
    real (kind=jprb), target :: cptl2__ciw  (kflevg,kproma)
    real (kind=jprb), target :: cptl2__rain (kflevg,kproma)
    real (kind=jprb), target :: cptl2__sp   (kflevg,kproma)
   
    real (kind=jprb), target :: r2__clear_out (nbtout)
    real (kind=jprb), target :: r2__out       (nbtout)
    real (kind=jprb), target :: r2__out_clear (nbtout)
    real (kind=jprb), target :: r2__total_out (nbtout)
    real (kind=jprb), target :: r2__clear     (nchannels)
    real (kind=jprb), target :: r2__cloudy    (nchannels)
    real (kind=jprb), target :: r2__total     (nchannels)
    real (kind=jprb), target :: r2__bt        (nchannels)
    real (kind=jprb), target :: r2__bt_clear  (nchannels)
    real (kind=jprb), target :: r2__upclear   (nchannels)
    real (kind=jprb), target :: r2__dnclear   (nchannels)
    real (kind=jprb), target :: r2__reflclear (nchannels)
    real (kind=jprb), target :: r2__freq_used (nchannels)
    real (kind=jprb), target :: r2__overcast  (kflevg,nchannels)
    real (Kind=jprb), target :: r2__downcld   (kflevg,nchannels)
    
    real (kind=jprb), target :: rtl__clear_out (nbtout)
    real (kind=jprb), target :: rtl__out       (nbtout)
    real (kind=jprb), target :: rtl__out_clear (nbtout)
    real (kind=jprb), target :: rtl__total_out (nbtout)
    real (kind=jprb), target :: rtl__clear     (nchannels)
    real (kind=jprb), target :: rtl__cloudy    (nchannels)
    real (kind=jprb), target :: rtl__total     (nchannels)
    real (kind=jprb), target :: rtl__bt        (nchannels)
    real (kind=jprb), target :: rtl__bt_clear  (nchannels)
    real (kind=jprb), target :: rtl__upclear   (nchannels)
    real (kind=jprb), target :: rtl__dnclear   (nchannels)
    real (kind=jprb), target :: rtl__reflclear (nchannels)
    real (kind=jprb), target :: rtl__freq_used (nchannels)
    real (kind=jprb), target :: rtl__overcast  (kflevg,nchannels)
    real (Kind=jprb), target :: rtl__downcld   (kflevg,nchannels)
    
    real (kind=jprb), target :: rtl2__clear_out (nbtout)
    real (kind=jprb), target :: rtl2__out       (nbtout)
    real (kind=jprb), target :: rtl2__out_clear (nbtout)
    real (kind=jprb), target :: rtl2__total_out (nbtout)
    real (kind=jprb), target :: rtl2__clear     (nchannels)
    real (kind=jprb), target :: rtl2__cloudy    (nchannels)
    real (kind=jprb), target :: rtl2__total     (nchannels)
    real (kind=jprb), target :: rtl2__bt        (nchannels)
    real (kind=jprb), target :: rtl2__bt_clear  (nchannels)
    real (kind=jprb), target :: rtl2__upclear   (nchannels)
    real (kind=jprb), target :: rtl2__dnclear   (nchannels)
    real (kind=jprb), target :: rtl2__reflclear (nchannels)
    real (kind=jprb), target :: rtl2__freq_used (nchannels)
    real (kind=jprb), target :: rtl2__overcast  (kflevg,nchannels)
    real (Kind=jprb), target :: rtl2__downcld   (kflevg,nchannels)
    
    real (kind=jprb), target :: r3__clear_out (nbtout)
    real (kind=jprb), target :: r3__out       (nbtout)
    real (kind=jprb), target :: r3__out_clear (nbtout)
    real (kind=jprb), target :: r3__total_out (nbtout)
    real (kind=jprb), target :: r3__clear     (nchannels)
    real (kind=jprb), target :: r3__cloudy    (nchannels)
    real (kind=jprb), target :: r3__total     (nchannels)
    real (kind=jprb), target :: r3__bt        (nchannels)
    real (kind=jprb), target :: r3__bt_clear  (nchannels)
    real (kind=jprb), target :: r3__upclear   (nchannels)
    real (kind=jprb), target :: r3__dnclear   (nchannels)
    real (kind=jprb), target :: r3__reflclear (nchannels)
    real (kind=jprb), target :: r3__freq_used (nchannels)
    real (kind=jprb), target :: r3__overcast  (kflevg,nchannels)
    real (Kind=jprb), target :: r3__downcld   (kflevg,nchannels)
    
!* AD   
    type (profile_type)              :: profiles_ad     (kproma)
    type (profile_cloud_type)        :: cld_profiles_ad (kproma)
    type (radiance_cloud_type)       :: radiance_ad  
               
    type (profile_type)              :: profiles_ad2     (kproma)
    type (profile_cloud_type)        :: cld_profiles_ad2 (kproma)
    type (radiance_cloud_type)       :: radiance_ad2  
 
    real (kind=jprb), dimension (nchannels) :: emissivity_ad 
    
    real (kind=jprb), target :: pad__p   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: pad__t   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: pad__q   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: pad__o3  (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: pad__clw (coef_rttov%nlevels,kproma)

    real (kind=jprb), target :: cpad__p    (kflevg,kproma)
    real (kind=jprb), target :: cpad__ph   (kflevg+1,kproma)
    real (kind=jprb), target :: cpad__t    (kflevg,kproma)
    real (kind=jprb), target :: cpad__q    (kflevg,kproma)
    real (kind=jprb), target :: cpad__cc   (kflevg,kproma)
    real (kind=jprb), target :: cpad__clw  (kflevg,kproma)
    real (kind=jprb), target :: cpad__ciw  (kflevg,kproma)
    real (kind=jprb), target :: cpad__rain (kflevg,kproma)
    real (kind=jprb), target :: cpad__sp   (kflevg,kproma)

    real (kind=jprb), target :: rad__clear_out (nbtout)
    real (kind=jprb), target :: rad__out       (nbtout)
    real (kind=jprb), target :: rad__out_clear (nbtout)
    real (kind=jprb), target :: rad__total_out (nbtout)
    real (kind=jprb), target :: rad__clear     (nchannels)
    real (kind=jprb), target :: rad__cloudy    (nchannels)
    real (kind=jprb), target :: rad__total     (nchannels)
    real (kind=jprb), target :: rad__bt        (nchannels)
    real (kind=jprb), target :: rad__bt_clear  (nchannels)
    real (kind=jprb), target :: rad__upclear   (nchannels)
    real (kind=jprb), target :: rad__dnclear   (nchannels)
    real (kind=jprb), target :: rad__reflclear (nchannels)
    real (kind=jprb), target :: rad__freq_used (nchannels)
    real (kind=jprb), target :: rad__overcast  (kflevg,nchannels)
    real (Kind=jprb), target :: rad__downcld   (kflevg,nchannels)
    
    real (kind=jprb), target :: pad2__p   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: pad2__t   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: pad2__q   (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: pad2__o3  (coef_rttov%nlevels,kproma)
    real (kind=jprb), target :: pad2__clw (coef_rttov%nlevels,kproma)

    real (kind=jprb), target :: cpad2__p    (kflevg,kproma)
    real (kind=jprb), target :: cpad2__ph   (kflevg+1,kproma)
    real (kind=jprb), target :: cpad2__t    (kflevg,kproma)
    real (kind=jprb), target :: cpad2__q    (kflevg,kproma)
    real (kind=jprb), target :: cpad2__cc   (kflevg,kproma)
    real (kind=jprb), target :: cpad2__clw  (kflevg,kproma)
    real (kind=jprb), target :: cpad2__ciw  (kflevg,kproma)
    real (kind=jprb), target :: cpad2__rain (kflevg,kproma)
    real (kind=jprb), target :: cpad2__sp   (kflevg,kproma)

    real (kind=jprb), target :: rad2__clear_out (nbtout)
    real (kind=jprb), target :: rad2__out       (nbtout)
    real (kind=jprb), target :: rad2__out_clear (nbtout)
    real (kind=jprb), target :: rad2__total_out (nbtout)
    real (kind=jprb), target :: rad2__clear     (nchannels)
    real (kind=jprb), target :: rad2__cloudy    (nchannels)
    real (kind=jprb), target :: rad2__total     (nchannels)
    real (kind=jprb), target :: rad2__bt        (nchannels)
    real (kind=jprb), target :: rad2__bt_clear  (nchannels)
    real (kind=jprb), target :: rad2__upclear   (nchannels)
    real (kind=jprb), target :: rad2__dnclear   (nchannels)
    real (kind=jprb), target :: rad2__reflclear (nchannels)
    real (kind=jprb), target :: rad2__freq_used (nchannels)
    real (kind=jprb), target :: rad2__overcast  (kflevg,nchannels)
    real (Kind=jprb), target :: rad2__downcld   (kflevg,nchannels)

    real (kind=jprb), dimension (nchannels) :: emissivity_ad2 
    
    
!* K   
    type (profile_type)              :: profiles_k     (nbtout)
    type (profile_cloud_type)        :: cld_profiles_k (nbtout)
               
    real (kind=jprb), dimension (nchannels) :: emissivity_k
    
    real (kind=jprb), target :: pk__p   (coef_rttov%nlevels,nbtout)
    real (kind=jprb), target :: pk__t   (coef_rttov%nlevels,nbtout)
    real (kind=jprb), target :: pk__q   (coef_rttov%nlevels,nbtout)
    real (kind=jprb), target :: pk__o3  (coef_rttov%nlevels,nbtout)
    real (kind=jprb), target :: pk__clw (coef_rttov%nlevels,nbtout)

    real (kind=jprb), target :: cpk__p    (kflevg,nbtout)
    real (kind=jprb), target :: cpk__ph   (kflevg+1,nbtout)
    real (kind=jprb), target :: cpk__t    (kflevg,nbtout)
    real (kind=jprb), target :: cpk__q    (kflevg,nbtout)
    real (kind=jprb), target :: cpk__cc   (kflevg,nbtout)
    real (kind=jprb), target :: cpk__clw  (kflevg,nbtout)
    real (kind=jprb), target :: cpk__ciw  (kflevg,nbtout)
    real (kind=jprb), target :: cpk__rain (kflevg,nbtout)
    real (kind=jprb), target :: cpk__sp   (kflevg,nbtout)

!* OTHER                      
    logical :: calcemiss (nchannels)

    integer (kind=jpim) :: nchan_act
    integer (kind=jpim) :: errorstatus (kproma)
    integer (kind=jpim) :: i_lev, i_proma, i_chan, ibtout, i_btout, i_lambda, i_fast 
  
    real    (kind=jprb) :: t_2m, q_2m, td_2m, u_10m, v_10m, ls, p_sfc, t_sfc, rlat, rlon
    real    (kind=jprb) :: lambda, epsilon, zdelta1, zdelta2, zdelta3, threshold, z
    real    (kind=jprb), dimension (kflevg) :: q_ppmv_d1, q_ppmv_d2, q_ppmv_tl
    Real    (Kind=jprb)       :: ratio(2)
    Real(Kind=jprb),    Allocatable :: radiance_total_ref (:)

  !- End of header ------------------------------------------------------

  threshold = 1.0E-09_JPRB 
  
!* FORWARD-MODEL TEST ***********************************************************************************
!* Set-up
    errorstatus = errorstatus_success
    emissivity_d1 (1:nchannels) = emissivity    (1:nchannels)
    calcemiss     (1:nchannels) = emissivity_d1 (1:nchannels) < 0.01_JPRB
    
!*  RTTOV/RTTOVSCATT arrays
    do i_proma = 1, kproma
       profiles_d1 (i_proma) % nlevels =  coef_rttov % nlevels 
       profiles_d1 (i_proma) % p   => p1__p   (:,i_proma)  
       profiles_d1 (i_proma) % t   => p1__t   (:,i_proma) 
       profiles_d1 (i_proma) % q   => p1__q   (:,i_proma) 
       profiles_d1 (i_proma) % o3  => p1__o3  (:,i_proma) 
       profiles_d1 (i_proma) % clw => p1__clw (:,i_proma)
    enddo

    do i_proma = 1, kproma
       cld_profiles_d1 (i_proma) % nlevels = kflevg 
       cld_profiles_d1 (i_proma) % p   => cp1__p    (:,i_proma)
       cld_profiles_d1 (i_proma) % ph  => cp1__ph   (:,i_proma)
       cld_profiles_d1 (i_proma) % t   => cp1__t    (:,i_proma)
       cld_profiles_d1 (i_proma) % q   => cp1__q    (:,i_proma)
       cld_profiles_d1 (i_proma) % cc  => cp1__cc   (:,i_proma)
       cld_profiles_d1 (i_proma) % clw => cp1__clw  (:,i_proma)
       cld_profiles_d1 (i_proma) % ciw => cp1__ciw  (:,i_proma)
       cld_profiles_d1 (i_proma) % rain=> cp1__rain (:,i_proma)
       cld_profiles_d1 (i_proma) % sp  => cp1__sp   (:,i_proma)
    enddo
       
    radiance_d1 % overcast    => r1__overcast
    radiance_d1 % clear_out   => r1__clear_out
    radiance_d1 % out         => r1__out
    radiance_d1 % out_clear   => r1__out_clear
    radiance_d1 % total_out   => r1__total_out
    radiance_d1 % clear       => r1__clear
    radiance_d1 % cloudy      => r1__cloudy
    radiance_d1 % total       => r1__total
    radiance_d1 % bt          => r1__bt
    radiance_d1 % bt_clear    => r1__bt_clear
    radiance_d1 % upclear     => r1__upclear
    radiance_d1 % dnclear     => r1__dnclear
    radiance_d1 % reflclear   => r1__reflclear
    radiance_d1 % downcld     => r1__downcld
   
    Allocate ( radiance_total_ref  ( nchannels ) )

!* Read profiles
    open (ioin,  file = '../data/profiles2_fmt', status = 'old')
    open (ioout, file='outputscatt.ascii',form='formatted')
  
    do i_proma = 1, kproma
!* Surface
      read (ioin,'(10e16.6)') rlon,   &         ! longitude (deg)
                            & rlat,   &         ! latitude (deg)
                            & ls,     &         ! land-sea mask (1=land)
                            & t_sfc,  &         ! surface temperature (K)
                            & p_sfc,  &         ! surface pressure (Pa)
                            & t_2m,   &         ! 2-meter temperature (K)
                            & q_2m,   &         ! 2-meter specific humidity (kg/kg)
                            & u_10m,  &         ! 10-meter wind u (m/s)
                            & v_10m             ! 10-meter wind u (m/s)
!* Profile    
      read (ioin,'(10e16.6)') cld_profiles_d1 (i_proma) % t    (1:kflevg)         ! temperature (K)
      read (ioin,'(10e16.6)') cld_profiles_d1 (i_proma) % q    (1:kflevg)         ! specific humidity (kg/kg)
      read (ioin,'(10e16.6)') cld_profiles_d1 (i_proma) % cc   (1:kflevg)         ! cloud cover
      read (ioin,'(10e16.6)') cld_profiles_d1 (i_proma) % clw  (1:kflevg)         ! liquid water (kg/kg)
      read (ioin,'(10e16.6)') cld_profiles_d1 (i_proma) % ciw  (1:kflevg)         ! ice water (kg/kg)
      read (ioin,'(10e16.6)') cld_profiles_d1 (i_proma) % rain (1:kflevg)         ! rain (kg/m2/s)
      read (ioin,'(10e16.6)') cld_profiles_d1 (i_proma) % sp   (1:kflevg)         ! solid precipitation (kg/m2/s)
      
      call rttov_setpressure (p_sfc, cld_profiles_d1 (i_proma) % p, cld_profiles_d1 (i_proma) % ph)
      
      cld_profiles_d1 (i_proma) % p  (:) = cld_profiles_d1 (i_proma) % p  (:) * 0.01_JPRB
      cld_profiles_d1 (i_proma) % ph (:) = cld_profiles_d1 (i_proma) % ph (:) * 0.01_JPRB

!* Fill in RTTOV/RTTOVSCATT arrays once per profile
      q_2m = q_2m / (1.0_JPRB - q_2m)
       
      profiles_d1 (i_proma) % p   (:) = coef_rttov % ref_prfl_p (:)
      profiles_d1 (i_proma) % clw (:) = 0.0_JPRB   ! warning
      profiles_d1 (i_proma) % o3  (:) = 1.0e-7_JPRB   ! warning
      profiles_d1 (i_proma) % s2m % p = p_sfc / 100.0_JPRB
      profiles_d1 (i_proma) % s2m % q = q_2m * q_mixratio_to_ppmv
      profiles_d1 (i_proma) % s2m % o = 0.0_JPRB
      profiles_d1 (i_proma) % s2m % t = t_2m  
      profiles_d1 (i_proma) % s2m % u = u_10m 
      profiles_d1 (i_proma) % s2m % v = v_10m 
      profiles_d1 (i_proma) % skin % surftype  = int (1.0_JPRB - ls)   
      profiles_d1 (i_proma) % skin % t         = t_sfc 
      profiles_d1 (i_proma) % skin % fastem(:) = fastem_land_coeff (:)

      profiles_d1 (i_proma) % ozone_data = .false.     ! warning
      profiles_d1 (i_proma) % co2_data   = .false.
      profiles_d1 (i_proma) % clw_data   = .false.
      profiles_d1 (i_proma) % zenangle   = zenangle
      profiles_d1 (i_proma) % azangle    = 0.0_JPRB    ! default value
      profiles_d1 (i_proma) % ctp        = 500.0_JPRB  ! default value
      profiles_d1 (i_proma) % cfraction  =   0.0_JPRB  ! default value
    
!* Convert kg/kg to ppmv
      q_ppmv_d1 (1:kflevg) = cld_profiles_d1 (i_proma) % q (1:kflevg) * q_mixratio_to_ppmv

! Interpolate input profile to rttov pressure levels
      call rttov_intex (kflevg                           , &                        
                     &  coef_rttov%nlevels               , &             
                     &  cld_profiles_d1 (i_proma) % p    , &   
                     &  profiles_d1     (i_proma) % p    , &   
                     &  cld_profiles_d1 (i_proma) % t    , &    
                     &  profiles_d1     (i_proma) % t)
                          
      call rttov_intex (kflevg                           , &
                     &  coef_rttov%nlevels               , &
                     &  cld_profiles_d1 (i_proma) % p    , &
                     &  profiles_d1     (i_proma) % p    , &
                     &  q_ppmv_d1       (1:kflevg)       , &
                     &  profiles_d1     (i_proma) % q)
   end do  
   close (ioin)  

!* Reference forward model run
   call rttov_scatt (errorstatus,        & ! out 
                   & kflevg,             & ! in
                   & coef_rttov%nlevels, & ! in
                   & nfrequencies,       & ! in
                   & nchannels,          & ! in
                   & nbtout,             & ! in
                   & kproma,             & ! in
                   & polarisations,      & ! in
                   & channels,           & ! in
                   & frequencies,        & ! in
                   & lprofiles,          & ! in
                   & lsprofiles,         & ! in
                   & profiles_d1,        & ! inout  
                   & cld_profiles_d1,    & ! in
                   & coef_rttov,         & ! in
                   & coef_scatt,         & ! in
                   & calcemiss,          & ! in
                   & emissivity_d1,      & ! inout
                   & radiance_d1 )         ! inout 

  ! main output:
  ! radiance_d1%total_out  = cloud-affected radiances
  ! radiance_d1%clear_out  = clear-sky radiances
  ! radiance_d1%out        = cloud-affected Tbs
  ! radiance_d1%out_clear  = clear-sky Tbs
  
  write(*,*) 'nfreq ', nfrequencies, 'nchan ', nchannels, 'nbtout ', nbtout

  write(ioout,*) 'This dataset is made of ',kproma,' ECMWF model profiles'
  write(ioout,*)
  write(ioout,*) 'Call to RTTOV_SCATT'
  write(ioout,*) '-------------------'
  write(ioout,*)
  write(ioout,*) 'Channel  cloudy Tb '

   do i_chan = 1, nbtout
      write (ioout,'(i4,3x,30e23.16)') i_chan, radiance_d1 % out (i_chan)
   enddo
  
!* TANGENT-LINEAR TEST ***********************************************************************************

  write(ioout,*)
  write(ioout,*) 'Test TL'
  write(ioout,*) '-------'
  write(ioout,*)

   epsilon = 0.01_JPRB

!* RTTOV/RTTOVSCATT arrays
   do i_proma = 1, kproma
      profiles_tl (i_proma) % nlevels =  coef_rttov % nlevels 
      profiles_tl (i_proma) % p   => ptl__p   (:,i_proma)  
      profiles_tl (i_proma) % t   => ptl__t   (:,i_proma) 
      profiles_tl (i_proma) % q   => ptl__q   (:,i_proma) 
      profiles_tl (i_proma) % o3  => ptl__o3  (:,i_proma) 
      profiles_tl (i_proma) % clw => ptl__clw (:,i_proma)
      
      cld_profiles_tl (i_proma) % nlevels = kflevg 
      cld_profiles_tl (i_proma) % p   => cptl__p    (:,i_proma)
      cld_profiles_tl (i_proma) % ph  => cptl__ph   (:,i_proma)
      cld_profiles_tl (i_proma) % t   => cptl__t    (:,i_proma)
      cld_profiles_tl (i_proma) % q   => cptl__q    (:,i_proma)
      cld_profiles_tl (i_proma) % cc  => cptl__cc   (:,i_proma)
      cld_profiles_tl (i_proma) % clw => cptl__clw  (:,i_proma)
      cld_profiles_tl (i_proma) % ciw => cptl__ciw  (:,i_proma)
      cld_profiles_tl (i_proma) % rain=> cptl__rain (:,i_proma)
      cld_profiles_tl (i_proma) % sp  => cptl__sp   (:,i_proma)
   enddo
       
   radiance_d3 % overcast    => r3__overcast
   radiance_d3 % clear_out   => r3__clear_out
   radiance_d3 % out         => r3__out
   radiance_d3 % out_clear   => r3__out_clear
   radiance_d3 % total_out   => r3__total_out
   radiance_d3 % clear       => r3__clear
   radiance_d3 % cloudy      => r3__cloudy
   radiance_d3 % total       => r3__total
   radiance_d3 % bt          => r3__bt
   radiance_d3 % bt_clear    => r3__bt_clear
   radiance_d3 % upclear     => r3__upclear
   radiance_d3 % dnclear     => r3__dnclear
   radiance_d3 % reflclear   => r3__reflclear
   radiance_d3 % downcld     => r3__downcld

   radiance_tl % overcast    => rtl__overcast
   radiance_tl % clear_out   => rtl__clear_out
   radiance_tl % out         => rtl__out
   radiance_tl % out_clear   => rtl__out_clear
   radiance_tl % total_out   => rtl__total_out
   radiance_tl % clear       => rtl__clear
   radiance_tl % cloudy      => rtl__cloudy
   radiance_tl % total       => rtl__total
   radiance_tl % bt          => rtl__bt
   radiance_tl % bt_clear    => rtl__bt_clear
   radiance_tl % upclear     => rtl__upclear
   radiance_tl % dnclear     => rtl__dnclear
   radiance_tl % reflclear   => rtl__reflclear
   radiance_tl % downcld     => rtl__downcld

   do i_proma = 1, kproma
!* Set perturbation
      cld_profiles_tl (i_proma) % p  (:) = cld_profiles_d1 (i_proma) % p  (:)                   * epsilon
      cld_profiles_tl (i_proma) % ph (:) = cld_profiles_d1 (i_proma) % ph (:)                   * epsilon

      cld_profiles_tl (i_proma) % t    (1:kflevg) = cld_profiles_d1 (i_proma) % t    (1:kflevg) * epsilon
      cld_profiles_tl (i_proma) % q    (1:kflevg) = cld_profiles_d1 (i_proma) % q    (1:kflevg) * epsilon
      cld_profiles_tl (i_proma) % cc   (1:kflevg) = cld_profiles_d1 (i_proma) % cc   (1:kflevg) * epsilon
      cld_profiles_tl (i_proma) % clw  (1:kflevg) = cld_profiles_d1 (i_proma) % clw  (1:kflevg) * epsilon
      cld_profiles_tl (i_proma) % ciw  (1:kflevg) = cld_profiles_d1 (i_proma) % ciw  (1:kflevg) * epsilon
      cld_profiles_tl (i_proma) % rain (1:kflevg) = cld_profiles_d1 (i_proma) % rain (1:kflevg) * epsilon
      cld_profiles_tl (i_proma) % sp   (1:kflevg) = cld_profiles_d1 (i_proma) % sp   (1:kflevg) * epsilon

      cld_profiles_tl (i_proma) % cc   (1:kflevg) = 0.0_JPRB  ! to avoid cc > 1

!* Fill in RTTOV/RTTOVSCATT arrays once per profile       
      profiles_tl (i_proma) % p   (:) = 0.0_JPRB
      profiles_tl (i_proma) % clw (:) = profiles_d1 (i_proma) % clw (:) * epsilon
      profiles_tl (i_proma) % o3  (:) = profiles_d1 (i_proma) % o3  (:) * epsilon
      profiles_tl (i_proma) % s2m % p = profiles_d1 (i_proma) % s2m % p * epsilon
      profiles_tl (i_proma) % s2m % q = profiles_d1 (i_proma) % s2m % q * epsilon
      profiles_tl (i_proma) % s2m % o = profiles_d1 (i_proma) % s2m % o * epsilon
      profiles_tl (i_proma) % s2m % t = profiles_d1 (i_proma) % s2m % t * epsilon
      profiles_tl (i_proma) % s2m % u = profiles_d1 (i_proma) % s2m % u * epsilon
      profiles_tl (i_proma) % s2m % v = profiles_d1 (i_proma) % s2m % v * epsilon
      profiles_tl (i_proma) % skin % surftype  = -1 
      profiles_tl (i_proma) % skin % t          = profiles_d1 (i_proma) % skin % t          * epsilon
      profiles_tl (i_proma) % skin % fastem (:) = profiles_d1 (i_proma) % skin % fastem (:) * epsilon

      profiles_tl (i_proma) % ozone_data = .false.    
      profiles_tl (i_proma) % co2_data   = .false.
      profiles_tl (i_proma) % clw_data   = .false.
      profiles_tl (i_proma) % zenangle   = -1
      profiles_tl (i_proma) % azangle    = -1

      profiles_tl (i_proma) % ctp        = profiles_d1 (i_proma) % ctp       * epsilon
      profiles_tl (i_proma) % cfraction  = profiles_d1 (i_proma) % cfraction * epsilon
   
      profiles_tl (i_proma) % t (:) = profiles_d1 (i_proma) % t (:) * epsilon
      profiles_tl (i_proma) % q (:) = profiles_d1 (i_proma) % q (:) * epsilon  
   enddo 
   
   emissivity_tl (1:nchannels) = emissivity_d1 (1:nchannels) * epsilon 
   calcemiss     (1:nchannels) = emissivity_d1 (1:nchannels) < 0.01_JPRB

   call rttov_scatt_tl (errorstatus,        &! out
                      & kflevg,             &! in
                      & coef_rttov%nlevels, &! in
                      & nfrequencies,       &! in
                      & nchannels,          &! in
                      & nbtout,             &! in
                      & kproma,             &! in
                      & polarisations,      &! in
                      & channels,           &! in
                      & frequencies,        &! in
                      & lprofiles,          &! in
                      & lsprofiles,         &! in
                      & profiles_d1,        &! inout  
                      & cld_profiles_d1,    &! in
                      & coef_rttov,         &! in
                      & coef_scatt,         &! in
                      & calcemiss,          &! in
                      & emissivity_d1,      &! inout
                      & profiles_tl,        &! in
                      & cld_profiles_tl,    &! in
                      & emissivity_tl,      &! inout
                      & radiance_d3,        &! inout
                      & radiance_tl )        ! inout 
		      

  ! Save radiance as a reference for the trajectory
  ! TL is used instead of rttov_scatt because
  !   calcemis = F and reflectivities have not been saved
  radiance_total_ref(:) = radiance_d1%total(:)


  !---------------------------
  ! second run of TL
  !---------------------------
  lambda = 0.5_JPRB
  
  
  !* RTTOV/RTTOVSCATT arrays
   do i_proma = 1, kproma
      profiles_tl2 (i_proma) % nlevels =  coef_rttov % nlevels 
      profiles_tl2 (i_proma) % p   => ptl2__p   (:,i_proma)  
      profiles_tl2 (i_proma) % t   => ptl2__t   (:,i_proma) 
      profiles_tl2 (i_proma) % q   => ptl2__q   (:,i_proma) 
      profiles_tl2 (i_proma) % o3  => ptl2__o3  (:,i_proma) 
      profiles_tl2 (i_proma) % clw => ptl2__clw (:,i_proma)
      
      cld_profiles_tl2 (i_proma) % nlevels = kflevg 
      cld_profiles_tl2 (i_proma) % p   => cptl2__p    (:,i_proma)
      cld_profiles_tl2 (i_proma) % ph  => cptl2__ph   (:,i_proma)
      cld_profiles_tl2 (i_proma) % t   => cptl2__t    (:,i_proma)
      cld_profiles_tl2 (i_proma) % q   => cptl2__q    (:,i_proma)
      cld_profiles_tl2 (i_proma) % cc  => cptl2__cc   (:,i_proma)
      cld_profiles_tl2 (i_proma) % clw => cptl2__clw  (:,i_proma)
      cld_profiles_tl2 (i_proma) % ciw => cptl2__ciw  (:,i_proma)
      cld_profiles_tl2 (i_proma) % rain=> cptl2__rain (:,i_proma)
      cld_profiles_tl2 (i_proma) % sp  => cptl2__sp   (:,i_proma)
   enddo
       
   radiance_d3 % overcast    => r3__overcast
   radiance_d3 % clear_out   => r3__clear_out
   radiance_d3 % out         => r3__out
   radiance_d3 % out_clear   => r3__out_clear
   radiance_d3 % total_out   => r3__total_out
   radiance_d3 % clear       => r3__clear
   radiance_d3 % cloudy      => r3__cloudy
   radiance_d3 % total       => r3__total
   radiance_d3 % bt          => r3__bt
   radiance_d3 % bt_clear    => r3__bt_clear
   radiance_d3 % upclear     => r3__upclear
   radiance_d3 % dnclear     => r3__dnclear
   radiance_d3 % reflclear   => r3__reflclear
   radiance_d3 % downcld     => r3__downcld

   radiance_tl2 % overcast    => rtl2__overcast
   radiance_tl2 % clear_out   => rtl2__clear_out
   radiance_tl2 % out         => rtl2__out
   radiance_tl2 % out_clear   => rtl2__out_clear
   radiance_tl2 % total_out   => rtl2__total_out
   radiance_tl2 % clear       => rtl2__clear
   radiance_tl2 % cloudy      => rtl2__cloudy
   radiance_tl2 % total       => rtl2__total
   radiance_tl2 % bt          => rtl2__bt
   radiance_tl2 % bt_clear    => rtl2__bt_clear
   radiance_tl2 % upclear     => rtl2__upclear
   radiance_tl2 % dnclear     => rtl2__dnclear
   radiance_tl2 % reflclear   => rtl2__reflclear
   radiance_tl2 % downcld     => rtl2__downcld

   do i_proma = 1, kproma
!* Set perturbation
      cld_profiles_tl2 (i_proma) % p  (:) = cld_profiles_tl (i_proma) % p  (:)                   * lambda
      cld_profiles_tl2 (i_proma) % ph (:) = cld_profiles_tl (i_proma) % ph (:)                   * lambda

      cld_profiles_tl2 (i_proma) % t    (1:kflevg) = cld_profiles_tl (i_proma) % t    (1:kflevg) * lambda
      cld_profiles_tl2 (i_proma) % q    (1:kflevg) = cld_profiles_tl (i_proma) % q    (1:kflevg) * lambda
      cld_profiles_tl2 (i_proma) % cc   (1:kflevg) = cld_profiles_tl (i_proma) % cc   (1:kflevg) * lambda
      cld_profiles_tl2 (i_proma) % clw  (1:kflevg) = cld_profiles_tl (i_proma) % clw  (1:kflevg) * lambda
      cld_profiles_tl2 (i_proma) % ciw  (1:kflevg) = cld_profiles_tl (i_proma) % ciw  (1:kflevg) * lambda
      cld_profiles_tl2 (i_proma) % rain (1:kflevg) = cld_profiles_tl (i_proma) % rain (1:kflevg) * lambda
      cld_profiles_tl2 (i_proma) % sp   (1:kflevg) = cld_profiles_tl (i_proma) % sp   (1:kflevg) * lambda

      cld_profiles_tl (i_proma) % cc   (1:kflevg) = 0.0_JPRB  ! to avoid cc > 1

!* Fill in RTTOV/RTTOVSCATT arrays once per profile       
      profiles_tl2 (i_proma) % p   (:) = 0.0_JPRB
      profiles_tl2 (i_proma) % clw (:) = profiles_tl (i_proma) % clw (:) * lambda
      profiles_tl2 (i_proma) % o3  (:) = profiles_tl (i_proma) % o3  (:) * lambda
      profiles_tl2 (i_proma) % s2m % p = profiles_tl (i_proma) % s2m % p * lambda
      profiles_tl2 (i_proma) % s2m % q = profiles_tl (i_proma) % s2m % q * lambda
      profiles_tl2 (i_proma) % s2m % o = profiles_tl (i_proma) % s2m % o * lambda
      profiles_tl2 (i_proma) % s2m % t = profiles_tl (i_proma) % s2m % t * lambda
      profiles_tl2 (i_proma) % s2m % u = profiles_tl (i_proma) % s2m % u * lambda
      profiles_tl2 (i_proma) % s2m % v = profiles_tl (i_proma) % s2m % v * lambda
      profiles_tl2 (i_proma) % skin % surftype  = -1 
      profiles_tl2 (i_proma) % skin % t          = profiles_tl (i_proma) % skin % t          * lambda
      profiles_tl2 (i_proma) % skin % fastem (:) = profiles_tl (i_proma) % skin % fastem (:) * lambda

      profiles_tl2 (i_proma) % ozone_data = .false.    
      profiles_tl2 (i_proma) % co2_data   = .false.
      profiles_tl2 (i_proma) % clw_data   = .false.
      profiles_tl2 (i_proma) % zenangle   = -1
      profiles_tl2 (i_proma) % azangle    = -1

      profiles_tl2 (i_proma) % ctp        = profiles_tl (i_proma) % ctp       * lambda
      profiles_tl2 (i_proma) % cfraction  = profiles_tl (i_proma) % cfraction * lambda
   
      profiles_tl2 (i_proma) % t (:) = profiles_tl (i_proma) % t (:) * lambda
      profiles_tl2 (i_proma) % q (:) = profiles_tl (i_proma) % q (:) * lambda  
   enddo 
   
   emissivity_tl2 (1:nchannels) = emissivity_tl (1:nchannels) *  lambda
   calcemiss     (1:nchannels) = emissivity_tl (1:nchannels) < 0.01_JPRB

  

   call rttov_scatt_tl (errorstatus,        &! out
                      & kflevg,             &! in
                      & coef_rttov%nlevels, &! in
                      & nfrequencies,       &! in
                      & nchannels,          &! in
                      & nbtout,             &! in
                      & kproma,             &! in
                      & polarisations,      &! in
                      & channels,           &! in
                      & frequencies,        &! in
                      & lprofiles,          &! in
                      & lsprofiles,         &! in
                      & profiles_d1,        &! inout  
                      & cld_profiles_d1,    &! in
                      & coef_rttov,         &! in
                      & coef_scatt,         &! in
                      & calcemiss,          &! in
                      & emissivity_d1,      &! inout
                      & profiles_tl2,        &! in
                      & cld_profiles_tl2,    &! in
                      & emissivity_tl2,      &! inout
                      & radiance_d1,        &! inout
                      & radiance_tl2 )        ! inout 

  
  
  
   !---------------------------

  do i_chan = 1, kproma
     if( abs(lambda * radiance_tl%clear(i_chan) - radiance_tl2%clear(i_chan)) > threshold ) then
        write(ioout,*) 'TL test fails for radiance_tl%clear for channel ', i_chan
       stop
     endif
     if( abs(lambda * radiance_tl%bt_clear(i_chan) - radiance_tl2%bt_clear(i_chan)) > threshold ) then
        write(ioout,*) 'TL test fails for radiance_tl%bt_clear for channel ', i_chan
        stop
     endif
     if( abs(lambda * radiance_tl%bt(i_chan) - radiance_tl2%bt(i_chan)) > threshold ) then
        write(ioout,*) 'TL test fails for radiance_tl%bt for channel ', i_chan
        stop
     endif
     if( abs(lambda * radiance_tl%total(i_chan) - radiance_tl2%total(i_chan)) > threshold ) then
        write(ioout,*) 'TL test fails for radiance_tl%total for channel ', i_chan
        stop
     endif
  end do
 
  
   
  
  ! Now run the Taylor test
  !-------------------------

		      

   do i_lambda = 10, 1, -1
      lambda = 10.0_JPRB ** (-1.0_JPRB * i_lambda) 
      
      errorstatus = errorstatus_success
    
      emissivity_d2 (1:nchannels) = emissivity_d1 (1:nchannels) + emissivity_tl (1:nchannels) * lambda
      calcemiss     (1:nchannels) = emissivity_d2 (1:nchannels) < 0.01_JPRB

!* RTTOV/RTTOVSCATT arrays
      do i_proma = 1, kproma
         profiles_d2 (i_proma) % nlevels =  coef_rttov % nlevels 
         profiles_d2 (i_proma) % p   => p2__p   (:,i_proma)  
         profiles_d2 (i_proma) % t   => p2__t   (:,i_proma) 
         profiles_d2 (i_proma) % q   => p2__q   (:,i_proma) 
         profiles_d2 (i_proma) % o3  => p2__o3  (:,i_proma) 
         profiles_d2 (i_proma) % clw => p2__clw (:,i_proma)

         cld_profiles_d2 (i_proma) % nlevels = kflevg 
         cld_profiles_d2 (i_proma) % p   => cp2__p    (:,i_proma)
         cld_profiles_d2 (i_proma) % ph  => cp2__ph   (:,i_proma)
         cld_profiles_d2 (i_proma) % t   => cp2__t    (:,i_proma)
         cld_profiles_d2 (i_proma) % q   => cp2__q    (:,i_proma)
         cld_profiles_d2 (i_proma) % cc  => cp2__cc   (:,i_proma)
         cld_profiles_d2 (i_proma) % clw => cp2__clw  (:,i_proma)
         cld_profiles_d2 (i_proma) % ciw => cp2__ciw  (:,i_proma)
         cld_profiles_d2 (i_proma) % rain=> cp2__rain (:,i_proma)
         cld_profiles_d2 (i_proma) % sp  => cp2__sp   (:,i_proma)
      enddo
       
      radiance_d2 % overcast    => r2__overcast
      radiance_d2 % clear_out   => r2__clear_out
      radiance_d2 % out         => r2__out
      radiance_d2 % out_clear   => r2__out_clear
      radiance_d2 % total_out   => r2__total_out
      radiance_d2 % clear       => r2__clear
      radiance_d2 % cloudy      => r2__cloudy
      radiance_d2 % total       => r2__total
      radiance_d2 % bt          => r2__bt
      radiance_d2 % bt_clear    => r2__bt_clear
      radiance_d2 % upclear     => r2__upclear
      radiance_d2 % dnclear     => r2__dnclear
      radiance_d2 % reflclear   => r2__reflclear
      radiance_d2 % downcld     => r2__downcld

      do i_proma = 1, kproma    
!* Add perturbations
        cld_profiles_d2 (i_proma) % p  (:) = cld_profiles_d1 (i_proma) % p  (:) + cld_profiles_tl (i_proma) % p  (:) * lambda
        cld_profiles_d2 (i_proma) % ph (:) = cld_profiles_d1 (i_proma) % ph (:) + cld_profiles_tl (i_proma) % ph (:) * lambda

        cld_profiles_d2 (i_proma) % t    (1:kflevg) = cld_profiles_d1 (i_proma) % t    (1:kflevg) &
	                                          & + cld_profiles_tl (i_proma) % t    (1:kflevg) * lambda
        cld_profiles_d2 (i_proma) % q    (1:kflevg) = cld_profiles_d1 (i_proma) % q    (1:kflevg) & 
	                                          & + cld_profiles_tl (i_proma) % q    (1:kflevg) * lambda
        cld_profiles_d2 (i_proma) % cc   (1:kflevg) = cld_profiles_d1 (i_proma) % cc   (1:kflevg) &
	                                          & + cld_profiles_tl (i_proma) % cc   (1:kflevg) * lambda
        cld_profiles_d2 (i_proma) % clw  (1:kflevg) = cld_profiles_d1 (i_proma) % clw  (1:kflevg) &
	                                          & + cld_profiles_tl (i_proma) % clw  (1:kflevg) * lambda
        cld_profiles_d2 (i_proma) % ciw  (1:kflevg) = cld_profiles_d1 (i_proma) % ciw  (1:kflevg) &
	                                          & + cld_profiles_tl (i_proma) % ciw  (1:kflevg) * lambda
        cld_profiles_d2 (i_proma) % rain (1:kflevg) = cld_profiles_d1 (i_proma) % rain (1:kflevg) &
	                                          & + cld_profiles_tl (i_proma) % rain (1:kflevg) * lambda
        cld_profiles_d2 (i_proma) % sp   (1:kflevg) = cld_profiles_d1 (i_proma) % sp   (1:kflevg) &
	                                          & + cld_profiles_tl (i_proma) % sp   (1:kflevg) * lambda 

!* Fill in RTTOV/RTTOVSCATT arrays once per profile
        profiles_d2 (i_proma) % p   (:) = profiles_d1 (i_proma) % p   (:)
        profiles_d2 (i_proma) % clw (:) = profiles_d1 (i_proma) % clw (:) + profiles_tl (i_proma) % clw (:) * lambda
        profiles_d2 (i_proma) % o3  (:) = profiles_d1 (i_proma) % o3  (:) + profiles_tl (i_proma) % o3  (:) * lambda
        profiles_d2 (i_proma) % s2m % p = profiles_d1 (i_proma) % s2m % p + profiles_tl (i_proma) % s2m % p * lambda
        profiles_d2 (i_proma) % s2m % q = profiles_d1 (i_proma) % s2m % q + profiles_tl (i_proma) % s2m % q * lambda
        profiles_d2 (i_proma) % s2m % o = profiles_d1 (i_proma) % s2m % o + profiles_tl (i_proma) % s2m % o * lambda
        profiles_d2 (i_proma) % s2m % t = profiles_d1 (i_proma) % s2m % t + profiles_tl (i_proma) % s2m % t * lambda
        profiles_d2 (i_proma) % s2m % u = profiles_d1 (i_proma) % s2m % u + profiles_tl (i_proma) % s2m % u * lambda
        profiles_d2 (i_proma) % s2m % v = profiles_d1 (i_proma) % s2m % v + profiles_tl (i_proma) % s2m % v * lambda
        profiles_d2 (i_proma) % skin % surftype   = profiles_d1 (i_proma) % skin % surftype
        profiles_d2 (i_proma) % skin % t          = profiles_d1 (i_proma) % skin % t          &
	                                          & + profiles_tl (i_proma) % skin % t          * lambda
        profiles_d2 (i_proma) % skin % fastem (:) = profiles_d1 (i_proma) % skin % fastem (:) & 
	                                          & + profiles_tl (i_proma) % skin % fastem (:) * lambda

        profiles_d2 (i_proma) % ozone_data = .false.    
        profiles_d2 (i_proma) % co2_data   = .false.
        profiles_d2 (i_proma) % clw_data   = .false.
        profiles_d2 (i_proma) % zenangle   = zenangle
        profiles_d2 (i_proma) % azangle    = 0.0_JPRB    ! default value
        profiles_d2 (i_proma) % ctp        = 500.0_JPRB  ! default value
        profiles_d2 (i_proma) % cfraction  =   0.0_JPRB  ! default value
        
        profiles_d2 (i_proma) % t (:) = profiles_d1 (i_proma) % t (:) + profiles_tl (i_proma) % t (:) * lambda
        profiles_d2 (i_proma) % q (:) = profiles_d1 (i_proma) % q (:) + profiles_tl (i_proma) % q (:) * lambda
     end do    

!* Reference forward model run
     call rttov_scatt (errorstatus,        & ! out 
                     & kflevg,             & ! in
                     & coef_rttov%nlevels, & ! in
                     & nfrequencies,       & ! in
                     & nchannels,          & ! in
                     & nbtout,             & ! in
                     & kproma,             & ! in
                     & polarisations,      & ! in
                     & channels,           & ! in
                     & frequencies,        & ! in
                     & lprofiles,          & ! in
                     & lsprofiles,         & ! in
                     & profiles_d2,        & ! inout  
                     & cld_profiles_d2,    & ! in
                     & coef_rttov,         & ! in
                     & coef_scatt,         & ! in
                     & calcemiss,          & ! in
                     & emissivity_d2,      & ! inout
                     & radiance_d2 )         ! inout 
               
	       
     write(ioout,*)
     write(ioout,*) 'Chan      Lambda              Cloudy Tb'
		  
    do i_chan = 1, nchannels
        ratio(1) = (radiance_d2 % bt(i_chan) - radiance_d1 % bt(i_chan)) / (lambda * radiance_tl % bt(i_chan))
        ratio(2) = (radiance_d2 % bt_clear(i_chan) - radiance_d1 % bt_clear(i_chan)) / (lambda * radiance_tl % bt_clear(i_chan))
        write (ioout,*) i_chan,  lambda, ratio(1)
     enddo
  enddo
  
!* ADJOINT TEST ***********************************************************************************

  write(ioout,*)
  write(ioout,*) 'Test AD'
  write(ioout,*) '-------'
  write(ioout,*)

  write(ioout,*) '1 - Test Linearity'
  write(ioout,*)
  
!
  !Allocate new profiles for AD code
!
!* RTTOV/RTTOVSCATT arrays
   do i_proma = 1, kproma
      profiles_ad (i_proma) % nlevels =  coef_rttov % nlevels 
      profiles_ad (i_proma) % p   => pad__p   (:,i_proma)  
      profiles_ad (i_proma) % t   => pad__t   (:,i_proma) 
      profiles_ad (i_proma) % q   => pad__q   (:,i_proma) 
      profiles_ad (i_proma) % o3  => pad__o3  (:,i_proma) 
      profiles_ad (i_proma) % clw => pad__clw (:,i_proma)
      
      cld_profiles_ad (i_proma) % nlevels = kflevg 
      cld_profiles_ad (i_proma) % p   => cpad__p    (:,i_proma)
      cld_profiles_ad (i_proma) % ph  => cpad__ph   (:,i_proma)
      cld_profiles_ad (i_proma) % t   => cpad__t    (:,i_proma)
      cld_profiles_ad (i_proma) % q   => cpad__q    (:,i_proma)
      cld_profiles_ad (i_proma) % cc  => cpad__cc   (:,i_proma)
      cld_profiles_ad (i_proma) % clw => cpad__clw  (:,i_proma)
      cld_profiles_ad (i_proma) % ciw => cpad__ciw  (:,i_proma)
      cld_profiles_ad (i_proma) % rain=> cpad__rain (:,i_proma)
      cld_profiles_ad (i_proma) % sp  => cpad__sp   (:,i_proma)
   enddo
       
   radiance_ad % overcast    => rad__overcast
   radiance_ad % clear_out   => rad__clear_out
   radiance_ad % out         => rad__out
   radiance_ad % out_clear   => rad__out_clear
   radiance_ad % total_out   => rad__total_out
   radiance_ad % clear       => rad__clear
   radiance_ad % cloudy      => rad__cloudy
   radiance_ad % total       => rad__total
   radiance_ad % bt          => rad__bt
   radiance_ad % bt_clear    => rad__bt_clear
   radiance_ad % upclear     => rad__upclear
   radiance_ad % dnclear     => rad__dnclear
   radiance_ad % reflclear   => rad__reflclear
   radiance_ad % downcld     => rad__downcld

   do i_proma = 1, kproma
!* Set perturbation
      cld_profiles_ad (i_proma) % p  (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % ph (:) = 0.0_JPRB  

      cld_profiles_ad (i_proma) % t    (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % q    (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % cc   (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % clw  (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % ciw  (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % rain (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % sp   (:) = 0.0_JPRB  

!* Fill in RTTOV/RTTOVSCATT arrays once per profile       
      profiles_ad (i_proma) % p   (:) = 0.0_JPRB
      profiles_ad (i_proma) % t   (:) = 0.0_JPRB 
      profiles_ad (i_proma) % q   (:) = 0.0_JPRB 
      profiles_ad (i_proma) % clw (:) = 0.0_JPRB 
      profiles_ad (i_proma) % o3  (:) = 0.0_JPRB 
      
      profiles_ad (i_proma) % s2m % p = 0.0_JPRB 
      profiles_ad (i_proma) % s2m % q = 0.0_JPRB 
      profiles_ad (i_proma) % s2m % o = 0.0_JPRB 
      profiles_ad (i_proma) % s2m % t = 0.0_JPRB 
      profiles_ad (i_proma) % s2m % u = 0.0_JPRB 
      profiles_ad (i_proma) % s2m % v = 0.0_JPRB 
      
      profiles_ad (i_proma) % skin % surftype   = -1 
      profiles_ad (i_proma) % skin % t          = 0.0_JPRB 
      profiles_ad (i_proma) % skin % fastem (:) = 0.0_JPRB 

      profiles_ad (i_proma) % ozone_data = .false.    
      profiles_ad (i_proma) % co2_data   = .false.
      profiles_ad (i_proma) % clw_data   = .false.
      profiles_ad (i_proma) % zenangle   = -1
      profiles_ad (i_proma) % azangle    = -1
      profiles_ad (i_proma) % ctp        = 0.0_JPRB 
      profiles_ad (i_proma) % cfraction  = 0.0_JPRB 
    
   enddo 
   
  emissivity_ad (1:nchannels) = 0.0_JPRB 
   
  
  ! Set perturbations
  !
     radiance_ad % clear_out(:)   = 0._JPRB
     radiance_ad % total_out(:)   = 0._JPRB
     radiance_ad % out_clear(:) = 0.05_JPRB * radiance_d1 % out_clear(:)
     radiance_ad % out(:)       = 0.05_JPRB * radiance_d1 % out(:)
  radiance_ad % clear(:)    = 0._JPRB   ! AD does not work for radiance_inc % clear(:) because of switchrad in RTTOV
  radiance_ad % cloudy   (:)   = 0._JPRB
  radiance_ad % upclear  (:)   = 0._JPRB
  radiance_ad % reflclear(:)   = 0._JPRB
  radiance_ad % overcast (:,:) = 0._JPRB
  radiance_ad % bt       (:)   = 0._JPRB
  radiance_ad % bt_clear (:)   = 0._JPRB
  radiance_ad % total    (:)   = 0._JPRB


   call rttov_scatt_ad (errorstatus,        &! out
                      & kflevg,             &! in
                      & coef_rttov%nlevels, &! in
                      & nfrequencies,       &! in
                      & nchannels,          &! in
                      & nbtout,             &! in
                      & kproma,             &! in
                      & polarisations,      &! in
                      & channels,           &! in
                      & frequencies,        &! in
                      & lprofiles,          &! in
                      & lsprofiles,         &! in
                      & profiles_d1,        &! inout  
                      & cld_profiles_d1,    &! in
                      & coef_rttov,         &! in
                      & coef_scatt,         &! in
                      & calcemiss,          &! in
                      & emissivity_d1,      &! inout
                      & profiles_ad,        &! in
                      & cld_profiles_ad,    &! in
                      & emissivity_ad,      &! inout
                      & radiance_d2,        &! inout
                      & radiance_ad )        ! inout 
   

  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
   do i_proma = 1, kproma
        If ( errorstatus(i_proma) == errorstatus_fatal ) Then
           write ( ioout, * ) 'rttov_scatt_ad error for profile',i_proma
        End If
     End Do
     Stop
  End If
  
  If ( Any( abs(radiance_total_ref(:) - radiance_d2%total(:)) > threshold * radiance_total_ref(:)  ))  Then
    write(default_err_unit,*) 'wrong forward model in AD'
    write(default_err_unit,*) radiance_total_ref(:)
    write(default_err_unit,*) abs(radiance_total_ref(:)-radiance_d2%total(:)) / (threshold * radiance_total_ref(:))
    Stop
  Endif


  !---------------------------
  ! Second run of AD

  !Allocate new profiles for AD code
  ! Profiles on RTTOV pressure levels

   do i_proma = 1, kproma
      profiles_ad2 (i_proma) % nlevels =  coef_rttov % nlevels 
      profiles_ad2 (i_proma) % p   => pad2__p   (:,i_proma)  
      profiles_ad2 (i_proma) % t   => pad2__t   (:,i_proma) 
      profiles_ad2 (i_proma) % q   => pad2__q   (:,i_proma) 
      profiles_ad2 (i_proma) % o3  => pad2__o3  (:,i_proma) 
      profiles_ad2 (i_proma) % clw => pad2__clw (:,i_proma)
      
      cld_profiles_ad2 (i_proma) % nlevels = kflevg 
      cld_profiles_ad2 (i_proma) % p   => cpad2__p    (:,i_proma)
      cld_profiles_ad2 (i_proma) % ph  => cpad2__ph   (:,i_proma)
      cld_profiles_ad2 (i_proma) % t   => cpad2__t    (:,i_proma)
      cld_profiles_ad2 (i_proma) % q   => cpad2__q    (:,i_proma)
      cld_profiles_ad2 (i_proma) % cc  => cpad2__cc   (:,i_proma)
      cld_profiles_ad2 (i_proma) % clw => cpad2__clw  (:,i_proma)
      cld_profiles_ad2 (i_proma) % ciw => cpad2__ciw  (:,i_proma)
      cld_profiles_ad2 (i_proma) % rain=> cpad2__rain (:,i_proma)
      cld_profiles_ad2 (i_proma) % sp  => cpad2__sp   (:,i_proma)
   enddo
       
   radiance_ad2 % overcast    => rad2__overcast
   radiance_ad2 % clear_out   => rad2__clear_out
   radiance_ad2 % out         => rad2__out
   radiance_ad2 % out_clear   => rad2__out_clear
   radiance_ad2 % total_out   => rad2__total_out
   radiance_ad2 % clear       => rad2__clear
   radiance_ad2 % cloudy      => rad2__cloudy
   radiance_ad2 % total       => rad2__total
   radiance_ad2 % bt          => rad2__bt
   radiance_ad2 % bt_clear    => rad2__bt_clear
   radiance_ad2 % upclear     => rad2__upclear
   radiance_ad2 % dnclear     => rad2__dnclear
   radiance_ad2 % reflclear   => rad2__reflclear
   radiance_ad2 % downcld     => rad2__downcld

   do i_proma = 1, kproma
!* Set perturbation
      cld_profiles_ad2 (i_proma) % p  (:) = 0.0_JPRB  
      cld_profiles_ad2 (i_proma) % ph (:) = 0.0_JPRB  

      cld_profiles_ad2 (i_proma) % t    (:) = 0.0_JPRB  
      cld_profiles_ad2 (i_proma) % q    (:) = 0.0_JPRB  
      cld_profiles_ad2 (i_proma) % cc   (:) = 0.0_JPRB  
      cld_profiles_ad2 (i_proma) % clw  (:) = 0.0_JPRB  
      cld_profiles_ad2 (i_proma) % ciw  (:) = 0.0_JPRB  
      cld_profiles_ad2 (i_proma) % rain (:) = 0.0_JPRB  
      cld_profiles_ad2 (i_proma) % sp   (:) = 0.0_JPRB  

!* Fill in RTTOV/RTTOVSCATT arrays once per profile       
      profiles_ad2 (i_proma) % p   (:) = 0.0_JPRB
      profiles_ad2 (i_proma) % t   (:) = 0.0_JPRB 
      profiles_ad2 (i_proma) % q   (:) = 0.0_JPRB 
      profiles_ad2 (i_proma) % clw (:) = 0.0_JPRB 
      profiles_ad2 (i_proma) % o3  (:) = 0.0_JPRB 
      
      profiles_ad2 (i_proma) % s2m % p = 0.0_JPRB 
      profiles_ad2 (i_proma) % s2m % q = 0.0_JPRB 
      profiles_ad2 (i_proma) % s2m % o = 0.0_JPRB 
      profiles_ad2 (i_proma) % s2m % t = 0.0_JPRB 
      profiles_ad2 (i_proma) % s2m % u = 0.0_JPRB 
      profiles_ad2 (i_proma) % s2m % v = 0.0_JPRB 
      
      profiles_ad2 (i_proma) % skin % surftype   = -1 
      profiles_ad2 (i_proma) % skin % t          = 0.0_JPRB 
      profiles_ad2 (i_proma) % skin % fastem (:) = 0.0_JPRB 

      profiles_ad2 (i_proma) % ozone_data = .false.    
      profiles_ad2 (i_proma) % co2_data   = .false.
      profiles_ad2 (i_proma) % clw_data   = .false.
      profiles_ad2 (i_proma) % zenangle   = -1
      profiles_ad2 (i_proma) % azangle    = -1
      profiles_ad2 (i_proma) % ctp        = 0.0_JPRB 
      profiles_ad2 (i_proma) % cfraction  = 0.0_JPRB 
    
   enddo 
   
  emissivity_ad2 (1:nchannels) = 0.0_JPRB 
   
  
  ! Set perturbations
  !
  lambda = 0.5_JPRB
     radiance_ad2 % clear_out(:)   = 0._JPRB
     radiance_ad2 % total_out(:)   = 0._JPRB
     radiance_ad2 % out_clear(:) = 0.05_JPRB * radiance_d1 % out_clear(:)* lambda
     radiance_ad2 % out(:)       = 0.05_JPRB * radiance_d1 % out(:)* lambda

  radiance_ad2 % clear(:)    = 0._JPRB   ! AD does not work for radiance_inc % clear(:) because of switchrad in RTTOV
  radiance_ad2 % cloudy   (:)   = 0._JPRB
  radiance_ad2 % upclear  (:)   = 0._JPRB
  radiance_ad2 % reflclear(:)   = 0._JPRB
  radiance_ad2 % overcast (:,:) = 0._JPRB
  radiance_ad2 % bt       (:)   = 0._JPRB
  radiance_ad2 % bt_clear (:)   = 0._JPRB
  radiance_ad2 % total    (:)   = 0._JPRB

   call rttov_scatt_ad (errorstatus,        &! out
                      & kflevg,             &! in
                      & coef_rttov%nlevels, &! in
                      & nfrequencies,       &! in
                      & nchannels,          &! in
                      & nbtout,             &! in
                      & kproma,             &! in
                      & polarisations,      &! in
                      & channels,           &! in
                      & frequencies,        &! in
                      & lprofiles,          &! in
                      & lsprofiles,         &! in
                      & profiles_d1,        &! inout  
                      & cld_profiles_d1,    &! in
                      & coef_rttov,         &! in
                      & coef_scatt,         &! in
                      & calcemiss,          &! in
                      & emissivity_d1,      &! inout
                      & profiles_ad2,        &! in
                      & cld_profiles_ad2,    &! in
                      & emissivity_ad2,      &! inout
                      & radiance_d2,        &! inout
                      & radiance_ad2 )        ! inout 


  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
   do i_proma = 1, kproma
        If ( errorstatus(i_proma) == errorstatus_fatal ) Then
           write ( ioout, * ) 'rttov_scatt_ad error for profile',i_proma
        End If
     End Do
     Stop
  End If


     do i_proma = 1, kproma
      do i_lev = 1, profiles_ad (i_proma) % nlevels
        if ( abs(lambda * profiles_ad (i_proma) % t (i_lev) - profiles_ad2 (i_proma) % t (i_lev)) > threshold ) then
           write(default_err_unit,*) 'test AD 1 fails', i_lev
           stop
        End If
        if ( abs(lambda * profiles_ad (i_proma) % q (i_lev) - profiles_ad2 (i_proma) % q (i_lev)) > threshold ) then
           write(default_err_unit,*) 'test AD 2 fails', i_lev
           stop
        End If
        if ( abs(lambda * profiles_ad (i_proma) % o3 (i_lev) - profiles_ad2 (i_proma) % o3 (i_lev)) > threshold ) then
           write(default_err_unit,*) 'test AD 3 fails', i_lev
           stop
        End If
       enddo
     enddo


     do i_proma = 1, kproma
      do i_lev = 1, cld_profiles_ad (i_proma) % nlevels

	
        if ( abs(lambda * cld_profiles_ad (i_proma) % p (i_lev) - cld_profiles_ad2 (i_proma) % p (i_lev)) > threshold ) then
           write(default_err_unit,*) 'test AD 4 fails', i_lev
           stop
        End If
        if ( abs(lambda * cld_profiles_ad (i_proma) % ph (i_lev) - cld_profiles_ad2 (i_proma) % ph (i_lev)) > threshold ) then
           write(default_err_unit,*) 'test AD 5 fails', i_lev
           stop
        End If
        if ( abs(lambda * cld_profiles_ad (i_proma) % t (i_lev) - cld_profiles_ad2 (i_proma) % t (i_lev)) > threshold ) then
           write(default_err_unit,*) 'test AD 6 fails', i_lev
           stop
        End If
        if ( abs(lambda * cld_profiles_ad (i_proma) % cc (i_lev) - cld_profiles_ad2 (i_proma) % cc (i_lev)) > threshold ) then
           write(default_err_unit,*) 'test AD 7 fails', i_lev
           stop
        End If
        if ( abs(lambda * cld_profiles_ad (i_proma) % clw (i_lev) - cld_profiles_ad2 (i_proma) % clw (i_lev)) > threshold ) then
           write(default_err_unit,*) 'test AD 8 fails', i_lev
           stop
        End If

        if ( abs(lambda * cld_profiles_ad (i_proma) % ciw (i_lev) - cld_profiles_ad2 (i_proma) % ciw (i_lev)) > threshold ) then
           write(default_err_unit,*) 'test AD 9 fails', i_lev
           stop
        End If
        if ( abs(lambda * cld_profiles_ad (i_proma) % rain (i_lev) - cld_profiles_ad2 (i_proma) % rain (i_lev)) > threshold ) then
           write(default_err_unit,*) 'test AD 10 fails', i_lev
           stop
        End If
        if ( abs(lambda * cld_profiles_ad (i_proma) % sp (i_lev) - cld_profiles_ad2 (i_proma) % sp (i_lev)) > threshold ) then
           write(default_err_unit,*) 'test AD 11 fails', i_lev
           stop
        End If
       enddo
     enddo

     do i_proma = 1, kproma
      if ( abs(lambda * profiles_ad (i_proma) % s2m % t - profiles_ad2 (i_proma) % s2m % t) > threshold ) Then
        write(default_err_unit,*) 'test AD 12 fails', i_proma
        stop
      End If
      if ( abs(lambda * profiles_ad (i_proma) % s2m % q - profiles_ad2 (i_proma) % s2m % q) > threshold ) Then
        write(default_err_unit,*) 'test AD 13 fails', i_proma
        stop
      End If
      if ( abs(lambda * profiles_ad (i_proma) % s2m % p - profiles_ad2 (i_proma) % s2m % p) > threshold ) Then
        write(default_err_unit,*) 'test AD 14 fails', i_proma
        stop
      End If
      if ( abs(lambda * profiles_ad (i_proma) % s2m % u - profiles_ad2 (i_proma) % s2m % u) > threshold ) Then
        write(default_err_unit,*) 'test AD 15 fails', i_proma
        stop
      End If
      if ( abs(lambda * profiles_ad (i_proma) % s2m % v - profiles_ad2 (i_proma) % s2m % v) > threshold ) Then
        write(default_err_unit,*) 'test AD 16 fails', i_proma
        stop
      End If
      if ( abs(lambda * profiles_ad (i_proma) % skin % t - profiles_ad2 (i_proma) % skin % t) > threshold ) Then
        write(default_err_unit,*) 'test AD 17 fails', i_proma
        stop
      End If
     enddo

  do i_chan = 1, nchannels
     if ( abs(lambda * emissivity_ad (i_chan) - emissivity_ad2 (i_chan) ) > threshold ) Then
        write(default_err_unit,*) 'test AD 18 fails', i_chan
        stop
     End If
  enddo



  write(ioout,*) '2 - Test Equality of Norms'
  write(ioout,*)
  
   do i_proma = 1, kproma
   enddo
       

   do i_proma = 1, kproma
!* Set perturbation
      cld_profiles_tl (i_proma) % p  (:) = cld_profiles_d1 (i_proma) % p  (:) * epsilon
      cld_profiles_tl (i_proma) % ph (:) = cld_profiles_d1 (i_proma) % ph (:) * epsilon

      cld_profiles_tl (i_proma) % t    (:) = cld_profiles_d1 (i_proma) % t    (:) * epsilon
      cld_profiles_tl (i_proma) % q    (:) = cld_profiles_d1 (i_proma) % q    (:) * epsilon
      cld_profiles_tl (i_proma) % cc   (:) = cld_profiles_d1 (i_proma) % cc   (:) * epsilon
      cld_profiles_tl (i_proma) % clw  (:) = cld_profiles_d1 (i_proma) % clw  (:) * epsilon
      cld_profiles_tl (i_proma) % ciw  (:) = cld_profiles_d1 (i_proma) % ciw  (:) * epsilon
      cld_profiles_tl (i_proma) % rain (:) = cld_profiles_d1 (i_proma) % rain (:) * epsilon
      cld_profiles_tl (i_proma) % sp   (:) = cld_profiles_d1 (i_proma) % sp   (:) * epsilon

!* Fill in RTTOV/RTTOVSCATT arrays once per profile       
      profiles_tl (i_proma) % clw (:) = profiles_d1 (i_proma) % clw (:) * epsilon
      profiles_tl (i_proma) % o3  (:) = profiles_d1 (i_proma) % o3  (:) * epsilon
      profiles_tl (i_proma) % t   (:) = profiles_d1 (i_proma) % t   (:) * epsilon
      profiles_tl (i_proma) % q   (:) = profiles_d1 (i_proma) % q   (:) * epsilon  
      
      profiles_tl (i_proma) % s2m % p = profiles_d1 (i_proma) % s2m % p * epsilon
      profiles_tl (i_proma) % s2m % q = profiles_d1 (i_proma) % s2m % q * epsilon
      profiles_tl (i_proma) % s2m % o = profiles_d1 (i_proma) % s2m % o * epsilon
      profiles_tl (i_proma) % s2m % t = profiles_d1 (i_proma) % s2m % t * epsilon
      profiles_tl (i_proma) % s2m % u = profiles_d1 (i_proma) % s2m % u * epsilon
      profiles_tl (i_proma) % s2m % v = profiles_d1 (i_proma) % s2m % v * epsilon

      profiles_tl (i_proma) % skin % surftype  = -1 
      profiles_tl (i_proma) % skin % t          = profiles_d1 (i_proma) % skin % t          * epsilon
      profiles_tl (i_proma) % skin % fastem (:) = profiles_d1 (i_proma) % skin % fastem (:) * epsilon

      profiles_tl (i_proma) % ozone_data = .false.    
      profiles_tl (i_proma) % co2_data   = .false.
      profiles_tl (i_proma) % clw_data   = .false.
      profiles_tl (i_proma) % zenangle   = -1
      profiles_tl (i_proma) % azangle    = -1

      profiles_tl (i_proma) % ctp        = profiles_d1 (i_proma) % ctp       * epsilon
      profiles_tl (i_proma) % cfraction  = profiles_d1 (i_proma) % cfraction * epsilon
          
!* Set perturbation
      cld_profiles_ad (i_proma) % p  (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % ph (:) = 0.0_JPRB  

      cld_profiles_ad (i_proma) % t    (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % q    (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % cc   (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % clw  (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % ciw  (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % rain (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % sp   (:) = 0.0_JPRB  

!* Fill in RTTOV/RTTOVSCATT arrays once per profile       
      profiles_ad (i_proma) % p   (:) = 0.0_JPRB
      profiles_ad (i_proma) % t   (:) = 0.0_JPRB 
      profiles_ad (i_proma) % q   (:) = 0.0_JPRB 
      profiles_ad (i_proma) % clw (:) = 0.0_JPRB 
      profiles_ad (i_proma) % o3  (:) = 0.0_JPRB 
      
      profiles_ad (i_proma) % s2m % p = 0.0_JPRB 
      profiles_ad (i_proma) % s2m % q = 0.0_JPRB 
      profiles_ad (i_proma) % s2m % o = 0.0_JPRB 
      profiles_ad (i_proma) % s2m % t = 0.0_JPRB 
      profiles_ad (i_proma) % s2m % u = 0.0_JPRB 
      profiles_ad (i_proma) % s2m % v = 0.0_JPRB 
      
      profiles_ad (i_proma) % skin % surftype   = -1 
      profiles_ad (i_proma) % skin % t          = 0.0_JPRB 
      profiles_ad (i_proma) % skin % fastem (:) = 0.0_JPRB 

      profiles_ad (i_proma) % ozone_data = .false.    
      profiles_ad (i_proma) % co2_data   = .false.
      profiles_ad (i_proma) % clw_data   = .false.
      profiles_ad (i_proma) % zenangle   = -1
      profiles_ad (i_proma) % azangle    = -1
      profiles_ad (i_proma) % ctp        = 0.0_JPRB 
      profiles_ad (i_proma) % cfraction  = 0.0_JPRB 
    
   enddo 
   
   emissivity_d1 (1:nchannels) = 0.0_JPRB 
   calcemiss     (1:nchannels) = emissivity_d1 (1:nchannels) < 0.01_JPRB
      
   emissivity_tl (1:nchannels) = emissivity_d1 (1:nchannels) * epsilon 
   emissivity_ad (1:nchannels) = 0.0_JPRB 
  
  radiance_tl % total(:)    = 0._JPRB
  radiance_tl % bt_clear(:) = 0._JPRB
  radiance_tl % bt(:)       = 0._JPRB


   call rttov_scatt_tl (errorstatus,        &! out
                      & kflevg,             &! in
                      & coef_rttov%nlevels, &! in
                      & nfrequencies,       &! in
                      & nchannels,          &! in
                      & nbtout,             &! in
                      & kproma,             &! in
                      & polarisations,      &! in
                      & channels,           &! in
                      & frequencies,        &! in
                      & lprofiles,          &! in
                      & lsprofiles,         &! in
                      & profiles_d1,        &! inout  
                      & cld_profiles_d1,    &! in
                      & coef_rttov,         &! in
                      & coef_scatt,         &! in
                      & calcemiss,          &! in
                      & emissivity_d1,      &! inout
                      & profiles_tl,        &! in
                      & cld_profiles_tl,    &! in
                      & emissivity_tl,      &! inout
                      & radiance_d3,        &! inout
                      & radiance_tl )        ! inout 

  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
     Do i_proma = 1, kproma
        If ( errorstatus(i_proma) == errorstatus_fatal ) Then
           write (ioout, * ) 'rttov_scatt_tl error for profile',i_proma
        End If
     End Do
     Stop
  End If


     radiance_tl % clear_out(:)    = 0._JPRB
     radiance_tl % total_out(:)    = 0._JPRB

  !* compute <subtl(delta_x),delta_z>

   zdelta1 = 0.0_JPRB
   
   do i_chan = 1, nbtout
      zdelta1 = zdelta1 + (radiance_tl % out (i_chan)) ** 2.0_JPRB 
   enddo
   
!* Initialize     
   radiance_ad % overcast    = 0.0_JPRB
   radiance_ad % clear_out   = 0.0_JPRB
   radiance_ad % out         = 0.0_JPRB
   radiance_ad % out_clear   = 0.0_JPRB
   radiance_ad % total_out   = 0.0_JPRB
   radiance_ad % clear       = 0.0_JPRB
   radiance_ad % cloudy      = 0.0_JPRB
   radiance_ad % total       = 0.0_JPRB
!   radiance_ad % bt          = radiance_tl % bt
   radiance_ad % out          = radiance_tl % out
   radiance_ad % bt_clear    = 0.0_JPRB
   radiance_ad % upclear     = 0.0_JPRB
   radiance_ad % dnclear     = 0.0_JPRB
   radiance_ad % reflclear   = 0.0_JPRB
   radiance_ad % downcld     = 0.0_JPRB
   
   radiance_d1 % overcast    = 0.0_JPRB
   radiance_d1 % clear_out   = 0.0_JPRB
   radiance_d1 % out         = 0.0_JPRB
   radiance_d1 % out_clear   = 0.0_JPRB
   radiance_d1 % total_out   = 0.0_JPRB
   radiance_d1 % clear       = 0.0_JPRB
   radiance_d1 % cloudy      = 0.0_JPRB
   radiance_d1 % total       = 0.0_JPRB
   radiance_d1 % bt          = 0.0_JPRB
   radiance_d1 % bt_clear    = 0.0_JPRB
   radiance_d1 % upclear     = 0.0_JPRB
   radiance_d1 % dnclear     = 0.0_JPRB
   radiance_d1 % reflclear   = 0.0_JPRB
   radiance_d1 % downcld     = 0.0_JPRB


  !---------------------------
  ! Now run AD code with TL radiances in input
  ! move TL results to AD radiance increments
   
   do i_proma = 1, kproma
!* Set perturbation
      cld_profiles_ad (i_proma) % p  (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % ph (:) = 0.0_JPRB  

      cld_profiles_ad (i_proma) % t    (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % q    (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % cc   (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % clw  (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % ciw  (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % rain (:) = 0.0_JPRB  
      cld_profiles_ad (i_proma) % sp   (:) = 0.0_JPRB  

!* Fill in RTTOV/RTTOVSCATT arrays once per profile       
      profiles_ad (i_proma) % p   (:) = 0.0_JPRB
      profiles_ad (i_proma) % t   (:) = 0.0_JPRB 
      profiles_ad (i_proma) % q   (:) = 0.0_JPRB 
      profiles_ad (i_proma) % clw (:) = 0.0_JPRB 
      profiles_ad (i_proma) % o3  (:) = 0.0_JPRB 
      
      profiles_ad (i_proma) % s2m % p = 0.0_JPRB 
      profiles_ad (i_proma) % s2m % q = 0.0_JPRB 
      profiles_ad (i_proma) % s2m % o = 0.0_JPRB 
      profiles_ad (i_proma) % s2m % t = 0.0_JPRB 
      profiles_ad (i_proma) % s2m % u = 0.0_JPRB 
      profiles_ad (i_proma) % s2m % v = 0.0_JPRB 
      
      profiles_ad (i_proma) % skin % surftype   = -1 
      profiles_ad (i_proma) % skin % t          = 0.0_JPRB 
      profiles_ad (i_proma) % skin % fastem (:) = 0.0_JPRB 

      profiles_ad (i_proma) % ozone_data = .false.    
      profiles_ad (i_proma) % co2_data   = .false.
      profiles_ad (i_proma) % clw_data   = .false.
      profiles_ad (i_proma) % zenangle   = -1
      profiles_ad (i_proma) % azangle    = -1
      profiles_ad (i_proma) % ctp        = 0.0_JPRB 
      profiles_ad (i_proma) % cfraction  = 0.0_JPRB 
    
   enddo!* Initialize     
  emissivity_ad(:) = 0._JPRB
  
   radiance_ad % overcast    = 0.0_JPRB
   radiance_ad % clear_out   = 0.0_JPRB
   radiance_ad % out         = 0.0_JPRB
   radiance_ad % out_clear   = 0.0_JPRB
   radiance_ad % total_out   = 0.0_JPRB
   radiance_ad % clear       = 0.0_JPRB
   radiance_ad % cloudy      = 0.0_JPRB
   radiance_ad % total       = 0.0_JPRB
   radiance_ad % bt          = 0.0_JPRB
   radiance_ad % bt_clear    = 0.0_JPRB
   radiance_ad % upclear     = 0.0_JPRB
   radiance_ad % dnclear     = 0.0_JPRB
   radiance_ad % reflclear   = 0.0_JPRB
   radiance_ad % downcld     = 0.0_JPRB
   
     radiance_ad % out_clear(:) = radiance_tl % out_clear(:)
     radiance_ad % out(:)       = radiance_tl % out(:)


   call rttov_scatt_ad (errorstatus,        &! out
                      & kflevg,             &! in
                      & coef_rttov%nlevels, &! in
                      & nfrequencies,       &! in
                      & nchannels,          &! in
                      & nbtout,             &! in
                      & kproma,             &! in
                      & polarisations,      &! in
                      & channels,           &! in
                      & frequencies,        &! in
                      & lprofiles,          &! in
                      & lsprofiles,         &! in
                      & profiles_d1,        &! inout  
                      & cld_profiles_d1,    &! in
                      & coef_rttov,         &! in
                      & coef_scatt,         &! in
                      & calcemiss,          &! in
                      & emissivity_d1,      &! inout
                      & profiles_ad,        &! in
                      & cld_profiles_ad,    &! in
                      & emissivity_ad,      &! inout
                      & radiance_d2,        &! inout
                      & radiance_ad )        ! inout 
   
  If ( any( errorstatus(:) == errorstatus_fatal ) ) Then
     Do i_proma = 1, kproma
        If ( errorstatus(i_proma) == errorstatus_fatal ) Then
           write ( ioout, * ) 'rttov_scatt_ad error for profile',i_proma
        End If
     End Do
     Stop
  End If

  !* compute <delta_x,subad(delta_z)>

   zdelta2 = 0.0_JPRB
   
   do i_proma = 1, kproma
      do i_lev = 1, kflevg
         zdelta2 = zdelta2 &
             &  + cld_profiles_tl (i_proma) % p    (i_lev) * cld_profiles_ad (i_proma) % p    (i_lev) &
             &  + cld_profiles_tl (i_proma) % t    (i_lev) * cld_profiles_ad (i_proma) % t    (i_lev) &
             &  + cld_profiles_tl (i_proma) % q    (i_lev) * cld_profiles_ad (i_proma) % q    (i_lev) & 
             &  + cld_profiles_tl (i_proma) % cc   (i_lev) * cld_profiles_ad (i_proma) % cc   (i_lev) &
             &  + cld_profiles_tl (i_proma) % clw  (i_lev) * cld_profiles_ad (i_proma) % clw  (i_lev) &
             &  + cld_profiles_tl (i_proma) % ciw  (i_lev) * cld_profiles_ad (i_proma) % ciw  (i_lev) &
             &  + cld_profiles_tl (i_proma) % rain (i_lev) * cld_profiles_ad (i_proma) % rain (i_lev) &
             &  + cld_profiles_tl (i_proma) % sp   (i_lev) * cld_profiles_ad (i_proma) % sp   (i_lev) 
      enddo
      
      do i_lev = 1, kflevg + 1
         zdelta2 = zdelta2 &
             &  + cld_profiles_tl (i_proma) % ph   (i_lev) * cld_profiles_ad (i_proma) % ph   (i_lev)
      enddo
     
      do i_lev = 1, coef_rttov % nlevels 
         zdelta2 = zdelta2 & 
             &  + profiles_tl (i_proma) % p   (i_lev) * profiles_ad (i_proma) % p   (i_lev) &
             &  + profiles_tl (i_proma) % t   (i_lev) * profiles_ad (i_proma) % t   (i_lev) &
             &  + profiles_tl (i_proma) % q   (i_lev) * profiles_ad (i_proma) % q   (i_lev) &  
             &  + profiles_tl (i_proma) % clw (i_lev) * profiles_ad (i_proma) % clw (i_lev) &
             &  + profiles_tl (i_proma) % o3  (i_lev) * profiles_ad (i_proma) % o3  (i_lev)
      enddo
                 
      zdelta2 = zdelta2 + profiles_tl (i_proma) % s2m % p * profiles_ad (i_proma) % s2m % p
      zdelta2 = zdelta2 + profiles_tl (i_proma) % s2m % q * profiles_ad (i_proma) % s2m % q
      zdelta2 = zdelta2 + profiles_tl (i_proma) % s2m % o * profiles_ad (i_proma) % s2m % o
      zdelta2 = zdelta2 + profiles_tl (i_proma) % s2m % t * profiles_ad (i_proma) % s2m % t
      zdelta2 = zdelta2 + profiles_tl (i_proma) % s2m % u * profiles_ad (i_proma) % s2m % u
      zdelta2 = zdelta2 + profiles_tl (i_proma) % s2m % v * profiles_ad (i_proma) % s2m % v
      
      zdelta2 = zdelta2 + profiles_tl (i_proma) % skin % t * profiles_ad (i_proma) % skin % t  
      
      do i_fast = 1, fastem_sp     
         zdelta2 = zdelta2 + profiles_tl (i_proma) % skin % fastem (i_fast) * profiles_ad (i_proma) % skin % fastem (i_fast)
      enddo         
      
      zdelta2 = zdelta2 + profiles_tl (i_proma) % ctp        * profiles_ad (i_proma) % ctp      
      zdelta2 = zdelta2 + profiles_tl (i_proma) % cfraction  * profiles_ad (i_proma) % cfraction
      
      do i_chan = 1, nchannels
         zdelta2 = zdelta2 + emissivity_tl (i_chan) * emissivity_ad (i_chan)
      enddo
   enddo 

  if (zdelta2 == 0._JPRB) then
    z = 1._JPRB
  else
    z = zdelta2
  endif

   
   write (ioout,*) 'delta1 = ', zdelta1
   write (ioout,*) 'delta1 = ', zdelta2

    write (ioout,fmt= &
   & '('' The difference is '',f9.3, '' times the zero of the machine '')') &
   & abs(zdelta2-zdelta1)/threshold/z

!* K-TEST ***********************************************************************************

  write(ioout,*)
  write(ioout,*) 'Test K'
  write(ioout,*) '------'
  write(ioout,*)


   do i_btout = 1, nbtout
      profiles_k (i_btout) % nlevels =  coef_rttov % nlevels 
      profiles_k (i_btout) % p   => pk__p   (:,i_btout)
      profiles_k (i_btout) % t   => pk__t   (:,i_btout)
      profiles_k (i_btout) % q   => pk__q   (:,i_btout)
      profiles_k (i_btout) % o3  => pk__o3  (:,i_btout)
      profiles_k (i_btout) % clw => pk__clw (:,i_btout)

      cld_profiles_k (i_btout) % nlevels = kflevg 
      cld_profiles_k (i_btout) % p   => cpk__p    (:,i_btout) 
      cld_profiles_k (i_btout) % ph  => cpk__ph   (:,i_btout)
      cld_profiles_k (i_btout) % t   => cpk__t    (:,i_btout)
      cld_profiles_k (i_btout) % q   => cpk__q    (:,i_btout)
      cld_profiles_k (i_btout) % cc  => cpk__cc   (:,i_btout)
      cld_profiles_k (i_btout) % clw => cpk__clw  (:,i_btout)
      cld_profiles_k (i_btout) % ciw => cpk__ciw  (:,i_btout)
      cld_profiles_k (i_btout) % rain=> cpk__rain (:,i_btout)
      cld_profiles_k (i_btout) % sp  => cpk__sp   (:,i_btout)

      cld_profiles_k (i_btout) % p  (:) = 0.0_JPRB  
      cld_profiles_k (i_btout) % ph (:) = 0.0_JPRB  

      cld_profiles_k (i_btout) % t    (1:kflevg) = 0.0_JPRB  
      cld_profiles_k (i_btout) % q    (1:kflevg) = 0.0_JPRB  
      cld_profiles_k (i_btout) % cc   (1:kflevg) = 0.0_JPRB  
      cld_profiles_k (i_btout) % clw  (1:kflevg) = 0.0_JPRB  
      cld_profiles_k (i_btout) % ciw  (1:kflevg) = 0.0_JPRB  
      cld_profiles_k (i_btout) % rain (1:kflevg) = 0.0_JPRB  
      cld_profiles_k (i_btout) % sp   (1:kflevg) = 0.0_JPRB  

      cld_profiles_k (i_btout) % cc   (1:kflevg) = 0.0_JPRB  

      profiles_k (i_btout) % p   (:) = 0.0_JPRB
      profiles_k (i_btout) % clw (:) = 0.0_JPRB 
      profiles_k (i_btout) % o3  (:) = 0.0_JPRB 
      profiles_k (i_btout) % s2m % p = 0.0_JPRB 
      profiles_k (i_btout) % s2m % q = 0.0_JPRB 
      profiles_k (i_btout) % s2m % o = 0.0_JPRB 
      profiles_k (i_btout) % s2m % t = 0.0_JPRB 
      profiles_k (i_btout) % s2m % u = 0.0_JPRB 
      profiles_k (i_btout) % s2m % v = 0.0_JPRB 
      profiles_k (i_btout) % skin % surftype   = -1 
      profiles_k (i_btout) % skin % t          = 0.0_JPRB 
      profiles_k (i_btout) % skin % fastem (:) = 0.0_JPRB 

      profiles_k (i_btout) % ozone_data = .false.    
      profiles_k (i_btout) % co2_data   = .false.
      profiles_k (i_btout) % clw_data   = .false.
      profiles_k (i_btout) % zenangle   = -1
      profiles_k (i_btout) % azangle    = -1
      profiles_k (i_btout) % ctp        = 0.0_JPRB 
      profiles_k (i_btout) % cfraction  = 0.0_JPRB 
    
      profiles_k (i_btout) % t (:) = 0.0_JPRB 
      profiles_k (i_btout) % q (:) = 0.0_JPRB 
   enddo

   emissivity_d1 (1:nchannels) = 0.0_JPRB 
   calcemiss     (1:nchannels) = emissivity_d1 (1:nchannels) < 0.01_JPRB      
   emissivity_k  (1:nchannels) = 0.0_JPRB 

   call rttov_scatt_k (errorstatus,        &! out
                     & kflevg,             &! in
                     & coef_rttov%nlevels, &! in
                     & nfrequencies,       &! in
                     & nchannels,          &! in
                     & nbtout,             &! in
                     & kproma,             &! in
                     & polarisations,      &! in
                     & channels,           &! in
                     & frequencies,        &! in
                     & lprofiles,          &! in
                     & lsprofiles,         &! in
                     & profiles_d1,        &! inout  
                     & cld_profiles_d1,    &! in
                     & coef_rttov,         &! in
                     & coef_scatt,         &! in
                     & calcemiss,          &! in
                     & emissivity_d1,      &! inout
                     & profiles_k,         &! in
                     & cld_profiles_k,     &! in
                     & emissivity_k,       &! inout
                     & radiance_d1)         ! inout

  If ( Any( abs(radiance_total_ref(:) - radiance_d1 % total(:)) > threshold * radiance_total_ref(:)  ))  Then
    write(ioout,*) 'wrong forward model in K'
    write(ioout,*) radiance_total_ref(:)
    write(ioout,*) radiance_d1 % total(:)
    write(ioout,*) abs(radiance_total_ref(:)-radiance_d1 %total(:)) / ( threshold * radiance_total_ref(:))
    Stop
  Endif

  !---------------------------
  ! Compares K to AD

!* Write out Jacobian matrices
   do i_btout = 1, nchannels  
      radiance_ad % overcast    = 0.0_JPRB
      radiance_ad % clear       = 0.0_JPRB
      radiance_ad % cloudy      = 0.0_JPRB
      radiance_ad % total       = 0.0_JPRB
      radiance_ad % bt          = 0.0_JPRB
      radiance_ad % bt_clear    = 0.0_JPRB
      radiance_ad % upclear     = 0.0_JPRB
      radiance_ad % dnclear     = 0.0_JPRB
      radiance_ad % reflclear   = 0.0_JPRB
      radiance_ad % downcld     = 0.0_JPRB
  enddo

   do i_btout = 1, nbtout   
      radiance_ad % clear_out   = 0.0_JPRB
      radiance_ad % out         = 0.0_JPRB
      radiance_ad % out_clear   = 0.0_JPRB
      radiance_ad % total_out   = 0.0_JPRB
   
!     radiance_ad % bt (i_btout) = 1.0_JPRB
      radiance_ad % out (i_btout) = 1.0_JPRB
              
      do i_proma = 1, kproma
!* Reset perturbations
         cld_profiles_ad (i_proma) % p  (:) = 0.0_JPRB  
         cld_profiles_ad (i_proma) % ph (:) = 0.0_JPRB  

         cld_profiles_ad (i_proma) % t    (:) = 0.0_JPRB  
         cld_profiles_ad (i_proma) % q    (:) = 0.0_JPRB  
         cld_profiles_ad (i_proma) % cc   (:) = 0.0_JPRB  
         cld_profiles_ad (i_proma) % clw  (:) = 0.0_JPRB  
         cld_profiles_ad (i_proma) % ciw  (:) = 0.0_JPRB  
         cld_profiles_ad (i_proma) % rain (:) = 0.0_JPRB  
         cld_profiles_ad (i_proma) % sp   (:) = 0.0_JPRB  

!* Fill in RTTOV/RTTOVSCATT arrays once per profile       
         profiles_ad (i_proma) % p   (:) = 0.0_JPRB
         profiles_ad (i_proma) % t   (:) = 0.0_JPRB 
         profiles_ad (i_proma) % q   (:) = 0.0_JPRB 
         profiles_ad (i_proma) % clw (:) = 0.0_JPRB 
         profiles_ad (i_proma) % o3  (:) = 0.0_JPRB 
      
         profiles_ad (i_proma) % s2m % p = 0.0_JPRB 
         profiles_ad (i_proma) % s2m % q = 0.0_JPRB 
         profiles_ad (i_proma) % s2m % o = 0.0_JPRB 
         profiles_ad (i_proma) % s2m % t = 0.0_JPRB 
         profiles_ad (i_proma) % s2m % u = 0.0_JPRB 
         profiles_ad (i_proma) % s2m % v = 0.0_JPRB 
      
         profiles_ad (i_proma) % skin % surftype   = -1 
         profiles_ad (i_proma) % skin % t          = 0.0_JPRB 
         profiles_ad (i_proma) % skin % fastem (:) = 0.0_JPRB 

         profiles_ad (i_proma) % ozone_data = .false.    
         profiles_ad (i_proma) % co2_data   = .false.
         profiles_ad (i_proma) % clw_data   = .false.
         profiles_ad (i_proma) % zenangle   = -1
         profiles_ad (i_proma) % azangle    = -1
         profiles_ad (i_proma) % ctp        = 0.0_JPRB 
         profiles_ad (i_proma) % cfraction  = 0.0_JPRB     
      enddo 
   
!      emissivity_d1 (1:nchannels) = 0.0_JPRB 
!      calcemiss     (1:nchannels) = emissivity_d1 (1:nchannels) < 0.01_JPRB      
      emissivity_ad (1:nchannels) = 0.0_JPRB    
   
   
      call rttov_scatt_ad (errorstatus,        &! out
                         & kflevg,             &! in
                         & coef_rttov%nlevels, &! in
                         & nfrequencies,       &! in
                         & nchannels,          &! in
                         & nbtout,             &! in
                         & kproma,             &! in
                         & polarisations,      &! in
                         & channels,           &! in
                         & frequencies,        &! in
                         & lprofiles,          &! in
                         & lsprofiles,         &! in
                         & profiles_d1,        &! inout  
                         & cld_profiles_d1,    &! in
                         & coef_rttov,         &! in
                         & coef_scatt,         &! in
                         & calcemiss,          &! in
                         & emissivity_d1,      &! inout
                         & profiles_ad,        &! in
                         & cld_profiles_ad,    &! in
                         & emissivity_ad,      &! inout
                         & radiance_d2,        &! inout
                         & radiance_ad )        ! inout 
  
 
 
 
       i_proma = lsprofiles2 (i_btout)
                     
      
     do i_lev = 1, profiles_ad(i_proma) % nlevels

      if ( abs (profiles_ad (i_proma) % p   (i_lev) - profiles_k (i_btout) % p   (i_lev)) > threshold ) then
                  write(ioout,*) 'test K 1 fails',i_lev
		  Stop
      End If
      if ( abs (profiles_ad (i_proma) % t   (i_lev) - profiles_k (i_btout) % t   (i_lev)) > threshold ) then
                  write(ioout,*) 'test K 2 fails',i_lev
		  Stop
      End If
      if ( abs (profiles_ad (i_proma) % q   (i_lev) - profiles_k (i_btout) % q  (i_lev)) > threshold ) then
                  write(ioout,*) 'test K 3 fails',i_lev
		  Stop
      End If
      if ( abs (profiles_ad (i_proma) % o3   (i_lev) - profiles_k (i_btout) % o3  (i_lev)) > threshold ) then
                  write(ioout,*) 'test K 4 fails',i_lev
		  Stop
      End If
     End Do

     do i_lev = 1, cld_profiles_ad(i_proma) % nlevels
      if ( abs (cld_profiles_ad (i_proma) % p   (i_lev) - cld_profiles_k (i_btout) % p   (i_lev)) > threshold ) then
                  write(ioout,*) 'test K 5 fails',' prof ', i_proma, 'level ',i_lev
		  Stop
      End If
      if ( abs (cld_profiles_ad (i_proma) % ph   (i_lev) - cld_profiles_k (i_btout) % ph   (i_lev)) > threshold ) then
                  write(ioout,*) 'test K 6 fails',i_lev
		  Stop
      End If
      if ( abs (cld_profiles_ad (i_proma) % t   (i_lev) - cld_profiles_k (i_btout) % t   (i_lev)) > threshold ) then
                  write(ioout,*) 'test K 7 fails',i_lev
		  Stop
      End If
      if ( abs (cld_profiles_ad (i_proma) % cc   (i_lev) - cld_profiles_k (i_btout) % cc   (i_lev)) > threshold ) then
                  write(ioout,*) 'test K 8 fails',i_lev
		  Stop
      End If
      if ( abs (cld_profiles_ad (i_proma) % clw   (i_lev) - cld_profiles_k (i_btout) % clw   (i_lev)) > threshold ) then
                  write(ioout,*) 'test K 9 fails', i_lev
		  Stop
      End If
      if ( abs (cld_profiles_ad (i_proma) % ciw   (i_lev) - cld_profiles_k (i_btout) % ciw   (i_lev)) > threshold ) then
                  write(ioout,*) 'test K 10 fails',i_lev
		  Stop
      End If
      if ( abs (cld_profiles_ad (i_proma) % rain   (i_lev) - cld_profiles_k (i_btout) % rain   (i_lev)) > threshold ) then
                  write(ioout,*) 'test K 11 fails',i_lev
		  Stop
      End If
      if ( abs (cld_profiles_ad (i_proma) % sp   (i_lev) - cld_profiles_k (i_btout) % sp   (i_lev)) > threshold ) then
                  write(ioout,*) 'test K 12 fails',i_lev
		  Stop
      End If
     End Do
     
      if ( abs (profiles_ad (i_proma)  % s2m % p - profiles_k (i_btout) %  s2m % p) > threshold ) then
                  write(ioout,*) 'test K 13 fails',i_lev
		  Stop
      End If
      if ( abs (profiles_ad (i_proma)  % s2m % q - profiles_k (i_btout) %  s2m % q) > threshold ) then
                  write(ioout,*) 'test K 14 fails',i_lev
		  Stop
      End If
      if ( abs (profiles_ad (i_proma)  % s2m % o - profiles_k (i_btout) %  s2m % o) > threshold ) then
                  write(ioout,*) 'test K 15 fails',i_lev
		  Stop
      End If
      if ( abs (profiles_ad (i_proma)  % s2m % t - profiles_k (i_btout) %  s2m % t) > threshold ) then
                  write(ioout,*) 'test K 16 fails',i_lev
		  Stop
      End If
      if ( abs (profiles_ad (i_proma)  % s2m % u - profiles_k (i_btout) %  s2m % u) > threshold ) then
                  write(ioout,*) 'test K 17 fails',i_lev
		  Stop
      End If
      if ( abs (profiles_ad (i_proma)  % s2m % v - profiles_k (i_btout) %  s2m % v) > threshold ) then
                  write(ioout,*) 'test K 18 fails',i_lev
		  Stop
      End If
      if ( abs (profiles_ad (i_proma)  % skin % t - profiles_k (i_btout) %  skin % t) > threshold ) then
                  write(ioout,*) 'test K 19 fails',i_lev
		  Stop
      End If
  
      if ( abs (profiles_ad (i_proma)  % ctp - profiles_k (i_btout) %  ctp) > threshold ) then
                  write(ioout,*) 'test K 21 fails',i_lev
		  Stop
      End If
      if ( abs (profiles_ad (i_proma)  % cfraction - profiles_k (i_btout) %  cfraction) > threshold ) then
                  write(ioout,*) 'test K 22 fails',i_lev
		  Stop
      End If
      if ( abs (emissivity_ad (i_proma) - emissivity_k (i_btout) ) > threshold ) then
                  write(ioout,*) 'test K 23 fails',i_lev
		  Stop
      End If
     
      
   enddo

  write(ioout,*) 'K is ok'
  write(ioout,*)
  write(ioout,*) 'End of RTTOVSCATT tests'
  close (ioout)


1111 format (a6,1x,100(1x,E12.6))
      
End subroutine rttov_scatt_test

!*******
 
