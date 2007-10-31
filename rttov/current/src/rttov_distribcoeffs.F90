!
Subroutine rttov_distribcoeffs  (&
      & kmyproc,       &! id proc
      & kioproc,       &! proc for io
      & coef           )! inout
  ! Description:
  !
  ! Communicate the coefficient reading by proc 1 to other procs
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
  ! Method:
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date     Comment
  ! -------   ----     -------
  !  1.0       13/05/2004  Original
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Parameters:
  Use rttov_const, Only :  gas_id_mixed, ngases_max

  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  ! subroutine arguments
  ! scalar arguments with intent(in):
  Integer(Kind=jpim), Intent(in) :: kmyproc  ! logical processor id
  Integer(Kind=jpim), Intent(in) :: kioproc  ! processor  dedicated for io

  ! scalar arguments with intent(inout):
  Type( rttov_coef ), Intent (inout) :: coef   ! coefficients

  ! Local Scalars:
  Integer(Kind=jpim)  :: kcoef(57)
  Real(Kind=JPRB)     :: zcoef(4)
  Character (len=144) :: ccoef
  Integer(Kind=jpim)  :: kdim,itag
  Integer(Kind=jpim)  :: i
  Logical             :: lallo

  ! Functions:

  !- End of header --------------------------------------------------------

  ! 0 Initialise variables
  !---------------------------------------------

  kcoef(:) = 0
  zcoef(:) = 0.0
  ccoef =  'xxxx'
  lallo = kmyproc.ne.kioproc

  If (kmyproc == kioproc) Then
    kcoef(1)= coef % id_platform
    kcoef(2)= coef % id_sat
    kcoef(3)= coef % id_inst
    kcoef(4)= coef % id_sensor
    kcoef(5)= coef % id_comp_lvl
    kcoef(6:8)= coef % id_creation_date
    kcoef(9)= coef % fmv_chn
    kcoef(10)= coef % fmv_gas
    kcoef(11)= coef % nmixed
    kcoef(12)= coef % nwater
    kcoef(13)= coef % nozone
    kcoef(14)= coef % nwvcont
    kcoef(15)= coef % nco2
    kcoef(16)= coef % nn2o
    kcoef(17)= coef % nco
    kcoef(18)= coef % nch4
    kcoef(19)= coef % nlevels
    kcoef(20)= coef % fastem_ver
    kcoef(21)= coef % fastem_coef_nb
    kcoef(22)= coef % ssirem_ver

! Check if array are associated in PE1 to send the information to other PEs

    kcoef(23:57)=0

    If (Associated( coef % fmv_gas_id))  kcoef(23)=1
    If (Associated( coef % fmv_gas_pos)) kcoef(24)=1
    If (Associated( coef % fmv_var))     kcoef(25)=1
    If (Associated( coef % fmv_lvl))     kcoef(26)=1
    If (Associated( coef % ff_ori_chn))  kcoef(27)=1
    If (Associated( coef % ff_val_chn))  kcoef(28)=1
    If (Associated( coef % ff_cwn))      kcoef(29)=1
    If (Associated( coef % ff_bco))      kcoef(30)=1
    If (Associated( coef % ff_bcs))      kcoef(31)=1
    If (Associated( coef % ff_gam))      kcoef(32)=1
    If (Associated( coef % fastem_polar)) kcoef(33)=1
    If (Associated( coef % ssirem_chn))  kcoef(34)=1
    If (Associated( coef % ssirem_a0))   kcoef(35)=1
    If (Associated( coef % ssirem_a1))   kcoef(36)=1
    If (Associated( coef % ssirem_a2))   kcoef(37)=1
    If (Associated( coef % ssirem_xzn1)) kcoef(38)=1
    If (Associated( coef % ssirem_xzn2)) kcoef(39)=1
    If (Associated( coef % fastem_coef)) kcoef(40)=1
    If (Associated( coef % gaz_units))   kcoef(41)=1
    If (Associated( coef % ref_prfl_t))  kcoef(42)=1
    If (Associated( coef % ref_prfl_mr)) kcoef(43)=1
    If (Associated( coef % lim_prfl_p))  kcoef(44)=1
    If (Associated( coef % lim_prfl_tmax)) kcoef(45)=1
    If (Associated( coef % lim_prfl_tmin)) kcoef(46)=1
    If (Associated( coef % lim_prfl_gmax)) kcoef(47)=1
    If (Associated( coef % lim_prfl_gmin)) kcoef(48)=1
    If (Associated( coef % mixedgas))    kcoef(49)=1
    If (Associated( coef % watervapour)) kcoef(50)=1
    If (Associated( coef % ozone))       kcoef(51)=1
    If (Associated( coef % wvcont))      kcoef(52)=1
    If (Associated( coef % co2))         kcoef(53)=1
    If (Associated( coef % n2o))         kcoef(54)=1
    If (Associated( coef % co))          kcoef(55)=1
    If (Associated( coef % ch4))         kcoef(56)=1
    If (Associated( coef % ref_prfl_p))  kcoef(57)=1

    ccoef= coef % id_creation//coef % id_Common_name//coef % fmv_model_def

    zcoef(1)= coef % fc_speedl
    zcoef(2)= coef % fc_planck_c1
    zcoef(3)= coef % fc_planck_c2
    zcoef(4)= coef % fc_sat_height
  End If

! Send 'constant' information to other PEs

    itag=7000
!    Call broadcint(kcoef,57,kioproc,ITAG+1)
!    Call broadcreal(zcoef,4,kioproc,ITAG+2)
!    Call broadcchar(ccoef,144,kioproc,ITAG+3)

    If (kmyproc.ne.kioproc) Then
      coef % id_platform=kcoef(1)
      coef % id_sat=kcoef(2)
      coef % id_inst=kcoef(3)
      coef % id_sensor=kcoef(4)
      coef % id_comp_lvl=kcoef(5)
      coef % id_creation_date=kcoef(6:8)
      coef % fmv_chn=kcoef(9)
      coef % fmv_gas=kcoef(10)
      coef % nmixed=kcoef(11)
      coef % nwater=kcoef(12)
      coef % nozone=kcoef(13)
      coef % nwvcont=kcoef(14)
      coef % nco2=kcoef(15)
      coef % nn2o=kcoef(16)
      coef % nco=kcoef(17)
      coef % nch4=kcoef(18)
      coef % nlevels=kcoef(19)
      coef % fastem_ver=kcoef(20)
      coef % fastem_coef_nb=kcoef(21)
      coef % ssirem_ver=kcoef(22)

      coef % id_creation=ccoef(1:80)
      coef % id_Common_name=ccoef(81:112)
      coef % fmv_model_def=ccoef(113:144)

      coef % fc_speedl=zcoef(1)
      coef % fc_planck_c1=zcoef(2)
      coef % fc_planck_c2=zcoef(3)
      coef % fc_sat_height=zcoef(4)
    End If

! Send 'arrays' information if associated to other PEs

  If( kcoef(23) == 1 ) Then
    if (lallo) Allocate(coef % fmv_gas_id ( coef % fmv_gas ))
  !  Call broadcint(coef % fmv_gas_id,coef % fmv_gas,kioproc,ITAG+23)
  End If

  If( kcoef(24) == 1 ) Then
    if (lallo) Allocate(coef % fmv_gas_pos (  ngases_max ))
  !  Call broadcint(coef % fmv_gas_pos, ngases_max,kioproc,ITAG+24)
  End If

  If( kcoef(25) == 1 ) Then
    if (lallo) Allocate(coef % fmv_var ( coef % fmv_gas ))
  !  Call broadcint(coef % fmv_var,coef % fmv_gas,kioproc,ITAG+25)
  End If

  If( kcoef(26) == 1 ) Then
    if (lallo) Allocate(coef % fmv_lvl ( coef % fmv_gas ))
  !  Call broadcint(coef % fmv_lvl,coef % fmv_gas,kioproc,ITAG+26)
  End If

  If( kcoef(27) == 1 ) Then
    if (lallo) Allocate(coef % ff_ori_chn ( coef % fmv_chn ))
  !  Call broadcint(coef % ff_ori_chn,coef % fmv_chn,kioproc,ITAG+27)
  End If

  If( kcoef(28) == 1 ) Then
    if (lallo) Allocate(coef % ff_val_chn ( coef % fmv_chn ))
  !  Call broadcint(coef % ff_val_chn,coef % fmv_chn,kioproc,ITAG+28)
  End If

  If( kcoef(29) == 1 ) Then
    if (lallo) Allocate(coef % ff_cwn ( coef % fmv_chn ))
  !  Call broadcreal(coef % ff_cwn,coef % fmv_chn,kioproc,ITAG+29)
  End If

  If( kcoef(30) == 1 ) Then
    if (lallo) Allocate(coef % ff_bco ( coef % fmv_chn ))
  !  Call broadcreal(coef % ff_bco,coef % fmv_chn,kioproc,ITAG+30)
  End If

  If( kcoef(31) == 1 ) Then
    if (lallo) Allocate(coef % ff_bcs ( coef % fmv_chn ))
  !  Call broadcreal(coef % ff_bcs,coef % fmv_chn,kioproc,ITAG+31)
  End If

  If( kcoef(32) == 1 ) Then
    if (lallo) Allocate(coef % ff_gam ( coef % fmv_chn ))
  !  Call broadcreal(coef % ff_gam,coef % fmv_chn,kioproc,ITAG+32)
  End If

  If( kcoef(33) == 1 ) Then
    if (lallo) Allocate(coef % fastem_polar ( coef % fmv_chn ))
  !  Call broadcint(coef % fastem_polar,coef % fmv_chn,kioproc,ITAG+33)
  End If

  If( kcoef(34) == 1 ) Then
    if (lallo) Allocate(coef % ssirem_chn ( coef % fmv_chn ))
  !  Call broadcint(coef % ssirem_chn,coef % fmv_chn,kioproc,ITAG+34)
  End If

  If( kcoef(35) == 1 ) Then
    if (lallo) Allocate(coef % ssirem_a0 ( coef % fmv_chn ))
  !  Call broadcreal(coef % ssirem_a0,coef % fmv_chn,kioproc,ITAG+35)
  End If

  If( kcoef(36) == 1 ) Then
    if (lallo) Allocate(coef % ssirem_a1 ( coef % fmv_chn ))
  !  Call broadcreal(coef % ssirem_a1,coef % fmv_chn,kioproc,ITAG+36)
  End If

  If( kcoef(37) == 1 ) Then
    if (lallo) Allocate(coef % ssirem_a2 ( coef % fmv_chn ))
  !  Call broadcreal(coef % ssirem_a2,coef % fmv_chn,kioproc,ITAG+37)
  End If

  If( kcoef(38) == 1 ) Then
    if (lallo) Allocate(coef % ssirem_xzn1 ( coef % fmv_chn ))
  !  Call broadcreal(coef % ssirem_xzn1,coef % fmv_chn,kioproc,ITAG+38)
  End If

  If( kcoef(39) == 1 ) Then
    if (lallo) Allocate(coef % ssirem_xzn2 ( coef % fmv_chn ))
  !  Call broadcreal(coef % ssirem_xzn2,coef % fmv_chn,kioproc,ITAG+39)
  End If

  If( kcoef(40) == 1 ) Then
    if (lallo) Allocate(coef % fastem_coef ( coef % fastem_coef_nb ))
  !  Call broadcreal(coef % fastem_coef,coef % fastem_coef_nb,kioproc,ITAG+40)
  End If

  If( kcoef(41) == 1 ) Then
    if (lallo) Allocate(coef % gaz_units ( coef % fmv_gas ))
  !  Call broadcint(coef % gaz_units,coef % fmv_gas,kioproc,ITAG+41)
  End If

  If( kcoef(42) == 1 ) Then
    if (lallo) Allocate(coef % ref_prfl_t ( &
                     & coef % fmv_lvl(gas_id_mixed),coef % fmv_gas ) )
    Kdim= coef % fmv_lvl(gas_id_mixed)*coef % fmv_gas
  !  Call broadcreal(coef % ref_prfl_t,kdim,kioproc,ITAG+42)
  End If

  If( kcoef(43) == 1 ) Then
    if (lallo) Allocate(coef % ref_prfl_mr ( &
                     & coef % fmv_lvl(gas_id_mixed),coef % fmv_gas ) )
    Kdim= coef % fmv_lvl(gas_id_mixed)*coef % fmv_gas
  !  Call broadcreal(coef % ref_prfl_mr,kdim,kioproc,ITAG+43)
  End If

  If( kcoef(44) == 1 ) Then
    if (lallo) Allocate(coef % lim_prfl_p ( &
                     & coef % fmv_lvl(gas_id_mixed)))
    Kdim= coef % fmv_lvl(gas_id_mixed)
  !  Call broadcreal(coef % lim_prfl_p,kdim,kioproc,ITAG+44)
  End If

  If( kcoef(45) == 1 ) Then
    if (lallo) Allocate(coef % lim_prfl_tmax ( &
                     & coef % fmv_lvl(gas_id_mixed)))
    Kdim= coef % fmv_lvl(gas_id_mixed)
  !  Call broadcreal(coef % lim_prfl_tmax,kdim,kioproc,ITAG+45)
  End If

  If( kcoef(46) == 1 ) Then
    if (lallo) Allocate(coef % lim_prfl_tmin ( &
                     & coef % fmv_lvl(gas_id_mixed)))
    Kdim= coef % fmv_lvl(gas_id_mixed)
  !  Call broadcreal(coef % lim_prfl_tmin,kdim,kioproc,ITAG+46)
  End If

  If( kcoef(47) == 1 ) Then
    if (lallo) Allocate(coef % lim_prfl_gmax ( &
                     & coef % fmv_lvl(gas_id_mixed),coef % fmv_gas ) )
    Kdim= coef % fmv_lvl(gas_id_mixed)*coef % fmv_gas
  !  Call broadcreal(coef % lim_prfl_gmax,kdim,kioproc,ITAG+47)
  End If

  If( kcoef(48) == 1 ) Then
    if (lallo) Allocate(coef % lim_prfl_gmin ( &
                     & coef % fmv_lvl(gas_id_mixed),coef % fmv_gas ) )
    Kdim= coef % fmv_lvl(gas_id_mixed)*coef % fmv_gas
  !  Call broadcreal(coef % lim_prfl_gmin,kdim,kioproc,ITAG+48)
  End If

  If( kcoef(49) == 1 ) Then
    if (lallo) Allocate(coef % mixedgas ( coef % nlevels, &
                     & coef % fmv_chn, coef % nmixed))
    Kdim=  coef % nlevels*coef % fmv_chn*coef % nmixed
  !  Call broadcreal(coef % mixedgas,kdim,kioproc,ITAG+49)
  End If

  If( kcoef(50) == 1 ) Then
    if (lallo) Allocate(coef % watervapour ( coef % nlevels, &
                     & coef % fmv_chn, coef % nwater))
    Kdim=  coef % nlevels*coef % fmv_chn*coef % nwater
  !  Call broadcreal(coef % watervapour,kdim,kioproc,ITAG+50)
  End If

  If( kcoef(51) == 1 ) Then
    if (lallo) Allocate(coef % ozone ( coef % nlevels, &
                     & coef % fmv_chn, coef % nozone))
    Kdim=  coef % nlevels*coef % fmv_chn*coef % nozone
  !  Call broadcreal(coef % ozone,kdim,kioproc,ITAG+51)
  End If

  If( kcoef(52) == 1 ) Then
    if (lallo) Allocate(coef % wvcont ( coef % nlevels, &
                     & coef % fmv_chn, coef % nwvcont))
    Kdim=  coef % nlevels*coef % fmv_chn*coef % nwvcont
  !  Call broadcreal(coef % wvcont,kdim,kioproc,ITAG+52)
  End If

  If( kcoef(53) == 1 ) Then
    if (lallo) Allocate(coef % co2 ( coef % nlevels, &
                     & coef % fmv_chn, coef % nco2))
    Kdim=  coef % nlevels*coef % fmv_chn*coef % nco2
  !  Call broadcreal(coef % co2,kdim,kioproc,ITAG+53)
  End If

  If( kcoef(54) == 1 ) Then
    if (lallo) Allocate(coef % n2o ( coef % nlevels, &
                     & coef % fmv_chn, coef % nn2o))
    Kdim=  coef % nlevels*coef % fmv_chn*coef % nn2o
  !  Call broadcreal(coef % n2o,kdim,kioproc,ITAG+54)
  End If

  If( kcoef(55) == 1 ) Then
    if (lallo) Allocate(coef % co ( coef % nlevels, &
                     & coef % fmv_chn, coef % nco))
    Kdim=  coef % nlevels*coef % fmv_chn*coef % nco
  !  Call broadcreal(coef % co,kdim,kioproc,ITAG+55)
  End If

  If( kcoef(56) == 1 ) Then
    if (lallo) Allocate(coef % ch4 ( coef % nlevels, &
                     & coef % fmv_chn, coef % nch4))
    Kdim=  coef % nlevels*coef % fmv_chn*coef % nch4
  !  Call broadcreal(coef % ch4,kdim,kioproc,ITAG+56)
  End If

  If( kcoef(57) == 1 ) Then
    if (lallo) Allocate(coef % ref_prfl_p ( &
                     & coef % fmv_lvl(gas_id_mixed)))
    Kdim= coef % fmv_lvl(gas_id_mixed)
  !  Call broadcreal(coef % ref_prfl_p,kdim,kioproc,ITAG+57)
  End If


End Subroutine rttov_distribcoeffs
