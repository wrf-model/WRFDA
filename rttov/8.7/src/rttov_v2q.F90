!
Subroutine rttov_v2q (&
      & h2o_unit,  &! in
      & h2o,       &! in
      & gaz_id,    &! in
      & v_gaz,     &! in
      & q_gaz     ) ! inout
  !
  ! Description:
  !   Conversion of volume mixing ratio to specific concentration.
  !   Gases are defined by the "gas_id_xxx" codes in the rttov_const module
  !   Method use an equivalent molecular weight of wet air
  !
  ! Copyright:
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
  !
  !
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version    Date      Comment
  !  1.0    27/01/2003  Original code. (P. Brunel)
  !  1.1    13/02/2003  Remove capability of array of gases (P. Brunel)
  !
  ! Code Description:
  !   FORTRAN 90, following AAPP standards
  !
  ! Declarations
  !
  ! Global variables:
  ! Modules used:
  ! Imported Parameters:
  Use rttov_const, Only :   &
       & mair                ,&
       & mh2o                ,&
       & mo3                 ,&
       & mco2                ,&
       & mn2o                ,&
       & mco                 ,&
       & mch4                ,&
       & gas_id_mixed        ,&
       & gas_id_watervapour  ,&
       & gas_id_ozone        ,&
       & gas_id_wvcont       ,&
       & gas_id_co2          ,&
       & gas_id_n2o          ,&
       & gas_id_co           ,&
       & gas_id_ch4          ,&
       & gas_unit_specconc   ,&
       & gas_unit_ppmv

  Use parkind1, Only : jpim     ,jprb
  Implicit None

  ! Subroutine arguments
  !   Scalar arguments with intent(in):
  Integer(Kind=jpim) , Intent (in) :: h2o_unit ! Water vapour input unit
                                               ! 1 = specific concent. (kg/kg)
                                               ! 2 = volume mixing ratio (ppmv)
                                               ! (see gaz id codes in module rttov_const)
  Real(Kind=jprb)    , Intent (in) :: h2o      ! Water Vapour content in unit h2o_unit

  Integer(Kind=jpim) , Intent (in) :: gaz_id   ! Gaz identification number
                                               ! (see gaz id codes in module rttov_const)
  Real(Kind=jprb)    , Intent (in)   :: v_gaz  ! volume mixing ratio for gaz (ppmv)
  Real(Kind=jprb)    , Intent (inout):: q_gaz  ! specific concentration for gaz (kg/kg)




  ! Local parameter
  Real(Kind=jprb), Parameter :: eps = mh2o / mair



  ! Local variables
  Real(Kind=jprb) :: Mwet  ! equivalent molecular weight of wet air (g)
  Real(Kind=jprb) :: v_h2o ! volume mixing ratio for Water Vapour (v:v)

  !- End of header --------------------------------------------------------

  ! Calculate volume mixing ratio (no unit) for Water Vapour
  If( h2o_unit == gas_unit_specconc ) then
     v_h2o = h2o / (eps * (1-h2o) + h2o)
  Else If( h2o_unit == gas_unit_ppmv ) then
     v_h2o = h2o * 1.e-06_JPRB
  Else
     v_h2o = 0._JPRB
  End If

  ! Humid air molar mass
  Mwet = (1 - v_h2o)*Mair + v_h2o*Mh2o

  ! calculate specific concentration for gaz (kg/kg)
  Select Case( gaz_id )
  Case( gas_id_mixed )
     ! keep same value for Mixed gases
     q_gaz = v_gaz

  Case( gas_id_watervapour )
     q_gaz = v_gaz * 1.e-06_JPRB * Mh2o / Mwet
     !q_gaz = v_gaz / 1.60771704e+6

  Case( gas_id_ozone )
     q_gaz = v_gaz * 1.e-06_JPRB * Mo3  / Mwet
     !q_gaz = v_gaz / 6.03504e+5

  Case( gas_id_wvcont )
     q_gaz = v_gaz * 1.e-06_JPRB * Mh2o / Mwet

  Case( gas_id_co2 )
     q_gaz = v_gaz * 1.e-06_JPRB * Mco2 / Mwet

  Case( gas_id_n2o )
     q_gaz = v_gaz * 1.e-06_JPRB * Mn2o / Mwet

  Case( gas_id_co )
     q_gaz = v_gaz * 1.e-06_JPRB * Mco  / Mwet

  Case( gas_id_ch4 )
     q_gaz = v_gaz * 1.e-06_JPRB * Mch4 / Mwet

  Case Default
     q_gaz = 0._JPRB

  End Select



End Subroutine rttov_v2q
