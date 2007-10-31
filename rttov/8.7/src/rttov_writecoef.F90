!
Subroutine Rttov_writecoef (errorstatus, coef, file_id, lbinary)
  ! Description:
  ! write on unit file_id the coef structure.
  ! If lbinary is false or not present the file is assumed as
  ! an ASCII sequential formatted, in other case it is sequential unformatted.
  ! I/O write status are only tested at the end of the code
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
  !  1.0       01/12/2002  New F90 code with structures (P Brunel A Smith)
  !  1.1       24/01/2003  insert I/O status (P Brunel)
  !                        one record per channel for coefficients in binary format
  !                        New header to allow checking R4<->R8
  !  1.2       02/06/2004  Update for RTTOV8 coefs (P. Brunel)
  !
  ! Code Description:
  !   Language:           Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  ! Declarations:
  ! Modules used:
  ! Imported Parameters:
  Use rttov_const, Only :   &
       & rttov_magic_string  ,&
       & rttov_magic_number  ,&
       & errorstatus_success ,&
       & errorstatus_fatal   ,&
       & errorstatus_info    ,&
       & gas_id_mixed        ,&
       & gas_id_watervapour  ,&
       & gas_id_ozone        ,&
       & gas_id_wvcont       ,&
       & gas_id_co2          ,&
       & gas_id_n2o          ,&
       & gas_id_co           ,&
       & gas_id_ch4          ,&
       & gas_name            ,&
       & gas_unit_name

  ! Imported Type Definitions:
  Use rttov_types, Only : &
       & rttov_coef

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_errorreport.interface"

  ! subroutine arguments
  ! scalar arguments with intent(in):
  Integer(Kind=jpim),            Intent (in) :: file_id      ! file logical unit number
  Type( rttov_coef ), Intent (in) :: coef         ! coefficients
  Logical, Optional,  Intent (in) :: lbinary      ! if binary file wanted
  ! scalar arguments with intent(in):
  Integer(Kind=jpim),            Intent (out) :: errorstatus  ! return code



  ! local scalars
  Integer(Kind=jpim) :: i, j, l, k
  Logical :: file_binary
  Integer(Kind=jpim) :: io_status
  Character (len=2) :: sensor
  Character (len=32) :: section

  Character (len=80) :: errMessage
  Character (len=16) :: NameOfRoutine = 'rttov_writecoef '
  !- End of header --------------------------------------------------------

  errorstatus     = errorstatus_success

  ! Consider lbinary option to create the option
  If(Present(lbinary)) Then
     file_binary = lbinary
  Else
     file_binary = .False.
  Endif


  If( file_binary ) Then
     ! Binary file
     Write( errMessage, '( "write coefficient to file_id ", i2, " in binary format")' ) &
           & file_id
     Call Rttov_ErrorReport (errorstatus_info, errMessage, NameOfRoutine)

     ! Write a string that could be displayed
     ! Write a real number to be able to check single/double precision
     Write(file_id, iostat=io_status) rttov_magic_string, rttov_magic_number

     Write(file_id, iostat=io_status)&
           & coef % id_platform,&
           & coef % id_sat,     &
           & coef % id_inst,    &
           & coef % id_sensor
     Write(file_id, iostat=io_status)&
           & coef % id_comp_lvl,      &
           & coef % id_creation_date, &
           & coef % id_creation,      &
           & coef % id_Common_name
     If( coef % id_comp_lvl == 7 ) then
        Write(file_id, iostat=io_status)&
             & coef % fmv_model_def,&
             & coef % fmv_chn,      &
             & coef % fmv_gas
     Else
        Write(file_id, iostat=io_status)&
             & coef % fmv_model_def,&
             & coef % fmv_model_ver,&
             & coef % fmv_chn,      &
             & coef % fmv_gas
     Endif
     Write(file_id, iostat=io_status)&
           & coef % fmv_gas_id,  &
           & coef % fmv_gas_pos, &
           & coef % fmv_var,     &
           & coef % fmv_lvl
     Write(file_id, iostat=io_status)&
           & coef % gaz_units
     Write(file_id, iostat=io_status)&
           & coef % ff_ori_chn, &
           & coef % ff_val_chn, &
           & coef % ff_cwn,     &
           & coef % ff_bco,     &
           & coef % ff_bcs,     &
           & coef % ff_gam
     Write(file_id, iostat=io_status)&
           & coef % fc_speedl,    &
           & coef % fc_planck_c1, &
           & coef % fc_planck_c2, &
           & coef % fc_sat_height
     Write(file_id, iostat=io_status)&
           & coef % fastem_ver,&
           & coef % ssirem_ver
     If( coef % fastem_ver >= 1 ) Then
        Write(file_id, iostat=io_status)&
              & coef % fastem_coef_nb
        Write(file_id, iostat=io_status)&
              & coef % fastem_coef,&
              & coef % fastem_polar
     Endif
     If( coef % ssirem_ver >= 1 ) Then
        Write(file_id, iostat=io_status)&
              & coef % ssirem_chn, &
              & coef % ssirem_a0,  &
              & coef % ssirem_a1,  &
              & coef % ssirem_a2,  &
              & coef % ssirem_xzn1,&
              & coef % ssirem_xzn2
     Endif
     Write(file_id, iostat=io_status)&
           & coef % ref_prfl_p,&
           & coef % ref_prfl_t,&
           & coef % ref_prfl_mr
     Write(file_id, iostat=io_status)&
           & coef % lim_prfl_p,    &
           & coef % lim_prfl_tmax, &
           & coef % lim_prfl_tmin, &
           & coef % lim_prfl_gmax, &
           & coef % lim_prfl_gmin

     ! Write coefficients with ONE record per Channel
     If ( coef % nmixed > 0 ) Then
        Do i = 1, coef % fmv_chn
           Write(file_id,iostat=io_status) coef % mixedgas( : , i, : )
        End Do
     Endif
     If ( coef % nwater > 0 ) Then
        Do i = 1, coef % fmv_chn
           Write(file_id,iostat=io_status) coef % watervapour( : , i, : )
        End Do
     Endif
     If ( coef % nozone > 0 ) Then
        Do i = 1, coef % fmv_chn
           Write(file_id,iostat=io_status) coef % ozone( : , i, : )
        End Do
     Endif
     If ( coef % nwvcont > 0 ) Then
        Do i = 1, coef % fmv_chn
           Write(file_id,iostat=io_status) coef % wvcont( : , i, : )
        End Do
     Endif
     If ( coef % nco2 > 0 ) Then
        Do i = 1, coef % fmv_chn
           Write(file_id,iostat=io_status) coef % co2( : , i, : )
        End Do
     Endif
     If ( coef % nn2o > 0 ) Then
        Do i = 1, coef % fmv_chn
           Write(file_id,iostat=io_status) coef % n2o( : , i, : )
        End Do
     Endif
     If ( coef % nco > 0 ) Then
        Do i = 1, coef % fmv_chn
           Write(file_id,iostat=io_status) coef % co( : , i, : )
        End Do
     Endif
     If ( coef % nch4 > 0 ) Then
        Do i = 1, coef % fmv_chn
           Write(file_id,iostat=io_status) coef % ch4( : , i, : )
        End Do
     Endif
     !
     ! Add here other gases or new sections
     !

  Else

     !ASCII file
     Write( errMessage, '( "write coefficient to file_id ", i2, " in ASCII format")' ) &
           & file_id
     Call Rttov_ErrorReport (errorstatus_info, errMessage, NameOfRoutine)

     Write(file_id,'(a)') ' ! RTTOV coefficient file '//Trim(coef % id_Common_name)
     Write(file_id,'(a)') ' ! automatic creation by subroutine Rttov_writecoef '
     Write(file_id,'(a)') ' ! ------------------------------------------------------'

     ! IDENTIFICATION
     section = 'IDENTIFICATION'
     Select Case (coef % id_sensor)
     Case (1_jpim)
        sensor = 'ir'
     Case (2_jpim)
        sensor = 'mw'
     Case (3_jpim)
        sensor = 'hi'
     End Select
     Write(file_id,'(a)') Trim(section)
     Write(file_id,'(a)') ' ! '
     Write(file_id,'(3i3,T20,a)')&
           & coef % id_platform, coef % id_sat, coef % id_inst,'! platform sat_id instrument'
     Write(file_id,'(1x,a)')  coef % id_Common_name
     Write(file_id,'(1x,a,T20,a)')  sensor,'! sensor type [ir,mw,hi]'
     Write(file_id,'(1x,i2,T20,a)') coef % id_comp_lvl,'! RTTOV coefficient file version number'
     Write(file_id,'(1x,a)') coef % id_creation
     Write(file_id,'(1x,i4,1x,i2.2,1x,i2.2,t20,a)') coef % id_creation_date,'! creation date'

     ! No LINE-BY-LINE section

     ! FAST_MODEL_VARIABLES
     section = 'FAST_MODEL_VARIABLES'
     Write(file_id,'(a)') ' ! ------------------------------------------------------'
     Write(file_id,'(a)') Trim(section)
     Write(file_id,'(a)') ' ! '
     Write(file_id,'(a)') ' !'
     Write(file_id,'(1x,a,t20,a)')   coef % fmv_model_def,    '! fast model name'
     If( coef % id_comp_lvl > 7 ) then
        Write(file_id,'(1x,i4,t20,a)')   coef % fmv_model_ver,    '! fast model version compatibility level'
     Endif
     Write(file_id,'(1x,i4,t20,a)')  coef % fmv_chn ,   '! Number of channels described in the coef file'
     Write(file_id,'(1x,i4,t20,a)')  coef % fmv_gas ,   '! Number of gases described in the coef file'
     Do i = 1, coef % fmv_gas
        Write(file_id,'(1x,a,t20,a)')  Trim(gas_name( coef % fmv_gas_id( i ) ) ),'! gas identification'
        Write(file_id,'(1x,2i4,t20,a)')  coef % fmv_var(i), coef % fmv_lvl(i), '! variables/predictors  levels (pressure/absorber)'
     End Do

     section = 'FILTER_FUNCTIONS'
     Write(file_id,'(a)') ' ! ------------------------------------------------------'
     Write(file_id,'(a)') Trim(section)
     Write(file_id,'(a)') ' ! '
     Write(file_id,'(a)') ' ! Channel Number (from instrument original description)'
     Write(file_id,'(a)') ' ! Channel status '
     Write(file_id,'(a)') ' ! Central Wavenumber'
     Write(file_id,'(a)') ' ! Band Correction coefficients(Offset,Slope)'
     Write(file_id,'(a)') ' ! Gamma correction factor'

     Do i = 1, coef % fmv_chn
        Write(file_id,'(1x,i4,1x,i4,4(1x,e18.10))') &
              & coef % ff_ori_chn(i), coef % ff_val_chn(i), coef % ff_cwn(i),&
              & coef % ff_bco(i), coef % ff_bcs(i), coef % ff_gam(i)
     End Do

     ! GAZ_UNITS
     section = 'GAZ_UNITS'
     Write(file_id,'(a)') ' ! ------------------------------------------------------'
     Write(file_id,'(a)') Trim(section)
     Write(file_id,'(a)') ' ! Gaz concentrations can be expressed in '
     Write(file_id,'(a)') ' ! volume mixing ratio (ppmv)'
     Write(file_id,'(a)') ' ! specific concentration (kg/kg)'
     Write(file_id,'(a)') ' ! '
     Do i = 1, coef % fmv_gas
        Write(file_id,'(a)') ' !     '//gas_name( coef % fmv_gas_id( i ) )
        Write(file_id,'(1x,i4,t20,"! ",a)') &
                 & coef % gaz_units( i ), gas_unit_name( coef % gaz_units( i ) )
     End Do


     section = 'FUNDAMENTAL_CONSTANTS'
     Write(file_id,'(a)') ' ! ------------------------------------------------------'
     Write(file_id,'(a)') Trim(section)
     Write(file_id,'(a)') ' ! '
     Write(file_id,'(a)') ' ! units of constants for spectral radiance'
     Write(file_id,'(a)') ' ! first radiation constant(mW/(m2.sr.cm-4))'
     Write(file_id,'(a)') ' ! second radiation constant (cm.K)'
     Write(file_id,'(1x,f14.1,t30,a)') coef % fc_speedl,'! speed of light (cm/s)'
     Write(file_id,'(1x,1p,e15.8,0p,f10.6,t30,a)') coef % fc_planck_c1, coef % fc_planck_c2,'! Planck constants'
     Write(file_id,'(1x,f8.1,t30,a)') coef % fc_sat_height,'! nominal satellite height (km)'

     If( coef % fastem_ver >= 1 ) Then
        section = 'FASTEM'
        Write(file_id,'(a)') ' ! ------------------------------------------------------'
        Write(file_id,'(a)') Trim(section)
        Write(file_id,'(a)') ' ! '
        Write(file_id,'(a)') ' ! S. English fast generic millimetre wave ocean emissivity model'
        Write(file_id,'(a)') ' ! Polarisation of each channel', &
              & ' !       MPOL=0:  0.5_JPRB*(V+H)', &
              & ' !       MPOL=1: polarisation angle=90-incidence angle', &
              & ' !       MPOL=2: polarisation angle=incidence angle', &
              & ' !       MPOL=3: vertical polarisation', &
              & ' !       MPOL=4: horizontal polarisation'
        Write(file_id,'(1x,i2,a)') coef % fastem_ver,'   ! version number'
        Write(file_id,'(1x,i3,a)') coef % fastem_coef_nb,'  ! number of coefficients'
        Write(file_id,'(5e14.6)') coef % fastem_coef
        Write(file_id,'(20i3)') (coef % fastem_polar(i), i= 1, coef % fmv_chn)
     Endif

     If( coef % ssirem_ver >= 1 ) Then
        section = 'SSIREM'
        Write(file_id,'(a)') ' ! ------------------------------------------------------'
        Write(file_id,'(a)') Trim(section)
        Write(file_id,'(a)') ' ! '
        Write(file_id,'(a)') ' ! Channel Number (from instrument original description)'
        Write(file_id,'(a)') ' ! 5 coefficients for emissivity model ssirem'
        Write(file_id,'(1x,i2,a)') coef % ssirem_ver,'   ! version number'

        Do i = 1, coef % fmv_chn
           Write(file_id,'(1x,i4,3f12.7,2f4.1)') &
                 & coef % ssirem_chn(i) , coef % ssirem_a0(i),&
                 & coef % ssirem_a1(i)  , coef % ssirem_a2(i),&
                 & coef % ssirem_xzn1(i), coef % ssirem_xzn2(i)
        End Do
     Endif

     section = 'REFERENCE_PROFILE'
     Write(file_id,'(a)') ' ! ------------------------------------------------------'
     Write(file_id,'(a)') Trim(section)
     Write(file_id,'(a)') ' ! '
     Write(file_id,'(a)') ' ! Ref.pressure (hPa)'
     Write(file_id,'(a)') ' ! Ref.Temp (K) Ref.Volume Mixing Ratio [ppmv] for each gas'
     Write(file_id,'(a)') ' ! Note for MxG that mixing ratio is "missing"'

     Do i = 1, coef % fmv_gas
        Write(file_id,'(a)') ' !     '//gas_name( coef % fmv_gas_id( i ) )
        Do l = 1, coef % fmv_lvl(i)
           Write(file_id,'(1x,f8.3,2x,f7.3,1x,e13.6)')&
                 & coef % ref_prfl_p(l), coef % ref_prfl_t(l,i), coef % ref_prfl_mr(l,i)
!!$                & coef % ref_prfl_p(l), coef % ref_prfl_t(l,i), ref_mr(l,i)
        End Do
     End Do

     section = 'PROFILE_LIMITS'
     Write(file_id,'(a)') ' ! ------------------------------------------------------'
     Write(file_id,'(a)') Trim(section)
     Write(file_id,'(a)') ' ! '
     Write(file_id,'(a)') ' ! Ref.pressure (hPa)'
     Write(file_id,'(a)') ' ! Temp Max (K) Temp Min (K)'
     Write(file_id,'(a)') ' ! Volume Mixing Ratio for  Max and Min [ppmv] for each gas'
     Write(file_id,'(a)') ' !      Temperature'
     Do l = 1, coef % fmv_lvl(1)
        Write(file_id,'(1x,f8.3,2(1x,f7.2))',iostat=io_status)&
              & coef % lim_prfl_p(l), coef % lim_prfl_tmax(l), coef % lim_prfl_tmin(l)
     End Do

     Do i = 1, coef % fmv_gas
        Write(file_id,'(a)') ' !     '//gas_name( coef % fmv_gas_id( i ) )
        Do l = 1, coef % fmv_lvl(i)
           Write(file_id,'(1x,f8.3,2x,e12.4,e12.4)',iostat=io_status)&
                 & coef % lim_prfl_p(l), coef % lim_prfl_gmax(l,i), coef % lim_prfl_gmin(l,i)
        End Do
     End Do


     section = 'FAST_COEFFICIENTS'
     Write(file_id,'(a)') ' ! ------------------------------------------------------'
     Write(file_id,'(a)') Trim(section)
     Write(file_id,'(a)') ' ! '
     Write(file_id,'(a)') ' ! transmission coefficients'
     Write(file_id,'(a)') ' ! Order of the gases:'
     Do i = 1, coef % fmv_gas
        Write(file_id,'(a)') ' !     '//gas_name( coef % fmv_gas_id ( i ) )
     End Do


     Do l = 1, coef % fmv_gas
        Write(file_id,'(a)') gas_name( coef % fmv_gas_id( l ) )

        Select Case( coef % fmv_gas_id(l) )

        Case(gas_id_mixed)
           Write(file_id,'(5(1x,e15.8))',iostat=io_status)  &
                 & (((coef % mixedgas(i,j,k)   &
                  & ,i = 1, coef % fmv_lvl(l) ) &
                  & ,j = 1, coef % fmv_chn    ) &
                  & ,k = 1, coef % fmv_var(l) )
        Case(gas_id_watervapour)
           Write(file_id,'(5(1x,e15.8))',iostat=io_status)  &
                 & (((coef % watervapour(i,j,k)   &
                  & ,i = 1, coef % fmv_lvl(l) ) &
                  & ,j = 1, coef % fmv_chn    ) &
                  & ,k = 1, coef % fmv_var(l) )
        Case(gas_id_ozone)
           Write(file_id,'(5(1x,e15.8))',iostat=io_status)  &
                 & (((coef % ozone(i,j,k)   &
                  & ,i = 1, coef % fmv_lvl(l) ) &
                  & ,j = 1, coef % fmv_chn    ) &
                  & ,k = 1, coef % fmv_var(l) )
        Case(gas_id_wvcont)
           Write(file_id,'(5(1x,e15.8))',iostat=io_status)  &
                 & (((coef % wvcont(i,j,k)   &
                  & ,i = 1, coef % fmv_lvl(l) ) &
                  & ,j = 1, coef % fmv_chn    ) &
                  & ,k = 1, coef % fmv_var(l) )
        Case(gas_id_co2)
           Write(file_id,'(5(1x,e15.8))',iostat=io_status)  &
                 & (((coef % co2(i,j,k)   &
                  & ,i = 1, coef % fmv_lvl(l) ) &
                  & ,j = 1, coef % fmv_chn    ) &
                  & ,k = 1, coef % fmv_var(l) )
        Case(gas_id_n2o)
           Write(file_id,'(5(1x,e15.8))',iostat=io_status)  &
                 & (((coef % n2o(i,j,k)   &
                  & ,i = 1, coef % fmv_lvl(l) ) &
                  & ,j = 1, coef % fmv_chn    ) &
                  & ,k = 1, coef % fmv_var(l) )
        Case(gas_id_co)
           Write(file_id,'(5(1x,e15.8))',iostat=io_status)  &
                 & (((coef % co(i,j,k)   &
                  & ,i = 1, coef % fmv_lvl(l) ) &
                  & ,j = 1, coef % fmv_chn    ) &
                  & ,k = 1, coef % fmv_var(l) )
        Case(gas_id_ch4)
           Write(file_id,'(5(1x,e15.8))',iostat=io_status)  &
                 & (((coef % ch4(i,j,k)   &
                  & ,i = 1, coef % fmv_lvl(l) ) &
                  & ,j = 1, coef % fmv_chn    ) &
                  & ,k = 1, coef % fmv_var(l) )
        End Select
     End Do

     section = 'END'
     Write(file_id,'(a)') ' ! ------------------------------------------------------'
     Write(file_id,'(a)') Trim(section)

  Endif

  If( io_status /= 0 ) Then
     Write( errMessage, '( "write IO error")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  End If

End Subroutine Rttov_writecoef
