Program example_fwd
  !
  !    This software was developed within the context of
  !    the EUMETSAT Satellite Application Facility on
  !    Numerical Weather Prediction (NWP SAF), under the
  !    Cooperation Agreement dated 25 November 1998, between
  !    EUMETSAT and the Met Office, UK, by one or more partners
  !    within the NWP SAF. The partners in the NWP SAF are
  !    the Met Office, ECMWF, KNMI and MeteoFrance.
  !
  !    Copyright 2004, EUMETSAT, All Rights Reserved.
  !
  !     *************************************************************
  !
  !     TEST PROGRAM FOR RTTOV SUITE FORWARD MODEL ONLY
  !          RTTOV VERSION 8
  ! To run this program you must have the following files
  ! either resident in the same directory or set up as a
  ! symbolic link:
  !   prof.dat       --    input profile
  !   rtcoef_platform_id_sensor.dat --  coefficient file to match
  !   the sensor you request in the input dialogue
  ! There are unix scripts available to set up the files above and
  ! run this program (e.g. tstrad_full.scr)
  ! The output is generated in a file called print.dat.
  !
  !
  ! If the user wants to use this example to create his own
  ! program he will have to modify the code between
  ! comment lines of that kind:
  !     !================================
  !     !======Read =====start===========
  !          code to be modified
  !     !======Read ===== end ===========
  !     !================================
  !
  ! Current Code Owner: SAF NWP
  !
  ! History:
  ! Version   Date        Comment
  ! -------   ----        -------
  !  1.0    27/04/2004   orginal (based on tstrad) P. Brunel
  !  1.1    09/08/2004   modified to allow for variable no. channels/per profile
  !                       R. Saunders
  ! Code Description:
  !   Language:          Fortran 90.
  !   Software Standards: "European Standards for Writing and
  !     Documenting Exchangeable Fortran 90 Code".
  !
  !
  Use rttov_const, Only :  &
       errorstatus_success,&
       errorstatus_warning,&
       errorstatus_fatal

  Use rttov_types, Only : &
       rttov_coef     ,&
       profile_Type   ,&
       transmission_Type ,&
       radiance_Type

  Use parkind1, Only : jpim     ,jprb
  !
  Implicit None
  !
#include "rttov_direct.interface"
#include "rttov_readcoeffs.interface"
#include "rttov_initcoeffs.interface"
#include "rttov_setupchan.interface"
#include "rttov_setupindex.interface"
#include "rttov_errorhandling.interface"
#include "rttov_dealloc_coef.interface"
#include "rttov_errorreport.interface"
  ! Commons
  !
  ! Functions


  !--------------------------
  !
  Integer(Kind=jpim) :: iup=20      ! unit for profile file
  Integer(Kind=jpim) :: ioout=21    ! unit for output

  ! One profile per run
  Integer (Kind=jpim) :: nprof = 1

  ! RTTOV_errorhandling interface
  !====================
  Integer :: Err_Unit        ! Logical error unit (<0 for default)
  Integer :: verbosity_level ! (<0 for default)

  ! RTTOV_readcoeffs interface
  !====================
  Integer(Kind=jpim) :: errorstatus
  Integer(Kind=jpim) :: instrument(3)
  Type( rttov_coef ) :: coef         ! coefficients
  Integer(Kind=jpim), Allocatable :: lchan(:)

  ! RTTOV interface
  !====================
  Integer(Kind=jpim), Allocatable :: rttov_errorstatus(:)  ! rttov error return code
  Integer(Kind=jpim) :: nfrequencies
  Integer(Kind=jpim) :: nchannels
  Integer(Kind=jpim) :: nbtout
  Integer(Kind=jpim), Allocatable :: channels   (:)
  Integer(Kind=jpim), Allocatable :: polarisations   (:,:)
  Integer(Kind=jpim), Allocatable :: lprofiles  (:)
  Type(profile_Type)  :: profiles(1)! ONE profile but need array
  !Logical              :: addcloud = .True.
  Logical              :: addcloud = .False.
  Logical, Allocatable         :: calcemis(:)
  Real(Kind=jprb), Allocatable :: emissivity (:)
  Type(transmission_Type)  :: transmission ! transmittances and layer optical depths
  Type(radiance_Type)      :: radiance

  Real(Kind=jprb),    Allocatable :: input_emissivity (:)
  Character (len=80) :: errMessage
  Character (len=6)  :: NameOfRoutine = 'tstrad'


  ! variables for input
  !====================
  ! Parameter for WV conversion used in all tstrad suite
  Real(Kind=jprb), Parameter :: q_mixratio_to_ppmv  = 1.60771704e+6_JPRB

  Integer(Kind=jpim), Parameter :: mxchn = 9000 ! max number of channels
  Integer(Kind=jpim) :: input_chan(mxchn)
  Real(Kind=jprb)    :: input_ems(mxchn)
  Real(Kind=jprb), Allocatable    :: ems(:)
  Real(Kind=jprb)    :: zenith
  Real(Kind=jprb)    :: azimut
  Integer(Kind=jpim) :: ivch, ich
  Real(Kind=jprb)    :: ems_val
  Integer(Kind=jpim), Allocatable :: nchan(:) ! number of channels per profile
  Integer(Kind=jpim) :: isurf


  ! printing arrays
  Real(Kind=jprb), Allocatable :: pr_radcld(:)
  Real(Kind=jprb), Allocatable :: pr_trans(:)
  Real(Kind=jprb), Allocatable :: pr_emis(:)
  Real(Kind=jprb), Allocatable :: pr_trans_lev(:,:)
  Real(Kind=jprb), Allocatable :: pr_upclr(:)
  Real(Kind=jprb), Allocatable :: pr_dncld(:,:)
  Real(Kind=jprb), Allocatable :: pr_refclr(:)
  Real(Kind=jprb), Allocatable :: pr_ovcst(:,:)

  ! loop variables
  Integer :: j, jpol
  Integer :: np
  Integer :: ilev, nprint
  Integer :: ios

  Integer(Kind=jpim) :: alloc_status(40)

  !- End of header --------------------------------------------------------

  errorstatus     = 0
  alloc_status(:) = 0

  !=====================================================
  !========== Interactive inputs == start ==============
  Write(0,*) 'enter platform number'
  Read(*,*) instrument(1)
  Write(0,*) 'enter satellite number '
  Read(*,*) instrument(2)
  Write(0,*) 'enter instrument number'
  Read(*,*) instrument(3)
  Write(0,*) 'enter surface type (0=land, 1=sea, 2=ice/snow)'
  Read(*,*) isurf
  Write(0,*) 'enter zenith angle in degrees'
  Read(*,*) zenith
  Write(0,*) 'enter azimut angle in degrees'
  Read(*,*) azimut
  !
  Allocate (nchan(nprof))
  nchan(:) = 0
  Read(*,*,iostat=ios) ich, ivch, ems_val ! channel number, validity, emissivity
  Do While (ios == 0 )
     If( ivch /= 0 ) Then
        nchan(nprof) = nchan(nprof) +1
        input_chan(nchan(nprof)) = ich
        input_ems(nchan(nprof)) = ems_val
     Endif
     Read(*,*,iostat=ios) ich, ivch, ems_val
  End Do

  !Pack channels and emmissivity arrays
  Allocate(lchan(nchan(nprof))) ! Note these array sizes nchan can vary per profile
  Allocate(ems(nchan(nprof)))      ! but for this example assume 1 profile/call with same channels
  lchan(:) = input_chan(1:nchan(nprof))
  ems(:)   = input_ems(1:nchan(nprof))
  !
  !========== Interactive inputs == end ==============
  !===================================================


  !Initialise error management with default value for
  ! the error unit number and
  ! Fatal error message output
  Err_unit = -1
  !verbosity_level = 1
  ! All error message output
  verbosity_level = 3
  Call rttov_errorhandling(Err_unit, verbosity_level)

  !Read and initialise coefficients
  !---------------------------------------------------------
  Call rttov_readcoeffs (errorstatus, coef, instrument, channels = lchan(:))
  If(errorstatus /= 0) Then
     Write(*,*) 'error rttov_readcoeffs :',errorstatus
     Stop "error rttov_readcoeffs"
  Else
     Write(*,*) 'rttov_readcoeffs OK:'
  Endif
  Call rttov_initcoeffs (errorstatus,coef)
  If(errorstatus /= 0) Then
     Write(*,*) 'error rttov_initcoeffs :',errorstatus
     Stop "error rttov_initcoeffs"
  Else
     Write(*,*) 'rttov_initcoeffs OK:'
  Endif

  ! security if input number of channels is higher than number
  ! stored in coeffs
  If( nchan(nprof) > coef % fmv_chn ) Then
     nchan(nprof) = coef % fmv_chn
  Endif

  !Open output file
  Open(IOOUT,file='print.dat',status='unknown',form='formatted',iostat=ios)
  If( ios /= 0 ) Then
     Write(*,*) 'error opening the output file ios= ',ios
     Stop
  Endif

  !===============================================
  !========== Read profile == start ==============
  Open(iup, file='prof.dat',status='old',iostat=ios)
  If( ios /= 0 ) Then
     Write(*,*) 'error opening profile file ios= ',ios
     Stop
  Endif

  ! Do allocation of profile arrays with the number of levels.
  ! Take care that the number and pressure levels should be
  ! the same as the ones of the coefficient file.
  profiles(1) % nlevels = coef % nlevels
  Allocate(profiles(1) % p(coef % nlevels)  ,stat= alloc_status(1))
  Allocate(profiles(1) % t(coef % nlevels)  ,stat= alloc_status(2))
  Allocate(profiles(1) % q(coef % nlevels)  ,stat= alloc_status(3))
  Allocate(profiles(1) % o3(coef % nlevels) ,stat= alloc_status(4))
  Allocate(profiles(1) % clw(coef % nlevels),stat= alloc_status(5))
  If( Any(alloc_status /= 0) ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "mem allocation error for profile")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Stop
  End If

  ! Presures are from reference profile
  profiles(1) % p(:) = coef % ref_prfl_p(:)

  ! read pressure, temp (K), WV (lnq), O3 (ppmv)
  ! take care of doing the unit conversions to
  ! hPa, K and ppmv
  Read(iup,*) profiles(1) % t(:)
  Read(iup,*) profiles(1) % q(:)
  Read(iup,*) profiles(1) % o3(:)
  Read(iup,*) profiles(1) % clw(:)
  ! 2 meter air variables
  Read(iup,*) profiles(1) % s2m % t ,&
       & profiles(1) % s2m % q ,&
       & profiles(1) % s2m % p ,&
       & profiles(1) % s2m % u ,&
       & profiles(1) % s2m % v

  !  Convert lnq to q in ppmv for profile
  profiles(1) %    q(:) = (Exp(profiles(1) %     q(:)) / 1000._JPRB) * q_mixratio_to_ppmv
  profiles(1) % s2m % q = (Exp(profiles(1) %  s2m % q) / 1000._JPRB) * q_mixratio_to_ppmv

  ! Skin variables
  Read(iup,*) profiles(1) % skin % t ,&
       & profiles(1) % skin % fastem

  ! Cloud variables
  Read(iup,*) profiles(1) % ctp,&
       & profiles(1) % cfraction

  ! we have an ozone profile
  profiles(1) % ozone_Data =.True.
  ! we do not have CO2 profile
  profiles(1) % co2_Data   =.False.
  ! check Cloud liquid water profile
  profiles(1) % clw_Data   = profiles(1) % clw(1) >= 0.0_JPRB


  ! Other variables from interactive inputs
  profiles(1) % skin % surftype  = isurf
  profiles(1) % zenangle   = zenith
  profiles(1) % azangle    = azimut

  !========== Read profile == end ==============
  !=============================================


  ! Setup default number of frequencies, channels , output BTs
  !   for the coeff file. These are then used by rttov_setupindex
  !   to set up channel and polarisation indices.
  ! Take care that this routine is only valid if
  !   the user has selected a list of channels (channels = )
  !   for the rttov_readcoeffs or rttov_setup routine
  Call rttov_setupchan(nprof,nchan,coef,nfrequencies, &
       & nchannels,nbtout)

  Allocate( rttov_errorstatus(1)           ,stat= alloc_status(1))
  Allocate( channels ( nfrequencies )      ,stat= alloc_status(2))
  Allocate( lprofiles  ( nfrequencies )    ,stat= alloc_status(3))
  Allocate( emissivity ( nchannels )       ,stat= alloc_status(4))
  Allocate( input_emissivity ( nchannels ) ,stat= alloc_status(5))
  Allocate( calcemis  ( nchannels )        ,stat= alloc_status(6))
  Allocate( polarisations(nchannels,3)     ,stat= alloc_status(7))

  ! allocate transmittance structure
  Allocate( transmission % tau_surf      ( nchannels )                 ,stat= alloc_status(8))
  Allocate( transmission % tau_layer     ( coef % nlevels, nchannels ) ,stat= alloc_status(9))
  Allocate( transmission % od_singlelayer( coef % nlevels, nchannels  ),stat= alloc_status(10))

  ! allocate radiance results arrays with number of channels
  Allocate( radiance % clear    ( nchannels ) ,stat= alloc_status(11))
  Allocate( radiance % cloudy   ( nchannels ) ,stat= alloc_status(12))
  Allocate( radiance % total    ( nchannels ) ,stat= alloc_status(13))
  Allocate( radiance % bt       ( nchannels ) ,stat= alloc_status(14))
  Allocate( radiance % bt_clear ( nchannels ) ,stat= alloc_status(15))
  Allocate( radiance % upclear  ( nchannels ) ,stat= alloc_status(16))
  Allocate( radiance % dnclear  ( nchannels ) ,stat= alloc_status(17))
  Allocate( radiance % reflclear( nchannels ) ,stat= alloc_status(18))
  Allocate( radiance % overcast ( coef % nlevels, nchannels ) ,stat= alloc_status(19))
  ! allocate the cloudy radiances with full size even
  ! if not used
  Allocate( radiance % downcld  ( coef % nlevels, nchannels ) ,stat= alloc_status(20))

  Allocate( radiance % out      ( nbtout ) ,stat= alloc_status(21))
  Allocate( radiance % out_clear( nbtout ) ,stat= alloc_status(22))
  Allocate( radiance % total_out( nbtout ) ,stat= alloc_status(23))
  Allocate( radiance % clear_out( nbtout ) ,stat= alloc_status(24))

  If( Any(alloc_status /= 0) ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "mem allocation error prior to rttov_direct")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Stop
  End If



  ! Build the list of channels/profiles indices
  !   outputs are lprofiles,channels,polarisations,emissivity
  ! Take care that this routine is only valid if
  !   the user has selected a list of channels
  !   for the rttov_readcoeffs or rttov_setup routine (channels = )
  Call rttov_setupindex (nchan,nprof,nfrequencies,nchannels,nbtout,coef,&
       & ems,lprofiles,channels,polarisations,emissivity)


  ! save input values of emissivities for all calculations
  ! calculate emissivity where the input emissivity value is less than 0.01
  input_emissivity(:) = emissivity(:)
  calcemis(:) = emissivity(:) < 0.01_JPRB

  ! Call RTTOV forward model
  Call rttov_direct( &
       rttov_errorstatus, & ! out
       nfrequencies,   & ! in
       nchannels,      & ! in
       nbtout,         & ! in
       nprof,          & ! in
       channels,       & ! in
       polarisations,  & ! in
       lprofiles,      & ! in
       profiles,       & ! in
       coef,           & ! in
       addcloud,       & ! in
       calcemis,       & ! in
       emissivity,     & ! inout
       transmission,   & ! out
       radiance        ) ! inout

  If ( Any( rttov_errorstatus(:) == errorstatus_warning ) ) Then
     Write ( ioout, * ) 'rttov_direct warning'
  End If

  If ( Any( rttov_errorstatus(:) == errorstatus_fatal ) ) Then
     Write ( 0, * ) 'rttov_direct error'
     Stop
  End If

  ! transfer data to printing arrays
  Allocate(pr_radcld(nbtout)  ,stat= alloc_status(1))
  Allocate(pr_trans(nbtout)   ,stat= alloc_status(2))
  Allocate(pr_emis(nbtout)    ,stat= alloc_status(3))
  Allocate(pr_trans_lev(coef % nlevels,nbtout) ,stat= alloc_status(4))
  Allocate(pr_upclr(nbtout)                    ,stat= alloc_status(5))
  Allocate(pr_dncld(coef % nlevels,nbtout)     ,stat= alloc_status(6))
  Allocate(pr_refclr(nbtout)                   ,stat= alloc_status(7))
  Allocate(pr_ovcst(coef % nlevels,nbtout)     ,stat= alloc_status(8))
  If( Any(alloc_status /= 0) ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "mem allocation error for printing arrays")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Stop
  End If

  pr_radcld(:) = 0.0_JPRB
  pr_trans(:)  = 0.0_JPRB
  pr_emis(:)   = 0.0_JPRB
  pr_trans_lev(:,:) = 0.0_JPRB
  pr_upclr(:)   = 0.0_JPRB
  pr_dncld(:,:) = 0.0_JPRB
  pr_refclr(:)  = 0.0_JPRB
  pr_ovcst(:,:) = 0.0_JPRB
  !
  Do j = 1 , nchannels
     jpol = polarisations(j,2)
     pr_radcld(jpol) = radiance % cloudy(j)
     pr_trans(jpol)  = Transmission % tau_surf(J)
     pr_emis(jpol)   = emissivity(j)
     pr_upclr(jpol)  = radiance % upclear(J)
     pr_refclr(jpol) = radiance % reflclear(J)
     Do ilev = 1 , coef % nlevels
        pr_trans_lev(ilev,jpol) = Transmission % tau_layer(ilev,J)
        pr_dncld(ilev,jpol)     = radiance % downcld(ILEV,J)
        pr_ovcst(ilev,jpol)     = radiance % overcast(ILEV,J)
     Enddo
  Enddo



  !     OUTPUT RESULTS
  !
  NPRINT = 1+ Int((nbtout-1)/10)
  Write(IOOUT,*)' -----------------'
  Write(IOOUT,*)' Instrument ', instrument(3)
  Write(IOOUT,*)' -----------------'
  Write(IOOUT,*)' '

  Write(IOOUT,777)instrument(2), profiles(1)%zenangle,profiles(1)%azangle,profiles(1)%skin%surftype
  Write(IOOUT,222) radiance % out(:)
  Write(IOOUT,*)' '
  Write(IOOUT,*)'CALCULATED RADIANCES: SAT =', instrument(2)
  Write(IOOUT,222) radiance % total_out(:)
  Write(IOOUT,*)' '
  Write(IOOUT,*)'CALCULATED OVERCAST RADIANCES: SAT =', instrument(2)
  Write(IOOUT,222) pr_radcld(:)
  Write (IOOUT,*)' '
  Write(IOOUT,*)'CALCULATED SURFACE TO SPACE TRANSMITTANCE: S'&
       &      ,'AT =',instrument(2)
  Write(IOOUT,4444) pr_trans(:)
  Write (IOOUT,*)' '
  Write(IOOUT,*)'CALCULATED SURFACE EMISSIVITIES '&
       &      ,'SAT =',instrument(2)
  Write(IOOUT,444) pr_emis(:)
  !
  !
  If(nchan(nprof) <= 20)Then
     Do  NP = 1 , NPRINT
        Write (IOOUT,*)' '
        Write (IOOUT,*)'Level to space transmittances for channels'
        Write(IOOUT,1115) (LCHAN(J),&
             &   J = 1+(NP-1)*10,Min(10+(NP-1)*10,nbtout))
        Do  ILEV = 1 , coef % NLEVELS
           Write(IOOUT,4445)ILEV,(pr_trans_lev(ilev,J),&
                &   J = 1+(NP-1)*10,Min(10+(NP-1)*10,nbtout))
        End Do
        Write(IOOUT,1115) (LCHAN(J),&
             &   J = 1+(NP-1)*10,Min(10+(NP-1)*10,nbtout))
     End Do
  Endif
  !
  ! deallocate model profiles atmospheric arrays
  Deallocate( profiles(1) % p   ,stat=alloc_status(1))
  Deallocate( profiles(1) % t   ,stat=alloc_status(2))
  Deallocate( profiles(1) % q   ,stat=alloc_status(3))
  Deallocate( profiles(1) % o3  ,stat=alloc_status(4))
  Deallocate( profiles(1) % clw ,stat=alloc_status(5))
  If( Any(alloc_status /= 0) ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "mem deallocation error")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Stop
  End If

  ! number of channels per RTTOV call is only nchannels
  Deallocate( channels   ,stat=alloc_status(2))
  Deallocate( lprofiles  ,stat=alloc_status(3))
  Deallocate( emissivity ,stat=alloc_status(4))
  Deallocate( calcemis   ,stat=alloc_status(5))

  ! allocate transmittance structure
  Deallocate( transmission % tau_surf      ,stat= alloc_status(6))
  Deallocate( transmission % tau_layer     ,stat= alloc_status(7))
  Deallocate( transmission % od_singlelayer,stat= alloc_status(8))

  ! allocate radiance results arrays with number of channels
  Deallocate( radiance % clear    ,stat=alloc_status(9))
  Deallocate( radiance % cloudy   ,stat=alloc_status(10))
  Deallocate( radiance % total    ,stat=alloc_status(11))
  Deallocate( radiance % bt       ,stat=alloc_status(12))
  Deallocate( radiance % bt_clear ,stat=alloc_status(13))
  Deallocate( radiance % upclear  ,stat=alloc_status(14))
  Deallocate( radiance % dnclear  ,stat=alloc_status(15))
  Deallocate( radiance % reflclear,stat=alloc_status(16))
  Deallocate( radiance % overcast ,stat=alloc_status(17))
  Deallocate( radiance % downcld  ,stat=alloc_status(18))
  Deallocate( radiance % out       ,stat= alloc_status(19))
  Deallocate( radiance % out_clear ,stat= alloc_status(20))
  Deallocate( radiance % total_out ,stat= alloc_status(21))
  Deallocate( radiance % clear_out ,stat= alloc_status(22))
  Deallocate(pr_radcld ,stat= alloc_status(31))
  Deallocate(pr_trans ,stat= alloc_status(32))
  Deallocate(pr_emis ,stat= alloc_status(33))
  Deallocate(pr_trans_lev ,stat= alloc_status(34))
  If( Any(alloc_status /= 0) ) Then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "mem deallocation error")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Stop
  End If

  Call rttov_dealloc_coef (errorstatus, coef)
  If(errorstatus /= errorstatus_success) Then
     Write( errMessage, '( "deallocation error")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
  Endif

  !Close output file
  Close(IOOUT,iostat=ios)
  If( ios /= 0 ) Then
     Write(*,*) 'error closing the output file ios= ',ios
     Stop
  Endif

1115 Format(3X,10I8)
222  Format(1X,10F8.2)
444  Format(1X,10F8.3)
4444 Format(1X,10F8.4)
4445 Format(1X,I2,10F8.4)
777  Format(1X,'CALCULATED BRIGHTNESS TEMPERATURES: SAT =',I2,&
       &' ZENITH ANGLE=',F6.2, &
       &' AZIMUTH ANGLE=',F7.2,' SURFACE TYPE=',I2)



End Program example_fwd
