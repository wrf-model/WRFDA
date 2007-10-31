Subroutine tstrad_tl(   &
     & nfrequencies,    &! in
     & nchannels,       &! in
     & nbtout,          &! in
     & nprofiles,       &! in
     & channels,        &! in
     & polarisations,   &! in
     & lprofiles,       &! in
     & frequencies,     &! in
     & profiles,        &! in
     & coef,            &! in
     & addcloud,        &! in
     & calcemis,        &! in
     & input_emissivity) ! in
  !
  ! only the first nchannels/nprofiles are output
  !
  Use rttov_const, Only :   &
       & errorstatus_success, &
       & errorstatus_fatal,   &
       & sensor_id_mw

  Use rttov_types, Only : &
       & rttov_coef     ,&
       & profile_Type   ,&
       & transmission_Type   ,&
       & radiance_Type

  Use mod_tstrad

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_errorreport.interface"
#include "rttov_direct.interface"
#include "rttov_tl.interface"

  !subroutine arguments:
  Integer(Kind=jpim),  Intent(in)    :: nchannels
  Integer(Kind=jpim),  Intent(in)    :: nfrequencies
  Integer(Kind=jpim),  Intent(in)    :: nbtout
  Integer(Kind=jpim),  Intent(in)    :: nprofiles
  Integer(Kind=jpim),  Intent(in)    :: channels(nfrequencies)
  Integer(Kind=jpim),  Intent(in)    :: polarisations(nchannels,3)
  Integer(Kind=jpim),  Intent(in)    :: lprofiles(nfrequencies)
  Logical,             Intent(in)    :: addcloud
  Type(profile_Type),  Intent(in)    :: profiles(nprofiles)
  Type(rttov_coef),    Intent(in)    :: coef
  Logical,             Intent(in)    :: calcemis(nchannels)
  Real(Kind=jprb),     Intent(in)    :: input_emissivity(nchannels)
  Integer(Kind=jpim),  Intent(in)    :: frequencies(nbtout)



  ! local
  Integer(Kind=jpim), Parameter :: jpnav  =  4       ! no. of profile variables
  Integer(Kind=jpim), Parameter :: jpnsav =  5       ! no. of surface air variables
  Integer(Kind=jpim), Parameter :: jpnssv =  6       ! no. of skin variables
  Integer(Kind=jpim), Parameter :: jpncv  =  2       ! no. of cloud variables
  Integer(Kind=jpim), Parameter :: sscvar = jpnsav+jpnssv+jpncv ! no of surface,skin,cloud vars

  Character (len=80) :: errMessage
  Character (len=10) :: NameOfRoutine = 'tstrad_tl '

  ! forward model outputs
  Type(transmission_Type)   :: transmission
  Type(radiance_Type)   :: radiancedata
  Type(radiance_Type)   :: radiance_fwd
  Real(Kind=jprb)   :: emissivity_fwd(nchannels)
  Real(Kind=jprb)   :: emissivity(nchannels)


  ! tl increments
  Type(profile_Type)  :: prof_inc(nprofiles)
  Type(profile_Type)  :: null_inc(nprofiles) ! all increments set to 0
  Real(Kind=jprb) :: emissivity_inc(nchannels)
  Real(Kind=jprb) :: null_emissivity_inc(nchannels)

  ! tl variables for rttov_tl calls
  Type(profile_Type)  :: profiles_tl(nprofiles)
  Type(transmission_Type)   :: transmission_tl
  Type(radiance_Type) :: radiancedata_tl
  Real(Kind=jprb)  :: emissivity_tl(nchannels)

  ! Brute force
  Type(profile_Type)    :: profiles_bf(nprofiles)
  Real(Kind=jprb)     :: emissivity_bf(nchannels)
  Logical  :: calcemis_bf(nchannels)



  Integer(Kind=jpim) :: nlev

  Integer(Kind=jpim) :: ixkav(coef%nlevels,jpnav,nbtout)
  Integer(Kind=jpim) :: ixkov(coef%nlevels,nbtout)
  Real(Kind=jprb)    :: pktav(coef%nlevels,jpnav,nbtout)

  Integer(Kind=jpim) :: ixkem(nbtout)

  Integer(Kind=jpim) :: ixksav(sscvar,nbtout)
  Real(Kind=jprb)    :: pktsav(sscvar,nbtout)

  ! Brute force results
  Integer(Kind=jpim) :: ixkdav(coef%nlevels,jpnav,nbtout)
  Real(Kind=jprb)    :: xktav (coef%nlevels,jpnav,nbtout)
  Integer(Kind=jpim) :: ixkdsav(sscvar,nbtout)
  Real(Kind=jprb)    :: xktsav (sscvar,nbtout)
  Integer(Kind=jpim) :: ixkdem(nbtout)
  Real(Kind=jprb)    :: xktem (nbtout)

  ! coefficients for printing
  Real(Kind=jprb) :: facpav(coef%nlevels,jpnav)
  Real(Kind=jprb) :: facovu(coef%nlevels)
  Real(Kind=jprb) :: facovd(coef%nlevels)
  Real(Kind=jprb) :: facem = 1._JPRB

  Real(Kind=jprb) :: facsav(sscvar) =&
        & (/10000._JPRB,0.1_JPRB,10000._JPRB,10000._JPRB,10000._JPRB,       &! 2m
        & 10000._JPRB,100.0_JPRB,100.0_JPRB,100.0_JPRB,100.0_JPRB,100.0_JPRB,  &! Skin
        & 10000._JPRB,100._JPRB/)                           ! cloud

  Real(Kind=jprb), Parameter :: q_mixratio_to_ppmv  = 1.60771704e+6_JPRB
  Real(Kind=jprb), Parameter :: o3_mixratio_to_ppmv = 6.03504e+5_JPRB

  Real(Kind=jprb) :: inc_val   ! increment value
  Real(Kind=jprb) :: inc_val_p ! increment value for printing (ppmv -> kg/kg)
  Real(Kind=jprb) :: diffr
  Real(Kind=jprb) :: sumr
  Real(Kind=jprb) :: sumrr
  Real(Kind=jprb) :: fac
  Integer(Kind=jpim) :: ioout = 2
  Integer(Kind=jpim) :: jch
  Integer(Kind=jpim) :: j, i, ii, jp, joff, ipol
  Integer(Kind=jpim) :: prof
  Integer(Kind=jpim) :: nchan_out

  Character (len=30) :: title(4) = &
        & (/' lev       temperature        ', &
          & ' lev       water vapour       ',   &
          & ' lev       ozone              ',   &
          & ' lev       liquid water       '/)

  Integer(Kind=jpim) :: errorstatus
  Integer(Kind=jpim) :: rttov_errorstatus(nprofiles)
  Integer(Kind=jpim) :: alloc_status(60)

  !- End of header --------------------------------------------------------

  errorstatus     = 0
  alloc_status(:) = 0
  rttov_errorstatus(:)    = 0

  nchan_out = nbtout/nprofiles
  nlev = coef % nlevels

  ! coefficients for atmospheric variables
  facpav(:,1) = 10000._JPRB
  facpav(:,2) = 0.1_JPRB
  facpav(:,3) = 0.001_JPRB
  facpav(:,4) = 0.1_JPRB
  facovu(:)   = 10000._JPRB
  facovd(:)   = 100000._JPRB

  allocate(xkbav(coef%nlevels,jpnav,nbtout),stat= alloc_status(1))
  allocate(xkradovu(coef%nlevels,nchannels),stat= alloc_status(2))
  allocate(xkradovd(coef%nlevels,nchannels),stat= alloc_status(3))
  allocate(xkradov1(coef%nlevels,nchannels),stat= alloc_status(4))
  allocate(xkradov2(coef%nlevels,nchannels),stat= alloc_status(5))
  allocate(xkbsav(sscvar,nbtout),stat= alloc_status(6))
  allocate(xkbem(nbtout),stat= alloc_status(7))
  If( any(alloc_status /= 0) ) then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "mem allocation error")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  End If


  xkbav(:,:,:)=0._JPRB
  pktav(:,:,:)=0._JPRB

  ! allocate and initialise the reference tl increments
  Do j = 1, nprofiles
     prof_inc(j) % nlevels =  coef % nlevels
     null_inc(j) % nlevels =  coef % nlevels
     profiles_tl(j) % nlevels =  coef % nlevels
     profiles_bf(j) % nlevels =  coef % nlevels
     Allocate( prof_inc(j) % p  ( coef % nlevels ) ,stat= alloc_status(1))
     Allocate( prof_inc(j) % t  ( coef % nlevels ) ,stat= alloc_status(2))
     Allocate( prof_inc(j) % q  ( coef % nlevels ) ,stat= alloc_status(3))
     Allocate( prof_inc(j) % o3 ( coef % nlevels ) ,stat= alloc_status(4))
     Allocate( prof_inc(j) % clw( coef % nlevels ) ,stat= alloc_status(5))
     Allocate( null_inc(j) % p  ( coef % nlevels ) ,stat= alloc_status(6))
     Allocate( null_inc(j) % t  ( coef % nlevels ) ,stat= alloc_status(7))
     Allocate( null_inc(j) % q  ( coef % nlevels ) ,stat= alloc_status(8))
     Allocate( null_inc(j) % o3 ( coef % nlevels ) ,stat= alloc_status(9))
     Allocate( null_inc(j) % clw( coef % nlevels ) ,stat= alloc_status(10))
     Allocate( profiles_tl(j) % p  ( coef % nlevels ) ,stat= alloc_status(11))
     Allocate( profiles_tl(j) % t  ( coef % nlevels ) ,stat= alloc_status(12))
     Allocate( profiles_tl(j) % q  ( coef % nlevels ) ,stat= alloc_status(13))
     Allocate( profiles_tl(j) % o3 ( coef % nlevels ) ,stat= alloc_status(14))
     Allocate( profiles_tl(j) % clw( coef % nlevels ) ,stat= alloc_status(15))
     Allocate( profiles_bf(j) % p  ( coef % nlevels ) ,stat= alloc_status(16))
     Allocate( profiles_bf(j) % t  ( coef % nlevels ) ,stat= alloc_status(17))
     Allocate( profiles_bf(j) % q  ( coef % nlevels ) ,stat= alloc_status(18))
     Allocate( profiles_bf(j) % o3 ( coef % nlevels ) ,stat= alloc_status(19))
     Allocate( profiles_bf(j) % clw( coef % nlevels ) ,stat= alloc_status(20))
     If( any(alloc_status /= 0) ) then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "mem allocation error")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If
  End Do

  ! allocate radiance results arrays with number of channels
  Allocate( radiancedata % clear    ( nchannels ) ,stat= alloc_status(1))
  Allocate( radiancedata % cloudy   ( nchannels ) ,stat= alloc_status(2))
  Allocate( radiancedata % total    ( nchannels ) ,stat= alloc_status(3))
  Allocate( radiancedata % bt       ( nchannels ) ,stat= alloc_status(4))
  Allocate( radiancedata % bt_clear ( nchannels ) ,stat= alloc_status(5))
  Allocate( radiancedata % upclear  ( nchannels ) ,stat= alloc_status(6))
  Allocate( radiancedata % dnclear  ( nchannels ) ,stat= alloc_status(34))
  Allocate( radiancedata % reflclear( nchannels ) ,stat= alloc_status(7))
  Allocate( radiancedata % overcast ( coef % nlevels, nchannels ) ,stat= alloc_status(8))
  Allocate( radiancedata % downcld  ( coef % nlevels, nchannels ) ,stat= alloc_status(9))
  Allocate( radiancedata % out      ( nbtout ) ,stat= alloc_status(10))
  Allocate( radiancedata % out_clear( nbtout ) ,stat= alloc_status(11))
  Allocate( radiancedata % total_out( nbtout ) ,stat= alloc_status(12))
  Allocate( radiancedata % clear_out( nbtout ) ,stat= alloc_status(13))
  Allocate( radiancedata_tl % clear ( nchannels ) ,stat= alloc_status(14))
  Allocate( radiancedata_tl % cloudy   ( nchannels ) ,stat= alloc_status(15))
  Allocate( radiancedata_tl % total    ( nchannels ) ,stat= alloc_status(16))
  Allocate( radiancedata_tl % bt       ( nchannels ) ,stat= alloc_status(17))
  Allocate( radiancedata_tl % bt_clear ( nchannels ) ,stat= alloc_status(18))
  Allocate( radiancedata_tl % upclear  ( nchannels ) ,stat= alloc_status(19))
  Allocate( radiancedata_tl % reflclear( nchannels ) ,stat= alloc_status(20))
  Allocate( radiancedata_tl % overcast ( coef % nlevels, nchannels ) ,stat= alloc_status(21))
  Allocate( radiancedata_tl % downcld  ( coef % nlevels, nchannels ) ,stat= alloc_status(22))
  Allocate( radiancedata_tl % out      ( nbtout ) ,stat= alloc_status(23))
  Allocate( radiancedata_tl % out_clear( nbtout ) ,stat= alloc_status(24))
  Allocate( radiancedata_tl % total_out( nbtout ) ,stat= alloc_status(25))
  Allocate( radiancedata_tl % clear_out( nbtout ) ,stat= alloc_status(26))
  Allocate( radiance_fwd % clear    ( nchannels ) ,stat= alloc_status(27))
  Allocate( radiance_fwd % cloudy   ( nchannels ) ,stat= alloc_status(28))
  Allocate( radiance_fwd % total    ( nchannels ) ,stat= alloc_status(29))
  Allocate( radiance_fwd % bt       ( nchannels ) ,stat= alloc_status(30))
  Allocate( radiance_fwd % bt_clear ( nchannels ) ,stat= alloc_status(31))
  Allocate( radiance_fwd % upclear  ( nchannels ) ,stat= alloc_status(32))
  Allocate( radiance_fwd % reflclear( nchannels ) ,stat= alloc_status(33))
  Allocate( radiance_fwd % overcast ( coef % nlevels, nchannels ) ,stat= alloc_status(34))
  Allocate( radiance_fwd % downcld  ( coef % nlevels, nchannels ) ,stat= alloc_status(35))
  Allocate( radiance_fwd % out      ( nbtout ) ,stat= alloc_status(36))
  Allocate( radiance_fwd % out_clear( nbtout ) ,stat= alloc_status(37))
  Allocate( radiance_fwd % total_out ( nbtout ) ,stat= alloc_status(38))
  Allocate( radiance_fwd % clear_out ( nbtout ) ,stat= alloc_status(39))
  Allocate( transmission % tau_surf   ( nchannels ) ,stat= alloc_status(40))
  Allocate( transmission % tau_layer   ( coef % nlevels, nchannels ) ,stat= alloc_status(41))
  Allocate( transmission % od_singlelayer( coef % nlevels, nchannels ) ,stat= alloc_status(42))
  Allocate( transmission_tl % tau_surf      ( nchannels ) ,stat= alloc_status(46))
  Allocate( transmission_tl % tau_layer     ( coef % nlevels, nchannels ) ,stat= alloc_status(47))
  Allocate( transmission_tl % od_singlelayer( coef % nlevels, nchannels ) ,stat= alloc_status(48))

  If( any(alloc_status /= 0) ) then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "mem allocation error")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  End If

  Do j = 1, nprofiles
     prof_inc(j) % ozone_Data = .False.  ! no meaning
     prof_inc(j) % co2_Data   = .False.  ! no meaning
     prof_inc(j) % clw_Data   = .False.  ! no meaning
     prof_inc(j) % zenangle   = -1  ! no meaning

     ! increments for atmospheric variables
     prof_inc(j) % p(:)   = 0._JPRB    ! no tl on pressure levels
     prof_inc(j) % t(:)   = -1._JPRB   ! 1k on temperarure
     prof_inc(j) % o3(:)  = -0.01_JPRB !  0.01_JPRB ppmv
     prof_inc(j) % clw(:) = 0.001_JPRB ! 1g/kg on clw
     prof_inc(j) % q(:) = -0.1_JPRB * profiles(j) % q(:)  ! - 10% on wv

     ! increments for air surface variables
     prof_inc(j) % s2m % t = -1._JPRB   ! 1k on temperarure
     prof_inc(j) % s2m % q = -1.6077_JPRB !  ppmv
     prof_inc(j) % s2m % p = -10._JPRB  ! -10 hpa on pressure
     prof_inc(j) % s2m % u = 0.01_JPRB   ! 0.01_JPRB m/s on wind components
     prof_inc(j) % s2m % v = 0.01_JPRB   ! 0.01_JPRB m/s on wind components

     ! increments for skin variables
     prof_inc(j) % skin % surftype = -1  ! no meaning
     prof_inc(j) % skin % t        = -1._JPRB   ! 1k on temperarure
     prof_inc(j) % skin % fastem   = (/-0.01_JPRB,-0.01_JPRB,-0.1_JPRB,-0.001_JPRB,-0.001_JPRB/)

     ! increments for cloud variables
     prof_inc(j) % ctp       = -10._JPRB  ! -10 hpa on pressure
     prof_inc(j) % cfraction = 0.1_JPRB   ! 0.1_JPRB on cloud fraction
  End Do

  ! emissivity
  emissivity_inc(:) = -0.01_JPRB


  Do j = 1, nprofiles
     null_inc(j) % ozone_Data = .False.  ! no meaning
     null_inc(j) % co2_Data   = .False.  ! no meaning
     null_inc(j) % clw_Data   = .False.  ! no meaning
     null_inc(j) % zenangle   = -1  ! no meaning
     null_inc(j) % p(:)   = 0._JPRB
     null_inc(j) % t(:)   = 0._JPRB
     null_inc(j) % q(:)   = 0._JPRB
     null_inc(j) % o3(:)  = 0._JPRB
     null_inc(j) % clw(:) = 0._JPRB
     null_inc(j) % s2m % t  = 0._JPRB
     null_inc(j) % s2m % q  = 0._JPRB
     null_inc(j) % s2m % p  = 0._JPRB
     null_inc(j) % s2m % u  = 0._JPRB
     null_inc(j) % s2m % v  = 0._JPRB
     null_inc(j) % skin % surftype = -1  ! no meaning
     null_inc(j) % skin % t        = 0._JPRB
     null_inc(j) % skin % fastem   = (/0._JPRB, 0._JPRB, 0._JPRB, 0._JPRB, 0._JPRB/)
     null_inc(j) % ctp       = 0._JPRB
     null_inc(j) % cfraction = 0._JPRB
  End Do
  null_emissivity_inc(:)  = 0._JPRB

  !..print out increments
  Write(ioout,*)' '
  Write(ioout,*)' input profile increments for tl'
  Write(ioout,444) prof_inc(1) % t(:)
  Write(ioout,444) prof_inc(1) % q(:)  /q_mixratio_to_ppmv
  Write(ioout,444) prof_inc(1) % o3(:) /o3_mixratio_to_ppmv
  Write(ioout,444) prof_inc(1) % clw(:)
  Write(ioout,444)&
        & prof_inc(1) % s2m % t, &
        & prof_inc(1) % s2m % q /q_mixratio_to_ppmv, &
        & prof_inc(1) % s2m % p, &
        & prof_inc(1) % s2m % u, &
        & prof_inc(1) % s2m % v
  Write(ioout,444)&
        & prof_inc(1) % skin % t, &
        & prof_inc(1) % skin % fastem
  Write(ioout,444)&
        & prof_inc(1) % ctp,&
        & prof_inc(1) % cfraction
  Write(ioout,*)' '
  !
  !...first do profile variables.....................
  Do  j =1,jpnav    ! yes clw too!
     Do  ii=1,nlev

        ! initialise all increments to 0
        Do jp = 1, nprofiles
           profiles_tl(jp) % p(:)   = null_inc(jp) % p(:)
           profiles_tl(jp) % t(:)   = null_inc(jp) % t(:)
           profiles_tl(jp) % q(:)   = null_inc(jp) % q(:)
           profiles_tl(jp) % o3(:)  = null_inc(jp) % o3(:)
           profiles_tl(jp) % clw(:) = null_inc(jp) % clw(:)
           profiles_tl(jp) % s2m    = null_inc(jp) % s2m
           profiles_tl(jp) % skin   = null_inc(jp) % skin
           profiles_tl(jp) % ctp    = null_inc(jp) % ctp
           profiles_tl(jp) % cfraction   = null_inc(jp) % cfraction
           profiles_tl(jp) % ozone_Data  = null_inc(jp) % ozone_Data
           profiles_tl(jp) % co2_Data    = null_inc(jp) % co2_Data
           profiles_tl(jp) % clw_Data    = null_inc(jp) % clw_Data
           profiles_tl(jp) % zenangle    = null_inc(jp) % zenangle

        End Do
        emissivity_tl(:) = null_emissivity_inc(:)


        Do jp = 1, nprofiles
           ! except the considered level/variable
           Select Case (j)
           Case (1_jpim)
              profiles_tl(jp) % t(ii)   = prof_inc(jp) % t(ii)
              inc_val   = prof_inc(1) % t(ii)
              inc_val_p = inc_val
           Case (2_jpim)
              profiles_tl(jp) % q(ii)   = prof_inc(jp) % q(ii)
              inc_val   = prof_inc(1) % q(ii)
              inc_val_p = inc_val / q_mixratio_to_ppmv
           Case (3_jpim)
              profiles_tl(jp) % o3(ii)  = prof_inc(jp) % o3(ii)
              inc_val   = prof_inc(1) % o3(ii)
              inc_val_p = inc_val / o3_mixratio_to_ppmv
           Case (4_jpim)
              profiles_tl(jp) % clw(ii) = prof_inc(jp) % clw(ii)
              inc_val   = prof_inc(1) % clw(ii)
              inc_val_p = inc_val
           End Select
        End Do

        ! use stored input emmisisvity
        emissivity(:) = input_emissivity(:)

        Call rttov_tl( &
             & rttov_errorstatus,  &! out
             & nfrequencies,    &! in
             & nchannels,       &! in
             & nbtout,          &! in
             & nprofiles,       &! in
             & channels,        &! in
             & polarisations,   &! in
             & lprofiles,       &! in
             & profiles,        &! in
             & coef,            &! in
             & addcloud,        &! in
             & calcemis,        &! in
             & emissivity,      &! inout
             & profiles_tl,     &! in
             & emissivity_tl,   &! inout
             & transmission,    &! inout
             & transmission_tl, &! inout
             & radiancedata,    &! out
             & radiancedata_tl ) ! inout

        If( any( rttov_errorstatus == errorstatus_fatal ) ) then
           Do jp = 1, nprofiles
              If( rttov_errorstatus(jp) == errorstatus_fatal ) then
                 Write( errMessage, '( "fatal error in rttov_tl")' )
                 Call Rttov_ErrorReport (rttov_errorstatus(jp), errMessage, NameOfRoutine)
              End If
           End Do
           Stop
        End If
        If( any( rttov_errorstatus /= errorstatus_success ) ) then
          Write( errMessage, '( "warning in rttov_tl")' )           
        End If

        If (inc_val == 0._JPRB) Then
           xkbav(ii,j,:)=0._JPRB
           pktav(ii,j,:)=0._JPRB
        Else
           pktav(ii,j,:)=radiancedata_tl%out(:)
           xkbav(ii,j,:)=radiancedata_tl%out(:) / inc_val
           If(addcloud .And. j==1)Then
              xkradovu(ii,:)=radiancedata_tl%overcast(ii,:)/inc_val
              xkradovd(ii,:)=radiancedata_tl%downcld (ii,:)/inc_val
              xkradov1(ii,:)=radiancedata_tl%upclear(:)/inc_val
              xkradov2(ii,:)=radiancedata_tl%reflclear(:)/inc_val
           Endif
        Endif
        ixkav(ii,j,:)=Nint(radiancedata_tl%out(:)*facpav(ii,j)/ inc_val_p)

     End Do
  End Do

  !          ... and print it.
  Write (ioout,*)' '
  Write (ioout,*)'k-matrix: tangent linear.'
!!$  Do  jch=1,nchannels
!!$     ixkav(:,:,jch)=Nint(xkbav(:,:,jch)*facpav(:,:))
!!$  End Do
  Write (ioout,*)' '
  Do j = 1 , jpnav                   ! lwp on
     Write (ioout,'(a30)')title(j)
     Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
     Do i = 1 , nlev
        Write (ioout,333)i,(ixkav(i,j,jch),jch=1,nchan_out)
     Enddo
     Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
     Write (ioout,*)' '
  Enddo


  ! optionally print full radiance arrays
  If(addcloud)Then
     Write (ioout,*)' '
     Write (ioout,*)'k-matrix: upwelling radiance tl '
     Do  jch=1,nbtout
        ixkov(:,jch)=Nint(xkradovu(:,jch)*facovu(:))
     End Do
     Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
     Do i = 1 , nlev
        Write (ioout,333)i,(ixkov(i,jch),jch=1,nchan_out)
     Enddo
     Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
     Write (ioout,*)' '
     Write (ioout,*)'k-matrix: downwelling radiance tl '
     Do  jch=1,nbtout
        ixkov(:,jch)=Nint(xkradovd(:,jch)*facovd(:))
     End Do
     Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
     Do i = 1 , nlev
        Write (ioout,333)i,(ixkov(i,jch),jch=1,nchan_out)
     Enddo
     Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
     Write (ioout,*)' '
     Write (ioout,*)'k-matrix: clear-sky radiance without reflection term tl '
     Do  jch=1,nbtout
        ixkov(:,jch)=Nint(xkradov1(:,jch)*facovu(:))
     End Do
     Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
     Do i = 1 , nlev
        Write (ioout,333)i,(ixkov(i,jch),jch=1,nchan_out)
     Enddo
     Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
     Write (ioout,*)' '
     Write (ioout,*)' '
     Write (ioout,*)'k-matrix: reflected clear-sky downwelling radiance tl '
     Do  jch=1,nbtout
        ixkov(:,jch)=Nint(xkradov2(:,jch)*facovd(:))*10._JPRB
     End Do
     Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
     Do i = 1 , nlev
        Write (ioout,333)i,(ixkov(i,jch),jch=1,nchan_out)
     Enddo
     Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
     Write (ioout,*)' '
  Endif


  ! initialise all increments to 0
  Do jp = 1, nprofiles
     profiles_tl(jp) % p(:)   = null_inc(jp) % p(:)
     profiles_tl(jp) % t(:)   = null_inc(jp) % t(:)
     profiles_tl(jp) % q(:)   = null_inc(jp) % q(:)
     profiles_tl(jp) % o3(:)  = null_inc(jp) % o3(:)
     profiles_tl(jp) % clw(:) = null_inc(jp) % clw(:)
     profiles_tl(jp) % s2m    = null_inc(jp) % s2m
     profiles_tl(jp) % skin   = null_inc(jp) % skin
     profiles_tl(jp) % ctp    = null_inc(jp) % ctp
     profiles_tl(jp) % cfraction   = null_inc(jp) % cfraction
     profiles_tl(jp) % ozone_Data  = null_inc(jp) % ozone_Data
     profiles_tl(jp) % co2_Data    = null_inc(jp) % co2_Data
     profiles_tl(jp) % clw_Data    = null_inc(jp) % clw_Data
     profiles_tl(jp) % zenangle    = null_inc(jp) % zenangle
  End Do
  emissivity_tl(:) = null_emissivity_inc(:)

  !.......now do surface, skin and cloud variables
  Do  j =1,sscvar

     Do jp = 1, nprofiles
        ! except the considered level/variable
        Select Case (j)
        Case (1_jpim)
           ! t 2m
           inc_val = prof_inc(jp) % s2m % t
           profiles_tl(jp) % s2m % t   = inc_val
        Case (2_jpim)
           ! wv 2m
           inc_val = prof_inc(jp) % s2m % q
           profiles_tl(jp) % s2m % q   = inc_val
        Case (3_jpim)
           ! surface pressure
           inc_val = prof_inc(jp) % s2m % p
           profiles_tl(jp) % s2m % p   = inc_val
        Case (4_jpim)
           ! wind speed u component
           inc_val = prof_inc(jp) % s2m % u
           profiles_tl(jp) % s2m % u   = inc_val
        Case (5_jpim)
           ! wind speed v component
           inc_val = prof_inc(jp) % s2m % v
           profiles_tl(jp) % s2m % v   = inc_val
        Case (6_jpim)
           ! skin temp
           inc_val = prof_inc(jp) % skin % t
           profiles_tl(jp) % skin % t   = inc_val
        Case (7_jpim)
           ! fastem land coef 1
           inc_val = prof_inc(jp) % skin % fastem(1)
           profiles_tl(jp) % skin % fastem(1)   = inc_val
        Case (8_jpim)
           ! fastem land coef 2
           inc_val = prof_inc(jp) % skin % fastem(2)
           profiles_tl(jp) % skin % fastem(2)   = inc_val
        Case (9_jpim)
           ! fastem land coef 3
           inc_val = prof_inc(jp) % skin % fastem(3)
           profiles_tl(jp) % skin % fastem(3)   = inc_val
        Case (10_jpim)
           ! fastem land coef 4
           inc_val = prof_inc(jp) % skin % fastem(4)
           profiles_tl(jp) % skin % fastem(4)   = inc_val
        Case (11_jpim)
           ! fastem land coef 5
           inc_val = prof_inc(jp) % skin % fastem(5)
           profiles_tl(jp) % skin % fastem(5)   = inc_val
        Case (12_jpim)
           ! cloud top pressure
           inc_val = prof_inc(jp) % ctp
           profiles_tl(jp) % ctp   = inc_val
        Case (13_jpim)
           ! cloud fraction
           inc_val = prof_inc(jp) % cfraction
           profiles_tl(jp) % cfraction   = inc_val
        End Select
     End Do

     ! use stored input emmisisvity
     emissivity(:) = input_emissivity(:)
     emissivity_tl(:) = null_emissivity_inc(:)

     Call rttov_tl( &
          & rttov_errorstatus,  &! out
          & nfrequencies,    &! in
          & nchannels,       &! in
          & nbtout,          &! in
          & nprofiles,       &! in
          & channels,        &! in
          & polarisations,   &! in
          & lprofiles,       &! in
          & profiles,        &! in
          & coef,            &! in
          & addcloud,        &! in
          & calcemis,        &! in
          & emissivity,      &! inout
          & profiles_tl,     &! in
          & emissivity_tl,   &! inout
          & transmission,    &! inout
          & transmission_tl, &! inout
          & radiancedata,    &! inout
          & radiancedata_tl ) ! inout

        If( any( rttov_errorstatus == errorstatus_fatal ) ) then
           Do jp = 1, nprofiles
              If( rttov_errorstatus(jp) == errorstatus_fatal ) then
                 Write( errMessage, '( "fatal error in rttov_tl")' )
                 Call Rttov_ErrorReport (rttov_errorstatus(jp), errMessage, NameOfRoutine)
              End If
           End Do
           Stop
        End If
        If( any( rttov_errorstatus /= errorstatus_success ) ) then
          Write( errMessage, '( "warning in rttov_tl")' )           
        End If

     Do jp = 1, nprofiles
        ! reset profile for next variable
        Select Case (j)
        Case (1_jpim)
           ! t 2m
           profiles_tl(jp) % s2m % t   = null_inc(jp) % s2m % t
        Case (2_jpim)
           ! wv 2m
           profiles_tl(jp) % s2m % q   = null_inc(jp) % s2m % q
        Case (3_jpim)
           ! surface pressure
           profiles_tl(jp) % s2m % p   = null_inc(jp) % s2m % p
        Case (4_jpim)
           ! wind speed u component
           profiles_tl(jp) % s2m % u   = null_inc(jp) % s2m % u
        Case (5_jpim)
           ! wind speed v component
           profiles_tl(jp) % s2m % v   = null_inc(jp) % s2m % v
        Case (6_jpim)
           ! skin temp
           profiles_tl(jp) % skin % t   = null_inc(jp) % skin % t
        Case (7_jpim)
           ! fastem land coef 1
           profiles_tl(jp) % skin % fastem(1)   = null_inc(jp) % skin % fastem(1)
        Case (8_jpim)
           ! fastem land coef 2
           profiles_tl(jp) % skin % fastem(2)   = null_inc(jp) % skin % fastem(2)
        Case (9_jpim)
           ! fastem land coef 3
           profiles_tl(jp) % skin % fastem(3)   = null_inc(jp) % skin % fastem(3)
        Case (10_jpim)
           ! fastem land coef 4
           profiles_tl(jp) % skin % fastem(4)   = null_inc(jp) % skin % fastem(4)
        Case (11_jpim)
           ! fastem land coef 5
           profiles_tl(jp) % skin % fastem(5)   = null_inc(jp) % skin % fastem(5)
        Case (12_jpim)
           ! cloud top pressure
           profiles_tl(jp) % ctp   = null_inc(jp) % ctp
        Case (13_jpim)
           ! cloud fraction
           profiles_tl(jp) % cfraction   = null_inc(jp) % cfraction
        End Select
     End Do

     If( inc_val == 0._JPRB ) Then
        pktsav(j,:) = 0._JPRB
        xkbsav(j,:) = 0._JPRB
     Else
        pktsav(j,:) = radiancedata_tl%out(:)
        xkbsav(j,:) = radiancedata_tl%out(:) / inc_val
     End If

  End Do

  Do  jch=1,nbtout
     ixksav(:,jch)=Nint(xkbsav(:,jch)*facsav(:))
  End Do

  !          ... and print it.
  Write (ioout,*)' surface variables '
  Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
  Write (ioout,*)' '
  Do i = 1 , jpnsav
     Write (ioout,333)i,(ixksav(i,jch),jch=1,nchan_out)
  Enddo
  Write (ioout,*)' '

  Write (ioout,*)' skin variables '
  Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
  Write (ioout,*)' '
  Do i = jpnsav+1 , jpnsav+jpnssv
     Write (ioout,333)i-jpnsav,(ixksav(i,jch),jch=1,nchan_out)
  Enddo
  Write (ioout,*)' '

  Write (ioout,*)' cloud variables '
  Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
  Write (ioout,*)' '
  Do i = jpnsav+jpnssv+1 , sscvar
     Write (ioout,333)i-jpnsav-jpnssv,(ixksav(i,jch),jch=1,nchan_out)
  Enddo
  Write (ioout,*)' '

  !.......now do surface emissivity
  Do  j =1,nbtout

     ! use stored input emmisisvity
     emissivity(:)    = input_emissivity(:)

     ! increment for only one channel
     emissivity_tl(:) = null_emissivity_inc(:)
     Do ipol=1, polarisations(frequencies(j),3)
        inc_val          = emissivity_inc(polarisations(frequencies(j),1)+ipol-1)
        emissivity_tl(polarisations(frequencies(j),1)+ipol-1) = inc_val
     End Do

     Call rttov_tl( &
          & rttov_errorstatus,  &! out
          & nfrequencies,    &! in
          & nchannels,       &! in
          & nbtout,          &! in
          & nprofiles,       &! in
          & channels,        &! in
          & polarisations,   &! in
          & lprofiles,       &! in
          & profiles,        &! in
          & coef,            &! in
          & addcloud,        &! in
          & calcemis,        &! in
          & emissivity,      &! inout
          & profiles_tl,     &! in
          & emissivity_tl,   &! inout
          & transmission,    &! inout
          & transmission_tl, &! inout
          & radiancedata,    &! inout
          & radiancedata_tl ) ! inout

        If( any( rttov_errorstatus == errorstatus_fatal ) ) then
           Do jp = 1, nprofiles
              If( rttov_errorstatus(jp) == errorstatus_fatal ) then
                 Write( errMessage, '( "fatal error in rttov_tl")' )
                 Call Rttov_ErrorReport (rttov_errorstatus(jp), errMessage, NameOfRoutine)
              End If
           End Do
           Stop
        End If
        If( any( rttov_errorstatus /= errorstatus_success ) ) then
          Write( errMessage, '( "warning in rttov_tl")' )           
        End If

     !.......Except for FASTEM-2 (input <0)
     !If( emissivity_tl(j) == 0. .Or. input_emissivity(j)<0.) Then
    If( emissivity_tl(polarisations(frequencies(j),1)) == 0._JPRB .Or. &
     & ( coef % fastem_ver >= 2 .and. calcemis(j)) ) Then
        xkbem(j) = 0._JPRB
     Else
        xkbem(j) = radiancedata_tl%out(j) / inc_val
     End If

  End Do
  ixkem(:)=Nint(xkbem(:)*facem)

  Write (ioout,*)' surface emissivity '
  Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
  Write (ioout,*)' '
  Do jp = 1, nprofiles
     joff = (jp-1) * nchan_out
     Write (ioout,333)jp,(ixkem(jch+joff),jch=1,nchan_out)
  Enddo
  Write (ioout,*)' '

  !
  ! Now compute Brute force matrix
  !
  !save forward model radiances and emissivities
  ! do not consider overcast radiances
  radiance_fwd % clear(:)    = radiancedata % clear(:)
  radiance_fwd % cloudy(:)   = radiancedata % cloudy(:)
  radiance_fwd % total_out(:)    = radiancedata % total_out(:)
  radiance_fwd % out(:)       = radiancedata % out(:)
  radiance_fwd % out_clear(:) = radiancedata % out_clear(:)
  emissivity_fwd(:)          = emissivity(:)

  Do  j =1,jpnav    ! yes clw too!
     Do  ii=1,nlev

        ! initialise profile with input profile
        Do jp = 1, nprofiles
           profiles_bf(jp) % p(:)   = profiles(jp) % p(:)
           profiles_bf(jp) % t(:)   = profiles(jp) % t(:)
           profiles_bf(jp) % q(:)   = profiles(jp) % q(:)
           profiles_bf(jp) % o3(:)  = profiles(jp) % o3(:)
           profiles_bf(jp) % clw(:) = profiles(jp) % clw(:)
           profiles_bf(jp) % s2m    = profiles(jp) % s2m
           profiles_bf(jp) % skin   = profiles(jp) % skin
           profiles_bf(jp) % ctp    = profiles(jp) % ctp
           profiles_bf(jp) % cfraction   = profiles(jp) % cfraction
           profiles_bf(jp) % ozone_Data  = profiles(jp) % ozone_Data
           profiles_bf(jp) % co2_Data    = profiles(jp) % co2_Data
           profiles_bf(jp) % clw_Data    = profiles(jp) % clw_Data
           profiles_bf(jp) % zenangle    = profiles(jp) % zenangle
           profiles_bf(jp) % azangle     = profiles(jp) % azangle
        End Do

        Do jp = 1, nprofiles
           ! except the considered level/variable
           Select Case (j)
           Case (1_jpim)
              inc_val = prof_inc(1) % t(ii)
              profiles_bf(jp) % t(ii) = profiles_bf(jp) % t(ii) + inc_val
           Case (2_jpim)
              inc_val = prof_inc(1) % q(ii)
              profiles_bf(jp) % q(ii)   = profiles_bf(jp) % q(ii) + inc_val
              ! change increment for output compatibilty
              inc_val = prof_inc(1) % q(ii)/q_mixratio_to_ppmv
           Case (3_jpim)
              inc_val = prof_inc(1) % o3(ii)
              profiles_bf(jp) % o3(ii)  = profiles_bf(jp) % o3(ii) + inc_val
              ! change increment for output compatibilty
              inc_val = prof_inc(1) % o3(ii)/o3_mixratio_to_ppmv
           Case (4_jpim)
              inc_val = prof_inc(1) % clw(ii)
              profiles_bf(jp) % clw(ii) = profiles_bf(jp) % clw(ii) + inc_val
           End Select
        End Do

        ! use stored input emmisisvity
        emissivity(:) = input_emissivity(:)

        Call rttov_direct( &
             & rttov_errorstatus,  &! out
             & nfrequencies,  &! in
             & nchannels,     &! in
             & nbtout,        &! in
             & nprofiles,     &! in
             & channels,      &! in
             & polarisations, &! in
             & lprofiles,     &! in
             & profiles_bf,   &! in
             & coef,          &! in
             & addcloud,      &! in
             & calcemis,      &! in
             & emissivity,    &! inout
             & transmission,  &! out
             & radiancedata)   ! inout

        If( any( rttov_errorstatus == errorstatus_fatal ) ) then
           Do jp = 1, nprofiles
              If( rttov_errorstatus(jp) == errorstatus_fatal ) then
                 Write( errMessage, '( "fatal error in rttov_tl")' )
                 Call Rttov_ErrorReport (rttov_errorstatus(jp), errMessage, NameOfRoutine)
              End If
           End Do
           Stop
        End If
        If( any( rttov_errorstatus /= errorstatus_success ) ) then
          Write( errMessage, '( "warning in rttov_tl")' )           
        End If

        If (inc_val == 0._JPRB) Then
           xktav(ii,j,:)=0._JPRB
        Else
           xktav(ii,j,:)=( radiancedata%out(:) - radiance_fwd%out(:) )/ inc_val
        Endif

     End Do
  End Do
  Do  jch=1,nbtout
     ixkdav(:,:,jch)=Nint(xktav(:,:,jch)*facpav(:,:))
  End Do

  ! initialise profile with input profile
  Do jp = 1, nprofiles
     profiles_bf(jp) % p(:)   = profiles(jp) % p(:)
     profiles_bf(jp) % t(:)   = profiles(jp) % t(:)
     profiles_bf(jp) % q(:)   = profiles(jp) % q(:)
     profiles_bf(jp) % o3(:)  = profiles(jp) % o3(:)
     profiles_bf(jp) % clw(:) = profiles(jp) % clw(:)
     profiles_bf(jp) % s2m    = profiles(jp) % s2m
     profiles_bf(jp) % skin   = profiles(jp) % skin
     profiles_bf(jp) % ctp    = profiles(jp) % ctp
     profiles_bf(jp) % cfraction   = profiles(jp) % cfraction
     profiles_bf(jp) % ozone_Data  = profiles(jp) % ozone_Data
     profiles_bf(jp) % co2_Data    = profiles(jp) % co2_Data
     profiles_bf(jp) % clw_Data    = profiles(jp) % clw_Data
     profiles_bf(jp) % zenangle    = profiles(jp) % zenangle
  End Do


  !.......now do surface, skin and cloud variables
  Do  j =1,sscvar


     Do jp = 1, nprofiles
        ! except the considered level/variable
        Select Case (j)
        Case (1_jpim)
           ! t 2m
           inc_val = prof_inc(jp) % s2m % t
           profiles_bf(jp) % s2m % t   = profiles_bf(jp) % s2m % t + inc_val
        Case (2_jpim)
           ! wv 2m
           inc_val = prof_inc(jp) % s2m % q
           profiles_bf(jp) % s2m % q   = profiles_bf(jp) % s2m % q + inc_val
        Case (3_jpim)
           ! surface pressure
           inc_val = prof_inc(jp) % s2m % p
           profiles_bf(jp) % s2m % p   = profiles_bf(jp) % s2m % p + inc_val
        Case (4_jpim)
           ! wind speed u component
           inc_val = prof_inc(jp) % s2m % u
           profiles_bf(jp) % s2m % u   = profiles_bf(jp) % s2m % u + inc_val
        Case (5_jpim)
           ! wind speed v component
           inc_val = prof_inc(jp) % s2m % v
           profiles_bf(jp) % s2m % v   = profiles_bf(jp) % s2m % v + inc_val
        Case (6_jpim)
           ! skin temp
           inc_val = prof_inc(jp) % skin % t
           profiles_bf(jp) % skin % t   = profiles_bf(jp) % skin % t + inc_val
        Case (7_jpim)
           ! fastem land coef 1
           inc_val = prof_inc(jp) % skin % fastem(1)
           profiles_bf(jp) % skin % fastem(1)   = profiles_bf(jp) % skin % fastem(1) + inc_val
        Case (8_jpim)
           ! fastem land coef 2
           inc_val = prof_inc(jp) % skin % fastem(2)
           profiles_bf(jp) % skin % fastem(2)   = profiles_bf(jp) % skin % fastem(2) + inc_val
        Case (9_jpim)
           ! fastem land coef 3
           inc_val = prof_inc(jp) % skin % fastem(3)
           profiles_bf(jp) % skin % fastem(3)   = profiles_bf(jp) % skin % fastem(3) + inc_val
        Case (10_jpim)
           ! fastem land coef 4
           inc_val = prof_inc(jp) % skin % fastem(4)
           profiles_bf(jp) % skin % fastem(4)   = profiles_bf(jp) % skin % fastem(4) + inc_val
        Case (11_jpim)
           ! fastem land coef 5
           inc_val = prof_inc(jp) % skin % fastem(5)
           profiles_bf(jp) % skin % fastem(5)   = profiles_bf(jp) % skin % fastem(5) + inc_val
        Case (12_jpim)
           ! cloud top pressure
           inc_val = prof_inc(jp) % ctp
           profiles_bf(jp) % ctp   = profiles_bf(jp) % ctp + inc_val
        Case (13_jpim)
           ! cloud fraction
           inc_val = prof_inc(jp) % cfraction
           profiles_bf(jp) % cfraction   = profiles_bf(jp) % cfraction + inc_val
        End Select
     End Do

     ! use stored input emmisisvity
     emissivity(:) = input_emissivity(:)

     Call rttov_direct( &
          & rttov_errorstatus,  &! out
          & nfrequencies,   &! in
          & nchannels,      &! in
          & nbtout,         &! in
          & nprofiles,      &! in
          & channels,       &! in
          & polarisations,  &! in
          & lprofiles,      &! in
          & profiles_bf,    &! in
          & coef,           &! in
          & addcloud,       &! in
          & calcemis,       &! in
          & emissivity,     &! inout
          & transmission,   &! out
          & radiancedata)    ! inout

        If( any( rttov_errorstatus == errorstatus_fatal ) ) then
           Do jp = 1, nprofiles
              If( rttov_errorstatus(jp) == errorstatus_fatal ) then
                 Write( errMessage, '( "fatal error in rttov_tl")' )
                 Call Rttov_ErrorReport (rttov_errorstatus(jp), errMessage, NameOfRoutine)
              End If
           End Do
           Stop
        End If
        If( any( rttov_errorstatus /= errorstatus_success ) ) then
          Write( errMessage, '( "warning in rttov_tl")' )           
        End If

     Do jp = 1, nprofiles
        ! reset profile for next variable
        Select Case (j)
        Case (1_jpim)
           ! t 2m
           profiles_bf(jp) % s2m % t   = profiles(jp) % s2m % t
        Case (2_jpim)
           ! wv 2m
           profiles_bf(jp) % s2m % q   = profiles(jp) % s2m % q
        Case (3_jpim)
           ! surface pressure
           profiles_bf(jp) % s2m % p   = profiles(jp) % s2m % p
        Case (4_jpim)
           ! wind speed u component
           profiles_bf(jp) % s2m % u   = profiles(jp) % s2m % u
        Case (5_jpim)
           ! wind speed v component
           profiles_bf(jp) % s2m % v   = profiles(jp) % s2m % v
        Case (6_jpim)
           ! skin temp
           profiles_bf(jp) % skin % t   = profiles(jp) % skin % t
        Case (7_jpim)
           ! fastem land coef 1
           profiles_bf(jp) % skin % fastem(1)   = profiles(jp) % skin % fastem(1)
        Case (8_jpim)
           ! fastem land coef 2
           profiles_bf(jp) % skin % fastem(2)   = profiles(jp) % skin % fastem(2)
        Case (9_jpim)
           ! fastem land coef 3
           profiles_bf(jp) % skin % fastem(3)   = profiles(jp) % skin % fastem(3)
        Case (10_jpim)
           ! fastem land coef 4
           profiles_bf(jp) % skin % fastem(4)   = profiles(jp) % skin % fastem(4)
        Case (11_jpim)
           ! fastem land coef 5
           profiles_bf(jp) % skin % fastem(5)   = profiles(jp) % skin % fastem(5)
        Case (12_jpim)
           ! cloud top pressure
           profiles_bf(jp) % ctp   = profiles(jp) % ctp
        Case (13_jpim)
           ! cloud fraction
           profiles_bf(jp) % cfraction   = profiles(jp) % cfraction
        End Select
     End Do

     If( inc_val == 0._JPRB ) Then
        xktsav(j,:) = 0._JPRB
     Else
        xktsav(j,:)=( radiancedata%out(:) - radiance_fwd%out(:) )/ inc_val
     End If

  End Do

  Do  jch=1,nbtout
     ixkdsav(:,jch)=Nint(xktsav(:,jch)*facsav(:))
  End Do


  !.......now do surface emissivity
  Do  j =1,nbtout

     ! increment for only one channel
     ! use emissivity from FWD model output
     emissivity_bf(:) = emissivity_fwd(:)
     calcemis_bf(:)   = .False.
     Do ipol=1, polarisations(frequencies(j),3)
        inc_val       = emissivity_inc(polarisations(frequencies(j),1)+ipol-1)
        emissivity_bf(polarisations(frequencies(j),1)+ipol-1) =  &
         & emissivity_bf(polarisations(frequencies(j),1)+ipol-1) + inc_val
     End Do

     Call rttov_direct( &
          & rttov_errorstatus,  &! out
          & nfrequencies,   &! in
          & nchannels,      &! in
          & nbtout,         &! in
          & nprofiles,      &! in
          & channels,       &! in
          & polarisations,  &! in
          & lprofiles,      &! in
          & profiles,       &! in
          & coef,           &! in
          & addcloud,       &! in
          & calcemis_bf,    &! in
          & emissivity_bf,  &! inout
          & transmission,   &! out
          & radiancedata)    ! inout

        If( any( rttov_errorstatus == errorstatus_fatal ) ) then
           Do jp = 1, nprofiles
              If( rttov_errorstatus(jp) == errorstatus_fatal ) then
                 Write( errMessage, '( "fatal error in rttov_tl")' )
                 Call Rttov_ErrorReport (rttov_errorstatus(jp), errMessage, NameOfRoutine)
              End If
           End Do
           Stop
        End If
        If( any( rttov_errorstatus /= errorstatus_success ) ) then
          Write( errMessage, '( "warning in rttov_tl")' )           
        End If

     If( emissivity_bf(j) == 0._JPRB ) Then
        xktem(j) = 0._JPRB
     Else
        xktem(j)=( radiancedata%out(j) - radiance_fwd%out(j) )/ inc_val
     End If

  End Do
  ixkdem(:)=Nint(xktem(:)*facem)

  !          ... and print it.
  Write (ioout,*)' '
  Write (ioout,*)'k-matrix: brute force '

  Write (ioout,*)' '
  Do j = 1 , jpnav                   ! lwp on
     Write (ioout,'(a30)')title(j)
     Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
     Do i = 1 , nlev
        Write (ioout,333)i,(ixkdav(i,j,jch),jch=1,nchan_out)
     Enddo
     Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
     Write (ioout,*)' '
  Enddo

  Write (ioout,*)' surface variables '
  Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
  Write (ioout,*)' '
  Do i = 1 , jpnsav
     Write (ioout,333)i,(ixkdsav(i,jch),jch=1,nchan_out)
  Enddo
  Write (ioout,*)' '

  Write (ioout,*)' skin variables '
  Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
  Write (ioout,*)' '
  Do i = jpnsav+1 , jpnsav+jpnssv
     Write (ioout,333)i-jpnsav,(ixkdsav(i,jch),jch=1,nchan_out)
  Enddo
  Write (ioout,*)' '

  Write (ioout,*)' cloud variables '
  Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
  Write (ioout,*)' '
  Do i = jpnsav+jpnssv+1 , sscvar
     Write (ioout,333)i-jpnsav-jpnssv,(ixkdsav(i,jch),jch=1,nchan_out)
  Enddo
  Write (ioout,*)' '
  Write (ioout,*)' surface emissivity '
  Write (ioout,3333)coef%ff_ori_chn(frequencies(1:nchan_out))
  Write (ioout,*)' '
  Do jp = 1, nprofiles
     joff = (jp-1) * nchan_out
     Write (ioout,333)jp,(ixkdem(jch+joff),jch=1,nchan_out)
  Enddo
  Write (ioout,*)' '


  !--------- CHECK CONSISTENCY BETWEEN TL AND BRUTE FORCE------------
  ! run TL with increments on all variables
  !
  emissivity(:)    = input_emissivity(:)
  emissivity_tl(:) = emissivity_inc(:)
  !
  ! if micro waves and  "calcemis" then mask out FASTEM2 (input <0)
  If (coef % id_sensor == sensor_id_mw .And.&
        & coef % fastem_ver >= 2 ) Then
     !Where (input_emissivity(:) < 0.0)
     Where ( calcemis )
        emissivity_tl(:) = 0._JPRB
     End Where
  Endif

  Call rttov_tl( &
       & rttov_errorstatus,  &! out
       & nfrequencies,    &! in
       & nchannels,       &! in
       & nbtout,          &! in
       & nprofiles,       &! in
       & channels,        &! in
       & polarisations,   &! in
       & lprofiles,       &! in
       & profiles,        &! in
       & coef,            &! in
       & addcloud,        &! in
       & calcemis,        &! in
       & emissivity,      &! inout
       & prof_inc,        &! in
       & emissivity_tl,   &! inout
       & transmission,    &! inout
       & transmission_tl, &! inout
       & radiancedata,    &! inout
       & radiancedata_tl ) ! out

        If( any( rttov_errorstatus == errorstatus_fatal ) ) then
           Do jp = 1, nprofiles
              If( rttov_errorstatus(jp) == errorstatus_fatal ) then
                 Write( errMessage, '( "fatal error in rttov_tl")' )
                 Call Rttov_ErrorReport (rttov_errorstatus(jp), errMessage, NameOfRoutine)
              End If
           End Do
           Stop
        End If
        If( any( rttov_errorstatus /= errorstatus_success ) ) then
          Write( errMessage, '( "warning in rttov_tl")' )           
        End If

  Do prof = 1, nprofiles
     sumrr = 0._JPRB
     Do jch=1,nbtout
        If(lprofiles(frequencies(jch)) == prof) Then
           sumrr = sumrr + radiancedata_tl % out(jch)
        Endif
     End Do
     Write (IOOUT,149)prof,SUMRR
149  Format (1X,'PROFILE NUMBER=',I2,'  TL=',E20.10)
  End Do


  Do  ii =1,15

     fac = 10.0_JPRB**(ii-1)

     ! initialise profile with input profile
     Do jp = 1, nprofiles
        profiles_bf(jp) % p(:)   = profiles(jp) % p(:)
        profiles_bf(jp) % t(:)   = profiles(jp) % t(:) + prof_inc(jp) % t(:) / fac
        profiles_bf(jp) % q(:)   = profiles(jp) % q(:) + prof_inc(jp) %  q(:)  / fac
        profiles_bf(jp) % o3(:)  = profiles(jp) % o3(:) + prof_inc(jp) % o3(:)  / fac
        profiles_bf(jp) % clw(:) = profiles(jp) % clw(:) + prof_inc(jp) %  clw(:) / fac
        profiles_bf(jp) % s2m    = profiles(jp) % s2m
        profiles_bf(jp) % s2m % t    = profiles(jp) % s2m % t  + prof_inc(jp) % s2m % t / fac
        profiles_bf(jp) % s2m % q   = profiles(jp) % s2m % q  + prof_inc(jp) % s2m % q / fac
        profiles_bf(jp) % s2m % p   = profiles(jp) % s2m % p  + prof_inc(jp) % s2m % p / fac
        profiles_bf(jp) % s2m % u   = profiles(jp) % s2m % u  + prof_inc(jp) % s2m % u / fac
        profiles_bf(jp) % s2m % v   = profiles(jp) % s2m % v  + prof_inc(jp) % s2m % v / fac
        profiles_bf(jp) % skin   = profiles(jp) % skin
        profiles_bf(jp) % skin % t   = profiles(jp) % skin % t + prof_inc(jp) % skin % t / fac
        profiles_bf(jp) % skin % fastem  = profiles(jp) % skin % fastem + prof_inc(jp) % skin % fastem / fac
        profiles_bf(jp) % ctp    = profiles(jp) % ctp + prof_inc(jp) % ctp  / fac
        profiles_bf(jp) % cfraction   = profiles(jp) % cfraction + prof_inc(jp) % cfraction / fac
        profiles_bf(jp) % ozone_Data  = profiles(jp) % ozone_Data
        profiles_bf(jp) % co2_Data    = profiles(jp) % co2_Data
        profiles_bf(jp) % clw_Data    = profiles(jp) % clw_Data
        profiles_bf(jp) % zenangle    = profiles(jp) % zenangle
     End Do

     ! use emissivity from FWD model output
     emissivity_bf(:) = emissivity_fwd(:) + emissivity_inc(:) / fac
     calcemis_bf(:)   = .False.

     ! if micro waves then mask out FASTEM2
     If (coef % id_sensor == sensor_id_mw .And.&
           & coef % fastem_ver >= 2 ) Then
        !Where (input_emissivity(:) < 0.0)
        Where ( calcemis )
           emissivity_bf(:) = input_emissivity(:)
           calcemis_bf(:)   = calcemis(:)
        End Where
     Endif

     Call rttov_direct( &
          & rttov_errorstatus,  &! out
          & nfrequencies,   &! in
          & nchannels,      &! in
          & nbtout,         &! in
          & nprofiles,      &! in
          & channels,       &! in
          & polarisations,  &! in
          & lprofiles,      &! in
          & profiles_bf,    &! in
          & coef,           &! in
          & addcloud,       &! in
          & calcemis_bf,    &! in
          & emissivity_bf,  &! inout
          & transmission,   &! out
          & radiancedata)    ! inout

        If( any( rttov_errorstatus == errorstatus_fatal ) ) then
           Do jp = 1, nprofiles
              If( rttov_errorstatus(jp) == errorstatus_fatal ) then
                 Write( errMessage, '( "fatal error in rttov_tl")' )
                 Call Rttov_ErrorReport (rttov_errorstatus(jp), errMessage, NameOfRoutine)
              End If
           End Do
           Stop
        End If
        If( any( rttov_errorstatus /= errorstatus_success ) ) then
          Write( errMessage, '( "warning in rttov_tl")' )           
        End If

     sumr = 0._JPRB
     prof = 1  ! tests only first profile
     Do jch=1, nbtout
        If(lprofiles(frequencies(jch)) == prof) Then
           sumr = sumr + radiancedata%out(jch) - radiance_fwd%out(jch)
        End If
     End Do
     sumr = sumr*fac
     DIFFR=SUMR/SUMRR
     Write (IOOUT,148) SUMR,DIFFR,II
148  Format (1X,'BRUTE FORCE:   ',2E20.10,I10)

  End Do

  Do j = 1, nprofiles
     Deallocate( prof_inc(j) % p   ,stat= alloc_status(1))
     Deallocate( prof_inc(j) % t   ,stat= alloc_status(2))
     Deallocate( prof_inc(j) % q   ,stat= alloc_status(3))
     Deallocate( prof_inc(j) % o3  ,stat= alloc_status(4))
     Deallocate( prof_inc(j) % clw ,stat= alloc_status(5))
     Deallocate( null_inc(j) % p   ,stat= alloc_status(6))
     Deallocate( null_inc(j) % t   ,stat= alloc_status(7))
     Deallocate( null_inc(j) % q   ,stat= alloc_status(8))
     Deallocate( null_inc(j) % o3  ,stat= alloc_status(9))
     Deallocate( null_inc(j) % clw ,stat= alloc_status(10))
     Deallocate( profiles_tl(j) % p   ,stat= alloc_status(11))
     Deallocate( profiles_tl(j) % t   ,stat= alloc_status(12))
     Deallocate( profiles_tl(j) % q   ,stat= alloc_status(13))
     Deallocate( profiles_tl(j) % o3  ,stat= alloc_status(14))
     Deallocate( profiles_tl(j) % clw ,stat= alloc_status(15))
     Deallocate( profiles_bf(j) % p   ,stat= alloc_status(16))
     Deallocate( profiles_bf(j) % t   ,stat= alloc_status(17))
     Deallocate( profiles_bf(j) % q   ,stat= alloc_status(18))
     Deallocate( profiles_bf(j) % o3  ,stat= alloc_status(19))
     Deallocate( profiles_bf(j) % clw ,stat= alloc_status(20))
     If( any(alloc_status /= 0) ) then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "mem deallocation error")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If
  End Do

  ! deallocate radiance results arrays with number of channels
  Deallocate( radiancedata % clear     ,stat= alloc_status(1))
  Deallocate( radiancedata % cloudy    ,stat= alloc_status(2))
  Deallocate( radiancedata % total     ,stat= alloc_status(3))
  Deallocate( radiancedata % bt        ,stat= alloc_status(4))
  Deallocate( radiancedata % bt_clear  ,stat= alloc_status(5))
  Deallocate( radiancedata % upclear   ,stat= alloc_status(6))
  Deallocate( radiancedata % dnclear   ,stat= alloc_status(34))
  Deallocate( radiancedata % reflclear ,stat= alloc_status(7))
  Deallocate( radiancedata % overcast  ,stat= alloc_status(8))
  Deallocate( radiancedata % downcld   ,stat= alloc_status(9))
  Deallocate( radiancedata % out       ,stat= alloc_status(10))
  Deallocate( radiancedata % out_clear ,stat= alloc_status(11))
  Deallocate( radiancedata % total_out ,stat= alloc_status(12))
  Deallocate( radiancedata % clear_out ,stat= alloc_status(13))
  Deallocate( radiancedata_tl % clear     ,stat= alloc_status(14))
  Deallocate( radiancedata_tl % cloudy    ,stat= alloc_status(15))
  Deallocate( radiancedata_tl % total     ,stat= alloc_status(16))
  Deallocate( radiancedata_tl % bt        ,stat= alloc_status(17))
  Deallocate( radiancedata_tl % bt_clear  ,stat= alloc_status(18))
  Deallocate( radiancedata_tl % upclear   ,stat= alloc_status(19))
  Deallocate( radiancedata_tl % reflclear ,stat= alloc_status(20))
  Deallocate( radiancedata_tl % overcast  ,stat= alloc_status(21))
  Deallocate( radiancedata_tl % downcld   ,stat= alloc_status(22))
  Deallocate( radiancedata_tl % out    ,stat= alloc_status(23))
  Deallocate( radiancedata_tl % out_clear    ,stat= alloc_status(24))
  Deallocate( radiancedata_tl % total_out ,stat= alloc_status(25))
  Deallocate( radiancedata_tl % clear_out ,stat= alloc_status(26))
  Deallocate( radiance_fwd % clear     ,stat= alloc_status(27))
  Deallocate( radiance_fwd % cloudy    ,stat= alloc_status(28))
  Deallocate( radiance_fwd % total     ,stat= alloc_status(29))
  Deallocate( radiance_fwd % bt        ,stat= alloc_status(30))
  Deallocate( radiance_fwd % bt_clear  ,stat= alloc_status(31))
  Deallocate( radiance_fwd % upclear   ,stat= alloc_status(32))
  Deallocate( radiance_fwd % reflclear ,stat= alloc_status(33))
  Deallocate( radiance_fwd % overcast  ,stat= alloc_status(34))
  Deallocate( radiance_fwd % downcld   ,stat= alloc_status(35))
  Deallocate( radiance_fwd % out       ,stat= alloc_status(36))
  Deallocate( radiance_fwd % out_clear ,stat= alloc_status(37))
  Deallocate( radiance_fwd % total_out ,stat= alloc_status(38))
  Deallocate( radiance_fwd % clear_out ,stat= alloc_status(39))
  Deallocate( transmission % tau_surf      ,stat= alloc_status(40))
  Deallocate( transmission % tau_layer     ,stat= alloc_status(41))
  Deallocate( transmission % od_singlelayer,stat= alloc_status(42))
  Deallocate( transmission_tl % tau_surf      ,stat= alloc_status(43))
  Deallocate( transmission_tl % tau_layer     ,stat= alloc_status(44))
  Deallocate( transmission_tl % od_singlelayer,stat= alloc_status(45))
  If( any(alloc_status /= 0) ) then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "mem deallocation error")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  End If

  Print *, ' tangent linear model test finished'
222 Format(1x,10f8.2)
333 Format(1x,i3,20i5)
444 Format(1x,10e8.2)
3333 Format(4x,20i5)
4444 Format(1x,10f8.3)


End Subroutine tstrad_tl
