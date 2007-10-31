Subroutine tstrad_ad(   &
     & nfrequencies,    &! in
     & nchannels,       &! in
     & nbtout,          &! in
     & nprofiles,       &! in
     & channels,        &! in
     & polarisations,   &! in
     & frequencies,     &! in
     & lprofiles,       &! in
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
       & errorstatus_fatal

  Use rttov_types, Only : &
       & rttov_coef     ,&
       & profile_Type   ,&
       & transmission_Type   ,&
       & radiance_Type

  Use mod_tstrad

  Use parkind1, Only : jpim     ,jprb
  Implicit None

#include "rttov_errorreport.interface"
#include "rttov_tl.interface"
#include "rttov_ad.interface"

  !subroutine arguments:
  Integer(Kind=jpim),  Intent(in)    :: nchannels
  Integer(Kind=jpim),  Intent(in)    :: nfrequencies
  Integer(Kind=jpim),  Intent(in)    :: nbtout
  Integer(Kind=jpim),  Intent(in)    :: nprofiles
  Integer(Kind=jpim),  Intent(in)    :: channels(nfrequencies)
  Integer(Kind=jpim),  Intent(in)    :: polarisations(nchannels,3)
  Integer(Kind=jpim),  Intent(in)    :: frequencies(nbtout)
  Integer(Kind=jpim),  Intent(in)    :: lprofiles(nfrequencies)
  Logical,             Intent(in)    :: addcloud
  Type(profile_Type),  Intent(in)    :: profiles(nprofiles)
  Type(rttov_coef),    Intent(in)    :: coef
  Logical,             Intent(in)    :: calcemis(nchannels)
  Real(Kind=jprb),     Intent(in)    :: input_emissivity(nchannels)



  ! local
  Integer(Kind=jpim), Parameter :: jpnav  =  4       ! no. of profile variables
  Integer(Kind=jpim), Parameter :: jpnsav =  5       ! no. of surface air variables
  Integer(Kind=jpim), Parameter :: jpnssv =  6       ! no. of skin variables
  Integer(Kind=jpim), Parameter :: jpncv  =  2       ! no. of cloud variables
  Integer(Kind=jpim), Parameter :: sscvar = jpnsav+jpnssv+jpncv ! no of surface,skin,cloud vars
  Integer(Kind=jpim)            :: errorstatus
  Character (len=80) :: errMessage
  Character (len=10) :: NameOfRoutine = 'tstrad_ad '

  ! forward model outputs
  logical :: switchrad
  Type(transmission_Type)   :: transmission
  Type(radiance_Type)   :: radiancedata
  Real(Kind=jprb)   :: emissivity(nchannels)

  ! AD variables for rttov_ad calls
  Type(profile_Type)  :: profiles_ad(nprofiles)
  Type(transmission_Type)   :: transmission_ad
  Type(radiance_Type) :: radiancedata_ad
  Real(Kind=jprb)  :: emissivity_ad(nchannels)

  ! TL variables
  Type(profile_Type)  :: profiles_tl(nprofiles)
  Type(transmission_Type)   :: transmission_tl
  Type(radiance_Type) :: radiancedata_tl
  Real(Kind=jprb)  :: emissivity_tl(nchannels)
  Logical    :: nocalcemis(nchannels)

  Integer(Kind=jpim) :: nlev
  Integer(Kind=jpim) :: ixkav(coef%nlevels,jpnav,nbtout)

  Integer(Kind=jpim) :: ixksav(sscvar,nbtout)

  ! Adjoint results
  Integer(Kind=jpim) :: ixkdav(coef%nlevels,jpnav,nbtout)
  Real(Kind=jprb)    :: xktav (coef%nlevels,jpnav,nbtout)
  Integer(Kind=jpim) :: ixkdsav(sscvar,nbtout)
  Real(Kind=jprb)    :: xktsav (sscvar,nbtout)
  Integer(Kind=jpim) :: ixdem(nbtout)
  Real(Kind=jprb)    :: xkaem(nbtout)

  ! coefficients for printing
  Real(Kind=jprb) :: facpav(coef%nlevels,jpnav)
  Real(Kind=jprb) :: facem = 1._JPRB

  Real(Kind=jprb) :: facsav(sscvar) =&
        & (/10000._JPRB,0.1_JPRB,10000._JPRB,10000._JPRB,10000._JPRB,       &! 2m
        & 10000._JPRB,100.0_JPRB,100.0_JPRB,100.0_JPRB,100.0_JPRB,100.0_JPRB,  &! Skin
        & 10000._JPRB,100._JPRB/)                           ! cloud

  Real(Kind=jprb) :: facdiff = 1.e+10_JPRB
  !Real :: facdiff = 1.

  Real(Kind=jprb), Parameter :: q_mixratio_to_ppmv  = 1.60771704e+6_JPRB
  Real(Kind=jprb), Parameter :: o3_mixratio_to_ppmv = 6.03504e+5_JPRB

  Integer(Kind=jpim) :: ioout = 2
  Integer(Kind=jpim) :: ich, jch, ipol
  Integer(Kind=jpim) :: j, i, ii, jp, joff, freq
  Integer(Kind=jpim) :: prof
  Integer(Kind=jpim) :: nchan_out
  Integer(Kind=jpim) :: lev
  Real(Kind=jprb) :: z
  Real(Kind=jprb) :: sump, sumr
  Real(Kind=jprb) :: eps

  Character (len=30) :: title(4) = &
        & (/' lev       temperature        ', &
          & ' lev       water vapour       ',   &
          & ' lev       ozone              ',   &
          & ' lev       liquid water       '/)

  Integer(Kind=jpim) :: alloc_status(60)
  Integer(Kind=jpim) :: rttov_errorstatus(nprofiles)

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

  ! coefficients compatibility with RTTOV7
  facpav(:,2) = facpav(:,2) *  q_mixratio_to_ppmv
  facpav(:,3) = facpav(:,3) * o3_mixratio_to_ppmv
  facsav(2)   = facsav(2)   *  q_mixratio_to_ppmv

  ! allocate and initialise the reference tl increments
  Do j = 1, nprofiles
     profiles_ad(j) % nlevels =  coef % nlevels
     Allocate( profiles_ad(j) % p  ( coef % nlevels ) ,stat= alloc_status(1))
     Allocate( profiles_ad(j) % t  ( coef % nlevels ) ,stat= alloc_status(2))
     Allocate( profiles_ad(j) % q  ( coef % nlevels ) ,stat= alloc_status(3))
     Allocate( profiles_ad(j) % o3 ( coef % nlevels ) ,stat= alloc_status(4))
     Allocate( profiles_ad(j) % clw( coef % nlevels ) ,stat= alloc_status(5))
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
  Allocate( radiancedata % out      ( nchannels ) ,stat= alloc_status(6))
  Allocate( radiancedata % out_clear( nchannels ) ,stat= alloc_status(7))
  Allocate( radiancedata % total_out( nchannels ) ,stat= alloc_status(8))
  Allocate( radiancedata % clear_out( nchannels ) ,stat= alloc_status(9))
  Allocate( radiancedata % upclear  ( nchannels ) ,stat=  alloc_status(10))
  Allocate( radiancedata % dnclear  ( nchannels ) ,stat= alloc_status(34))
  Allocate( radiancedata % reflclear( nchannels ) ,stat= alloc_status(11))
  Allocate( radiancedata % overcast ( coef % nlevels, nchannels ) ,stat= alloc_status(12))
  Allocate( radiancedata % downcld  ( coef % nlevels, nchannels ) ,stat= alloc_status(13))
  Allocate( radiancedata_ad % clear    ( nchannels ) ,stat= alloc_status(14))
  Allocate( radiancedata_ad % cloudy   ( nchannels ) ,stat= alloc_status(15))
  Allocate( radiancedata_ad % total_out ( nchannels ) ,stat= alloc_status(16))
  Allocate( radiancedata_ad % clear_out ( nchannels ) ,stat= alloc_status(17))
  Allocate( radiancedata_ad % total    ( nchannels ) ,stat= alloc_status(18))
  Allocate( radiancedata_ad % bt       ( nchannels ) ,stat= alloc_status(19))
  Allocate( radiancedata_ad % bt_clear ( nchannels ) ,stat= alloc_status(20))
  Allocate( radiancedata_ad % out      ( nchannels ) ,stat= alloc_status(21))
  Allocate( radiancedata_ad % out_clear( nchannels ) ,stat= alloc_status(22))
  Allocate( radiancedata_ad % upclear  ( nchannels ) ,stat= alloc_status(23))
  Allocate( radiancedata_ad % reflclear( nchannels ) ,stat= alloc_status(24))
  Allocate( radiancedata_ad % overcast ( coef % nlevels, nchannels ) ,stat= alloc_status(25))
  Allocate( radiancedata_ad % downcld  ( coef % nlevels, nchannels ) ,stat= alloc_status(26))
  Allocate( transmission % tau_surf              ( nchannels ) ,stat= alloc_status(27))
  Allocate( transmission % tau_layer             ( coef % nlevels, nchannels ) ,stat= alloc_status(28))
  Allocate( transmission % od_singlelayer        ( coef % nlevels, nchannels ) ,stat= alloc_status(29))
  Allocate( transmission_ad % tau_surf           ( nchannels ) ,stat= alloc_status(30))
  Allocate( transmission_ad % tau_layer          ( coef % nlevels, nchannels ) ,stat= alloc_status(31))
  Allocate( transmission_ad % od_singlelayer     ( coef % nlevels, nchannels ) ,stat= alloc_status(32))

  If( any(alloc_status /= 0) ) then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "mem allocation error")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  End If



  !
  !...do adjoint.....................
  Do  ich =1,nbtout
     freq = frequencies(ich)
     prof = lprofiles(freq)

     ! initialise AD output profile variables
     Do i = 1, nprofiles
        profiles_ad(i) % ozone_Data = .False.  ! no meaning
        profiles_ad(i) % co2_Data   = .False.  ! no meaning
        profiles_ad(i) % clw_Data   = .False.  ! no meaning
        profiles_ad(i) % zenangle   = -1       ! no meaning
        profiles_ad(i) % p(:)   = 0._JPRB ! no AD on pressure levels
        profiles_ad(i) % t(:)   = 0._JPRB ! temperarure
        profiles_ad(i) % o3(:)  = 0._JPRB ! O3 ppmv
        profiles_ad(i) % clw(:) = 0._JPRB ! clw
        profiles_ad(i) % q(:)   = 0._JPRB ! WV
        profiles_ad(i) % s2m % t = 0._JPRB! temperarure
        profiles_ad(i) % s2m % q = 0      ! WV
        profiles_ad(i) % s2m % p = 0._JPRB! pressure
        profiles_ad(i) % s2m % u = 0._JPRB! wind components
        profiles_ad(i) % s2m % v = 0._JPRB! wind components
        profiles_ad(i) % skin % surftype = -1  ! no meaning
        profiles_ad(i) % skin % t        = 0._JPRB  ! on temperarure
        profiles_ad(i) % skin % fastem   = 0._JPRB  ! Fastem
        profiles_ad(i) % ctp       = 0._JPRB  ! cloud top pressure
        profiles_ad(i) % cfraction = 0._JPRB  ! cloud fraction
     End Do

     ! initialise AD output emissivity
     emissivity_ad(:) = 0._JPRB

     ! initialise all radiance increments to 0
     radiancedata_ad % clear(:)      = 0._JPRB
     radiancedata_ad % clear_out(:)      = 0._JPRB
     radiancedata_ad % cloudy(:)     = 0._JPRB
     radiancedata_ad % total(:)      = 0._JPRB
     radiancedata_ad % total_out(:)      = 0._JPRB
     radiancedata_ad % bt(:)         = 0._JPRB
     radiancedata_ad % bt_clear(:)   = 0._JPRB
     radiancedata_ad % out(:)         = 0._JPRB
     radiancedata_ad % out_clear(:)   = 0._JPRB
     radiancedata_ad % upclear(:)    = 0._JPRB
     radiancedata_ad % reflclear(:)  = 0._JPRB
     radiancedata_ad % overcast(:,:) = 0._JPRB
     radiancedata_ad % downcld(:,:)  = 0._JPRB
     transmission_ad % tau_surf(:)   = 0._JPRB
     transmission_ad % tau_layer(:,:)       = 0._JPRB
     transmission_ad % od_singlelayer(:,:)  = 0._JPRB
     radiancedata_ad % out(ich)       = 1._JPRB ! increment channel br. temp by 1K
     switchrad= .true.

     ! use stored input emmisisvity
     emissivity(:) = input_emissivity(:)

     call rttov_ad( &
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
          & switchrad,       &! in
          & calcemis,        &! in
          & emissivity,      &! inout  direct model
          & profiles_ad,     &! inout  adjoint
          & emissivity_ad,   &! inout  adjoint
          & transmission,    &! inout  direct model
          & transmission_ad, &! inout  adjoint input
          & radiancedata,    &! inout  direct model   (input due to pointers alloc)
          & radiancedata_ad ) ! inout  adjoint input  (output if converstion Bt -> rad)

        If( any( rttov_errorstatus == errorstatus_fatal ) ) then
           Do jp = 1, nprofiles
              If( rttov_errorstatus(jp) == errorstatus_fatal ) then
                 Write( errMessage, '( "fatal error in rttov_ad")' )
                 Call Rttov_ErrorReport (rttov_errorstatus(jp), errMessage, NameOfRoutine)
              End If
           End Do
           Stop
        End If
        If( any( rttov_errorstatus /= errorstatus_success ) ) then
          Write( errMessage, '( "warning in rttov_ad")' )           
        End If

     Do  j =1,jpnav    ! yes clw too!
        Do  ii=1,nlev

           Select Case (j)
           Case (1_jpim)
              xktav(ii,j,ich) = profiles_ad(prof) % t(ii)
           Case (2_jpim)
              xktav(ii,j,ich) = profiles_ad(prof) % q(ii)
           Case (3_jpim)
              xktav(ii,j,ich) = profiles_ad(prof) % o3(ii)
           Case (4_jpim)
              xktav(ii,j,ich) = profiles_ad(prof) % clw(ii)
           End Select

           if(prof == 1) then
              ixkav(ii,j,ich) = nint(xktav(ii,j,ich) * facpav(ii,j))
              ixkdav(ii,j,ich) = nint((xkbav(ii,j,ich)-xktav(ii,j,ich)) *&
                     & facpav(ii,j) * facdiff)
           endif
        End Do
     End Do

     !.......now do surface, skin and cloud variables
     Do  j =1,sscvar

        Select Case (j)
        Case (1_jpim)
           ! t 2m
           xktsav(j,ich) = profiles_ad(prof) % s2m % t
        Case (2_jpim)
           ! wv 2m
           xktsav(j,ich) = profiles_ad(prof) % s2m % q
        Case (3_jpim)
           ! surface pressure
           xktsav(j,ich) = profiles_ad(prof) % s2m % p
        Case (4_jpim)
           ! wind speed u component
           xktsav(j,ich) = profiles_ad(prof) % s2m % u
        Case (5_jpim)
           ! wind speed v component
           xktsav(j,ich) = profiles_ad(prof) % s2m % v
        Case (6_jpim)
           ! skin temp
           xktsav(j,ich) = profiles_ad(prof) % skin % t
        Case (7_jpim)
           ! fastem land coef 1
           xktsav(j,ich) = profiles_ad(prof) % skin % fastem(1)
        Case (8_jpim)
           ! fastem land coef 2
           xktsav(j,ich) = profiles_ad(prof) % skin % fastem(2)
        Case (9_jpim)
           ! fastem land coef 3
           xktsav(j,ich) = profiles_ad(prof) % skin % fastem(3)
        Case (10_jpim)
           ! fastem land coef 4
           xktsav(j,ich) = profiles_ad(prof) % skin % fastem(4)
        Case (11_jpim)
           ! fastem land coef 5
           xktsav(j,ich) = profiles_ad(prof) % skin % fastem(5)
        Case (12_jpim)
           ! cloud top pressure
           xktsav(j,ich) = profiles_ad(prof) % ctp
        Case (13_jpim)
           ! cloud fraction
           xktsav(j,ich) = profiles_ad(prof) % cfraction
        End Select
     End Do
     if(prof == 1) then
        ixksav(:,ich)  = nint(xktsav(:,ich) * facsav(:))
        ixkdsav(:,ich) = nint((xkbsav(:,ich)-xktsav(:,ich)) *&
               & facsav(:) * facdiff)
     endif

     xkaem(ich) = 0.0_JPRB
     Do ipol=1, polarisations(frequencies(ich),3)
        xkaem(ich) = xkaem(ich) + emissivity_ad(polarisations(frequencies(ich),1)+ipol-1)
     End Do

     ixdem(ich) = nint(( xkaem(ich) - xkbem(ich)) * facem *1000._JPRB)
     If( coef % fastem_ver >= 1 .and. calcemis(ich) ) Then
     !if(input_emissivity(ich) < 0.0 ) then
        ixdem(ich) = 0._JPRB
     endif

  End Do

  !          ... and print it.
  Write (ioout,*)' '
  Write (ioout,*)'TL - Adjoint difference*10**10.'
  Write (ioout,*)' '
  Do j = 1 , jpnav
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
     Write (ioout,333)jp,(ixdem(jch+joff),jch=1,nchan_out)
  Enddo
  Write (ioout,*)' '

  ! following 3 lines are only for comparison with RTTOV7 reference results
  ! due to adjoint code inside prfinad
  xktav(:,2,:) = xktav(:,2,:) * q_mixratio_to_ppmv
  xktav(:,3,:) = xktav(:,3,:) * o3_mixratio_to_ppmv
  xktsav(2,:)  = xktsav(2,:)  * q_mixratio_to_ppmv
  do prof = 1, nprofiles
     sump = 0._JPRB

     Do jch=1, nbtout
        freq = frequencies(jch)
        If(lprofiles(freq) == prof) Then
           sump = sump + SUM(xktav(:,:,jch))
           sump = sump + SUM(xktsav(:,jch))
           sump = sump + xkaem(jch)
        End If
     End Do

     WRITE (IOOUT,1149)prof,SUMP
1149 FORMAT (1X,'PROFILE NUMBER=',I2,'  AD=',E20.10)
  end do

  !          CHECK CONSISTENCY OF ADJOINT AND TL FOR FASTEM/ISEM
  !
  ! initialise AD output profile variables
  Do i = 1, nprofiles
     profiles_ad(i) % ozone_Data = .False.  ! no meaning
     profiles_ad(i) % co2_Data   = .False.  ! no meaning
     profiles_ad(i) % clw_Data   = .False.  ! no meaning
     profiles_ad(i) % zenangle   = -1       ! no meaning
     profiles_ad(i) % p(:)   = 0._JPRB ! no AD on pressure levels
     profiles_ad(i) % t(:)   = 0._JPRB ! temperarure
     profiles_ad(i) % o3(:)  = 0._JPRB ! O3 ppmv
     profiles_ad(i) % clw(:) = 0._JPRB ! clw
     profiles_ad(i) % q(:)   = 0._JPRB ! WV
     profiles_ad(i) % s2m % t = 0._JPRB! temperarure
     profiles_ad(i) % s2m % q = 0      ! WV
     profiles_ad(i) % s2m % p = 0._JPRB! pressure
     profiles_ad(i) % s2m % u = 0._JPRB! wind components
     profiles_ad(i) % s2m % v = 0._JPRB! wind components
     profiles_ad(i) % skin % surftype = -1  ! no meaning
     profiles_ad(i) % skin % t        = 0._JPRB  ! on temperarure
     profiles_ad(i) % skin % fastem   = 0._JPRB  ! Fastem
     profiles_ad(i) % ctp       = 0._JPRB  ! cloud top pressure
     profiles_ad(i) % cfraction = 0._JPRB  ! cloud fraction
  End Do

  ! initialise AD output emissivity
  emissivity_ad(:) = 0._JPRB

  Do  ich =1,nbtout
     CALL RANDOM_NUMBER(Z)
     radiancedata_ad % out(ich)       = z ! increment channel br. temp
  End Do
  radiancedata_ad % clear(:)      = 0._JPRB
  radiancedata_ad % cloudy(:)     = 0._JPRB
  radiancedata_ad % total(:)      = 0._JPRB
  radiancedata_ad % bt(:)   = 0._JPRB
  radiancedata_ad % bt_clear(:)   = 0._JPRB
  radiancedata_ad % out_clear(:)   = 0._JPRB
  radiancedata_ad % upclear(:)    = 0._JPRB
  radiancedata_ad % reflclear(:)  = 0._JPRB
  radiancedata_ad % overcast(:,:) = 0._JPRB
  radiancedata_ad % downcld(:,:)  = 0._JPRB
  transmission_ad % tau_surf(:)   = 0._JPRB
  transmission_ad % tau_layer(:,:)       = 0._JPRB
  transmission_ad % od_singlelayer(:,:)  = 0._JPRB
  switchrad= .true.


  ! do not use stored input emmisisvity but fwd output
  nocalcemis(:) = .false.

  call rttov_ad( &
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
       & switchrad,       &! in
       & nocalcemis,      &! in
       & emissivity,      &! inout  direct model
       & profiles_ad,     &! inout  adjoint
       & emissivity_ad,   &! inout  adjoint
       & transmission,    &! inout  direct model
       & transmission_ad, &! inout  adjoint input
       & radiancedata,    &! inout  direct model   (input due to pointers alloc)
       & radiancedata_ad ) ! inout  adjoint input  (output if converstion Bt -> rad)

        If( any( rttov_errorstatus == errorstatus_fatal ) ) then
           Do jp = 1, nprofiles
              If( rttov_errorstatus(jp) == errorstatus_fatal ) then
                 Write( errMessage, '( "fatal error in rttov_ad")' )
                 Call Rttov_ErrorReport (rttov_errorstatus(jp), errMessage, NameOfRoutine)
              End If
           End Do
           Stop
        End If
        If( any( rttov_errorstatus /= errorstatus_success ) ) then
          Write( errMessage, '( "warning in rttov_ad")' )           
        End If

  Do j = 1, nprofiles
     profiles_tl(j) % nlevels =  coef % nlevels
     Allocate( profiles_tl(j) % p  ( coef % nlevels ) ,stat= alloc_status(1))
     Allocate( profiles_tl(j) % t  ( coef % nlevels ) ,stat= alloc_status(2))
     Allocate( profiles_tl(j) % q  ( coef % nlevels ) ,stat= alloc_status(3))
     Allocate( profiles_tl(j) % o3 ( coef % nlevels ) ,stat= alloc_status(4))
     Allocate( profiles_tl(j) % clw( coef % nlevels ) ,stat= alloc_status(5))
     If( any(alloc_status /= 0) ) then
        errorstatus = errorstatus_fatal
        Write( errMessage, '( "mem allocation error")' )
        Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
        Return
     End If
  End Do
  Allocate( radiancedata_tl % clear    ( nchannels ) ,stat= alloc_status(1))
  Allocate( radiancedata_tl % cloudy   ( nchannels ) ,stat= alloc_status(2))
  Allocate( radiancedata_tl % total    ( nchannels ) ,stat= alloc_status(3))
  Allocate( radiancedata_tl % bt       ( nchannels ) ,stat= alloc_status(4))
  Allocate( radiancedata_tl % bt_clear ( nchannels ) ,stat= alloc_status(5))
  Allocate( radiancedata_tl % out      ( nchannels ) ,stat= alloc_status(6))
  Allocate( radiancedata_tl % out_clear( nchannels ) ,stat= alloc_status(7))
  Allocate( radiancedata_tl % total_out ( nchannels ) ,stat= alloc_status(8))
  Allocate( radiancedata_tl % clear_out ( nchannels ) ,stat= alloc_status(9))
  Allocate( radiancedata_tl % upclear  ( nchannels ) ,stat= alloc_status(10))
  Allocate( radiancedata_tl % reflclear( nchannels ) ,stat= alloc_status(11))
  Allocate( radiancedata_tl % overcast ( coef % nlevels, nchannels ) ,stat= alloc_status(12))
  Allocate( radiancedata_tl % downcld  ( coef % nlevels, nchannels ) ,stat= alloc_status(13))
  Allocate( transmission_tl % tau_surf            ( nchannels )                    ,stat= alloc_status(14))
  Allocate( transmission_tl % tau_layer           ( coef % nlevels, nchannels )    ,stat= alloc_status(14))
  Allocate( transmission_tl % od_singlelayer      ( coef % nlevels, nchannels )    ,stat= alloc_status(16))
  If( any(alloc_status /= 0) ) then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "mem allocation error")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  End If


  Do j = 1, nprofiles
     profiles_tl(j) % ozone_Data = .False.  ! no meaning
     profiles_tl(j) % co2_Data   = .False.  ! no meaning
     profiles_tl(j) % clw_Data   = .False.  ! no meaning
     profiles_tl(j) % zenangle   = -1  ! no meaning

     ! increments for atmospheric variables
     profiles_tl(j) % p(:)   = 0._JPRB    ! no tl on pressure levels
     do lev = 1, profiles_tl(j) % nlevels
        Call random_number( z )
        profiles_tl(j) % t(lev)   = -1._JPRB * z  ! 1k on temperarure
        Call random_number( z )
        Call random_number( z )
        profiles_tl(j) % o3(lev)  = -0.01_JPRB * z   !  0.01 ppmv
        Call random_number( z )
        profiles_tl(j) % clw(lev) = 0.001_JPRB* z   ! 1g/kg on clw
        Call random_number( z )
        profiles_tl(j) % q(lev) = -0.1_JPRB * profiles(j) % q(lev) * z   ! - 10% on wv
    End Do

    ! increments for air surface variables
    Call random_number( z )
    profiles_tl(j) % s2m % t = -1._JPRB *z  ! 1k on temperarure
    Call random_number( z )
    profiles_tl(j) % s2m % q = -1.6077_JPRB *z !  ppmv
    Call random_number( z )
    profiles_tl(j) % s2m % p =  -10._JPRB *z ! -10 hpa on pressure
    Call random_number( z )
    profiles_tl(j) % s2m % u = 0.1_JPRB  *z ! 0.1 m/s on wind components
    Call random_number( z )
    profiles_tl(j) % s2m % v = 0.1_JPRB  *z ! 0.1 m/s on wind components

    ! increments for skin variables
    profiles_tl(j) % skin % surftype = -1  ! no meaning
    Call random_number( z )
    profiles_tl(j) % skin % t        = -1._JPRB *z  ! 1k on temperarure
    Call random_number( z )
    profiles_tl(j) % skin % fastem(1)   = -0.01_JPRB  *z
    Call random_number( z )
    profiles_tl(j) % skin % fastem(2)   = -0.01_JPRB  *z
    Call random_number( z )
    profiles_tl(j) % skin % fastem(3)   = -0.1_JPRB   *z
    Call random_number( z )
    profiles_tl(j) % skin % fastem(4)   = -0.001_JPRB *z
    Call random_number( z )
    profiles_tl(j) % skin % fastem(5)   = -0.001_JPRB *z

     ! increments for cloud variables
    Call random_number( z )
    profiles_tl(j) % ctp       = -10._JPRB *z  ! -10 hpa on pressure
    Call random_number( z )
    profiles_tl(j) % cfraction = 0.1_JPRB  *z  ! 0.1_JPRB on cloud fraction
  End Do

  ! emissivity
  emissivity_tl(:) = -0.01_JPRB
  Do  ich =1,nchannels
     CALL RANDOM_NUMBER(Z)
     emissivity_tl(ich) = -0.01_JPRB *z
  End Do

  ! do not use stored input emmisisvity but fwd output
  nocalcemis(:) = .false.

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
       & nocalcemis,      &! in
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
                 Write( errMessage, '( "fatal error in rttov_ad")' )
                 Call Rttov_ErrorReport (rttov_errorstatus(jp), errMessage, NameOfRoutine)
              End If
           End Do
           Stop
        End If
        If( any( rttov_errorstatus /= errorstatus_success ) ) then
          Write( errMessage, '( "warning in rttov_ad")' )           
        End If

  do prof = 1, nprofiles
     sumr = 0._JPRB
     sump = 0._JPRB
     Do jch=1, nbtout
        freq = frequencies(jch)
        If(lprofiles(freq) == prof) Then
           sumr = sumr + radiancedata_ad%out(jch) * radiancedata_tl%out(jch)
        End If
     End Do

     sump = sump + SUM(profiles_ad(prof) %   t(:) * profiles_tl(prof) %   t(:))
     sump = sump + SUM(profiles_ad(prof) %   q(:) * profiles_tl(prof) %   q(:))
     sump = sump + SUM(profiles_ad(prof) %  o3(:) * profiles_tl(prof) %  o3(:))
     sump = sump + SUM(profiles_ad(prof) % clw(:) * profiles_tl(prof) % clw(:))

     sump = sump + profiles_ad(prof) % skin % t * profiles_tl(prof) %  skin % t
     sump = sump + SUM(profiles_ad(prof) % skin % fastem(:) * profiles_tl(prof) %  skin % fastem(:))

     sump = sump + profiles_ad(prof) % s2m % t * profiles_tl(prof) %  s2m % t
     sump = sump + profiles_ad(prof) % s2m % q * profiles_tl(prof) %  s2m % q
     sump = sump + profiles_ad(prof) % s2m % p * profiles_tl(prof) %  s2m % p
     sump = sump + profiles_ad(prof) % s2m % u * profiles_tl(prof) %  s2m % u
     sump = sump + profiles_ad(prof) % s2m % v * profiles_tl(prof) %  s2m % v

     sump = sump + profiles_ad(prof) % ctp * profiles_tl(prof) %  ctp
     sump = sump + profiles_ad(prof) % cfraction * profiles_tl(prof) %  cfraction

     Do jch=1, nchannels
        freq = polarisations(jch,2)
        If(lprofiles(freq) == prof) Then
           sump = sump + emissivity_ad(jch) * emissivity_tl(jch)
        End If
     End Do

  eps = 1._JPRB
  do while ((1+eps) > 1._JPRB)
    eps = eps /2._JPRB
  enddo

     Write (ioout, 555) prof, sumr, sump
555  FORMAT (1X,'PROFILE=',I2,' SUMRAD=',E20.10,' SUMPROF=',E20.10)

  End Do


  Do j = 1, nprofiles
     Deallocate( profiles_ad(j) % p   ,stat= alloc_status(1))
     Deallocate( profiles_ad(j) % t   ,stat= alloc_status(2))
     Deallocate( profiles_ad(j) % q   ,stat= alloc_status(3))
     Deallocate( profiles_ad(j) % o3  ,stat= alloc_status(4))
     Deallocate( profiles_ad(j) % clw ,stat= alloc_status(5))
     Deallocate( profiles_tl(j) % p   ,stat= alloc_status(6))
     Deallocate( profiles_tl(j) % t   ,stat= alloc_status(7))
     Deallocate( profiles_tl(j) % q   ,stat= alloc_status(8))
     Deallocate( profiles_tl(j) % o3  ,stat= alloc_status(9))
     Deallocate( profiles_tl(j) % clw ,stat= alloc_status(10))
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
  Deallocate( radiancedata % out       ,stat= alloc_status(6))
  Deallocate( radiancedata % out_clear ,stat= alloc_status(7))
  Deallocate( radiancedata % total_out ,stat= alloc_status(8))
  Deallocate( radiancedata % clear_out ,stat= alloc_status(9))
  Deallocate( radiancedata % upclear   ,stat= alloc_status(10))
  Deallocate( radiancedata % reflclear ,stat= alloc_status(11))
  Deallocate( radiancedata % overcast  ,stat= alloc_status(12))
  Deallocate( radiancedata % downcld   ,stat= alloc_status(13))
  Deallocate( radiancedata % dnclear   ,stat= alloc_status(34))
  Deallocate( radiancedata_ad % clear     ,stat= alloc_status(14))
  Deallocate( radiancedata_ad % cloudy    ,stat= alloc_status(15))
  Deallocate( radiancedata_ad % total     ,stat= alloc_status(16))
  Deallocate( radiancedata_ad % bt        ,stat= alloc_status(17))
  Deallocate( radiancedata_ad % bt_clear  ,stat= alloc_status(18))
  Deallocate( radiancedata_ad % out       ,stat= alloc_status(19))
  Deallocate( radiancedata_ad % out_clear ,stat= alloc_status(20))
  Deallocate( radiancedata_ad % upclear   ,stat= alloc_status(21))
  Deallocate( radiancedata_ad % reflclear ,stat= alloc_status(22))
  Deallocate( radiancedata_ad % overcast  ,stat= alloc_status(23))
  Deallocate( radiancedata_ad % downcld   ,stat= alloc_status(24))
  Deallocate( radiancedata_ad % total_out ,stat= alloc_status(25))
  Deallocate( radiancedata_ad % clear_out ,stat= alloc_status(26))
  Deallocate( radiancedata_tl % clear     ,stat= alloc_status(27))
  Deallocate( radiancedata_tl % cloudy    ,stat= alloc_status(28))
  Deallocate( radiancedata_tl % total     ,stat= alloc_status(29))
  Deallocate( radiancedata_tl % bt        ,stat= alloc_status(30))
  Deallocate( radiancedata_tl % bt_clear  ,stat= alloc_status(31))
  Deallocate( radiancedata_tl % out       ,stat= alloc_status(32))
  Deallocate( radiancedata_tl % out_clear ,stat= alloc_status(33))
  Deallocate( radiancedata_tl % upclear   ,stat= alloc_status(34))
  Deallocate( radiancedata_tl % reflclear ,stat= alloc_status(35))
  Deallocate( radiancedata_tl % overcast  ,stat= alloc_status(36))
  Deallocate( radiancedata_tl % downcld   ,stat= alloc_status(37))
  Deallocate( radiancedata_tl % total_out ,stat= alloc_status(38))
  Deallocate( radiancedata_tl % clear_out ,stat= alloc_status(39))
  DeAllocate( transmission % tau_surf               ,stat= alloc_status(40))
  DeAllocate( transmission % tau_layer              ,stat= alloc_status(41))
  DeAllocate( transmission % od_singlelayer         ,stat= alloc_status(42))
  DeAllocate( transmission_tl % tau_surf            ,stat= alloc_status(43))
  DeAllocate( transmission_tl % tau_layer           ,stat= alloc_status(44))
  DeAllocate( transmission_tl % od_singlelayer      ,stat= alloc_status(45))
  DeAllocate( transmission_ad % tau_surf            ,stat= alloc_status(46))
  DeAllocate( transmission_ad % tau_layer           ,stat= alloc_status(47))
  DeAllocate( transmission_ad % od_singlelayer      ,stat= alloc_status(48))
  If( any(alloc_status /= 0) ) then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "mem deallocation error")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  End If

  Print *, ' Adjoint model test finished'
  Return
222 Format(1x,10f8.2)
333 Format(1x,i3,20i5)
444 Format(1x,10e8.2)
3333 Format(4x,20i5)
4444 Format(1x,10f8.3)


End Subroutine tstrad_ad
