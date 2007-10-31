Subroutine tstrad_k(    &
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
       & errorstatus_success ,&
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
#include "rttov_k.interface"

  !subroutine arguments:
  Integer(Kind=jpim),  Intent(in)    :: nfrequencies
  Integer(Kind=jpim),  Intent(in)    :: nchannels
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
  Character (len=10) :: NameOfRoutine = 'tstrad_k  '

  ! forward model outputs
  logical :: switchrad
  Type(transmission_Type)   :: transmission
  Type(radiance_Type)   :: radiancedata
  Real(Kind=jprb)   :: emissivity(nchannels)


  ! AD variables for rttov_k calls
  Type(profile_Type)  :: profiles_k(nchannels)
  Type(transmission_Type)   :: transmission_k
  Real(Kind=jprb)  :: emissivity_k(nchannels)


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
  Real(Kind=jprb) :: facovu(coef%nlevels)
  Real(Kind=jprb) :: facovd(coef%nlevels)
  Real(Kind=jprb) :: facem = 1._JPRB

  Real(Kind=jprb) :: facsav(sscvar) =&
        & (/10000._JPRB,0.1_JPRB,10000._JPRB,10000._JPRB,10000._JPRB,       &! 2m
        & 10000._JPRB,100.0_JPRB,100.0_JPRB,100.0_JPRB,100.0_JPRB,100.0_JPRB,  &! Skin
        & 10000._JPRB,100._JPRB/)                           ! cloud

  Real(Kind=jprb) :: facdiff = 1.e+10_JPRB
  ! Real :: facdiff = 1.

  Real(Kind=jprb), Parameter :: q_mixratio_to_ppmv  = 1.60771704e+6_JPRB
  Real(Kind=jprb), Parameter :: o3_mixratio_to_ppmv = 6.03504e+5_JPRB

  Integer(Kind=jpim) :: ioout = 2
  Integer(Kind=jpim) :: ich, jch
  Integer(Kind=jpim) :: j, i, ii, jp, joff, freq, ipol
  Integer(Kind=jpim) :: nchan_out

  Character (len=30) :: title(4) = &
        & (/' lev       temperature        ', &
          & ' lev       water vapour       ',   &
          & ' lev       ozone              ',   &
          & ' lev       liquid water       '/)

  Integer(Kind=jpim) :: alloc_status(30)
  Integer(Kind=jpim) :: rttov_errorstatus(nprofiles)

  !- End of header --------------------------------------------------------

  errorstatus     = 0
  alloc_status(:) = 0

  nchan_out = nbtout/nprofiles
  nlev = coef % nlevels

  ! coefficients for atmospheric variables
  facpav(:,1) = 10000._JPRB
  facpav(:,2) = 0.1_JPRB
  facpav(:,3) = 0.001_JPRB
  facpav(:,4) = 0.1_JPRB
  facovu(:)   = 10000._JPRB
  facovd(:)   = 100000._JPRB

  ! coefficients compatibility with RTTOV7
  facpav(:,2) = facpav(:,2) *  q_mixratio_to_ppmv
  facpav(:,3) = facpav(:,3) * o3_mixratio_to_ppmv
  facsav(2)   = facsav(2)   *  q_mixratio_to_ppmv

  ! allocate and initialise the reference tl increments
  Do j = 1, nbtout
     profiles_k(j) % nlevels =  coef % nlevels
     Allocate( profiles_k(j) % p  ( coef % nlevels ) ,stat= alloc_status(1))
     Allocate( profiles_k(j) % t  ( coef % nlevels ) ,stat= alloc_status(2))
     Allocate( profiles_k(j) % q  ( coef % nlevels ) ,stat= alloc_status(3))
     Allocate( profiles_k(j) % o3 ( coef % nlevels ) ,stat= alloc_status(4))
     Allocate( profiles_k(j) % clw( coef % nlevels ) ,stat= alloc_status(5))
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
  Allocate( radiancedata % out      ( nchannels ) ,stat= alloc_status(4))
  Allocate( radiancedata % out_clear( nchannels ) ,stat= alloc_status(5))
  Allocate( radiancedata % upclear  ( nchannels ) ,stat= alloc_status(6))
  Allocate( radiancedata % dnclear  ( nchannels ) ,stat= alloc_status(18))
  Allocate( radiancedata % reflclear( nchannels ) ,stat= alloc_status(7))
  Allocate( radiancedata % overcast ( coef % nlevels, nchannels ) ,stat= alloc_status(8))
  Allocate( radiancedata % downcld  ( coef % nlevels, nchannels ) ,stat= alloc_status(9))
  Allocate( radiancedata % total_out ( nchannels ) ,stat= alloc_status(10))
  Allocate( radiancedata % clear_out ( nchannels ) ,stat= alloc_status(11))
  ! allocate transmission structures
  Allocate( transmission % tau_surf          ( nchannels )                  ,stat= alloc_status(12))
  Allocate( transmission % tau_layer         ( coef % nlevels, nchannels )  ,stat= alloc_status(13))
  Allocate( transmission % od_singlelayer    ( coef % nlevels, nchannels )  ,stat= alloc_status(14))
  Allocate( transmission_k % tau_surf        ( nchannels )                  ,stat= alloc_status(15))
  Allocate( transmission_k % tau_layer       ( coef % nlevels, nchannels )  ,stat= alloc_status(16))
  Allocate( transmission_k % od_singlelayer  ( coef % nlevels, nchannels )  ,stat= alloc_status(17))
  If( any(alloc_status /= 0) ) then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "mem allocation error")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  End If



  !
  !...do K.....................


  ! use stored input emmisisvity
  emissivity(:) = input_emissivity(:)

  Do i = 1, nbtout
     profiles_k(i) % ozone_Data = .False.  ! no meaning
     profiles_k(i) % co2_Data   = .False.  ! no meaning
     profiles_k(i) % clw_Data   = .False.  ! no meaning
     profiles_k(i) % zenangle   = -1       ! no meaning

     ! increments for atmospheric variables
     profiles_k(i) % p(:)   = 0._JPRB ! no AD on pressure levels
     profiles_k(i) % t(:)   = 0._JPRB ! temperarure
     profiles_k(i) % o3(:)  = 0._JPRB ! O3 ppmv
     profiles_k(i) % clw(:) = 0._JPRB ! clw
     profiles_k(i) % q(:)   = 0._JPRB ! WV

     ! increments for air surface variables
     profiles_k(i) % s2m % t = 0._JPRB !  temperarure
     profiles_k(i) % s2m % q = 0       !  WV
     profiles_k(i) % s2m % p = 0._JPRB !  pressure
     profiles_k(i) % s2m % u = 0._JPRB !  wind components
     profiles_k(i) % s2m % v = 0._JPRB !  wind components

     ! increments for skin variables
     profiles_k(i) % skin % surftype = -1  ! no meaning
     profiles_k(i) % skin % t        = 0._JPRB  ! on temperarure
     profiles_k(i) % skin % fastem   = 0._JPRB

     ! increments for cloud variables
     profiles_k(i) % ctp       = 0._JPRB  ! pressure
     profiles_k(i) % cfraction = 0._JPRB  ! cloud fraction
  End Do

  emissivity_k(:) = 0._JPRB
  transmission_k % tau_surf(:)   = 0._JPRB
  transmission_k % tau_layer(:,:)       = 0._JPRB
  transmission_k % od_singlelayer(:,:)  = 0._JPRB

  switchrad= .true.
  call rttov_k( &
       & rttov_errorstatus,  &! out
       & nfrequencies,       &! in
       & nchannels,          &! in
       & nbtout,             &! in
       & nprofiles,          &! in
       & channels,           &! in
       & polarisations,      &! in
       & lprofiles,          &! in
       & profiles,           &! in
       & coef,               &! in
       & addcloud,           &! in
       & switchrad,          &! in
       & calcemis,           &! in
       & emissivity,         &! inout  direct model
       & profiles_k,         &! inout  adjoint
       & emissivity_k,       &! inout  adjoint
       & transmission,       &! inout  adjoint
       & transmission_k,     &! inout  adjoint
       & radiancedata)        ! inout  direct model
                              !   (input due to pointers alloc)

        If( any( rttov_errorstatus == errorstatus_fatal ) ) then
           Do jp = 1, nprofiles
              If( rttov_errorstatus(jp) == errorstatus_fatal ) then
                 Write( errMessage, '( "fatal error in rttov_k")' )
                 Call Rttov_ErrorReport (rttov_errorstatus(jp), errMessage, NameOfRoutine)
              End If
           End Do
           Stop
        End If
        If( any( rttov_errorstatus /= errorstatus_success ) ) then
          Write( errMessage, '( "warning in rttov_k")' )           
        End If


  Do  ich =1, nbtout
     freq = frequencies(Ich)
     Do  j =1,jpnav    ! yes clw too!
        !Write(0,*) 'adjoint de la variable atmosphere ',j
        ii = 1
              xktav(ii,j,ich) = profiles_k(ich) % t(ii)
        Do  ii=1,nlev

           Select Case (j)
           Case (1_jpim)
              xktav(ii,j,ich) = profiles_k(ich) % t(ii)
              !Write(0,*) 'T ',profiles_k(prof) % t(ii)
           Case (2_jpim)
              xktav(ii,j,ich) = profiles_k(ich) % q(ii)
              !Write(0,*) 'Q ',profiles_k(prof) % q(ii)
           Case (3_jpim)
              xktav(ii,j,ich) = profiles_k(ich) % o3(ii)
           Case (4_jpim)
              xktav(ii,j,ich) = profiles_k(ich) % clw(ii)
           End Select

           if(lprofiles(freq) == 1) then
              !Write(0,*) 'level xkb, xkt ',ii,xkbav(ii,j,ich),xktav(ii,j,ich)

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
           xktsav(j,ich) = profiles_k(ich) % s2m % t
        Case (2_jpim)
           ! wv 2m
           xktsav(j,ich) = profiles_k(ich) % s2m % q
        Case (3_jpim)
           ! surface pressure
           xktsav(j,ich) = profiles_k(ich) % s2m % p
        Case (4_jpim)
           ! wind speed u component
           xktsav(j,ich) = profiles_k(ich) % s2m % u
        Case (5_jpim)
           ! wind speed v component
           xktsav(j,ich) = profiles_k(ich) % s2m % v
        Case (6_jpim)
           ! skin temp
           xktsav(j,ich) = profiles_k(ich) % skin % t
        Case (7_jpim)
           ! fastem land coef 1
           xktsav(j,ich) = profiles_k(ich) % skin % fastem(1)
        Case (8_jpim)
           ! fastem land coef 2
           xktsav(j,ich) = profiles_k(ich) % skin % fastem(2)
        Case (9_jpim)
           ! fastem land coef 3
           xktsav(j,ich) = profiles_k(ich) % skin % fastem(3)
        Case (10_jpim)
           ! fastem land coef 4
           xktsav(j,ich) = profiles_k(ich) % skin % fastem(4)
        Case (11_jpim)
           ! fastem land coef 5
           xktsav(j,ich) = profiles_k(ich) % skin % fastem(5)
        Case (12_jpim)
           ! cloud top pressure
           xktsav(j,ich) = profiles_k(ich) % ctp
        Case (13_jpim)
           ! cloud fraction
           xktsav(j,ich) = profiles_k(ich) % cfraction
        End Select
     End Do

     if(lprofiles(freq) == 1) then
       ixksav(:,ich)  = nint(xktsav(:,ich) * facsav(:))
       ixkdsav(:,ich) = nint((xkbsav(:,ich)-xktsav(:,ich)) *&
       &  facsav(:) * facdiff)
     endif

     xkaem(ich) = 0.0_JPRB
     Do ipol=1, polarisations(frequencies(ich),3)
        xkaem(ich) = xkaem(ich) + emissivity_k(polarisations(frequencies(ich),1)+ipol-1)
     End Do

     ixdem(ich) = nint(( xkaem(ich) - xkbem(ich)) * facem *1000._JPRB)

     If( coef % fastem_ver >= 1 .and. calcemis(ich) ) Then
     !if(input_emissivity(ich) < 0.0_JPRB ) then
        ixdem(ich) = 0._JPRB
     endif

  end do

  !          ... and print it.
  Write (ioout,*)' '
  Write (ioout,*)'TL - K difference*10**10.'
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

  write(ioout,*)' calculated br. temps in rttovk'
  write(ioout,222) (radiancedata % out (1:nchan_out))
  write(ioout,*)' '
  write(ioout,*)' calculated radiances in rttovk'
  write(ioout,222) (radiancedata % total_out (1:nchan_out))
  write(ioout,*)' '
  write (ioout,*)' '


  Do j = 1, nbtout
     Deallocate( profiles_k(j) % p   ,stat= alloc_status(1))
     Deallocate( profiles_k(j) % t   ,stat= alloc_status(2))
     Deallocate( profiles_k(j) % q   ,stat= alloc_status(3))
     Deallocate( profiles_k(j) % o3  ,stat= alloc_status(4))
     Deallocate( profiles_k(j) % clw ,stat= alloc_status(5))
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
  Deallocate( radiancedata % out       ,stat= alloc_status(4))
  Deallocate( radiancedata % out_clear ,stat= alloc_status(5))
  Deallocate( radiancedata % upclear   ,stat= alloc_status(6))
  Deallocate( radiancedata % dnclear   ,stat= alloc_status(18))
  Deallocate( radiancedata % reflclear ,stat= alloc_status(7))
  Deallocate( radiancedata % overcast  ,stat= alloc_status(8))
  Deallocate( radiancedata % downcld   ,stat= alloc_status(9))
  Deallocate( radiancedata % total_out ,stat= alloc_status(10))
  Deallocate( radiancedata % clear_out ,stat= alloc_status(11))
  ! deallocate transmission structures
  Deallocate( transmission   % tau_surf      ,stat= alloc_status(12))
  Deallocate( transmission   % tau_layer     ,stat= alloc_status(13))
  Deallocate( transmission   % od_singlelayer,stat= alloc_status(14))
  Deallocate( transmission_k % tau_surf      ,stat= alloc_status(15))
  Deallocate( transmission_k % tau_layer     ,stat= alloc_status(16))
  Deallocate( transmission_k % od_singlelayer,stat= alloc_status(17))
  If( any(alloc_status /= 0) ) then
     errorstatus = errorstatus_fatal
     Write( errMessage, '( "mem deallocation error")' )
     Call Rttov_ErrorReport (errorstatus, errMessage, NameOfRoutine)
     Return
  End If

  Print *, ' K matrix test finished'
  Return
222 Format(1x,10f8.2)
333 Format(1x,i3,20i5)
444 Format(1x,10e8.2)
3333 Format(4x,20i5)
4444 Format(1x,10f8.3)


End Subroutine tstrad_k
