!
subroutine da_rtm_direct( inst, isflg, nchanl, nprofiles, &
                          nfrequencies, nchannels, nbtout , &
                          con_vars, aux_vars, &
                          tb, calcemis_out, emisv, emish, emissivity_out )
!------------------------------------------------------------------------------
!  PURPOSE: interface to the foreward subroutine of RTTOV8_5.
!
!  METHOD:
!
!  HISTORY: 28/07/2005 - Creation            Zhiquan Liu
!
!------------------------------------------------------------------------------

 implicit none

#ifdef RTTOV
#include "rttov_direct.interface"
#endif

 integer             ,  intent (in) :: inst, isflg, nchanl, nprofiles
 integer             ,  intent (in) :: nfrequencies, nchannels, nbtout
 type (con_vars_type),  intent (in) :: con_vars (nprofiles)
 type (aux_vars_type),  intent (in) :: aux_vars (nprofiles)
 real                , intent (out) :: tb(nprofiles, nchanl)
 logical             , intent (out) :: calcemis_out(nprofiles*nchanl)
 real             ,  intent    (in) :: emisv(nprofiles*nchanl)
 real             ,  intent    (in) :: emish(nprofiles*nchanl)
 real             ,  intent   (out) :: emissivity_out(nchannels)
 
! local variables
 integer             :: n, nc, ios, ich, i, ichannel, pol_id
 Integer             :: alloc_status(40), emis_scheme

! RTTOV input parameters
! integer             :: nfrequencies, nchannels, nbtout
 integer             :: nchan(nprofiles)
 integer , pointer   :: lprofiles(:)
 type(rttov_coef)    :: coef
 type(profile_type)  :: profiles(nprofiles) 
 logical             :: addcloud
 real ,    pointer   :: surfem(:)
 integer , pointer   :: channels (:), polarisations(:,:)
 logical , pointer   :: calcemis (:)

! RTTOV out parameters
 integer             :: errorstatus(nprofiles)

! RTTOV inout parameters
 real    , pointer        :: emissivity (:)
 type (radiance_type)     :: radiance
 type (transmission_type) :: transmission

!------------------------

 nchan (:) = nchanl 
 coef = coefs(inst)
 addcloud = .false. 
 alloc_status (:) = 0

 do n = 1, nprofiles
    profiles(n) % nlevels    = con_vars(n) % nlevels
    allocate ( profiles(n)%p(profiles(n) % nlevels), stat=alloc_status(1) )
    allocate ( profiles(n)%t(profiles(n) % nlevels), stat=alloc_status(2) )
    allocate ( profiles(n)%q(profiles(n) % nlevels), stat=alloc_status(3) )
    allocate ( profiles(n)%o3(profiles(n) % nlevels), stat=alloc_status(4) )
    allocate ( profiles(n)%co2(profiles(n) % nlevels), stat=alloc_status(5) )
    allocate ( profiles(n)%clw(profiles(n) % nlevels), stat=alloc_status(6) )

   If( Any(alloc_status /= 0) ) Then
     WRITE(UNIT=errmsg(1),FMT='(A,I5)') &
       "mem allocation error to profile",n
     call da_error(__FILE__,__LINE__,errmsg(1))
   End If

    profiles(n) % ozone_data = .false.
    profiles(n) % co2_data   = .false.
    profiles(n) % clw_data   = .false. 

    profiles(n) % p(:)       = coef%ref_prfl_p(:)
    profiles(n) % t(:)       = con_vars(n)%t(:)
    profiles(n) % q(:)       = con_vars(n)%q(:)
    profiles(n) % o3(:)      = 0.0 !con_vars(n)%o3(:)
    profiles(n) % co2(:)     = 0.0 !con_vars(n)%co2(:)
    profiles(n) % clw(:)     = 0.0 !con_vars(n)%clw(:)

 if (isflg == 0 .or. isflg == 4) then  ! sea
    profiles(n) % skin % surftype   = 1 ! aux_vars (n) % surftype
!    profiles(n) % skin % fastem (:) = 0.
 else if (isflg == 1 .or. isflg == 5) then  ! sea-ice with snow
    profiles(n) % skin % surftype   = 2
!    profiles(n) % skin % fastem (1) = 2.2
!    profiles(n) % skin % fastem (2) = 3.7
!    profiles(n) % skin % fastem (3) = 122.0
!    profiles(n) % skin % fastem (4) = 0.0
!    profiles(n) % skin % fastem (5) = 0.15
 else
    profiles(n) % skin % surftype   = 0 ! land (Deep dry snow)
!    profiles(n) % skin % fastem (1) = 3.0
!    profiles(n) % skin % fastem (2) = 24.0
!    profiles(n) % skin % fastem (3) = 60.0
!    profiles(n) % skin % fastem (4) = 0.1
!    profiles(n) % skin % fastem (5) = 0.15
 end if
!    profiles(n) % skin % surftype   = aux_vars (n) % surftype   
    profiles(n) % skin % t          = aux_vars (n) % surft    
    profiles(n) % skin % fastem (:) = 0. ! aux_vars (n) % fastem (:)

    profiles(n) % s2m  % t    = aux_vars (n) % t2m
    profiles(n) % s2m  % q    = aux_vars (n) % q2m
    profiles(n) % s2m  % o    = 0.0 !aux_vars (n) % o3
    profiles(n) % s2m  % p    = con_vars (n) % ps
    profiles(n) % s2m  % u    = aux_vars (n) % u10
    profiles(n) % s2m  % v    = aux_vars (n) % v10

    profiles(n) % zenangle    = aux_vars (n) % satzen
    profiles(n) % azangle     = aux_vars (n) % satazi

    profiles(n) % ctp         = 500.
    profiles(n) % cfraction   = 0.
 end do

!#ifdef RTTOV
! call rttov_setupchan(nprofiles, nchan, coef, &             ! in
!                    nfrequencies, nchannels, nbtout )       ! out
!#endif


  Allocate ( lprofiles(nfrequencies), stat = alloc_status(31) )
  Allocate ( channels (nfrequencies), stat = alloc_status(32) )
  Allocate ( polarisations(nchannels, 3), stat = alloc_status(33) )
  Allocate ( emissivity( nchannels ), stat = alloc_status(34) )
  Allocate ( calcemis( nchannels ), stat = alloc_status(35) )
  Allocate ( surfem( nchannels ), stat = alloc_status(36) )

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
     call da_error(__FILE__,__LINE__, &
       "mem allocation error prior to rttov_direct")
  End If

  surfem (:) = 0.
#ifdef RTTOV
 call rttov_setupindex(nchan, nprofiles, nfrequencies, &    ! in
               nchannels, nbtout, coef, surfem,  &          ! in
               lprofiles, channels, polarisations,     &    ! out
               emissivity  )                                ! out                       
#endif

!  surface emissivity scheme
!----------------------------------------------------------

!  For Infrared sensors
!-----------------------------
if (coef%id_sensor == 1 .or. coef%id_sensor == 3)  then 
 if (profiles(1) % skin % surftype == 1) then  
   calcemis (1:nchannels)   = .true.           ! using ISSEM over sea
   emissivity (1:nchannels) = 0.               
 else if (profiles(1) % skin % surftype == 2) then
   calcemis (1:nchannels)   = .false.          
   emissivity (1:nchannels) = 0.98             ! over sea-ice
 else                                        
   if(isflg == 2 .or. isflg == 6) then
     calcemis (1:nchannels)   = .false.
     emissivity (1:nchannels) = 0.97           ! land without snow
   end if
   if(isflg == 3 .or. isflg == 7) then
     calcemis (1:nchannels)   = .false.
     emissivity (1:nchannels) = 1.0            ! land with snow
   end if
 end if
end if

!  For Microwave sensors
!-----------------------------
if (coef%id_sensor == 2)  then
!
!  1.0 over sea
!
!  print *, 'mw_emis_sea=', mw_emis_sea
  if ( profiles(1) % skin % surftype == 1) then 
    if (mw_emis_sea == 0) then
       calcemis  (1:nchannels) = .true.
       emissivity(1:nchannels) = -1.   ! RTTOV fastem-2
    else                              ! Weng's model
       calcemis  (1:nchannels) = .false.
       do i = 1, nfrequencies   !  loop for channels
          ichannel = polarisations(i,1)  ! position of first polar at chanl i
          pol_id = coef%fastem_polar(i) + 1
          if (polarisations(i,3) == 2) then ! number of polar at chanl i
             emissivity(ichannel) = emisv(i)
             emissivity(ichannel+1) = emish(i)
          else if (polarisations(i,3) == 4) then
             emissivity(ichannel)   = emisv(i)
             emissivity(ichannel+1) = emish(i)
             emissivity(ichannel+2) = 0.
             emissivity(ichannel+3) = 0.
          else   ! == 1 V or H polar
             if (pol_id == 4) emissivity(ichannel)   = emisv(i)
             if (pol_id == 5) emissivity(ichannel)   = emish(i)
          end if
       end do
    end if
!
!  1.0 over sea-ice/snow/land
!
  else 
       calcemis  (1:nchannels) = .false.
       do i = 1, nfrequencies   !  loop for channels
          ichannel = polarisations(i,1)  ! position of first polar at chanl i
          pol_id = coef%fastem_polar(i) + 1
          if (polarisations(i,3) == 2) then ! number of polar at chanl i
             emissivity(ichannel) = emisv(i)
             emissivity(ichannel+1) = emish(i)
          else if (polarisations(i,3) == 4) then
             emissivity(ichannel)   = emisv(i)
             emissivity(ichannel+1) = emish(i)
             emissivity(ichannel+2) = 0.
             emissivity(ichannel+3) = 0.
          else   ! == 1 V or H polar
             if (pol_id == 4) emissivity(ichannel)   = emisv(i)
             if (pol_id == 5) emissivity(ichannel)   = emish(i)
          end if
       end do
  end if
end if

!-----------------------------------
!  calling RTTOV foreward model
!----------------------------------
#ifdef RTTOV
 call rttov_direct( &
     & errorstatus,     &! out
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
     & transmission,    &! inout
     & radiance )        ! inout
#endif

 if ( any(errorstatus(:) /= 0 )) then
     !WRITE (UNIT=stdout,FMT=*) 'rttov_direct error code = ', errorstatus(:)
     !WRITE (UNIT=stdout,FMT=*) 'nfrequencies = ', nfrequencies
     !WRITE (UNIT=stdout,FMT=*) 'nchannels    = ', nchannels
     !WRITE (UNIT=stdout,FMT=*) 'nbtout       = ', nbtout
     !WRITE (UNIT=stdout,FMT=*) 'nprofiles    = ', nprofiles
     !WRITE (UNIT=stdout,FMT=*) 'channels     = ', channels
     !WRITE (UNIT=stdout,FMT=*) 'polarisations= ', polarisations
     !WRITE (UNIT=stdout,FMT=*) 'lprofiles    = ', lprofiles
     !WRITE (UNIT=stdout,FMT=*) 'profiles%s2m   = ', profiles(1)%s2m
     !WRITE (UNIT=stdout,FMT=*) 'profiles%skin   = ', profiles(1)%skin
     write(UNIT=stdout,FMT='(6f12.2)') &
       profiles(1)%s2m%p,profiles(1)%skin%t,profiles(1)%s2m%t, &
       profiles(1)%s2m%q,profiles(1)%s2m%u,profiles(1)%s2m%v
     !WRITE (UNIT=stdout,FMT=*) 'profiles%zenangle = ', profiles(1)%zenangle
     !WRITE (UNIT=stdout,FMT=*) 'profiles%azangle = ', profiles(1)%azangle
     !WRITE (UNIT=stdout,FMT=*) 'profiles%p   = ', profiles(1)%p
     !WRITE (UNIT=stdout,FMT=*) 'profiles%t   = ', profiles(1)%t
     !WRITE (UNIT=stdout,FMT=*) 'profiles%q   = ', profiles(1)%q
     !do i=coef%nlevels,1,-1
     !  write(UNIT=stdout,FMT='(i4,3f12.2)') i, profiles(1)%p(i),profiles(1)%t(i),profiles(1)%q(i)
     !end do
     !WRITE (UNIT=stdout,FMT=*) 'addcloud     = ', addcloud
     !WRITE (UNIT=stdout,FMT=*) 'calcemis     = ', calcemis
     !WRITE (UNIT=stdout,FMT=*) 'emissivity   = ', emissivity
     !WRITE (UNIT=stdout,FMT=*) 'radiance = ', radiance%out_clear
     !stop ' in da_rtm_direct '
 endif
 
  nc = nbtout / nprofiles
 do n = 1, nprofiles
  tb(n,1:nc) = radiance % out_clear((n-1)*nc+1:n*nc)
 end do
  calcemis_out  (:) = calcemis(:)
  emissivity_out(:) = emissivity(:)

    deallocate ( lprofiles )
    deallocate ( channels )
    deallocate ( polarisations )
    deallocate ( emissivity )
    deallocate ( calcemis )
    deallocate ( surfem )
   do n = 1, nprofiles 
    deallocate ( profiles(n)%p )
    deallocate ( profiles(n)%t )
    deallocate ( profiles(n)%q )
    deallocate ( profiles(n)%o3 )
    deallocate ( profiles(n)%co2 )
    deallocate ( profiles(n)%clw )
   end do

  ! deallocate transmittance structure
  Deallocate( transmission % tau_surf      ,stat= alloc_status(6))
  Deallocate( transmission % tau_layer     ,stat= alloc_status(7))
  Deallocate( transmission % od_singlelayer,stat= alloc_status(8))

  ! deallocate radiance results arrays with number of channels
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

  If( Any(alloc_status /= 0) ) Then
     call da_error(__FILE__,__LINE__, &
       "mem deallocation error")
  End If

 return

end subroutine da_rtm_direct
