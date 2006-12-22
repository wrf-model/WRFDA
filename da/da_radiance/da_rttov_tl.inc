#ifdef RTTOV
subroutine da_rttov_tl( inst, nchanl, nprofiles, con_vars, aux_vars, &
                      con_vars_tl, aux_vars_tl, tb )

   !---------------------------------------------------------------------------
   !  PURPOSE: interface to the tangent linear subroutine of RTTOV8_5
   !---------------------------------------------------------------------------

   implicit none

#include "rttov_tl.interface"

   integer             ,  intent (in) :: inst, nchanl, nprofiles
   type (con_vars_type),  intent (in) :: con_vars (nprofiles)
   type (con_vars_type),  intent (in) :: con_vars_tl (nprofiles)
   type (aux_vars_type),  intent (in) :: aux_vars (nprofiles)
   type (aux_vars_type),  intent (in) :: aux_vars_tl (nprofiles)
   real                , intent (out) :: tb(nprofiles, nchanl)

   ! local variables
   integer             :: n, nc, ios
   Integer             :: alloc_status(140)

   ! RTTOV input parameters
   integer             :: nfrequencies, nchannels, nbtout
   integer             :: nchan(nprofiles)
   integer , pointer   :: lprofiles(:)
   type(rttov_coef)    :: coef
   type(profile_type)  :: profiles(nprofiles), profiles_tl(nprofiles) 
   logical             :: addcloud
   real , pointer      :: surfem(:)
   integer , pointer   :: channels (:), polarisations(:,:)
   logical , pointer   :: calcemis (:)

   ! RTTOV out parameters
   integer             :: errorstatus(nprofiles)

   ! RTTOV inout parameters
   real    , pointer        :: emissivity (:), emissivity_tl (:)
   type (radiance_type)     :: radiance, radiance_tl
   type (transmission_type) :: transmission, transmission_tl

   call da_trace_entry("da_rttov_tl")

   nchan (:) = nchanl 
   coef = coefs(inst)
   addcloud = .false. 
   alloc_status(:) = 0

   do n = 1, nprofiles
      profiles(n) % nlevels    = con_vars(n) % nlevels
      allocate ( profiles(n)%p(profiles(n) % nlevels), stat=alloc_status(1) )
      allocate ( profiles(n)%t(profiles(n) % nlevels), stat=alloc_status(2) )
      allocate ( profiles(n)%q(profiles(n) % nlevels), stat=alloc_status(3) )
      allocate ( profiles(n)%o3(profiles(n) % nlevels), stat=alloc_status(4) )
      allocate ( profiles(n)%co2(profiles(n) % nlevels), stat=alloc_status(5) )
      allocate ( profiles(n)%clw(profiles(n) % nlevels), stat=alloc_status(6) )
      If ( Any(alloc_status /= 0) ) Then
         WRITE(UNIT=errmsg(1),FMT='(A,I5)') &
            "mem allocation error to for profiles",n
         call da_error(__FILE__,__LINE__,errmsg(1:1))
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

      profiles(n) % skin % surftype   = aux_vars (n) % surftype
      profiles(n) % skin % t          = aux_vars (n) % surft    
      profiles(n) % skin % fastem (:) = 0.  ! aux_vars (n) % fastem (:)

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

      profiles_tl(n) % nlevels    = con_vars_tl(n) % nlevels
      allocate ( profiles_tl(n)%p(profiles_tl(n) % nlevels), stat=alloc_status(1) )
      allocate ( profiles_tl(n)%t(profiles_tl(n) % nlevels), stat=alloc_status(2) )
      allocate ( profiles_tl(n)%q(profiles_tl(n) % nlevels), stat=alloc_status(3) )
      allocate ( profiles_tl(n)%o3(profiles_tl(n) % nlevels), stat=alloc_status(4) )
      allocate ( profiles_tl(n)%co2(profiles_tl(n) % nlevels), stat=alloc_status(5) )
      allocate ( profiles_tl(n)%clw(profiles_tl(n) % nlevels), stat=alloc_status(6) )
      If (Any(alloc_status /= 0)) Then
         WRITE(UNIT=errmsg(1),FMT='(A,I5)') &
            "mem allocation error to for profiles_tl",n
         call da_error(__FILE__,__LINE__,errmsg(1:1))
      End If

      profiles_tl(n) % ozone_data = .false.
      profiles_tl(n) % co2_data   = .false.
      profiles_tl(n) % clw_data   = .false.

      profiles_tl(n) % p(:)       = 0.
      profiles_tl(n) % t(:)       = con_vars_tl(n)%t(:)
      profiles_tl(n) % q(:)       = con_vars_tl(n)%q(:)
      profiles_tl(n) % o3(:)      = 0.0 !con_vars(n)%o3(:)
      profiles_tl(n) % co2(:)     = 0.0 !con_vars(n)%co2(:)
      profiles_tl(n) % clw(:)     = 0.0 !con_vars(n)%clw(:)

      profiles_tl(n) % skin % surftype   = -1
      profiles_tl(n) % skin % t          = 0.  !aux_vars_tl (n) % surft
      profiles_tl(n) % skin % fastem (:) = 0.  ! aux_vars (n) % fastem (:)

      profiles_tl(n) % s2m  % t    = 0. !aux_vars_tl (n) % t2m
      profiles_tl(n) % s2m  % q    = 0. !aux_vars_tl (n) % q2m
      profiles_tl(n) % s2m  % o    = 0. !aux_vars_tl (n) % o3
      profiles_tl(n) % s2m  % p    = con_vars_tl (n) % ps
      profiles_tl(n) % s2m  % u    = 0. !aux_vars_tl (n) % u10
      profiles_tl(n) % s2m  % v    = 0. !aux_vars_tl (n) % v10

      profiles_tl(n) % zenangle    = -1
      profiles_tl(n) % azangle     = -1

      profiles_tl(n) % ctp         = 0. !500.
      profiles_tl(n) % cfraction   = 0.

   end do

#ifdef RTTOV
   call rttov_setupchan(nprofiles, nchan, coef, &             ! in
                     nfrequencies, nchannels, nbtout )       ! out
#endif


   Allocate ( lprofiles(nfrequencies), stat = alloc_status(31) )
   Allocate ( channels (nfrequencies), stat = alloc_status(32) )
   Allocate ( polarisations(nchannels, 3), stat = alloc_status(33) )
   Allocate ( emissivity( nchannels ), stat = alloc_status(34) )
   Allocate ( emissivity_tl( nchannels ), stat = alloc_status(134) )
   Allocate ( calcemis( nchannels ), stat = alloc_status(35) )
   Allocate ( surfem ( nchannels ), stat = alloc_status(36) )

   ! allocate transmittance structure
   Allocate( transmission % tau_surf      ( nchannels )                 ,stat= alloc_status(8))
   Allocate( transmission % tau_layer     ( coef % nlevels, nchannels ) ,stat= alloc_status(9))
   Allocate( transmission % od_singlelayer( coef % nlevels, nchannels  ),stat= alloc_status(10))

   Allocate( transmission_tl % tau_surf      ( nchannels )                 ,stat= alloc_status(108))
   Allocate( transmission_tl % tau_layer     ( coef % nlevels, nchannels ) ,stat= alloc_status(109))
   Allocate( transmission_tl % od_singlelayer( coef % nlevels, nchannels  ),stat= alloc_status(110))


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

   Allocate( radiance % out      ( nbtout ) ,stat= alloc_status(121))
   Allocate( radiance % out_clear( nbtout ) ,stat= alloc_status(122))
   Allocate( radiance % total_out( nbtout ) ,stat= alloc_status(123))
   Allocate( radiance % clear_out( nbtout ) ,stat= alloc_status(124))

   ! allocate radiance results arrays with number of channels
   Allocate( radiance_tl % clear    ( nchannels ) ,stat= alloc_status(111))
   Allocate( radiance_tl % cloudy   ( nchannels ) ,stat= alloc_status(112))
   Allocate( radiance_tl % total    ( nchannels ) ,stat= alloc_status(113))
   Allocate( radiance_tl % bt       ( nchannels ) ,stat= alloc_status(114))
   Allocate( radiance_tl % bt_clear ( nchannels ) ,stat= alloc_status(115))
   Allocate( radiance_tl % upclear  ( nchannels ) ,stat= alloc_status(116))
   Allocate( radiance_tl % dnclear  ( nchannels ) ,stat= alloc_status(117))
   Allocate( radiance_tl % reflclear( nchannels ) ,stat= alloc_status(118))
   Allocate( radiance_tl % overcast ( coef % nlevels, nchannels ) ,stat= alloc_status(119))
   ! allocate the cloudy radiances with full size even
   ! if not used
   Allocate( radiance_tl % downcld  ( coef % nlevels, nchannels ) ,stat= alloc_status(120))

   Allocate( radiance_tl % out      ( nbtout ) ,stat= alloc_status(121))
   Allocate( radiance_tl % out_clear( nbtout ) ,stat= alloc_status(122))
   Allocate( radiance_tl % total_out( nbtout ) ,stat= alloc_status(123))
   Allocate( radiance_tl % clear_out( nbtout ) ,stat= alloc_status(124))

   If (Any(alloc_status /= 0) ) Then
      call da_error(__FILE__,__LINE__, &
         (/"mem allocation error prior to rttov_tl"/))
   End If

   surfem (:) = 0.
#ifdef RTTOV
   call rttov_setupindex(nchan, nprofiles, nfrequencies, &    ! in
                nchannels, nbtout, coef, surfem,  &          ! in
                lprofiles, channels, polarisations,     &    ! out
                emissivity  )                                ! out                       
#endif

   nc = nchannels/nprofiles

   if (coef%id_sensor == 1)  then        ! infrared sensor 
      calcemis (1:nchannels)   = .true.
      emissivity (1:nchannels) = 0.
      emissivity_tl (1:nchannels) = 0.
   else if (coef%id_sensor == 2)  then   ! microwave sensor
      do n = 1, nprofiles
         if ( profiles(n) % skin % surftype == 1) then  ! sea  
            calcemis ((n-1)*nc+1:n*nc) = .true.
            emissivity ((n-1)*nc+1:n*nc) = 0.
            emissivity_tl ((n-1)*nc+1:n*nc) = 0.
         else                                       ! 0:land ; 2:sea-ice
            calcemis ((n-1)*nc+1:n*nc) = .false.
            emissivity ((n-1)*nc+1:n*nc) = 0.9
            emissivity_tl ((n-1)*nc+1:n*nc) = 0.
         end if
      end do
   end if

#ifdef RTTOV
   call rttov_tl( & 
      & errorstatus,   &! out
      & nfrequencies,  &! in
      & nchannels,     &! in
      & nbtout,        &! in
      & nprofiles,     &! in
      & channels,      &! in
      & polarisations, &! in
      & lprofiles,     &! in
      & profiles,      &! in
      & coef,          &! in
      & addcloud,      &! in
      & calcemis,      &! in
      & emissivity,    &! inout
      & profiles_tl,     &! in
      & emissivity_tl,   &! inout
      & transmission,    &! inout
      & transmission_tl, &! inout
      & radiance,    &! inout
      & radiance_tl ) ! inout
#endif

   if ( any(errorstatus(:) /= 0 )) then
       write (errmsg(1),*)  'rttov_direct error code = ', errorstatus(:)
       write (errmsg(2),*)  'nfrequencies            = ', nfrequencies
       write (errmsg(3),*)  'nchannels               = ', nchannels
       write (errmsg(4),*)  'nbtout                  = ', nbtout
       write (errmsg(5),*)  'nprofiles               = ', nprofiles
       write (errmsg(6),*)  'channels                = ', channels
       write (errmsg(7),*)  'polarisations           = ', polarisations
       write (errmsg(8),*)  'lprofiles               = ', lprofiles
       write (errmsg(9),*)  'addcloud                = ', addcloud
       write (errmsg(10),*) 'calcemis                = ', calcemis
       write (errmsg(11),*) 'profiles%s2m            = ', profiles(1)%s2m
       write (errmsg(12),*) 'profiles%skin           = ', profiles(1)%skin
       write (errmsg(13),*) 'profiles%zenangle       = ', profiles(1)%zenangle
       write (errmsg(14),*) 'profiles%azangle        = ', profiles(1)%azangle
       write (errmsg(15),*) 'profiles%p              = ', profiles(1)%p
       write (errmsg(16),*) 'profiles%t              = ', profiles(1)%t
       write (errmsg(17),*) 'profiles%q              = ', profiles(1)%q
       write (errmsg(18),*) 'emissivity              = ', emissivity
       write (errmsg(19),*) 'radiance                = ', radiance%out_clear
       write (errmsg(20),*) 'profiles_tl%s2m         = ', profiles_tl(1)%s2m
       write (errmsg(21),*) 'profiles_tl%skin        = ', profiles_tl(1)%skin
       write (errmsg(22),*) 'profiles_tl%zenangle    = ', profiles_tl(1)%zenangle
       write (errmsg(23),*) 'profiles_tl%azangle     = ', profiles_tl(1)%azangle
       write (errmsg(24),*) 'profiles_tl%p           = ', profiles_tl(1)%p 
       write (errmsg(25),*) 'profiles_tl%t           = ', profiles_tl(1)%t 
       write (errmsg(26),*) 'profiles_tl%q           = ', profiles_tl(1)%q 
       write (errmsg(27),*) 'emissivity_tl           = ', emissivity_tl    
       write (errmsg(28),*) 'radiance_tl             = ', radiance_tl%out_clear
       call da_error(__FILE__,__LINE__,errmsg(1:28)) 
   endif

   nc = nbtout / nprofiles
   do n = 1, nprofiles
      tb(n,1:nc) = radiance_tl % out_clear((n-1)*nc+1:n*nc)
   end do

   deallocate ( lprofiles )
   deallocate ( channels )
   deallocate ( polarisations )
   deallocate ( emissivity )
   deallocate ( emissivity_tl )
   deallocate ( calcemis )
   deallocate ( surfem )
   do n = 1, nprofiles 
      deallocate ( profiles(n)%p )
      deallocate ( profiles(n)%t )
      deallocate ( profiles(n)%q )
      deallocate ( profiles(n)%o3 )
      deallocate ( profiles(n)%co2 )
      deallocate ( profiles(n)%clw )

      deallocate ( profiles_tl(n)%p )
      deallocate ( profiles_tl(n)%t )
      deallocate ( profiles_tl(n)%q )
      deallocate ( profiles_tl(n)%o3 )
      deallocate ( profiles_tl(n)%co2 )
      deallocate ( profiles_tl(n)%clw )
   end do

   ! deallocate transmittance structure
   Deallocate( transmission % tau_surf      ,stat= alloc_status(6))
   Deallocate( transmission % tau_layer     ,stat= alloc_status(7))
   Deallocate( transmission % od_singlelayer,stat= alloc_status(8))

   ! deallocate transmittance structure
   Deallocate( transmission_tl % tau_surf      ,stat= alloc_status(106))
   Deallocate( transmission_tl % tau_layer     ,stat= alloc_status(107))
   Deallocate( transmission_tl % od_singlelayer,stat= alloc_status(108))

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

   Deallocate( radiance_tl % clear    ,stat=alloc_status(109))
   Deallocate( radiance_tl % cloudy   ,stat=alloc_status(110))
   Deallocate( radiance_tl % total    ,stat=alloc_status(111))
   Deallocate( radiance_tl % bt       ,stat=alloc_status(112))
   Deallocate( radiance_tl % bt_clear ,stat=alloc_status(113))
   Deallocate( radiance_tl % upclear  ,stat=alloc_status(114))
   Deallocate( radiance_tl % dnclear  ,stat=alloc_status(115))
   Deallocate( radiance_tl % reflclear,stat=alloc_status(116))
   Deallocate( radiance_tl % overcast ,stat=alloc_status(117))
   Deallocate( radiance_tl % downcld  ,stat=alloc_status(118))
   Deallocate( radiance_tl % out       ,stat= alloc_status(119))
   Deallocate( radiance_tl % out_clear ,stat= alloc_status(120))
   Deallocate( radiance_tl % total_out ,stat= alloc_status(121))
   Deallocate( radiance_tl % clear_out ,stat= alloc_status(122))


   If( Any(alloc_status /= 0) ) Then
     call da_error(__FILE__,__LINE__, &
       (/"mem deallocation error"/))
   End If

   call da_trace_exit("da_rttov_tl")

end subroutine da_rttov_tl
#endif


