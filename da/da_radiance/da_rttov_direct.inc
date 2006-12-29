#ifdef RTTOV
subroutine da_rttov_direct( inst, isflg, nchanl, nprofiles, &
                          con_vars, aux_vars, &
                          tb, emisv, emish, emissivity_out )

   !---------------------------------------------------------------------------
   !  Purpose: interface to the forward subroutine of RTTOV8_5
   !---------------------------------------------------------------------------

   implicit none

#include "rttov_direct.interface"

   integer             ,  intent (in) :: inst, isflg(nprofiles), nchanl, nprofiles
   type (con_vars_type),  intent (in) :: con_vars (nprofiles)
   type (aux_vars_type),  intent (in) :: aux_vars (nprofiles)
   real                , intent (out) :: tb(nchanl,nprofiles)
   real             ,  intent    (in) :: emisv(nprofiles*nchanl)
   real             ,  intent    (in) :: emish(nprofiles*nchanl)
   real             ,  intent   (out) :: emissivity_out(:,:)

   ! local variables
   integer             :: n, nc, ios, ich, i, ichannel, pol_id
   Integer             :: emis_scheme

   ! RTTOV input parameters
   integer             :: nfrequencies, nchannels, nbtout
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

   call da_trace_entry("da_rttov_direct")

   nchan (:) = nchanl 
   coef = coefs(inst)
   addcloud = .false. 

   do n = 1, nprofiles
      profiles(n) % nlevels    = con_vars(n) % nlevels
      allocate (profiles(n)%p(profiles(n) % nlevels))
      allocate (profiles(n)%t(profiles(n) % nlevels))
      allocate (profiles(n)%q(profiles(n) % nlevels))
      allocate (profiles(n)%o3(profiles(n) % nlevels))
      allocate (profiles(n)%co2(profiles(n) % nlevels))
      allocate (profiles(n)%clw(profiles(n) % nlevels))

      profiles(n) % ozone_data = .false.
      profiles(n) % co2_data   = .false.
      profiles(n) % clw_data   = .false. 

      profiles(n) % p(:)       = coef%ref_prfl_p(:)
      profiles(n) % t(:)       = con_vars(n)%t(:)
      profiles(n) % q(:)       = con_vars(n)%q(:)
      profiles(n) % o3(:)      = 0.0 !con_vars(n)%o3(:)
      profiles(n) % co2(:)     = 0.0 !con_vars(n)%co2(:)
      profiles(n) % clw(:)     = 0.0 !con_vars(n)%clw(:)

      if (isflg(n) == 0 .or. isflg(n) == 4) then  ! sea
         profiles(n) % skin % surftype   = 1 ! aux_vars (n) % surftype
         ! profiles(n) % skin % fastem (:) = 0.
      else if (isflg(n) == 1 .or. isflg(n) == 5) then  ! sea-ice with snow
         profiles(n) % skin % surftype   = 2
         ! profiles(n) % skin % fastem (1) = 2.2
         ! profiles(n) % skin % fastem (2) = 3.7
         ! profiles(n) % skin % fastem (3) = 122.0
         ! profiles(n) % skin % fastem (4) = 0.0
         ! profiles(n) % skin % fastem (5) = 0.15
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

call da_oi(__FILE__,__LINE__)

#ifdef RTTOV
   call rttov_setupchan(nprofiles, nchan, coef, &             ! in
                       nfrequencies, nchannels, nbtout )       ! out
#endif

write (0,*) "nprofiles",nprofiles
write (0,*) "nfrequencies, nchannels, nbtout",nfrequencies, nchannels, nbtout

   allocate (lprofiles(nfrequencies))
   allocate (channels (nfrequencies))
   allocate (polarisations(nchannels, 3))
   allocate (emissivity( nchannels))
   allocate (calcemis( nchannels))
   allocate (surfem( nchannels))
call da_oi(__FILE__,__LINE__)

   ! allocate transmittance structure
   allocate (transmission % tau_surf      (nchannels))
   allocate (transmission % tau_layer     (coef % nlevels, nchannels))
   allocate (transmission % od_singlelayer(coef % nlevels, nchannels))

   ! allocate radiance results arrays with number of channels
   allocate (radiance % clear    (nchannels))
   allocate (radiance % cloudy   (nchannels))
   allocate (radiance % total    (nchannels))
   allocate (radiance % bt       (nchannels))
   allocate (radiance % bt_clear (nchannels))
   allocate (radiance % upclear  (nchannels))
   allocate (radiance % dnclear  (nchannels))
   allocate (radiance % reflclear(nchannels))
   allocate (radiance % overcast (coef % nlevels, nchannels))
   ! allocate the cloudy radiances with full size even
   ! if not used
   allocate (radiance % downcld  (coef % nlevels, nchannels))

   allocate (radiance % out      (nbtout))
   allocate (radiance % out_clear(nbtout))
   allocate (radiance % total_out(nbtout))
   allocate (radiance % clear_out(nbtout))
call da_oi(__FILE__,__LINE__)

   surfem (:) = 0.
   call rttov_setupindex(nchan, nprofiles, nfrequencies, &    ! in
                nchannels, nbtout, coef, surfem,  &          ! in
                lprofiles, channels, polarisations,     &    ! out
                emissivity  )                                ! out                       

   !  surface emissivity scheme
   !----------------------------------------------------------

call da_oi(__FILE__,__LINE__)
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
         if (isflg(n) == 2 .or. isflg(n) == 6) then
            calcemis (1:nchannels)   = .false.
            emissivity (1:nchannels) = 0.97           ! land without snow
         end if
         if (isflg(n) == 3 .or. isflg(n) == 7) then
            calcemis (1:nchannels)   = .false.
            emissivity (1:nchannels) = 1.0            ! land with snow
         end if
      end if
   end if
call da_oi(__FILE__,__LINE__)

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
      
         !  1.0 over sea-ice/snow/land
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
call da_oi(__FILE__,__LINE__)

   !-----------------------------------
   !  calling RTTOV forward model
   !----------------------------------

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
call da_oi(__FILE__,__LINE__)

   if ( any(errorstatus(:) /= 0 )) then
      !write (unit=stderr,fmt=*) 'rttov_direct error code = ', errorstatus(:)
      !write (unit=stderr,fmt=*) 'nfrequencies = ', nfrequencies
      !write (unit=stderr,fmt=*) 'nchannels    = ', nchannels
      !write (unit=stderr,fmt=*) 'nbtout       = ', nbtout
      !write (unit=stderr,fmt=*) 'nprofiles    = ', nprofiles
      !write (unit=stderr,fmt=*) 'channels     = ', channels
      !write (unit=stderr,fmt=*) 'polarisations= ', polarisations
      !write (unit=stderr,fmt=*) 'lprofiles    = ', lprofiles
      !write (unit=stderr,fmt=*) 'profiles%s2m   = ', profiles(1)%s2m
      !write (unit=stderr,fmt=*) 'profiles%skin   = ', profiles(1)%skin
      write(unit=stderr,fmt='(6f12.2)') &
         profiles(1)%s2m%p,profiles(1)%skin%t,profiles(1)%s2m%t, &
         profiles(1)%s2m%q,profiles(1)%s2m%u,profiles(1)%s2m%v
      !write (unit=stderr,fmt=*) 'profiles%zenangle = ', profiles(1)%zenangle
      !write (unit=stderr,fmt=*) 'profiles%azangle = ', profiles(1)%azangle
      !write (unit=stderr,fmt=*) 'profiles%p   = ', profiles(1)%p
      !write (unit=stderr,fmt=*) 'profiles%t   = ', profiles(1)%t
      !write (unit=stderr,fmt=*) 'profiles%q   = ', profiles(1)%q
      !do i=coef%nlevels,1,-1
      !  write(unit=stderr,fmt='(i4,3f12.2)') i, profiles(1)%p(i),profiles(1)%t(i),profiles(1)%q(i)
      !end do
      !write (unit=stderr,fmt=*) 'addcloud     = ', addcloud
      !write (unit=stderr,fmt=*) 'calcemis     = ', calcemis
      !write (unit=stderr,fmt=*) 'emissivity   = ', emissivity
      !write (unit=stderr,fmt=*) 'radiance = ', radiance%out_clear
      !call da_error(__FILE__,__LINE__,(/"Erk"/))
   end if
call da_oi(__FILE__,__LINE__)

   nc = nbtout / nprofiles
   do n = 1, nprofiles
      tb(1:nc,n) = radiance % out_clear((n-1)*nc+1:n*nc)
      emissivity_out(:,n) = emissivity((n-1)*nc+1:n*nc)
   end do

   deallocate (lprofiles )
   deallocate (channels )
   deallocate (polarisations )
   deallocate (emissivity )
   deallocate (calcemis )
   deallocate (surfem )
   do n = 1, nprofiles 
      deallocate (profiles(n)%p )
      deallocate (profiles(n)%t )
      deallocate (profiles(n)%q )
      deallocate (profiles(n)%o3 )
      deallocate (profiles(n)%co2 )
      deallocate (profiles(n)%clw )
   end do

   ! deallocate transmittance structure
   Deallocate (transmission % tau_surf)
   Deallocate (transmission % tau_layer)
   Deallocate (transmission % od_singlelayer)

   ! deallocate radiance results arrays with number of channels
   Deallocate (radiance % clear)
   Deallocate (radiance % cloudy)
   Deallocate (radiance % total)
   Deallocate (radiance % bt)
   Deallocate (radiance % bt_clear)
   Deallocate (radiance % upclear)
   Deallocate (radiance % dnclear)
   Deallocate (radiance % reflclear)
   Deallocate (radiance % overcast)
   Deallocate (radiance % downcld)
   Deallocate (radiance % out)
   Deallocate (radiance % out_clear)
   Deallocate (radiance % total_out)
   Deallocate (radiance % clear_out)
call da_oi(__FILE__,__LINE__)

   call da_trace_exit("da_rttov_direct")

end subroutine da_rttov_direct


#endif
