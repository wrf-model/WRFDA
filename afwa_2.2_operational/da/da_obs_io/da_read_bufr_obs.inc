subroutine da_read_bufr_obs (ob, xp, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Read BUFR observation file for input to wrfvar
   !---------------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(in) :: xp           ! Domain decomposition vars.
   type (ob_type), intent(inout) :: ob

   character(*),   intent (in), optional :: filename

#ifdef BUFR

   type (multi_level_type)      :: platform
   logical                      :: outside, outside_all
   integer                      :: total_obs, &
                                   num_sound, &
                                   num_synop, &
                                   num_pilot, &
                                   num_geoamv, &
                                   num_polaramv, &
                                   num_satem, &
                                   num_airep, &
                                   num_metar, &
                                   num_ships, &
                                   num_gpspw, &
                                   num_gpsref, &
                                   num_ssmi_retrieval, &
                                   num_ssmi_tb      , &
                                   num_ssmt1, &
                                   num_ssmt2, &
                                   num_pseudo, num_qscat, &
                                   num_profiler, num_buoy, num_bogus

   character(len=40)     :: obstr,drift,hdstr,qmstr,oestr
   character(len=8)      :: subset, c8id
   real,dimension(7)     :: hdr
   real,dimension(2,1)   :: pmo
   ! real,dimension(9,255) :: drf
   real,dimension(9,255) :: obs,qms,oes
   real                  :: time,woe,qob,toe,qoe,poe,pob,tob,zob,rob,roe

   integer               :: iost, ndup, n, i, j, k, surface_level, report
   integer               :: iret, idate, kx, nlevels, nrecs,miscd, t29, jx, ix
   integer               :: cat,zqm,pqm,qqm,tqm,wqm,pwq,pmq,rqm
   integer               :: iunit

   integer, parameter    :: qflag=4         ! Flag for retaining data
   equivalence     (hdr(1), c8id)

   total_obs = ob%total_obs
   num_sound = ob%num_sound
   num_synop = ob%num_synop
   num_pilot = ob%num_pilot
   num_geoamv = ob%num_geoamv
   num_polaramv = ob%num_polaramv
   num_satem = ob%num_satem
   num_airep = ob%num_airep
   num_metar = ob%num_metar
   num_ships = ob%num_ships
   num_gpspw = ob%num_gpspw
   num_gpsref = ob%num_gpsref
   num_ssmi_retrieval = ob%num_ssmi_retrieval
   num_ssmi_tb       = ob%num_ssmi_tb
   num_ssmt1 = ob%num_ssmt1
   num_ssmt2 = ob%num_ssmt2
   num_qscat = ob%num_qscat
   num_profiler = ob%num_profiler
   num_buoy = ob%num_buoy
   num_bogus= ob%num_bogus
   num_pseudo = 0

   ! open file
   !  ---------
   call da_get_unit(iunit)
   if (present(filename)) then
      open(unit   = iunit, FILE   = trim(filename), &
         iostat =  iost, form = 'unformatted', STATUS = 'OLD')
      if (iost /= 0) then
         write(unit=stdout, fmt='(/A,I3,3(2X,A)/)') &
              'ERROR in OBS inPUT FILE unit ',iunit, &
              'OBS FILENAME:', trim(filename), &
              'FOR GTS OBSERVATIONS CANNOT BE FOUND OR CANNOT BE openED'
         return
      end if
   end if

   hdstr='SID XOB YOB DHR TYP ELV T29     '
   obstr='POB QOB TOB ZOB UOB VOB PWO ROB CAT '
   drift='XDR YDR HRDR                    '
   qmstr='PQM QQM TQM ZQM WQM NUL PWQ PMQ RQM '
   oestr='POE QOE TOE NUL WOE NUL PWE ROE '

   nrecs = 0
   miscd = 0

   ! read bufr observation file
   ! call openbf(iunit,'in',iunit)
   call readmg(iunit,subset,idate,iret)

   if (iret/=0) then
      write(unit=stdout, fmt='(a, i4)') &
           'return code from readmg:', iret, &
           'Reach the end of obs unit: ', iunit

      call da_error(__FILE__,__LINE__, &
         (/"No BUFR observations"/))
   end if

   report = 0 ! report number in file
   reports: do
      report = report+1

      call readsb(iunit,iret)

      if (iret/=0) then
         call readmg(iunit,subset,idate,iret)

         if (iret/=0) then
            write(unit=stdout, fmt='(a, i4)') &
                 'return code from readmg:', iret, &
                 'Reach the end of prepbufr obs unit: ', iunit

            exit reports
         end if

         cycle reports
      end if

      nrecs=nrecs+1

      call ufbint(iunit,hdr,7,1,iret,hdstr)
      
      platform  % info % name(1:8) = subset

      platform  % info % id        = c8id(1:5)
      if (hdr(2) >  180.0) hdr(2)=hdr(2)-360.0

      ! Put a check on Lat
      
      hdr(3) = MAX(hdr(3), -89.95)
      hdr(3) = Min(hdr(3),  89.95)
      platform%info%lon = hdr(2)
      platform%info%lat = hdr(3)

      ! Put a check on Lat

      if (platform%info%lon == 180.0  ) platform%info%lon =-180.000
      ! Fix funny wind direction at Poles
      if (platform%info%lat <= -89.95 .or. platform%info%lat >= 89.95) then
         platform%info%lon = 0.0
      end if

      ! Restrict to a range of reports, useful for debugging

      if (report < report_start) then
         cycle
      end if

      if (report > report_end) then
         exit
      end if

      call da_ll_to_xy (platform%info, platform%loc,xp, outside, outside_all)

      if (outside_all) cycle reports

      if (print_detail_obs) then
         ! Simplistic approach, could be improved to get it all done on PE 0
         if (.NOT. outside) then
            write(unit=stdout,fmt='(A,I4,A,F8.2,A,F8.2,A,I3,A,2I3)') &
               "Report",report,"at",platform%info%lon,"E",platform%info%lat,"N", &
               "on processor", myproc,"position", platform%loc%x,platform%loc%y
         end if
      end if

      time=hdr(4)
      if (time >  3.0) time=3.0
      if (time < -3.0) time=-3.0

      t29 = int(0.1 + hdr(7))
      kx=int(0.1+hdr(5))

      if (kx .eq. 183) then          ! reset kx based on t29
         if (t29 .eq. 511) kx = 181
         if (t29 .eq. 512) kx = 187
         if (t29 .eq. 522) kx = 180
         if (t29 .eq. 523) kx = 180
         if (t29 .eq. 531) kx = 180
         if (t29 .eq. 561) kx = 180
         if (t29 .eq. 562) kx = 180
      end if

      ! WHY
      ! if ((kx >= 160) .and. (kx <= 179)) then    ! bypass satellite data
      ! if (t29 .eq. 61 .or. t29 .eq. 63 .or. t29 .ge. 571) then
      !    cycle reports
      ! end if
      ob % total_obs = ob % total_obs + 1


      ! Conventional data

      call ufbint(iunit,pmo,2,1,nlevels,'PMO PMQ')
      ! write(unit=stdout,fmt=*) 'PMO = ',pmo(1,1)

      call ufbint(iunit,obs,9,255,nlevels,obstr)
      call ufbint(iunit,qms,9,255,nlevels,qmstr)
      call ufbint(iunit,oes,9,255,nlevels,oestr)
      if (c8id(1:5) .eq. 'RJOR ') then
         ! if (kx .eq. 180) then
         write(unit=stdout,fmt=*) 'report, ',c8id(1:5),' typ = ',kx, &
            ' nlevels = ',nlevels
         do ix = 1,nlevels
            write(unit=stdout,fmt='(9(f10.4,2x))') (obs(jx,ix),jx=1,9)
            write(unit=stdout,fmt='(9(f10.4,2x))') (qms(jx,ix),jx=1,9)
            write(unit=stdout,fmt=*) 'PMO = ',pmo(1,1)
         end do
         !    kx = 180
         ! end if
      end if

      ! WHY
      ! no need for balloon drift
      ! select case(kx)
      !    case (120, 132, 220:221, 232) ;       ! read drift
      !       call ufbint(iunit,drf,9,255,iret,drift)
      !       do k=1,nlevels
      !          if (drf(1,k) .ge. 360.)drf(1,k)=drf(1,k)-360.
      !          if (drf(1,k) .lt. 0.)drf(1,k)=drf(1,k)+360.
      !          if (abs(drf(2,k)) .gt. 1000. .or. abs(drf(1,k)) .gt. 1000.) then
      !             drf(2,k)=hdr(3)
      !             drf(1,k)=hdr(2)
      !          end if
      !          if (abs(drf(3,k)) .gt. 3.)then
      !             drf(3,k)=time
      !          end if
      !       end do
      !    case default ;                         ! Do nothing
      ! end select

      platform  % info % levels   =nlevels
      platform  % info % elv      =hdr(6)

      platform % loc % slp %inv  = missing_r     
      platform % loc % slp %qc   = missing_data
      platform % loc % slp %error=200.           
      platform % loc % pw %inv  = missing_r     
      platform % loc % pw %qc   = missing_data
      platform % loc % pw %error=.2             

      do i=1,max_ob_levels
         platform % each (i) % height  = missing_r
         platform % each (i) % height_qc = missing_data

         platform % each (i) % zk = missing_r
            
         platform % each (i) % u % inv = missing_r
         platform % each (i) % u % qc  = missing_data
         platform % each (i) % u % error =10.       

         platform % each (i) % v = platform % each (i) % u

         platform % each (i) % t % inv = missing_r
         platform % each (i) % t % qc  = missing_data
         platform % each (i) % t % error =5.       

         platform % each (i) % p % inv = missing_r      
         platform % each (i) % p % qc  = missing_data
         platform % each (i) % p % error =200.    

         platform % each (i) % q % inv = missing_r
         platform % each (i) % q % qc  = missing_data
         platform % each (i) % q % error =.1      
      end do 

      do k = 1, platform % info % levels
         pob=obs(1,k)
         ! scale q and compute t from tv and set units to Kelvin, if they aren't missing

         if (obs(2,k) .gt. 0. .and. obs(2,k) .lt. 1e9) then   
            obs(2,k) = obs(2,k)*1e-6
            if (obs(3,k) .gt. -200. .and. obs(3,k) .lt. 300.) then
               obs(3,k) = (obs(3,k)+t_kelvin) / (1. + 0.608 * obs(2,k))
            end if
         else
            if (obs(3,k) .gt. -200. .and. obs(3,k) .lt. 300.) then
               obs(3,k) = obs(3,k) + t_kelvin
           end if
         end if
         qob=obs(2,k)
         tob=obs(3,k)
         zob=obs(4,k)
         rob=obs(8,k)
         pqm = qflag
         if (abs(qms(1,k)) < qflag) pqm=nint(qms(1,k))
         qqm = qflag
         if (abs(qms(2,k)) < qflag) qqm=nint(qms(2,k))
         tqm = qflag
         if (abs(qms(3,k)) < qflag) tqm=nint(qms(3,k))
         zqm = qflag
         if (abs(qms(4,k)) < qflag) zqm=nint(qms(4,k))
         wqm = qflag
         if (abs(qms(5,k)) < qflag) wqm=nint(qms(5,k))
         pwq = qflag
         if (abs(qms(7,k)) < qflag) pwq=nint(qms(7,k))
         rqm = qflag
         if (abs(qms(9,k)) < qflag) rqm=nint(qms(9,k))
         pmq = qflag
         if (abs(pmo(2,1)) < qflag) pmq=nint(pmo(2,1))
         cat=nint(obs(9,k))

         if (pmq < qflag) then
            platform % loc % slp % inv =pmo(1,1)*100.
            platform % loc % slp % qc  =pmq
            platform % loc % slp % error = 200.0
         end if

         if (pwq < qflag) then
            platform % loc % pw % inv = obs(7,k) * 0.1    ! convert to cm
            platform % loc % pw % qc  =  pwq
            platform % loc % pw % error = 0.2     ! hardwired to 0.2 cm
         end if

         poe=100.0

         if (tqm<qflag) then
            toe=oes(3,k)
            platform % each (k) % t % inv =obs(3,k)
            platform % each (k) % t % qc  =tqm
            platform % each (k) % t % error =toe
         end if

         if (wqm < qflag) then
            woe=oes(5,k)
            ! WHY
            ! if (wqm==0) woe=woe*0.9
            ! if (wqm==3) woe=woe*1.2
            platform % each (k) % u % inv =obs(5,k)
            platform % each (k) % v % inv =obs(6,k)
            platform % each (k) % u % qc  =wqm
            platform % each (k) % u % error =woe
            platform % each (k) % v % qc  =wqm
            platform % each (k) % v % error =woe

            ! Convert earth wind to model wind.
            call da_earth_2_model_wind(obs(5,k), obs(6,k), &
               platform % each (k) % u % inv, &
               platform % each (k) % v % inv, &
               platform%info%lon)
            if (platform % each (k) % u % inv == 0.0 .and. &
                platform % each (k) % v % inv == 0.0) then
               platform % each (k) % u % inv =  missing_r  
               platform % each (k) % v % inv = missing_r  
               platform % each (k) % u % qc  = missing_data
               platform % each (k) % v % qc  = missing_data
            end if
         end if

         if (qqm<qflag .and. obs(2,k)<1.0e9) then
            qoe=oes(2,k)
            platform % each (k) % q % inv =obs(2,k)
            platform % each (k) % q % qc  =qqm
            platform % each (k) % q % error =qoe
         end if

         if (zqm<qflag)then
            platform % each (k) % height  =zob
            platform % each (k) % height_qc =zqm
         end if

         if (rqm<qflag)then
            roe = oes(8,k)
            platform % each (k) % u % inv =rob
            platform % each (k) % u % qc =rqm
            platform % each (k) % u % error =roe
         end if

         if (pqm<qflag)then
            platform % each (k) % p % inv =obs(1,k)*100.
            platform % each (k) % p % qc  =pqm
            platform % each (k) % p % error =poe
         end if
      end do

      nlevels    = platform%info%levels

      if (nlevels > max_ob_levels) then
         nlevels = max_ob_levels

         write(unit=stderr, fmt='(/a/)') &
               'Warning: Too many levels.'

          write(unit=stderr, fmt='(/2a/2a/2x,a,2f8.2,2(2x,a,f9.2)/2(2x,a,i4)/)') &
               'Subset:   ', platform%info%name(1:8), &
               'Platfrom: ', trim(platform%info%platform), &
               'Loc(lat, lon): ', platform%info%lat, platform%info%lon, &
               'elv:   ', platform%info%elv, &
               'pstar: ', platform%info%pstar, &
               'level: ', platform%info%levels, &
               'kx:    ', kx
      else if ((nlevels < 1) .and. ((kx /= 164) .or. (kx /= 174))) then
         write(unit=stderr, fmt='(/a/)') &
               'Warning: Too few levels.'
  
         write(unit=stderr, fmt='(/2a/2a/2x,a,2f8.2,2(2x,a,f9.2)/2(2x,a,i4)/)') &
               'Subset:   ', platform%info%name(1:8), &
               'Platfrom: ', trim(platform%info%platform), &
               'Loc(lat, lon): ', platform%info%lat, platform%info%lon, &
               'elv:   ', platform%info%elv, &
               'pstar: ', platform%info%pstar, &
               'level: ', platform%info%levels, &
               'kx:    ', kx

         cycle reports
      end if

      !---------------------------------------------------------------------------
      ! This is basically converting  rh to q i
      ! Method : 
      !  if rh, temp and pr all available computes Qs otherwise sets Qs= missing
      !  if rh > 100 sets q = qs otherwise q = rh*Qs/100. 
      ! Note: Currently da_obs_proc_station is active only for ob_format_ascii
      !      call da_obs_proc_station(platform)
      !---------------------------------------------------------------------------

      ! Loop over duplicating obs for global
      ndup = 1
      if (global .and. &
         (platform%loc%i < xp%ids .or. platform%loc%i >= xp%ide)) ndup= 2

      ! It is possible that logic for counting obs is incorrect for the
      ! global case with >1 MPI tasks due to obs duplication, halo, etc.
      ! TBH:  20050913
      if (.not.outside) then
         if (print_detail_obs .and. ndup > 1) then
            write(unit=stdout, fmt = '(A12,1X,A19,1X,A40,1X,I6,3(F12.3,11X),6X,A5)') &  
               platform%info%platform,    &
               platform%info%date_char,   &
               platform%info%name,        &
               platform%info%levels,      &
               platform%info%lat,         &
               platform%info%lon,         &
               platform%info%elv,         &
               platform%info%id

            write(unit=stdout, fmt = '(a,2i5,4e20.10)') &
               ' duplicating obs since loc% i,j,dx,dxm,dy & dym ', &
               platform%loc%i,  platform%loc%j,   &
               platform%loc%dx, platform%loc%dxm, &
               platform%loc%dy, platform%loc%dym
         end if
      end if

      dup_loop: do n = 1, ndup
         select case(t29)
         case (11, 12, 13, 22, 23, 31)
            select case (kx)
            case (120, 122, 132, 220, 222, 232) ;         ! Sound
               ob%num_sound_glo = ob%num_sound_glo + 1
               if (outside) then
                  cycle reports
               end if

               if (.not.Use_SoundObs) cycle reports

               platform  % info % platform ='FM-35 TEMP'
               num_sound = num_sound + 1
               ! Track serial obs index for reassembly of obs during 
               ! bit-for-bit tests with different numbers of MPI tasks.
               platform%loc%obs_global_index = ob%num_sound_glo

               ob%sonde_sfc(num_sound)%info = platform%info
               ob%sonde_sfc(num_sound)%loc  = platform%loc

               ! Search to see if we have surface obs.

               surface_level = 0

               do i = 1, nlevels
                  ! if (elevation and height are the same, it is surface)
                  if (abs(platform%info%elv - &
                     platform%each(i)%height) < 0.1) then
                     surface_level = i

                     ! Save surface pressure.
                     ob%sonde_sfc(num_sound)%h = platform%each(i)%height
                     ob%sonde_sfc(num_sound)%u = platform%each(i)%u
                     ob%sonde_sfc(num_sound)%v = platform%each(i)%v
                     ob%sonde_sfc(num_sound)%t = platform%each(i)%t
                     ob%sonde_sfc(num_sound)%q = platform%each(i)%q
                     ob%sonde_sfc(num_sound)%p = platform%each(i)%p
                     exit
                  end if
               end do

               ! processing the sound_sfc data:

               ob%sound(num_sound)%info = platform%info
               ob%sound(num_sound)%loc  = platform%loc

               if (surface_level > 0) then
                  nlevels = nlevels - 1
               else
                  ob%sonde_sfc(num_sound)%h = missing_r
                  ob%sonde_sfc(num_sound)%u%inv   = missing_r
                  ob%sonde_sfc(num_sound)%u%qc    = missing
                  ob%sonde_sfc(num_sound)%u%error = abs(missing_r)
                  ob%sonde_sfc(num_sound)%v = ob%sonde_sfc(num_sound)%u
                  ob%sonde_sfc(num_sound)%t = ob%sonde_sfc(num_sound)%u
                  ob%sonde_sfc(num_sound)%p = ob%sonde_sfc(num_sound)%u
                  ob%sonde_sfc(num_sound)%q = ob%sonde_sfc(num_sound)%u
               end if

               if (nlevels < 1) cycle reports

               allocate (ob%sound(num_sound)%h (1:nlevels))
               allocate (ob%sound(num_sound)%p (1:nlevels))
               allocate (ob%sound(num_sound)%zk(1:nlevels))
               allocate (ob%sound(num_sound)%u (1:nlevels))
               allocate (ob%sound(num_sound)%v (1:nlevels))
               allocate (ob%sound(num_sound)%t (1:nlevels))
               allocate (ob%sound(num_sound)%q (1:nlevels))

               j = 0
               do i = 1, ob%sound(num_sound)%info%levels
                  if (i == surface_level) cycle

                  j=j+1

                  ob%sound(num_sound)%h(j) = platform%each(i)%height
                  ob%sound(num_sound)%p(j) = platform%each(i)%p%inv
                  ob%sound(num_sound)%u(j) = platform%each(i)%u
                  ob%sound(num_sound)%v(j) = platform%each(i)%v
                  ob%sound(num_sound)%t(j) = platform%each(i)%t
                  ob%sound(num_sound)%q(j) = platform%each(i)%q
               end do

               ob%sound(num_sound)%info%levels = nlevels
            case (221) ;           ! Pilot 
               ob%num_pilot_glo = ob%num_pilot_glo + 1
               if (outside) then
                  cycle reports
               end if

               if (.not.Use_PilotObs) cycle reports
                  num_pilot = num_pilot + 1
                  ! Track serial obs index for reassembly of obs during 
                  ! bit-for-bit tests with different numbers of MPI tasks.
                  platform%loc%obs_global_index = ob%num_pilot_glo

                  ob%pilot(num_pilot)%info = platform%info
                  ob%pilot(num_pilot)%loc  = platform%loc

                  allocate (ob%pilot(num_pilot)%p (1:nlevels))
                  allocate (ob%pilot(num_pilot)%zk(1:nlevels))
                  allocate (ob%pilot(num_pilot)%u (1:nlevels))
                  allocate (ob%pilot(num_pilot)%v (1:nlevels))

                  do i = 1, nlevels
                     ob%pilot(num_pilot)%p(i) = platform%each(i)%p%inv
                     ob%pilot(num_pilot)%u(i) = platform%each(i)%u
                     ob%pilot(num_pilot)%v(i) = platform%each(i)%v
                  end do
            end select

         case (41)
            ! case (130:131, 133, 230:231, 233) ; ! Airep
            ob%num_airep_glo = ob%num_airep_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.Use_AirepObs) cycle reports

            platform  % info % platform ='FM-97 AIREP'
            num_airep = num_airep + 1
            ! Track serial obs index for reassembly of obs during 
            ! bit-for-bit tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_airep_glo

            ob % airep (num_airep) % info = platform % info
            ob % airep (num_airep) % loc  = platform % loc

            nlevels= ob % airep (num_airep) % info % levels
            allocate (ob % airep (num_airep) % h (1:nlevels))
            allocate (ob % airep (num_airep) % p (1:nlevels))
            allocate (ob % airep (num_airep) % zk(1:nlevels))
            allocate (ob % airep (num_airep) % u (1:nlevels))
            allocate (ob % airep (num_airep) % v (1:nlevels))
            allocate (ob % airep (num_airep) % t (1:nlevels))

            do i = 1, nlevels
               ob % airep (num_airep) % h(i) = platform % each(i) % height
               ob % airep (num_airep) % p(i) = platform % each(i) % p % inv
               ob % airep (num_airep) % u(i) = platform % each(i) % u
               ob % airep (num_airep) % v(i) = platform % each(i) % v
               ob % airep (num_airep) % t(i) = platform % each(i) % t
               ob % airep (num_airep) %zk(i) = platform % each(i) % zk
            end do

            ! case (180, 182, 280, 282) ;         ! Ships and  buoys

         case (522, 523);        ! Ships
            ob%num_ships_glo = ob%num_ships_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.Use_ShipsObs) cycle reports

            platform  % info % platform ='FM-13 SHIP '
            num_ships  = num_ships  + 1
            ! Track serial obs index for reassembly of obs during 
            ! bit-for-bit tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_ships_glo

            ob % ships (num_ships) % info = platform % info
            ob % ships (num_ships) % loc  = platform % loc

            ob % ships (num_ships) % h = platform % each(1) % height
            ob % ships (num_ships) % u = platform % each(1) % u
            ob % ships (num_ships) % v = platform % each(1) % v
            ob % ships (num_ships) % t = platform % each(1) % t
            ! WHY
            ! ob % ships (num_ships) % p = platform % loc     % slp
            ob % ships (num_ships) % p = platform % each(1) % p
            ob % ships (num_ships) % q = platform % each(1) % q
            ob % ships (num_ships) % zk= platform % each(1) % zk

         case (531, 561, 562) ;          ! Buoy  
            ob%num_buoy_glo = ob%num_buoy_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_BuoyObs) cycle reports
            platform  % info % platform ='FM-18 BUOY '
            num_buoy  = num_buoy  + 1
            platform%loc%obs_global_index = ob%num_buoy_glo

            ob%buoy(num_buoy)%info = platform%info
            ob%buoy(num_buoy)%loc  = platform%loc

            ob%buoy(num_buoy)%h = platform%each(1)%height
            ob%buoy(num_buoy)%u = platform%each(1)%u
            ob%buoy(num_buoy)%v = platform%each(1)%v
            ob%buoy(num_buoy)%t = platform%each(1)%t
            ob%buoy(num_buoy)%p = platform%each(1)%p
            ob%buoy(num_buoy)%q = platform%each(1)%q

         case (511)
            ! case (181, 281) ;                   ! Synop

            ob%num_synop_glo = ob%num_synop_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.Use_SynopObs) cycle reports

            platform  % info % platform ='FM-12 SYNOP'

            num_synop  = num_synop  + 1
            platform%loc%obs_global_index = ob%num_synop_glo

            ob % synop (num_synop) % info = platform % info
            ob % synop (num_synop) % loc  = platform % loc

            ob % synop (num_synop) % h = platform % each(1) % height
            ob % synop (num_synop) % u = platform % each(1) % u
            ob % synop (num_synop) % v = platform % each(1) % v
            ob % synop (num_synop) % t = platform % each(1) % t
            ! WHY
            ! ob % synop (num_synop) % p = platform % loc     % slp
            ob % synop (num_synop) % p = platform % each(1) % p
            ob % synop (num_synop) % q = platform % each(1) % q
            ob % synop (num_synop) % zk= platform % each(1) % zk

            if (ob % synop(num_synop) % h < ob % synop (num_synop) % info % elv) then
               ob % synop(num_synop) % h = ob % synop (num_synop) % info % elv
            end if

         case (512)
            ! case (187, 287) ;                        ! Metar
            ob%num_metar_glo = ob%num_metar_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.Use_MetarObs) cycle reports

            platform  % info % platform ='FM-15 METAR'
            num_metar  = num_metar  + 1
            platform%loc%obs_global_index = ob%num_metar_glo

            ob % metar (num_metar) % info = platform % info
            ob % metar (num_metar) % loc  = platform % loc

            ob % metar (num_metar) % h = platform % each(1) % height
            ob % metar (num_metar) % u = platform % each(1) % u
            ob % metar (num_metar) % v = platform % each(1) % v
            ob % metar (num_metar) % t = platform % each(1) % t
            ob % metar (num_metar) % p = platform % each(1) % p
            ob % metar (num_metar) % q = platform % each(1) % q
            ob % metar (num_metar) % zk= platform % each(1) % zk

         case (63)
            ! case (242:246, 252:253, 255) ;         ! Geo. CMVs
            ob%num_geoamv_glo = ob%num_geoamv_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.Use_geoamvObs) cycle reports
            platform  % info % platform ='FM-88 SATOB'
            num_geoamv = num_geoamv + 1  
            platform%loc%obs_global_index = ob%num_geoamv_glo

            ob % geoamv (num_geoamv) % info = platform % info
            ob % geoamv (num_geoamv) % loc  = platform % loc
            allocate (ob%geoamv(num_geoamv)%p (1:nlevels))
            allocate (ob%geoamv(num_geoamv)%zk(1:nlevels))
            allocate (ob%geoamv(num_geoamv)%u (1:nlevels))
            allocate (ob%geoamv(num_geoamv)%v (1:nlevels))

            do i = 1, nlevels
               ob % geoamv (num_geoamv) % p(i)  = platform % each(i) % p % inv
               ob % geoamv (num_geoamv) % u(i)  = platform % each(i) % u
               ob % geoamv (num_geoamv) % v(i)  = platform % each(i) % v
                  ob % geoamv (num_geoamv) %zk(i)  = platform % each(i) % zk
            end do

         case (582)
            ob%num_qscat_glo = ob%num_qscat_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_qscatobs) cycle reports
            platform  % info % platform ='FM-281 Quiks'
            num_qscat  = num_qscat  + 1 
            platform%loc%obs_global_index = ob%num_qscat_glo

            ob%qscat(num_qscat)%info = platform%info
            ob%qscat(num_qscat)%loc  = platform%loc
            ! WHY
            ! ob%qscat(num_qscat)%h = platform%each(1)%height
            ! prepbufr uses pressure not height, so hardwire height to 
            ! 0 (sea-level)
            ob%qscat(num_qscat)%h = 0.
            ob%qscat(num_qscat)%u = platform%each(1)%u
            ob%qscat(num_qscat)%v = platform%each(1)%v
            ob%qscat(num_qscat)%u%error = max(platform%each(1)%u%error,1.0)
            ob%qscat(num_qscat)%v%error = max(platform%each(1)%v%error,1.0)

         case (583)       ! GPS PW
            ob%num_gpspw_glo = ob%num_gpspw_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_GpspwObs) cycle reports
            platform  % info % platform ='FM-111 GPSPW'
            num_gpspw  = num_gpspw + 1
            platform%loc%obs_global_index = ob%num_gpspw_glo

            ob%gpspw(num_gpspw)%info = platform%info
            ob%gpspw(num_gpspw)%loc  = platform%loc 
            ob%gpspw(num_gpspw)%tpw  = platform%loc%pw

         case (584)       ! GPS REF
            ob%num_gpsref_glo = ob%num_gpsref_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_GpsrefObs) cycle reports
            platform  % info % platform ='FM-116 GPSRF'
            num_gpsref  = num_gpsref + 1
            platform%loc%obs_global_index = ob%num_gpsref_glo

            ob%gpsref(num_gpsref)%info = platform%info
            ob%gpsref(num_gpsref)%loc  = platform%loc

            allocate (ob%gpsref(num_gpsref)%h (1:nlevels))
            allocate (ob%gpsref(num_gpsref)%zk(1:nlevels))
            allocate (ob%gpsref(num_gpsref)%ref(1:nlevels))
            allocate (ob%gpsref(num_gpsref)%  p(1:nlevels))
            allocate (ob%gpsref(num_gpsref)%  t(1:nlevels))
            allocate (ob%gpsref(num_gpsref)%  q(1:nlevels))

            do i = 1, nlevels
               ob%gpsref(num_gpsref)%h(i)   = platform%each(i)%height
               ! u is used to store ref in this routine (ascii version uses td)
               ob%gpsref(num_gpsref)%ref(i) = platform%each(i)%u
               ! Keep the retrieved p and t (and q) as "field_type":
               ob%gpsref(num_gpsref)%p(i)   = platform%each(i)%p
               ob%gpsref(num_gpsref)%t(i)   = platform%each(i)%t
               ob%gpsref(num_gpsref)%q(i)   = platform%each(i)%q
            end do

         case (71, 72)
            ! case (223, 224 )        ;         !  Profiler & VADWND - NEXRAD winds    
            ob%num_profiler_glo = ob%num_profiler_glo + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_ProfilerObs) cycle reports
            platform  % info % platform ='FM-132 PRFLR'
            num_profiler = num_profiler + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ob%num_profiler_glo

            ob%profiler(num_profiler)%info = platform%info
            ob%profiler(num_profiler)%loc  = platform%loc

            allocate (ob%profiler(num_profiler)%p (1:nlevels))
            allocate (ob%profiler(num_profiler)%zk(1:nlevels))
            allocate (ob%profiler(num_profiler)%u (1:nlevels))
            allocate (ob%profiler(num_profiler)%v (1:nlevels))

            do i = 1, nlevels
               ob%profiler(num_profiler)%p(i) = platform%each(i)%p%inv
               ob%profiler(num_profiler)%u(i) = platform%each(i)%u
               ob%profiler(num_profiler)%v(i) = platform%each(i)%v
            end do

         case default 
            select case (kx)
            case (111 , 210)    ;         !  Tropical Cyclone Bogus
               ! Note Tropical cyclone Bougus is given type 135 in Obs-ascii
               ob%num_bogus_glo = ob%num_bogus_glo + 1
               if (outside) then
                  cycle reports
               end if

               if (.not.use_BogusObs) cycle reports
               platform  % info % platform ='FM-135 TCBOG'
               num_bogus = num_bogus + 1
               ! Track serial obs index for reassembly of obs during bit-for-bit
               ! tests with different numbers of MPI tasks.  
               platform%loc%obs_global_index = ob%num_bogus_glo

               if (num_bogus > max_bogus_input) then
                 write(unit=message(1),fmt='(A,I6,A,I6)') &
                   'Bogus #=', num_bogus, ' > max_bogus_input=', max_bogus_input
                 call da_error(__FILE__,__LINE__,message(1:1))
               end if

               ob%bogus(num_bogus)%info = platform%info
               ob%bogus(num_bogus)%loc  = platform%loc

               allocate (ob%bogus(num_bogus)%h (1:nlevels))
               allocate (ob%bogus(num_bogus)%p (1:nlevels))
               allocate (ob%bogus(num_bogus)%zk(1:nlevels))
               allocate (ob%bogus(num_bogus)%u (1:nlevels))
               allocate (ob%bogus(num_bogus)%v (1:nlevels))
               allocate (ob%bogus(num_bogus)%t (1:nlevels))
               allocate (ob%bogus(num_bogus)%q (1:nlevels))

               do i = 1, nlevels
                  ob%bogus(num_bogus)%h(i) = platform%each(i)%height
                  ob%bogus(num_bogus)%p(i) = platform%each(i)%p%inv
                  ob%bogus(num_bogus)%u(i) = platform%each(i)%u
                  ob%bogus(num_bogus)%v(i) = platform%each(i)%v
                  ob%bogus(num_bogus)%t(i) = platform%each(i)%t
                  ob%bogus(num_bogus)%q(i) = platform%each(i)%q
               end do

               ob%bogus(num_bogus)%slp    = platform%loc%slp

            case default
               write(unit=message(1), fmt='(a, i12)') &
                  'unsaved obs found with type: ',kx
               call da_warning(__FILE__,__LINE__,message(1:1))
            end select
         end select

         if (global .and. n < 2) then
           if (Testing_WRFVAR) exit dup_loop
           if (platform%loc % i >= xp % ide) then
               platform%loc%i = platform%loc % i - xp % ide
            else if (platform%loc % i < xp % ids) then
               platform%loc%i = platform%loc % i + xp % ide
            end if
            platform%loc%proc_domain = .not. platform%loc%proc_domain
         end if
      end do dup_loop   
   end do reports

   call closbf(iunit)
   close(iunit)
   call da_free_unit(iunit)

   ob%num_sound = num_sound
   ob%num_synop = num_synop
   ob%num_pilot = num_pilot
   ob%num_satem = num_satem
   ob%num_geoamv = num_geoamv
   ob%num_polaramv = num_polaramv
   ob%num_airep = num_airep
   ob%num_gpspw = num_gpspw
   ob%num_gpsref = num_gpsref
   ob%num_metar = num_metar
   ob%num_ships = num_ships
   ob%num_qscat = num_qscat
   ob%num_buoy  = num_buoy
   ob%num_profiler = num_profiler
   ob%num_bogus = num_bogus

   ! WHY
   ! ob%num_ssmt1 = num_ssmt1
   ! ob%num_ssmt2 = num_ssmt2
   ! ob%num_ssmi_tb        = num_ssmi_tb
   ! ob%num_ssmi_retrieval = num_ssmi_retrieval

   if (print_detail_obs) then
      write(unit=stdout, fmt='(/a,i6/)') 'ob%current_ob_time=', ob%current_ob_time

      write(unit=stdout, fmt='(5x,a,i6,a)') &
           'Read:  ', num_sound,           ' SOUND          reports,', &
           'Read:  ', num_synop,           ' SYNOP          reports,', &
           'Read:  ', num_pilot,           ' PILOT          reports,', &
           'Read:  ', num_satem,           ' SATEM          reports,', &
           'Read:  ', num_geoamv,          ' Geo AMV        reports,', &
           'Read:  ', num_polaramv,        ' Polar AMV      reports,', &
           'Read:  ', num_airep,           ' AIREP          reports,', &
           'Read:  ', num_gpspw ,          ' GPSPW/GPSZD    reports,', &
           'Read:  ', num_gpsref,          ' GPSRF          reports,', &
           'Read:  ', num_metar,           ' METAR          reports,', &
           'Read:  ', num_ships ,          ' SHIP           reports,', &
           'Read:  ', num_ssmi_retrieval , ' SSMI_RETRIEVAL reports,', &
           'Read:  ', num_ssmi_tb,         ' SSMI_TB        reports,', &
           'Read:  ', num_ssmt1,           ' SSMT1          reports,', &
           'Read:  ', num_ssmt2,           ' SSMT2          reports,', &
           'Read:  ', num_qscat,           ' QSCAT          reports,', &
           'Read:  ', num_profiler,        ' Profiler       reports,', &
           'Read:  ', num_buoy,            ' Buoy           reports,', &
           'Read:  ', num_bogus,           ' Bogus          reports,', &
           'Read:  ', total_obs,           ' Total reports.', &
           'There are ', total_obs - num_sound - num_synop &
                                   - num_pilot - num_satem &
                                   - num_geoamv - num_polaramv - num_airep &
                                   - num_metar - num_ships &
                                   - num_ssmi_retrieval  &
                                   - num_ssmi_tb - num_ssmt1 - num_ssmt2 &
                                   - num_gpspw - num_gpsref - num_qscat  &
                                   - num_profiler - num_buoy, &
                                   - num_bogus - num_bogus, &
                                   '  reports unsaved.'
      write(unit=stdout, fmt=*) ' '
   end if

   if ((ob%ob_numb(ob%current_ob_time)%sound /= ob%num_sound) .or. &
      (ob%ob_numb(ob%current_ob_time)%synop /= ob%num_synop) .or. &
      (ob%ob_numb(ob%current_ob_time)%pilot /= ob%num_pilot) .or. &
      (ob%ob_numb(ob%current_ob_time)%satem /= ob%num_satem) .or. &
      (ob%ob_numb(ob%current_ob_time)%geoamv /= ob%num_geoamv) .or. &
      (ob%ob_numb(ob%current_ob_time)%polaramv /= ob%num_polaramv) .or. &
      (ob%ob_numb(ob%current_ob_time)%airep /= ob%num_airep) .or. &
      (ob%ob_numb(ob%current_ob_time)%gpspw /= ob%num_gpspw) .or. &
      (ob%ob_numb(ob%current_ob_time)%gpsref /= ob%num_gpsref) .or. &
      (ob%ob_numb(ob%current_ob_time)%metar /= ob%num_metar) .or. &
      (ob%ob_numb(ob%current_ob_time)%ships /= ob%num_ships) .or. &
      (ob%ob_numb(ob%current_ob_time)%qscat /= ob%num_qscat) .or. &
      (ob%ob_numb(ob%current_ob_time)%buoy  /= ob%num_buoy) .or. &
      (ob%ob_numb(ob%current_ob_time)%bogus /= ob%num_bogus) .or. &
      (ob%ob_numb(ob%current_ob_time)%ssmt2 /= ob%num_ssmt1) .or. &
      (ob%ob_numb(ob%current_ob_time)%ssmt2 /= ob%num_ssmt2) .or. &
      (ob%ob_numb(ob%current_ob_time)%profiler /= ob%num_profiler)) then

      write(unit=stderr, fmt='(a, i6, 2x, a, i6)') &
           'ob%ob_numb(ob%current_ob_time)%sound=', ob%ob_numb(ob%current_ob_time)%sound, &
           'ob%num_sound=', ob%num_sound, &
           'ob%ob_numb(ob%current_ob_time)%synop=', ob%ob_numb(ob%current_ob_time)%synop, &
           'ob%num_synop=', ob%num_synop, &
           'ob%ob_numb(ob%current_ob_time)%pilot=', ob%ob_numb(ob%current_ob_time)%pilot, &
           'ob%num_pilot=', ob%num_pilot, &
           'ob%ob_numb(ob%current_ob_time)%satem=', ob%ob_numb(ob%current_ob_time)%satem, &
           'ob%num_satem=', ob%num_satem, &
           'ob%ob_numb(ob%current_ob_time)%geoamv=', ob%ob_numb(ob%current_ob_time)%geoamv, &
           'ob%num_geoamv=', ob%num_geoamv, &
           'ob%ob_numb(ob%current_ob_time)%polaramv=', ob%ob_numb(ob%current_ob_time)%polaramv, &
           'ob%num_polaramv=', ob%num_polaramv, &
           'ob%ob_numb(ob%current_ob_time)%airep=', ob%ob_numb(ob%current_ob_time)%airep, &
           'ob%num_airep=', ob%num_airep, &
           'ob%ob_numb(ob%current_ob_time)%gpspw=', ob%ob_numb(ob%current_ob_time)%gpspw, &
           'ob%num_gpspw=', ob%num_gpspw, &
           'ob%ob_numb(ob%current_ob_time)%gpsref=', ob%ob_numb(ob%current_ob_time)%gpsref,&
           'ob%num_gpsref=', ob%num_gpsref, &
           'ob%ob_numb(ob%current_ob_time)%metar=', ob%ob_numb(ob%current_ob_time)%metar, &
           'ob%num_metar=', ob%num_metar, &
           'ob%ob_numb(ob%current_ob_time)%ships=', ob%ob_numb(ob%current_ob_time)%ships, &
           'ob%num_ships=', ob%num_ships, &
           'ob%ob_numb(ob%current_ob_time)%qscat=', ob%ob_numb(ob%current_ob_time)%qscat, &
           'ob%num_qscat=', ob%num_qscat, &
           'ob%ob_numb(ob%current_ob_time)%buoy =', ob%ob_numb(ob%current_ob_time)%buoy , &
           'ob%num_buoy =', ob%num_buoy , &
           'ob%ob_numb(ob%current_ob_time)%bogus=', ob%ob_numb(ob%current_ob_time)%bogus, &
           'ob%num_bogus=', ob%num_bogus, &
           'ob%ob_numb(ob%current_ob_time)%ssmt1=', ob%ob_numb(ob%current_ob_time)%ssmt1, &
           'ob%num_ssmt1=', ob%num_ssmt1, &
           'ob%ob_numb(ob%current_ob_time)%ssmt2=', ob%ob_numb(ob%current_ob_time)%ssmt2, &
           'ob%num_ssmt2=', ob%num_ssmt2, &
           'ob%ob_numb(ob%current_ob_time)%profiler=', ob%ob_numb(ob%current_ob_time)%profiler, &
           'ob%num_profiler=', ob%num_profiler

      call da_error(__FILE__,__LINE__,(/"Obs mismatch"/))
   end if

#endif

end subroutine da_read_bufr_obs


