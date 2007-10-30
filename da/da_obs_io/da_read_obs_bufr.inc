subroutine da_read_obs_bufr (iv, filename)

   !---------------------------------------------------------------------------
   ! Purpose: Read BUFR observation file for input to wrfvar
   !---------------------------------------------------------------------------

   implicit none

   type (iv_type),             intent(inout) :: iv
   character(len=*), optional, intent(in)    :: filename

#ifdef BUFR

   type (multi_level_type)      :: platform
   logical                      :: outside, outside_all
   integer                      :: nlocal(num_ob_indexes)
   integer                      :: ntotal(num_ob_indexes)

   character(len=40)     :: obstr,drift,hdstr,qmstr,oestr
   character(len=8)      :: subset, c8id
   real                  :: hdr(7)
   real                  :: pmo(2,1)
   ! real :: drf9,255)
   real                  :: obs(9,255),qms(9,255),oes(9,255)
   real                  :: time,woe,qob,toe,qoe,poe,pob,tob,zob,rob,roe

   integer               :: iost, ndup, n, i, j, k, surface_level, report
   integer               :: iret, idate, kx, nlevels, nrecs,miscd, t29, jx, ix
   integer               :: cat,zqm,pqm,qqm,tqm,wqm,pwq,pmq,rqm
   integer               :: iunit

   integer, parameter    :: qflag=4         ! Flag for retaining data
   equivalence     (hdr(1), c8id)

   if (trace_use) call da_trace_entry("da_read_obs_bufr")

   nlocal(:) = iv%plocal(iv%time-1,:)
   ntotal(:) = iv%ptotal(iv%time-1,:)

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

      call da_llxy (platform%info, platform%loc,outside, outside_all)

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

      if (kx == 183) then          ! reset kx based on t29
         if (t29 .eq. 511) kx = 181
         if (t29 .eq. 512) kx = 187
         if (t29 .eq. 522) kx = 180
         if (t29 .eq. 523) kx = 180
         if (t29 .eq. 531) kx = 180
         if (t29 .eq. 561) kx = 180
         if (t29 .eq. 562) kx = 180
      end if

      ! WHY?
      ! if ((kx >= 160) .and. (kx <= 179)) then    ! bypass satellite data
      ! if (t29 .eq. 61 .or. t29 .eq. 63 .or. t29 .ge. 571) then
      !    cycle reports
      ! end if

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

      ! WHY?
      ! no need for balloon drift
      ! select case(kx)
      !    case (120, 132, 220:221, 232) ;       ! read drift
      !       call ufbint(iunit,drf,9,255,iret,drift)
      !       do k=1,nlevels
      !          if (drf(1,k) .ge. 360.0)drf(1,k)=drf(1,k)-360.
      !          if (drf(1,k) .lt. 0.0)drf(1,k)=drf(1,k)+360.
      !          if (abs(drf(2,k)) .gt. 1000.0 .or. abs(drf(1,k)) .gt. 1000.0) then
      !             drf(2,k)=hdr(3)
      !             drf(1,k)=hdr(2)
      !          end if
      !          if (abs(drf(3,k)) .gt. 3.0)then
      !             drf(3,k)=time
      !          end if
      !       end do
      !    case default ;                         ! Do nothing
      ! end select

      platform  % info % levels   =nlevels
      platform  % info % elv      =hdr(6)

      platform % loc % slp %inv  = missing_r     
      platform % loc % slp %qc   = missing_data
      platform % loc % slp %error=200.0           
      platform % loc % pw %inv  = missing_r     
      platform % loc % pw %qc   = missing_data
      platform % loc % pw %error=0.2             

      do i=1,max_ob_levels
         platform % each (i) % height  = missing_r
         platform % each (i) % height_qc = missing_data

         platform % each (i) % zk = missing_r
            
         platform % each (i) % u % inv = missing_r
         platform % each (i) % u % qc  = missing_data
         platform % each (i) % u % error =10.0       

         platform % each (i) % v = platform % each (i) % u

         platform % each (i) % t % inv = missing_r
         platform % each (i) % t % qc  = missing_data
         platform % each (i) % t % error =5.0       

         platform % each (i) % p % inv = missing_r      
         platform % each (i) % p % qc  = missing_data
         platform % each (i) % p % error =200.0  

         platform % each (i) % q % inv = missing_r
         platform % each (i) % q % qc  = missing_data
         platform % each (i) % q % error =0.1      
      end do 

      do k = 1, platform % info % levels
         pob=obs(1,k)
         ! scale q and compute t from tv and set units to Kelvin, if they aren't missing

         if (obs(2,k) > 0.0 .and. obs(2,k) < 1e9) then   
            obs(2,k) = obs(2,k)*1e-6
            if (obs(3,k) > -200.0 .and. obs(3,k) < 300.0) then
               obs(3,k) = (obs(3,k)+t_kelvin) / (1.0 + 0.608 * obs(2,k))
            end if
         else
            if (obs(3,k) > -200.0 .and. obs(3,k) < 300.0) then
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
            platform % loc % slp % inv =pmo(1,1)*100.0
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
            ! WHY?
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
            if (platform % each (k) % u % inv == 0.0 .and. platform % each (k) % v % inv == 0.0) then
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
            platform % each (k) % p % inv =obs(1,k)*100.0
            platform % each (k) % p % qc  =pqm
            platform % each (k) % p % error =poe
         end if
      end do

      nlevels    = platform%info%levels

      if (nlevels > max_ob_levels) then
         nlevels = max_ob_levels

         write(unit=stderr, fmt='(/a/)') 'Warning: Too many levels.'

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
      !  if rh > 100 sets q = qs otherwise q = rh*Qs/100.0 
      ! Note: Currently da_obs_proc_station is active only for ob_format_ascii
      !      call da_obs_proc_station(platform)
      !---------------------------------------------------------------------------

      ! Loop over duplicating obs for global
      ndup = 1
      if (global .and. &
         (platform%loc%i < ids .or. platform%loc%i >= ide)) ndup= 2

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
               ntotal(sound) = ntotal(sound) + 1
               if (outside) then
                  cycle reports
               end if

               if (.not.use_soundobs) cycle reports

               platform  % info % platform ='FM-35 TEMP'
               nlocal(sound) = nlocal(sound) + 1
               ! Track serial obs index for reassembly of obs during 
               ! bit-for-bit tests with different numbers of MPI tasks.
               platform%loc%obs_global_index = ntotal(sound)

               iv%sonde_sfc(nlocal(sound))%info = platform%info
               iv%sonde_sfc(nlocal(sound))%loc  = platform%loc

               ! Search to see if we have surface obs.

               surface_level = 0

               do i = 1, nlevels
                  ! if (elevation and height are the same, it is surface)
                  if (abs(platform%info%elv - &
                     platform%each(i)%height) < 0.1) then
                     surface_level = i

                     ! Save surface pressure.
                     iv%sonde_sfc(nlocal(sound))%h = platform%each(i)%height
                     iv%sonde_sfc(nlocal(sound))%u = platform%each(i)%u
                     iv%sonde_sfc(nlocal(sound))%v = platform%each(i)%v
                     iv%sonde_sfc(nlocal(sound))%t = platform%each(i)%t
                     iv%sonde_sfc(nlocal(sound))%q = platform%each(i)%q
                     iv%sonde_sfc(nlocal(sound))%p = platform%each(i)%p
                     exit
                  end if
               end do

               ! processing the sound_sfc data:

               iv%sound(nlocal(sound))%info = platform%info
               iv%sound(nlocal(sound))%loc  = platform%loc

               if (surface_level > 0) then
                  nlevels = nlevels - 1
               else
                  iv%sonde_sfc(nlocal(sound))%h = missing_r
                  iv%sonde_sfc(nlocal(sound))%u%inv   = missing_r
                  iv%sonde_sfc(nlocal(sound))%u%qc    = missing
                  iv%sonde_sfc(nlocal(sound))%u%error = abs(missing_r)
                  iv%sonde_sfc(nlocal(sound))%v = iv%sonde_sfc(nlocal(sound))%u
                  iv%sonde_sfc(nlocal(sound))%t = iv%sonde_sfc(nlocal(sound))%u
                  iv%sonde_sfc(nlocal(sound))%p = iv%sonde_sfc(nlocal(sound))%u
                  iv%sonde_sfc(nlocal(sound))%q = iv%sonde_sfc(nlocal(sound))%u
               end if

               if (nlevels < 1) cycle reports

               allocate (iv%sound(nlocal(sound))%h (1:nlevels))
               allocate (iv%sound(nlocal(sound))%p (1:nlevels))
               allocate (iv%sound(nlocal(sound))%zk(1:nlevels))
               allocate (iv%sound(nlocal(sound))%u (1:nlevels))
               allocate (iv%sound(nlocal(sound))%v (1:nlevels))
               allocate (iv%sound(nlocal(sound))%t (1:nlevels))
               allocate (iv%sound(nlocal(sound))%q (1:nlevels))

               j = 0
               do i = 1, iv%sound(nlocal(sound))%info%levels
                  if (i == surface_level) cycle

                  j=j+1

                  iv%sound(nlocal(sound))%h(j) = platform%each(i)%height
                  iv%sound(nlocal(sound))%p(j) = platform%each(i)%p%inv
                  iv%sound(nlocal(sound))%u(j) = platform%each(i)%u
                  iv%sound(nlocal(sound))%v(j) = platform%each(i)%v
                  iv%sound(nlocal(sound))%t(j) = platform%each(i)%t
                  iv%sound(nlocal(sound))%q(j) = platform%each(i)%q
               end do

               iv%sound(nlocal(sound))%info%levels = nlevels
            case (221) ;           ! Pilot 
               ntotal(pilot) = ntotal(pilot) + 1
               if (outside) then
                  cycle reports
               end if

               if (.not.use_pilotobs) cycle reports
                  nlocal(pilot) = nlocal(pilot) + 1
                  ! Track serial obs index for reassembly of obs during 
                  ! bit-for-bit tests with different numbers of MPI tasks.
                  platform%loc%obs_global_index = ntotal(pilot)

                  iv%pilot(nlocal(pilot))%info = platform%info
                  iv%pilot(nlocal(pilot))%loc  = platform%loc

                  allocate (iv%pilot(nlocal(pilot))%p (1:nlevels))
                  allocate (iv%pilot(nlocal(pilot))%zk(1:nlevels))
                  allocate (iv%pilot(nlocal(pilot))%u (1:nlevels))
                  allocate (iv%pilot(nlocal(pilot))%v (1:nlevels))

                  do i = 1, nlevels
                     iv%pilot(nlocal(pilot))%p(i) = platform%each(i)%p%inv
                     iv%pilot(nlocal(pilot))%u(i) = platform%each(i)%u
                     iv%pilot(nlocal(pilot))%v(i) = platform%each(i)%v
                  end do
            end select

         case (41)
            ! case (130:131, 133, 230:231, 233) ; ! Airep
            ntotal(airep) = ntotal(airep) + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_airepobs) cycle reports

            platform  % info % platform ='FM-97 AIREP'
            nlocal(airep) = nlocal(airep) + 1
            ! Track serial obs index for reassembly of obs during 
            ! bit-for-bit tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ntotal(airep)

            iv % airep (nlocal(airep)) % info = platform % info
            iv % airep (nlocal(airep)) % loc  = platform % loc

            nlevels= iv % airep (nlocal(airep)) % info % levels
            allocate (iv % airep (nlocal(airep)) % h (1:nlevels))
            allocate (iv % airep (nlocal(airep)) % p (1:nlevels))
            allocate (iv % airep (nlocal(airep)) % zk(1:nlevels))
            allocate (iv % airep (nlocal(airep)) % u (1:nlevels))
            allocate (iv % airep (nlocal(airep)) % v (1:nlevels))
            allocate (iv % airep (nlocal(airep)) % t (1:nlevels))

            do i = 1, nlevels
               iv % airep (nlocal(airep)) % h(i) = platform % each(i) % height
               iv % airep (nlocal(airep)) % p(i) = platform % each(i) % p % inv
               iv % airep (nlocal(airep)) % u(i) = platform % each(i) % u
               iv % airep (nlocal(airep)) % v(i) = platform % each(i) % v
               iv % airep (nlocal(airep)) % t(i) = platform % each(i) % t
               iv % airep (nlocal(airep)) %zk(i) = platform % each(i) % zk
            end do

            ! case (180, 182, 280, 282) ;         ! Ships and  buoys

         case (522, 523);        ! Ships
            ntotal(ships) = ntotal(ships) + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_shipsobs) cycle reports

            platform  % info % platform ='FM-13 SHIP '
            nlocal(ships)  = nlocal(ships)  + 1
            ! Track serial obs index for reassembly of obs during 
            ! bit-for-bit tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ntotal(ships)

            iv % ships (nlocal(ships)) % info = platform % info
            iv % ships (nlocal(ships)) % loc  = platform % loc

            iv % ships (nlocal(ships)) % h = platform % each(1) % height
            iv % ships (nlocal(ships)) % u = platform % each(1) % u
            iv % ships (nlocal(ships)) % v = platform % each(1) % v
            iv % ships (nlocal(ships)) % t = platform % each(1) % t
            ! WHY?
            ! iv % ships (nlocal(ships)) % p = platform % loc     % slp
            iv % ships (nlocal(ships)) % p = platform % each(1) % p
            iv % ships (nlocal(ships)) % q = platform % each(1) % q
            iv % ships (nlocal(ships)) % zk= platform % each(1) % zk

         case (531, 561, 562) ;          ! Buoy  
            ntotal(buoy) = ntotal(buoy) + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_buoyobs) cycle reports
            platform  % info % platform ='FM-18 BUOY '
            nlocal(buoy)  = nlocal(buoy)  + 1
            platform%loc%obs_global_index = ntotal(buoy)

            iv%buoy(nlocal(buoy))%info = platform%info
            iv%buoy(nlocal(buoy))%loc  = platform%loc

            iv%buoy(nlocal(buoy))%h = platform%each(1)%height
            iv%buoy(nlocal(buoy))%u = platform%each(1)%u
            iv%buoy(nlocal(buoy))%v = platform%each(1)%v
            iv%buoy(nlocal(buoy))%t = platform%each(1)%t
            iv%buoy(nlocal(buoy))%p = platform%each(1)%p
            iv%buoy(nlocal(buoy))%q = platform%each(1)%q

         case (511)
            ! case (181, 281) ;                   ! Synop

            ntotal(synop) = ntotal(synop) + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_synopobs) cycle reports

            platform  % info % platform ='FM-12 SYNOP'

            nlocal(synop)  = nlocal(synop)  + 1
            platform%loc%obs_global_index = ntotal(synop)

            iv % synop (nlocal(synop)) % info = platform % info
            iv % synop (nlocal(synop)) % loc  = platform % loc

            iv % synop (nlocal(synop)) % h = platform % each(1) % height
            iv % synop (nlocal(synop)) % u = platform % each(1) % u
            iv % synop (nlocal(synop)) % v = platform % each(1) % v
            iv % synop (nlocal(synop)) % t = platform % each(1) % t
            ! WHY?
            ! iv % synop (nlocal(synop)) % p = platform % loc     % slp
            iv % synop (nlocal(synop)) % p = platform % each(1) % p
            iv % synop (nlocal(synop)) % q = platform % each(1) % q
            iv % synop (nlocal(synop)) % zk= platform % each(1) % zk

            if (iv % synop(nlocal(synop)) % h < iv % synop (nlocal(synop)) % info % elv) then
               iv % synop(nlocal(synop)) % h = iv % synop (nlocal(synop)) % info % elv
            end if

         case (512)
            ! case (187, 287) ;                        ! Metar
            ntotal(metar) = ntotal(metar) + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_metarobs) cycle reports

            platform  % info % platform ='FM-15 METAR'
            nlocal(metar)  = nlocal(metar)  + 1
            platform%loc%obs_global_index = ntotal(metar)

            iv % metar (nlocal(metar)) % info = platform % info
            iv % metar (nlocal(metar)) % loc  = platform % loc

            iv % metar (nlocal(metar)) % h = platform % each(1) % height
            iv % metar (nlocal(metar)) % u = platform % each(1) % u
            iv % metar (nlocal(metar)) % v = platform % each(1) % v
            iv % metar (nlocal(metar)) % t = platform % each(1) % t
            iv % metar (nlocal(metar)) % p = platform % each(1) % p
            iv % metar (nlocal(metar)) % q = platform % each(1) % q
            iv % metar (nlocal(metar)) % zk= platform % each(1) % zk

         case (63)
            ! case (242:246, 252:253, 255) ;         ! Geo. CMVs
            ntotal(geoamv) = ntotal(geoamv) + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_geoamvobs) cycle reports
            platform  % info % platform ='FM-88 SATOB'
            nlocal(geoamv) = nlocal(geoamv) + 1  
            platform%loc%obs_global_index = ntotal(geoamv)

            iv % geoamv (nlocal(geoamv)) % info = platform % info
            iv % geoamv (nlocal(geoamv)) % loc  = platform % loc
            allocate (iv%geoamv(nlocal(geoamv))%p (1:nlevels))
            allocate (iv%geoamv(nlocal(geoamv))%zk(1:nlevels))
            allocate (iv%geoamv(nlocal(geoamv))%u (1:nlevels))
            allocate (iv%geoamv(nlocal(geoamv))%v (1:nlevels))

            do i = 1, nlevels
               iv % geoamv (nlocal(geoamv)) % p(i)  = platform % each(i) % p % inv
               iv % geoamv (nlocal(geoamv)) % u(i)  = platform % each(i) % u
               iv % geoamv (nlocal(geoamv)) % v(i)  = platform % each(i) % v
                  iv % geoamv (nlocal(geoamv)) %zk(i)  = platform % each(i) % zk
            end do

         case (582)
            ntotal(qscat) = ntotal(qscat) + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_qscatobs) cycle reports
            platform  % info % platform ='FM-281 Quiks'
            nlocal(qscat) = nlocal(qscat)  + 1 
            platform%loc%obs_global_index = ntotal(qscat)

            iv%qscat(nlocal(qscat))%info = platform%info
            iv%qscat(nlocal(qscat))%loc  = platform%loc
            ! WHY?
            ! iv%qscat(nlocal(qscat))%h = platform%each(1)%height
            ! prepbufr uses pressure not height, so hardwire height to 
            ! 0 (sea-level)
            iv%qscat(nlocal(qscat))%h = 0.0
            iv%qscat(nlocal(qscat))%u = platform%each(1)%u
            iv%qscat(nlocal(qscat))%v = platform%each(1)%v
            iv%qscat(nlocal(qscat))%u%error = max(platform%each(1)%u%error,1.0)
            iv%qscat(nlocal(qscat))%v%error = max(platform%each(1)%v%error,1.0)

         case (583)       ! GPS PW
            ntotal(gpspw) = ntotal(gpspw) + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_gpspwobs) cycle reports
            platform  % info % platform ='FM-111 GPSPW'
            nlocal(gpspw)  = nlocal(gpspw) + 1
            platform%loc%obs_global_index = ntotal(gpspw)

            iv%gpspw(nlocal(gpspw))%info = platform%info
            iv%gpspw(nlocal(gpspw))%loc  = platform%loc 
            iv%gpspw(nlocal(gpspw))%tpw  = platform%loc%pw

         case (584)       ! GPS REF
            ntotal(gpsref) = ntotal(gpsref) + 1
            if (outside) then
               cycle reports
            end if

            if (.not.use_gpsrefobs) cycle reports
            platform  % info % platform ='FM-116 GPSRF'
            nlocal(gpsref)  = nlocal(gpsref) + 1
            platform%loc%obs_global_index = ntotal(gpsref)

            iv%gpsref(nlocal(gpsref))%info = platform%info
            iv%gpsref(nlocal(gpsref))%loc  = platform%loc

            allocate (iv%gpsref(nlocal(gpsref))%h (1:nlevels))
            allocate (iv%gpsref(nlocal(gpsref))%zk(1:nlevels))
            allocate (iv%gpsref(nlocal(gpsref))%ref(1:nlevels))
            allocate (iv%gpsref(nlocal(gpsref))%  p(1:nlevels))
            allocate (iv%gpsref(nlocal(gpsref))%  t(1:nlevels))
            allocate (iv%gpsref(nlocal(gpsref))%  q(1:nlevels))

            do i = 1, nlevels
               iv%gpsref(nlocal(gpsref))%h(i)   = platform%each(i)%height
               ! u is used to store ref in this routine (ascii version uses td)
               iv%gpsref(nlocal(gpsref))%ref(i) = platform%each(i)%u
               ! Keep the retrieved p and t (and q) as "field_type":
               iv%gpsref(nlocal(gpsref))%p(i)   = platform%each(i)%p
               iv%gpsref(nlocal(gpsref))%t(i)   = platform%each(i)%t
               iv%gpsref(nlocal(gpsref))%q(i)   = platform%each(i)%q
            end do

         case (71, 72)
            ! case (223, 224 )        ;         !  Profiler & VADWND - NEXRAD winds    
            ntotal(profiler) = ntotal(profiler)
            if (outside) then
               cycle reports
            end if

            if (.not.use_profilerobs) cycle reports
            platform  % info % platform ='FM-132 PRFLR'
            nlocal(profiler) = nlocal(profiler) + 1
            ! Track serial obs index for reassembly of obs during bit-for-bit
            ! tests with different numbers of MPI tasks.  
            platform%loc%obs_global_index = ntotal(profiler)

            iv%profiler(nlocal(profiler))%info = platform%info
            iv%profiler(nlocal(profiler))%loc  = platform%loc

            allocate (iv%profiler(nlocal(profiler))%p (1:nlevels))
            allocate (iv%profiler(nlocal(profiler))%zk(1:nlevels))
            allocate (iv%profiler(nlocal(profiler))%u (1:nlevels))
            allocate (iv%profiler(nlocal(profiler))%v (1:nlevels))

            do i = 1, nlevels
               iv%profiler(nlocal(profiler))%p(i) = platform%each(i)%p%inv
               iv%profiler(nlocal(profiler))%u(i) = platform%each(i)%u
               iv%profiler(nlocal(profiler))%v(i) = platform%each(i)%v
            end do

         case default 
            select case (kx)
            case (111 , 210)    ;         !  Tropical Cyclone Bogus
               ! Note Tropical cyclone Bougus is given type 135 in Obs-ascii
               ntotal(bogus) = ntotal(bogus) + 1
               if (outside) then
                  cycle reports
               end if

               if (.not.use_bogusobs) cycle reports
               platform  % info % platform ='FM-135 TCBOG'
               nlocal(bogus) = nlocal(bogus) + 1
               ! Track serial obs index for reassembly of obs during bit-for-bit
               ! tests with different numbers of MPI tasks.  
               platform%loc%obs_global_index = ntotal(bogus)

               if (nlocal(bogus) > max_bogus_input) then
                 write(unit=message(1),fmt='(A,I6,A,I6)') &
                   'Bogus #=', nlocal(bogus), ' > max_bogus_input=', max_bogus_input
                 call da_error(__FILE__,__LINE__,message(1:1))
               end if

               iv%bogus(nlocal(bogus))%info = platform%info
               iv%bogus(nlocal(bogus))%loc  = platform%loc

               allocate (iv%bogus(nlocal(bogus))%h (1:nlevels))
               allocate (iv%bogus(nlocal(bogus))%p (1:nlevels))
               allocate (iv%bogus(nlocal(bogus))%zk(1:nlevels))
               allocate (iv%bogus(nlocal(bogus))%u (1:nlevels))
               allocate (iv%bogus(nlocal(bogus))%v (1:nlevels))
               allocate (iv%bogus(nlocal(bogus))%t (1:nlevels))
               allocate (iv%bogus(nlocal(bogus))%q (1:nlevels))

               do i = 1, nlevels
                  iv%bogus(nlocal(bogus))%h(i) = platform%each(i)%height
                  iv%bogus(nlocal(bogus))%p(i) = platform%each(i)%p%inv
                  iv%bogus(nlocal(bogus))%u(i) = platform%each(i)%u
                  iv%bogus(nlocal(bogus))%v(i) = platform%each(i)%v
                  iv%bogus(nlocal(bogus))%t(i) = platform%each(i)%t
                  iv%bogus(nlocal(bogus))%q(i) = platform%each(i)%q
               end do

               iv%bogus(nlocal(bogus))%slp    = platform%loc%slp

            case default
               write(unit=message(1), fmt='(a, i12)') &
                  'unsaved obs found with type: ',kx
               call da_warning(__FILE__,__LINE__,message(1:1))
            end select
         end select

         if (global .and. n < 2) then
           if (test_wrfvar) exit dup_loop
           if (platform%loc % i >= ide) then
               platform%loc%i = platform%loc % i - ide
            else if (platform%loc % i < ids) then
               platform%loc%i = platform%loc % i + ide
            end if
            platform%loc%proc_domain = .not. platform%loc%proc_domain
         end if
      end do dup_loop   
   end do reports

   call closbf(iunit)
   close(iunit)
   call da_free_unit(iunit)

   if (trace_use) call da_trace_exit("da_read_obs_bufr")
#else
   call da_error(__FILE__,__LINE__,(/"must compile with BUFR library"/))
#endif

end subroutine da_read_obs_bufr


