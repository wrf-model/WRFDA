SUBROUTINE da_setup_obs_structures_bufr( xp, ob, iv )

!------------------------------------------------------------------------------
! PURPOSE: Define, allocate and read of observation structure.
!
! METHOD:  Define, allocate and read of observation structure.
!
! HISTORY: 05/30/2001 - read in from bufr               Wan-Shu Wu     
!
! PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   IMPLICIT NONE
   
   TYPE (xpose_type), INTENT(IN) :: xp         ! Domain decomposition vars.
   TYPE ( y_type), INTENT(OUT)  :: ob          ! Observation structure.
   TYPE (ob_type), INTENT(OUT)  :: iv          ! O-B structure.

   TYPE (each_level_type)       :: each          ! For llh to xyz           
   TYPE (multi_level_type)      :: platform   ! To use the original subroutines

   LOGICAL                      :: cycle_report
   LOGICAL                      :: inside_halo, outside    ! With respect to local domain


   character(len=40) :: obstr,drift,hdstr,qmstr,oestr
   character(len=10) :: date
   character(len=8 ) :: subset

   real,dimension(6)     :: hdr
   real,dimension(8,255) :: drf,obs,qms,oes

   integer   :: cat,zqm,pqm,qqm,tqm,wqm,pwq,landsea,ncount
   integer   :: nme,nar,nsh,nst,levs,nairep,nships,ngeoamv,npolaramv,nmetar,nso,nsy
   integer   :: i,k,nsound,iy,iret,idate,noqc,nsynop,kx
   integer   :: id,im,ih,nmrecs,nmdisc,nmindm

   integer   :: nlevels

   real      :: time,woe,qob,toe,qoe,poe,pob,tob,zob,dg2ew,dg2rad
   real      :: nodata

!-----------------------------------------------------------------------------

   hdstr='SID XOB YOB DHR TYP ELV         '
   obstr='POB QOB TOB ZOB UOB VOB PWO CAT '
   drift='XDR YDR HRDR                    '
   qmstr='PQM QQM TQM ZQM WQM NUL PWQ     '
   oestr='POE QOE TOE NUL WOE NUL PWE     '

   nodata=-888888.
   noqc=-88
   dg2rad = atan(1.0)/45.0
   dg2ew  = 360.0*dg2rad

   nsound=0
   nmetar=0
   nsynop=0
   nships=0
   nmrecs=0
   ngeoamv=0
   npolaramv=0

   nmdisc=0
   nmindm=0

!-------------------------------- 
!  open bufr file then check date        
!--------------------------------

   call datelen(10)

   call openbf(bufr_iunit,'IN',bufr_iunit)

   call readmg(bufr_iunit,subset,idate,iret)

   if(iret/=0) then
      write(unit=*, fmt='(a, i4)') &
           'return code from readmg:', iret, &
           'Reach the end of obs unit: ', bufr_iunit

      CALL wrf_shutdown

      STOP "Stopped. No observations."
   end if

   write(date,'( i10)') idate
   read (date,'(i4,3i2)') iy,im,id,ih
   write(6,*)'READ_BUFR:  bufr file date is ',iy,im,id,ih

   header_loop: do
      call readsb(bufr_iunit,iret)

      if(iret/=0) then
         call readmg(bufr_iunit,subset,idate,iret)

         if(iret/=0) then
            write(unit=*, fmt='(a, i4)') &
                 'return code from readmg:', iret, &
                 'Reach the end of obs unit: ', bufr_iunit
   
            exit header_loop
         endif

         cycle header_loop
      end if

      nmrecs=nmrecs+1

      call ufbint(bufr_iunit,hdr,6,1,iret,hdstr)

      if(hdr(2) >= 360.) hdr(2)=hdr(2)-360.
      if(hdr(2) <    0.) hdr(2)=hdr(2)+360.

      platform%info%lon = hdr(2)
      platform%info%lat = hdr(3)

      call DA_ll_to_xy (platform%info, platform%loc, &
                        xp, outside, inside_halo )

      if(print_detail > 100) then
         print *, 'nmrecs, outside, inside_halo=', &
                   nmrecs, outside, inside_halo

         write(unit=*, fmt='(2(a,2f12.4, 4x), 2(a, 2i6, 4x))') &
              'loc(lon,lat)=', platform%info%lon, platform%info%lat, &
              'loc(  x,  y)=', platform%loc%x,   platform%loc%y, &
              'loc(  i,  j)=', platform%loc%i,   platform%loc%j, &
              'Tile(ie, je)=', xp%ite, xp%jte
      endif

      if(outside .or. (.not. inside_halo)) cycle header_loop

      nmindm=nmindm+1

      kx=int(0.1+hdr(5))

      SELECT CASE(kx)
         CASE (181, 281) ;			! Synop
            nsynop=nsynop+1
         CASE (187) ;				! Metar
            nmetar=nmetar+1
         CASE (180, 182, 280, 282) ;		! Ships
            nships=nships+1
         CASE (242:246, 252:253, 255) ;         ! Geo. CMVs
            ngeoamv= ngeoamv+1
         CASE (120, 132, 220, 223, 232) ;	! Sound
            nsound=nsound+1
         CASE (130:131, 133, 230:231, 233) ;	! Airep
            nairep=nairep+1
         CASE DEFAULT ;
            nmdisc=nmdisc+1
!           write(unit=*, fmt='(/a, i8, 4x, a, e20.12/)') &
!                'Unkown kx=', kx, 'Unhandled hdr(5)=', hdr(5)
      END SELECT
   enddo header_loop

   write(6,*) 'READ_BUFR:  total bufr records =',nmrecs
   write(6,*) 'READ_BUFR:  number in domain   =',nmindm
   write(6,*) 'READ_BUFR:  number Unhandled   =',nmdisc


   write(unit=*, fmt='(a, i8)') &
        'iv%num_synop=', nsynop, &
        'iv%num_sound=', nsound, &
        'iv%num_metar=', nmetar, &
        'iv%num_ships=', nships, &
        'iv%num_airep=', nairep, &
        'iv%num_geoamv=', ngeoamv, &
        'Discarded ob=', nmdisc

   if (.not.Use_SynopObs ) nsynop=0
   if (.not.Use_MetarObs ) nmetar=0
   if (.not.Use_SoundObs ) nsound=0
   if (.not.Use_ShipsObs ) nships=0
   if (.not.Use_AirepObs ) nairep=0
   if (.not.Use_geoamvObs ) ngeoamv=0
   if (.not.Use_polaramvObs ) npolaramv=0


   iv % num_pilot = 0              
   iv % num_satem = 0              
   iv % num_gpspw = 0              
   iv % num_ssmi_tb        = 0
   iv % num_ssmi_retrieval = 0
  
   iv % missing = 0
   iv % ptop    = 0

   iv%num_synop=nsynop
   iv%num_sound=nsound
   iv%num_metar=nmetar
   iv%num_ships=nships
   iv%num_airep=nairep
   iv%num_geoamv=ngeoamv
   iv%num_polaramv=npolaramv


   if(iv%num_sound > 0) then
      ALLOCATE (iv % sound(1:iv%num_sound))
      ALLOCATE (iv % sonde_sfc(1:iv%num_sound))
   endif

   if(iv%num_synop > 0) &
      ALLOCATE (iv % synop(1:iv%num_synop))
   if(iv%num_metar > 0) &
      ALLOCATE (iv % metar(1:iv%num_metar))
   if(iv%num_airep > 0) &
      ALLOCATE (iv % airep(1:iv%num_airep))
   if(iv%num_geoamv > 0) &
      ALLOCATE (iv % geoamv(1:iv%num_geoamv))
   if(iv%num_polaramv > 0) &
      ALLOCATE (iv % polaramv(1:iv%num_polaramv))
   if(iv%num_ships > 0) &
      ALLOCATE (iv % ships(1:iv%num_ships))

   nme=0;nsy=0;nso=0;nsh=0;nar=0;nst=0

   iv%total_obs=0                                    

!-----------------------------------------------------------------------------
!  READ MAIN BODY OF GTS OBSERVATION FILE
!------------------------------------------------------------------------------

   rewind(bufr_iunit)

!--Big loop over buffer file

   buffer_loop: do
      call readsb(bufr_iunit,iret)

      if(iret/=0) then
         call readmg(bufr_iunit,subset,idate,iret)

         if(iret/=0) then
            write(unit=*, fmt='(a, i4)') &
                 'return code from readmg:', iret, &
                 'Reach the end of obs unit: ', bufr_iunit

            exit buffer_loop
         endif

         cycle buffer_loop
      end if

      call ufbint(bufr_iunit,hdr,6,1,iret,hdstr)
      
      platform  % info % name(1:8) = subset
      platform  % info % id        = hdstr(1:5)

      if(hdr(2) >= 360.) hdr(2)=hdr(2)-360.
      if(hdr(2) <    0.) hdr(2)=hdr(2)+360.

      platform%info%lon = hdr(2)
      platform%info%lat = hdr(3)

      call DA_ll_to_xy (platform%info, platform%loc, &
                        xp, outside, inside_halo )

      if(outside .or. (.not. inside_halo) ) cycle buffer_loop

      time=hdr(4)
      if(time >  3.) time=3.
      if(time < -3.) time=-3.

      kx=int(0.1+hdr(5))

      if((kx >= 160) .and. (kx <= 179)) then	! sat data
         cycle buffer_loop
      endif

      iv % total_obs = iv % total_obs + 1

!-----Conventional data

      call ufbint(bufr_iunit,obs,8,255,levs,obstr)
      call ufbint(bufr_iunit,qms,8,255,levs,qmstr)
      call ufbint(bufr_iunit,oes,8,255,levs,oestr)

      SELECT CASE(kx)
         CASE ( 120, 132, 220:221, 232) ;	! read drift
            call ufbint(bufr_iunit,drf,8,255,iret,drift)
            do k=1,levs
               if(drf(1,k) .ge. 360.)drf(1,k)=drf(1,k)-360.
               if(drf(1,k) .lt. 0.)drf(1,k)=drf(1,k)+360.
               if(abs(drf(2,k)) .gt. 1000. .or. abs(drf(1,k)) .gt. 1000.)then
                 drf(2,k)=hdr(3)
                 drf(1,k)=hdr(2)
               end if
               if(abs(drf(3,k)) .gt. 3.)then
                 drf(3,k)=time
               end if
            end do
         CASE DEFAULT ;                      	! Do nothing
      END SELECT

      platform  % info % levels   =levs
      platform  % info % lon      =hdr(2)
      platform  % info % lat      =hdr(3)
      platform  % info % elv      =hdr(6)

      platform % loc % slp %inv  =nodata         
      platform % loc % slp %qc   =noqc
      platform % loc % slp %error=200.           
      platform % loc % pw %inv  =nodata         
      platform % loc % pw %qc   =noqc
      platform % loc % pw %error=.2             

      loop_init_value: do i=1,max_ob_levels
         platform % each (i) % height  =nodata
         platform % each (i) % height_qc =noqc

         platform % each (i) % zk = missing_r
            
         platform % each (i) % u % inv =nodata
         platform % each (i) % u % qc  =noqc
         platform % each (i) % u % error =10.       

         platform % each (i) % v = platform % each (i) % u

         platform % each (i) % t % inv =nodata
         platform % each (i) % t % qc  =noqc
         platform % each (i) % t % error =5.       

         platform % each (i) % p % inv =nodata          
         platform % each (i) % p % qc  =noqc
         platform % each (i) % p % error =200.    

         platform % each (i) % q % inv =nodata
         platform % each (i) % q % qc  =noqc
         platform % each (i) % q % error =.1      
      enddo loop_init_value

      loop_level: DO k = 1, platform % info % levels
         pob=obs(1,k)
         qob=obs(2,k)*1e-6
         tob=obs(3,k)
         zob=obs(4,k)

         pqm=nint(qms(1,k))
         qqm=nint(qms(2,k))
         tqm=nint(qms(3,k))
         zqm=nint(qms(4,k))
         wqm=nint(qms(5,k))
         pwq=nint(qms(7,k))
         cat=nint(obs(8,k))


         if(cat==0.and.zqm<4.and.pqm<4)then
            poe=oes(1,k)*1e-3
            if(pqm==0) poe=poe*0.9
            if(pqm==3) poe=poe*1.2
            platform % loc % slp % inv =obs(1,k)*100.
            platform % loc % slp % qc  =pqm
            platform % loc % slp % error =poe
         endif

         poe=100.
         if(pqm==0) poe=poe*0.9
         if(pqm==3) poe=poe*1.2

         if(tqm<4.and.pqm<4) then
            toe=oes(3,k)
            if(tqm==0) toe=toe*0.9
            if(tqm==3) toe=toe*1.2
            platform % each (k) % t % inv =obs(3,k)+273.15
            platform % each (k) % t % qc  =tqm
            platform % each (k) % t % error =toe
         endif

         if(wqm<4.and.pqm<4) then
            woe=oes(5,k)
            if(wqm==0) woe=woe*0.9
            if(wqm==3) woe=woe*1.2
            platform % each (k) % u % qc  =wqm
            platform % each (k) % u % error =woe
            platform % each (k) % v % qc  =wqm
            platform % each (k) % v % error =woe

!           platform % each (k) % u % inv =obs(5,k)
!           platform % each (k) % v % inv =obs(6,k)
!-----------Convert earth wind to model wind.
            call earth_2_model_wind(obs(5,k), obs(6,k), &
                 platform % each (k) % u % inv, &
                 platform % each (k) % v % inv, &
                 platform%info%lon )
         endif

         if(qqm<4.and.pqm<4.and.obs(2,k)<1.0e9) then
            qoe=oes(2,k)*0.1
            if(qqm==0) qoe=qoe*0.9
            if(qqm==3) qoe=qoe*1.2
            platform % each (k) % q % inv =obs(2,k)*1e-6
            platform % each (k) % q % qc  =qqm
            platform % each (k) % q % error =qoe
         endif

         if(zqm<4)then
            platform % each (k) % height  =zob
            platform % each (k) % height_qc =zqm
         endif

         if(pqm<4)then
            platform % each (k) % p % inv =obs(1,k)*100.
            platform % each (k) % p % qc  =pqm
            platform % each (k) % p % error =poe
         endif

      ENDDO loop_level

      nlevels = platform%info%levels

      if(nlevels > max_ob_levels) then
         nlevels = max_ob_levels

         write(unit=*, fmt='(/a/)') &
               'WARNING: Too many levels.'

          write(unit=*, fmt='(/2a/2a/2x,a,2f8.2,2(2x,a,f9.2)/2(2x,a,i4)/)') &
               'Subset:   ', platform%info%name(1:8), &
               'Platfrom: ', trim(platform%info%platform), &
               'Loc(lat, lon): ', platform%info%lat, platform%info%lon, &
               'elv:   ', platform%info%elv, &
               'pstar: ', platform%info%pstar, &
               'level: ', platform%info%levels, &
               'kx:    ', kx
       else if( (nlevels < 1) .and. ((kx /= 164) .or. (kx /= 174))) then
          write(unit=*, fmt='(/a/)') &
               'WARNING: Too few levels.'
  
          write(unit=*, fmt='(/2a/2a/2x,a,2f8.2,2(2x,a,f9.2)/2(2x,a,i4)/)') &
               'Subset:   ', platform%info%name(1:8), &
               'Platfrom: ', trim(platform%info%platform), &
               'Loc(lat, lon): ', platform%info%lat, platform%info%lon, &
               'elv:   ', platform%info%elv, &
               'pstar: ', platform%info%pstar, &
               'level: ', platform%info%levels, &
               'kx:    ', kx

           cycle buffer_loop
      endif

!---------------------------------------------------------------------------
      CALL DA_Obs_Proc_Station(platform)
!---------------------------------------------------------------------------

      SELECT CASE(kx)
         CASE (120, 132, 220, 223, 232) ;    ! Sound
            if (.not.Use_SoundObs) cycle buffer_loop

            platform  % info % platform ='FM-035 SOUND'

            nso=nso+1
            iv % sound ( nso ) % info = platform % info
            iv % sound ( nso ) % loc  = platform % loc

            iv % sonde_sfc ( nso ) % info = platform % info
            iv % sonde_sfc ( nso ) % loc  = platform % loc
            iv % sonde_sfc ( nso ) % h    = platform  % info % elv
            iv % sonde_sfc ( nso ) % zk   = missing_r

            iv % sonde_sfc ( nso ) % p    = platform % loc % slp

            iv % sonde_sfc ( nso ) % u % inv   = missing_r
            iv % sonde_sfc ( nso ) % u % qc    = missing
            iv % sonde_sfc ( nso ) % u % error = missing_r
            iv % sonde_sfc ( nso ) % v = iv % sonde_sfc ( nso ) % u
            iv % sonde_sfc ( nso ) % t = iv % sonde_sfc ( nso ) % u
            iv % sonde_sfc ( nso ) % q = iv % sonde_sfc ( nso ) % u

            levs=iv % sound ( nso ) % info % levels
            allocate (iv % sound ( nso ) % h(1:levs))
            allocate (iv % sound ( nso ) %zk(1:levs))
            allocate (iv % sound ( nso ) % u(1:levs))
            allocate (iv % sound ( nso ) % v(1:levs))
            allocate (iv % sound ( nso ) % t(1:levs))
            allocate (iv % sound ( nso ) % p(1:levs))
            allocate (iv % sound ( nso ) % q(1:levs))

            do i = 1, levs
               iv % sound ( nso ) % h(i) = platform % each(i) % height
               iv % sound ( nso ) % p(i) = platform % each(i) % p % inv
               iv % sound ( nso ) % u(i) = platform % each(i) % u
               iv % sound ( nso ) % v(i) = platform % each(i) % v
               iv % sound ( nso ) % t(i) = platform % each(i) % t
               iv % sound ( nso ) % q(i) = platform % each(i) % q
               iv % sound ( nso ) %zk(i) = platform % each(i) % zk
            enddo

         CASE (130:131, 133, 230:231, 233) ; ! Airep
            if (.not.Use_AirepObs) cycle buffer_loop

            platform  % info % platform ='FM-096 AIREP'

            nar=nar+1
            iv % airep ( nar ) % info = platform % info
            iv % airep ( nar ) % loc  = platform % loc

            levs= iv % airep ( nar ) % info % levels
            allocate (iv % airep (nar) % h (1:levs))
            allocate (iv % airep (nar) % p (1:levs))
            allocate (iv % airep (nar) % zk(1:levs))
            allocate (iv % airep (nar) % u (1:levs))
            allocate (iv % airep (nar) % v (1:levs))
            allocate (iv % airep (nar) % t (1:levs))

            do i = 1, levs
               iv % airep ( nar ) % h(i) = platform % each(i) % height
               iv % airep ( nar ) % p(i) = platform % each(i) % p % inv
               iv % airep ( nar ) % u(i) = platform % each(i) % u
               iv % airep ( nar ) % v(i) = platform % each(i) % v
               iv % airep ( nar ) % t(i) = platform % each(i) % t
               iv % airep ( nar ) %zk(i) = platform % each(i) % zk
            enddo

         CASE (180, 182, 280, 282) ;         ! Ships
            if (.not.Use_ShipsObs) cycle buffer_loop

            platform  % info % platform ='FM-013 SHIP '

            nsh=nsh+1
            iv % ships ( nsh ) % info = platform % info
            iv % ships ( nsh ) % loc  = platform % loc

            iv % ships ( nsh ) % h = platform % each(1) % height
            iv % ships ( nsh ) % u = platform % each(1) % u
            iv % ships ( nsh ) % v = platform % each(1) % v
            iv % ships ( nsh ) % t = platform % each(1) % t
            iv % ships ( nsh ) % p = platform % loc     % slp
            iv % ships ( nsh ) % q = platform % each(1) % q
            iv % ships ( nsh ) % zk= platform % each(1) % zk
 
         CASE (181, 281) ;                   ! Synop
            if (.not.Use_SynopObs) cycle buffer_loop
            if (platform%loc%slp%qc>=4 .or. platform%loc%slp%qc<0)cycle buffer_loop

            platform  % info % platform ='FM-012 SYNOP'

            nsy=nsy+1
            iv % synop ( nsy ) % info = platform % info
            iv % synop ( nsy ) % loc  = platform % loc
     
            iv % synop ( nsy ) % h = platform % each(1) % height
            iv % synop ( nsy ) % u = platform % each(1) % u
            iv % synop ( nsy ) % v = platform % each(1) % v
            iv % synop ( nsy ) % t = platform % each(1) % t
            iv % synop ( nsy ) % p = platform % loc     % slp
            iv % synop ( nsy ) % q = platform % each(1) % q
            iv % synop ( nsy ) % zk= platform % each(1) % zk

            if(iv % synop(nsy) % h < iv % synop (nsy) % info % elv) then
               iv % synop(nsy) % h = iv % synop (nsy) % info % elv
            endif

         CASE (187) ;                        ! Metar
            if (.not.Use_MetarObs) cycle buffer_loop

            platform  % info % platform ='FM-015 METAR'

            nme=nme+1
            iv % metar ( nme ) % info = platform % info
            iv % metar ( nme ) % loc  = platform % loc
        
            iv % metar ( nme ) % h = platform % each(1) % height
            iv % metar ( nme ) % u = platform % each(1) % u
            iv % metar ( nme ) % v = platform % each(1) % v
            iv % metar ( nme ) % t = platform % each(1) % t
            iv % metar ( nme ) % p = platform % each(1) % p
            iv % metar ( nme ) % q = platform % each(1) % q
            iv % metar ( nme ) % zk= platform % each(1) % zk
     

         CASE (242:246, 252:253, 255) ;         ! Geo. CMVs
            if (.not.Use_geoamvObs) cycle buffer_loop
            platform  % info % platform ='FM-088 SATOB'
            nst=nst+1
            iv % geoamv (nst) % info = platform % info
            iv % geoamv (nst) % loc  = platform % loc

            levs= iv % geoamv ( nst ) % info % levels

            do i = 1, levs
            iv % geoamv (nst) % p(k)  = platform % each(i) % p % inv
            iv % geoamv (nst) % u(k)  = platform % each(i) % u
            iv % geoamv (nst) % v(k)  = platform % each(i) % v
            iv % geoamv (nst) % zk(k) = platform % each(i) % zk
            end do


         CASE (150, 152, 283) ;			! SPSSMI
            cycle buffer_loop
         CASE (221) ;				! ADPUPA
            cycle buffer_loop
         CASE (224) ;				! VADWND
            cycle buffer_loop
         CASE (251) ;				! SATWND
            cycle buffer_loop
         CASE (285) ;				! QKSWND
            cycle buffer_loop
         CASE (183, 284, 287) ;			! ADPSFC
            cycle buffer_loop
         CASE (160:179) ;			! Sat
!           sat= (kx>=160.and.kx<=179)
            cycle buffer_loop
!        CASE (164:165, 174:175) ;		! GOESND
!           goessat= (kx==164.or.kx==165.or.kx==174.or.kx==175)
!           cycle buffer_loop
         CASE DEFAULT ;
            platform  % info % platform ='FM-999 UNKNO'

            write(unit=*, fmt='(/a, i8, 4x, a, e20.12/)') &
                 'Unkown kx=', kx, 'Unhandled hdr(5)=', hdr(5)
            write(unit=*, fmt='(3a, i8)') &
                 'file:', __FILE__, ', line:', __LINE__
            write(unit=*, fmt='(2(2x,a))') &
                 'subset:', trim(subset)
            write(unit=*, fmt='(2(2x,a))') &
                 'hdstr:', trim(hdstr)
            write(unit=*, fmt='(2(2x,a))') &
                 'obstr:', trim(obstr)
            write(unit=*, fmt='(2x,a,i12))') &
                 'idate:', idate
            write(unit=*, fmt='(2x,a,2f8.2/2(2x,a,f9.2)/2x,a,i4/)') &
               'Loc(lat, lon): ', platform%info%lat, platform%info%lon, &
               'elv:  ', platform%info%elv, &
               'pstar:', platform%info%pstar, &
               'levx: ', levs

            do k = 1, levs
               write(unit=*, fmt='(2x,a,i4)') 'lev:', k

               write(unit=*, fmt='(2x,a,e20.12)') &
                    'p obs(1,k)=', obs(1,k), &
                    'q obs(2,k)=', obs(2,k), &
                    't obs(3,k)=', obs(3,k), &
                    'z obs(4,k)=', obs(4,k), &
                    'u obs(5,k)=', obs(5,k), &
                    'v obs(6,k)=', obs(6,k), &
                    'p obs(7,k)=', obs(7,k), &
                    'c obs(8,k)=', obs(8,k)

               write(unit=*, fmt='(/)')

               write(unit=*, fmt='(2x,a,i12)') &
                    'p qms(1,k)=', nint(qms(1,k),8), &
                    'q qms(2,k)=', nint(qms(2,k),8), &
                    't qms(3,k)=', nint(qms(3,k),8), &
                    'z qms(4,k)=', nint(qms(4,k),8), &
                    'u qms(5,k)=', nint(qms(5,k),8), &
                    'v qms(6,k)=', nint(qms(6,k),8), &
                    'p qms(7,k)=', nint(qms(7,k),8), &
                    'c qms(8,k)=', nint(qms(8,k),8)
            enddo
      END SELECT

!---------------------------------------------------------------------------

   enddo buffer_loop

!-----------------------------------------------------------------------
   call closbf(bufr_iunit)
   close(bufr_iunit)
!-----------------------------------------------------------------------

   write(6,*) 'READ_BUFR:  number of bufr data records read  =',nmrecs

   if(iv%num_sound > nso) &
      iv%sound(nso+1:iv%num_sound)%info%levels=0
   if(iv%num_airep > nar) &
      iv%airep(nar+1:iv%num_airep)%info%levels=0
   if(iv%num_pilot > 0) &
      iv%pilot(1:iv%num_pilot)%info%levels=0

   iv%num_synop=nsy
   iv%num_metar=nme
   iv%num_sound=nso
   iv%num_ships=nsh
   iv%num_airep=nar
   iv%num_geoamv=nst
   iv%num_polaramv=nst


   write(6,*)'obs no with bufr=',nsy,nso,nsh,nar,nst,nme

!  PRINT OUT
!  =============

   write(unit=*, fmt='(a)')  ' '

   write(unit=*, fmt='(5x,a,i6,a)') &
        'Read:  ', iv % num_sound, ' SOUND reports,', &
        'Read:  ', iv % num_synop, ' SYNOP reports,', &
        'Read:  ', iv % num_pilot, ' PILOT reports,', &
        'Read:  ', iv % num_geoamv,' Geo. AMVs reports,', &
        'Read:  ', iv % num_polaramv,' Polat AMVs reports,', &
        'Read:  ', iv % num_satem, ' SATEM reports,', &
        'Read:  ', iv % num_airep, ' AIREP reports,', &
        'Read:  ', iv % num_gpspw, ' GPSPW reports,', &
        'Read:  ', iv % num_metar, ' METAR reports,', &
        'Read:  ', iv % num_ships , ' SHIP  reports,', &
        'Read:  ', iv % num_ssmi_retrieval , ' SSMI_RETRIEVAL reports,', &
        'Read:  ', iv % num_ssmi_tb       , ' SSMI_TB       reports,', &
        'Read:  ', iv % total_obs, ' Total Observations.', &
     'There are ', iv % total_obs - iv % num_sound - iv % num_synop &
                 - iv % num_geoamv - iv%num_polaramv - iv % num_airep &
                 - iv % num_pilot - iv % num_satem &
                 - iv % num_metar - iv % num_ships &
                 - iv % num_ssmi_retrieval  &
                 - iv % num_ssmi_tb        &
                 - iv % num_gpspw, '  Observations unsaved.'

!-----------------------------------------------------------------------------
!  [3.0] Calculate innovation vector (O-B) and create (smaller) ob structure:
!-----------------------------------------------------------------------------

   CALL da_fill_obs_structures( iv, ob )

END SUBROUTINE da_setup_obs_structures_bufr

