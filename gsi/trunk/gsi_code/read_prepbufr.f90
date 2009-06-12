subroutine read_prepbufr(nread,ndata,nodata,infile,obstype,lunout,twindin,sis,&
     prsl_full)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_prepbuf                read obs from prepbufr file
!   prgmmr: parrish          org: np22                date: 1990-10-07
!
! abstract:  This routine reads conventional data found in the prepbufr
!            file.  Specific observation types read by this routine 
!            include surface pressure, temperature, winds (components
!            and speeds), moisture and total precipitable water.  
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   1990-10-07  parrish
!   1998-05-15  weiyu yang 
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-02-13  derber, j. - clean up and modify vertical weighting
!   2004-06-16  treadon - update documentation
!   2004-07-23  derber  - modify to include conventional sst
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-07-30  derber  - generalize number of data records per obs type
!   2004-08-26  derber  - fix many errors in reading of sst data
!   2004-08-27  kleist  - modify pressure calculation
!   2004-10-28  kleist  - correct array index bug in hybrid pressure calculation
!   2004-11-16  treadon - deallocate(etabl) prior to exiting routine
!   2005-02-10  treadon - add call destroygrids for obstype = sst
!   2005-05-24  pondeca - add surface analysis option
!   2005-02-24  treadon - remove hardwired setting of metar ps obs error
!   2005-05-27  derber  - reduce t, uv, ps error limits
!   2005-05-27  kleist/derber - add option to read in new ob error table
!   2005-07-19  derber - clean up code a bit
!   2005-07-27  derber  - add print of monitoring and reject data
!   2005-08-02  derber - modify to use convinfo file
!   2005-09-08  derber - modify to use input group time window
!   2005-10-11  treadon - change convinfo read to free format
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-10-26  treadon - add routine tag to convinfo printout
!   2006-02-03  derber  - modify to count read/keep data and new obs control
!   2006-02-03  treadon - use interpolated guess 3d pressure field in errormod
!   2006-02-08  derber  - modify to use new convinfo module
!   2006-02-09  treadon - save height for wind observations
!   2006-02-23  kistler - modify to add optional data thinning
!   2006-02-23  kistler - raob instument as subtype and solar elv angle computed
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-04-03  derber  - modify to properly handle height of surface obs
!   2006-04-05  wu - changes to read in GPS IPW (type 153)
!   2006-05-18  middlecoff/treadon - add huge_i_kind upper limit on nint
!   2006-05-29  treadon - increase nreal to pass more information to setup routines
!   2006-06-08  su - added the option to turn off oiqc
!   2006-06-21  wu - deallocate etabl array
!   2006-07-28  derber  - temporarily add subtype for meteosat winds based on sat ID
!   2006-07-31  kleist  - change to surface pressure ob error from ln(ps) to ps(cb)
!   2007-02-13  parrish - add ability to use obs files with ref time different from analysis time
!   2007-02-20  wu - correct errors in quality mark checks
!   2007-03-15  su - remove the error table reading part to a subroutine
!   2007-04-24  wu - add TAMDAR (134) to be used as sensible T
!   2007-05-17  kleist - generalize flag for virtual/sensible temperature obs
!   2008-08-25  hu - add code to read cloud observation from METAR and NESDIS cloud products
!
!   input argument list:
!     infile   - unit from which to read BUFR data
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!     prsl_full- 3d pressure on full domain grid
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     nodata   - number of individual "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!     twindin  - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,r_double,i_kind
  use constants, only: zero,one_tenth,one,deg2rad,fv,t0c,half,&
       three,four,rad2deg,tiny_r_kind,huge_r_kind,huge_i_kind
  use gridmod, only: diagnostic_reg,regional,nlon,nlat,nsig,&
       tll2xy,txy2ll,rotate_wind_ll2xy,rotate_wind_xy2ll,&
       rlats,rlons,twodvar_regional
  use convinfo, only: nconvtype,ctwind,cgross,cermax,cermin,cvar_b,cvar_pg, &
        ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,ioctype, &
		ithin_conv,rmesh_conv, npred_conv, &
		id_bias_ps,id_bias_t,id_bias_spd,conv_bias_ps,conv_bias_t,conv_bias_spd

  use obsmod, only: iadate,oberrflg,offtime_data
  use converr,only: etabl
  use qcmod, only: errormod,noiqc
  use convthin, only: make3grids,map3grids,thin_count,kount,del3grids

  implicit none

! Declare passed variables
  character(10),intent(in):: infile,obstype
  character(20),intent(in):: sis
  integer(i_kind),intent(in):: lunout
  integer(i_kind),intent(inout):: nread,ndata,nodata
  real(r_kind),intent(in):: twindin
  real(r_kind),dimension(nlat,nlon,nsig),intent(in):: prsl_full

! Declare local parameters
  real(r_kind),parameter:: r0_001=0.001_r_kind
  real(r_kind),parameter:: r0_5 = 0.5_r_kind
  real(r_kind),parameter:: r0_75 = 0.75_r_kind
  real(r_kind),parameter:: r0_7 = 0.7_r_kind
  real(r_kind),parameter:: r0_8 = 0.8_r_kind
  real(r_kind),parameter:: r1_2 = 1.2_r_kind
  real(r_kind),parameter:: r1_5 = 1.5_r_kind
  real(r_kind),parameter:: r10  = 10.0_r_kind
  real(r_kind),parameter:: r20  = 20.0_r_kind
  real(r_kind),parameter:: r50  = 50.0_r_kind
  real(r_kind),parameter:: r90  = 90.0_r_kind
  real(r_kind),parameter:: r100 = 100.0_r_kind
  real(r_kind),parameter:: r300 = 300.0_r_kind
  real(r_kind),parameter:: r360 = 360.0_r_kind
  real(r_kind),parameter:: r500 = 500.0_r_kind
  real(r_kind),parameter:: r999 = 999.0_r_kind
  real(r_kind),parameter:: r1100= 1100.0_r_kind
  real(r_kind),parameter:: r2000= 2000.0_r_kind
  real(r_kind),parameter:: convert= 1.0e-6_r_kind
  real(r_kind),parameter:: emerr= 0.2_r_kind

! Declare local variables
  logical tob,qob,uvob,spdob,sstob,pwob,psob
  logical metarcldobs,geosctpobs
  logical outside,driftl,convobs,inflate_error
  logical listexist,lprov !MPondeca

  character(40) drift,hdstr,qcstr,oestr,sststr
  character(40) metarcldstr,geoscldstr,metarvisstr,metarwthstr
  character(80) obstr
  character(10) date
  character(8) subset

  character(8) prvstr,sprvstr     !MPondeca
  character(8) c_prvstg,c_sprvstg !MPondeca
  character(16) cprovider(200)    !MPondeca
  character(80) cstring           !MPondeca

  integer(i_kind) io,lunin,i,maxobs,sstmeas,j
  integer(i_kind) kk,klon1,klat1,klonp1,klatp1
  integer(i_kind) nc,id,ier
  integer(i_kind) jdate,ihh,idd,idate,iret,im,iy,k,levs
  integer(i_kind) metarcldlevs,metarwthlevs
  integer(i_kind) kx,nreal,nchanl,ilat,ilon,ithin
  integer(i_kind) cat,zqm,pwq,sstq,qm,lim_qm,lim_zqm
  integer(i_kind) lim_tqm,lim_qqm
  integer(i_kind),dimension(255):: pqm,qqm,tqm,wqm

! bias and thinning variables
  logical luse
  integer(i_kind) n,nda,itt,ntmp,itx,iobsout,ip,iprint/0/

  integer(i_kind) mkx(1000)
  real(r_kind) crit1,timedif,pflag,xmesh,pmesh
! solar angle variables
  real(r_kind) dangl,sdgl,cdgl,tsnoon,solar
  integer(i_kind) idayr,idaysy

  real(r_kind) time,timex,time_drift
  real(r_kind) qtflg,tdry,rmesh,ediff,usage
  real(r_kind) u0,v0,uob,vob,dx,dy,dx1,dy1,w00,w10,w01,w11
  real(r_kind) qoe,qobcon,pwoe,pwmerr,dlnpob,ppb,poe,qmaxerr,wtype
  real(r_kind) toe,woe,errout,oelev,dlat,dlon,sstoe,dlat_earth,dlon_earth
  real(r_kind) selev,elev,stnelev
  real(r_kind),dimension(nsig):: presl
  real(r_kind),dimension(nsig-1):: dpres
  real(r_kind),allocatable,dimension(:,:):: cdata_all

  real(r_double) rstation_id

  real(r_double),dimension(8):: hdr
  real(r_double),dimension(8,255):: drfdat,qcmark,obserr
  real(r_double),dimension(9,255):: obsdat
  real(r_double),dimension(8,1):: sstdat
  real(r_double),dimension(2,10):: metarcld
  real(r_double),dimension(1,10):: metarwth
  real(r_double),dimension(1,1) :: metarvis
  real(r_double),dimension(4,1) :: geoscld
  real(r_double),dimension(1,1):: r_prvstg,r_sprvstg !MPondeca
  real(r_kind),dimension(255)::plevs
  real(r_kind),dimension(1000):: twind,gross,ermax,ermin,var_b,var_pg
  integer(i_kind),dimension(1000):: itype,iuse,numgrp,ngroup,nmiter

  real(r_kind),dimension(255):: tvflg
  real(r_double),dimension(255,20):: tpc
  real(r_double) vtcd
  real(r_kind) bmiss/10.e10/
  
  real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
  real(r_kind) vdisterr,vdisterrmax,u00,v00
  integer(i_kind) ntest,nvtest,iosub,ixsub,isubsub,iosubsub,iobsub

  integer(i_kind) ierrcode,numbcast,kl,k1,k2
  integer(i_kind) l,m,ikx,itypex
  integer(i_kind) nprov! MPondeca
  real(r_kind) del,terrmin,werrmin,perrmin,qerrmin,pwerrmin

  integer(i_kind) idate5(5),minobs,minan
  real(r_kind) time_correction

  equivalence(r_prvstg(1,1),c_prvstg) !MPondeca
  equivalence(r_sprvstg(1,1),c_sprvstg) !MPondeca

  data hdstr  /'SID XOB YOB DHR TYP ELV SAID '/
  data obstr  /'POB QOB TOB ZOB UOB VOB PWO CAT PRSS' /
  data drift  /'XDR YDR HRDR                    '/
  data sststr /'MSST DBSS SST1 SSTQM SSTOE           '/
  data qcstr  /'PQM QQM TQM ZQM WQM NUL PWQ     '/
  data oestr  /'POE QOE TOE NUL WOE NUL PWE     '/
  data prvstr /'PRVSTG'/                           !MPondeca
  data sprvstr /'SPRVSTG'/                         !MPondeca
  data metarcldstr /'CLAM HOCB'/      ! cloud amount and cloud base height
  data metarwthstr /'PRWE'/      ! present weather
  data metarvisstr /'HOVI'/ ! visibility
  data geoscldstr /'CDTP TOCC GCDTT CDTP_QM'/   ! NESDIS cloud products: cloud top pressure, temperature,amount

  data lunin / 13 /
  data ithin / -9 /
  data rmesh / -99.999 /
  
!**************************************************************************
! added by jw for speedup
  integer(i_kind) ntb,imsg,isub,ntab,i1st,j1st,iunit
  integer(i_kind),parameter:: mxtb=5000000           
  real(8) tab(3,mxtb)
! added by woollen, kistler for speedup
  integer(i_kind),parameter:: nmsgmax=10000 ! max message count
  logical*1 lmsg(nmsgmax,100:299)           ! set true when convinfo entry id found in a message
  integer(i_kind),dimension(nmsgmax):: nrep ! number of reports per message - enables skipping
  integer(i_kind) nmsg,match                ! message index, match index
!**************************************************************************
! Initialize variables

  maxobs=2e5
  lmsg = .false.
  nreal=0
  tob = obstype == 't'
  uvob = obstype == 'uv'
  spdob = obstype == 'spd'
  psob = obstype == 'ps'
  qob = obstype == 'q'
  pwob = obstype == 'pw'
  sstob = obstype == 'sst'
  metarcldobs = obstype == 'mta_cld'
  geosctpobs = obstype == 'gos_ctp'
  convobs = tob .or. uvob .or. spdob .or. qob
  if(tob)then
     nreal=16
  else if(uvob) then 
     nreal=16
  else if(spdob) then
     nreal=16
  else if(psob) then
     nreal=15
  else if(qob) then
     nreal=17
  else if(pwob) then
     nreal=16
  else if(sstob) then
     nreal=16
  else if(metarcldobs) then
     nreal=20
  else if(geosctpobs) then
     nreal=7
  else 
     write(6,*) ' illegal obs type in READ_PREPBUFR '
     call stop2(94)
  end if


  allocate(cdata_all(nreal,maxobs))

! Read in mesonet provider names from the uselist          !MPondeca  - start block
 nprov=0
 inquire(file='mesonetuselist',exist=listexist)
 if(listexist) then
     open (88,file='mesonetuselist',form='formatted')
     do m=1,3
      read(88,*,end=135) cstring
     enddo
     nprov=0
130  continue
     nprov=nprov+1
     read(88,*,end=135) cprovider(nprov)
     goto 130
135  continue
     nprov=nprov-1
  endif                                             !MPondeca  - end block

!------------------------------------------------------------------------
! Open, then read date from bufr data
  call closbf(lunin)
  open(lunin,file=infile,form='unformatted')
  call openbf(lunin,'IN',lunin)
  call datelen(10)
  call readmg(lunin,subset,idate,iret)

! Obtain program code (VTCD) associated with "VIRTMP" step
  call ufbqcd(lunin,'VIRTMP',vtcd)

  nmsg=1
  ntb = 0
  loop_report: do 
	 call readsb(lunin,iret)
	 if(iret/=0) then
		call readmg(lunin,subset,idate,iret)
		if(iret/=0) then
			exit loop_report ! end of file
		else
		  nmsg=nmsg+1
		  nrep(nmsg) = 0
		  if (nmsg>nmsgmax) then
			write(6,*)'READ_PREPBUFR: messages exceed maximum ',nmsgmax
			call stop2(50)
		  endif
		endif
		cycle loop_report
	 else
		nrep(nmsg)=nrep(nmsg)+1  ! count reports per message 
	 endif
	 !    Extract type, date, and location information
	 call ufbint(lunin,hdr,7,1,iret,hdstr)
	 kx=hdr(5)
	 ntb = ntb+1
	 if (ntb>mxtb) then
		write(6,*)'READ_PREPBUFR: reports exceed maximum ',mxtb
		call stop2(50)
	 endif
	 tab(1,ntb)=kx
	 lmsg(nmsg,kx) = .true.
  enddo loop_report
  write(6,*)'READ_PREPBUFR: messages/reports = ',nmsg,'/',ntb
!------------------------------------------------------------------------

! loop over convinfo file entries; operate on matches
  
  nread=0
  loop_convinfo: do nc=1, nconvtype
  if ( trim(ioctype(nc)) /= trim(obstype) ) cycle loop_convinfo

  ithin=ithin_conv(nc)
  rmesh=rmesh_conv(nc)
  id=ictype(nc)

  disterrmax=zero
  vdisterrmax=zero
  ntest=0
  nvtest=0
  nchanl=0
  ilon=2
  ilat=3
  mkx = 0
  if (ithin > 0 ) then
    write(6,*)'READ_PREPBUFR: id,ithin,rmesh :',trim(ioctype(nc)),ithin,rmesh 
	! hardwired to native guess structure
	pflag=0. ; if (pflag .eq. 0) pmesh=nsig
	!xmesh=0. ! force subroutines to be called but no thinning
	xmesh=rmesh
  else
	xmesh=0.
  endif
  call make3grids(xmesh,pmesh,pflag)


  call closbf(lunin)
  open(lunin,file=infile,form='unformatted')
  call openbf(lunin,'IN',lunin)
  call datelen(10)

! Big loop over prepbufr file	

   ntb = 0
   nmsg = 0
   match = 0
   loop_readsb: do 
		 ! use msg lookup table to decide which messages to skip
		 ! use report id lookup table to only process matching reports
		 call readsb(lunin,iret) ! read next report of current message
		 if(iret/=0) then ! read next message 
			loop_msg: do
				call readmg(lunin,subset,idate,iret)
				if(iret/=0) exit loop_readsb ! end of file
				nmsg=nmsg+1
				if (lmsg(nmsg,id)) then
					exit loop_msg ! match found, break message loop
				else
					ntb=ntb+nrep(nmsg) ! no reports this mesage, skip ahead report count
				endif
			enddo loop_msg
			cycle loop_readsb
		 endif
		 ntb = ntb+1; kx=tab(1,ntb)
		 mkx(kx)=mkx(kx)+1
		 if (kx /= id) cycle loop_readsb
		 ikx=0
		 i=nc

         iobsub = 0           ! temporary until put in bufr file
         if(kx == 243 .or. kx == 253 .or. kx == 254) then
            iobsub = kx
         end if

		 if(trim(obstype) == trim(ioctype(i)) .and. kx == ictype(i)  &
						 .and. abs(icuse(i))== 1)then
		   if(icsubtype(i) == iobsub) then
		       ikx=i
		   else
		       if(trim(obstype) == trim(ioctype(i)) .and. kx == ictype(i)  &
						 .and. abs(icuse(i))== 1)then
				   ixsub=icsubtype(i)/10
				   iosub=iobsub/10
				   isubsub=icsubtype(i)-ixsub*10
				   if((ixsub == iosub .and. isubsub == 0) .or. icsubtype(i) == 0) then
					  ikx=i
				   endif
			   end if
		   end if
		 end if
	     if(ikx == 0) cycle loop_convinfo             ! not ob type used

	!------------------------------------------------------------------------

         if(offtime_data) then

!       in time correction for observations to account for analysis
!                    time being different from obs file time.
            write(date,'( i10)') idate
            read (date,'(i4,3i2)') iy,im,idd,ihh
            idate5(1)=iy
            idate5(2)=im
            idate5(3)=idd
            idate5(4)=ihh
            idate5(5)=0
            call w3fs21(idate5,minobs)    !  obs ref time in minutes relative to historic date
            idate5(1)=iadate(1)
            idate5(2)=iadate(2)
            idate5(3)=iadate(3)
            idate5(4)=iadate(4)
            idate5(5)=0
            call w3fs21(idate5,minan)    !  analysis ref time in minutes relative to historic date

!           Add obs reference time, then subtract analysis time to get obs time relative to analysis

            time_correction=float(minobs-minan)/60._r_kind
            
         else
            time_correction=zero
         end if

	! On temperature task, write out date information.  If date in prepbufr
	! file does not agree with analysis date, print message and stop program
	! execution.

	 if(tob) then
	   if (j1st == 0) then
		 write(date,'( i10)') idate
		 read (date,'(i4,3i2)') iy,im,idd,ihh
		 write(6,*)'READ_PREPBUFR: bufr file date is ',iy,im,idd,ihh
		 if(iy/=iadate(1).or.im/=iadate(2).or.idd/=iadate(3).or.&
			  ihh/=iadate(4)) then
			write(6,*)'***READ_PREPBUFR ERROR*** incompatable analysis ',&
				 'and observation date/time'
			write(6,*)' year  anal/obs ',iadate(1),iy
			write(6,*)' month anal/obs ',iadate(2),im
			write(6,*)' day   anal/obs ',iadate(3),idd
			write(6,*)' hour  anal/obs ',iadate(4),ihh
			call stop2(94)
		 end if
	! compute day of the year, # days in year, true solar noon for today's date

		 call w3fs13(iadate(1),iadate(2),iadate(3),idayr)  ! day of year
		 call w3fs13(iadate(1),12,31,idaysy)               ! #days in year
		 dangl  = 6.2831853_r_kind * (real(idayr) - 79._r_kind)/real(idaysy)
		 sdgl   = sin(dangl)
		 cdgl   = cos(dangl)
		 tsnoon = -.030_r_kind*sdgl-.120_r_kind*cdgl+.330_r_kind*sdgl*cdgl+ &
					.0016_r_kind*sdgl**2-.0008_r_kind

	     j1st=1
	   end if
	 end if

!    Extract type, date, and location information
     call ufbint(lunin,hdr,7,1,iret,hdstr)
     rstation_id=hdr(1)
     if(hdr(2)>= r360)hdr(2)=hdr(2)-r360
     if(hdr(2) < zero)hdr(2)=hdr(2)+r360
     dlon_earth=hdr(2)*deg2rad
     dlat_earth=hdr(3)*deg2rad
     if(regional)then
        call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)    ! convert to rotated coordinate
        if(diagnostic_reg) then
           call txy2ll(dlon,dlat,rlon00,rlat00)
           ntest=ntest+1
           cdist=sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
                (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00))
           cdist=max(-one,min(cdist,one))
           disterr=acosd(cdist)
           disterrmax=max(disterrmax,disterr)
        end if
        if(outside) cycle loop_readsb   ! check to see if outside regional domain
     else
        dlat = dlat_earth
        dlon = dlon_earth
        call grdcrd(dlat,1,rlats,nlat,1)
        call grdcrd(dlon,1,rlons,nlon,1)
     endif

     time=hdr(4) + time_correction
     stnelev=hdr(6)


!    If running in 2d-var (surface analysis) mode, check to see if observation
!    is surface type.  If not, read next observation report from bufr file
     if ( twodvar_regional .and. &
          (kx<180 .or. kx>289 .or. (kx>189 .and. kx<280)) ) cycle loop_readsb

!    For mesonet winds, check to see if mesonet provider is in the    !MPondeca - start block
!    uselist. If not, read next observation report from bufr file
     if (kx.eq.288 .and. listexist) then
        call ufbint(lunin,r_prvstg,1,1,iret,prvstr)
        call ufbint(lunin,r_sprvstg,1,1,iret,sprvstr)

        lprov=.false.
        do m=1,nprov
         if (trim(c_prvstg//c_sprvstg) == trim(cprovider(m))) then
          lprov=.true.
         endif
        enddo
        if (.not.lprov) cycle loop_readsb
!       print*,'kx,prvstg=',kx,c_prvstg
!       print*,'kx,sprvstg=',kx,c_sprvstg
     endif                                             !MPondeca - end block

!    Balloon drift information available for these data
     driftl=kx==120.or.kx==220.or.kx==221
     if((real(abs(time)) > real(ctwind(ikx)) .or. real(abs(time)) > real(twindin)) &
             .and. .not. driftl)cycle loop_readsb ! outside time window
     timex=time
     
!    Extract data information on levels
     call ufbint(lunin,obsdat,9,255,levs,obstr)
     call ufbint(lunin,qcmark,8,255,levs,qcstr)
     call ufbint(lunin,obserr,8,255,levs,oestr)
     nread=nread+levs
     if(uvob)nread=nread+levs
     sstdat=1.e11
     if(sstob)call ufbint(lunin,sstdat,8,1,levs,sststr)
     if(metarcldobs) then
        metarcld=1.e11_r_kind
        metarwth=1.e11_r_kind
        metarvis=1.e11_r_kind
        call ufbint(lunin,metarcld,2,10,metarcldlevs,metarcldstr)
        call ufbint(lunin,metarwth,1,10,metarwthlevs,metarwthstr)
        call ufbint(lunin,metarvis,1,1,iret,metarvisstr)
        if(levs .ne. 1 ) then
          write(6,*) 'READ_PREPBUFR: error in Metar observations, levs sould be 1 !!!'
          call stop2(110)
        endif
     endif
     if(geosctpobs) then
        geoscld=1.e11_r_kind
        call ufbint(lunin,geoscld,4,1,levs,geoscldstr)
     endif


!    If available, get obs errors from error table
     if(oberrflg)then

!      Set lower limits for observation errors
       terrmin=r0_5
       werrmin=one
       perrmin=r0_5
       qerrmin=one_tenth
       pwerrmin=one

        do k=1,levs
           itypex=kx
           ppb=obsdat(1,k)
           if(kx==153)ppb=obsdat(9,k)*0.01
           ppb=max(zero,min(ppb,r2000))
           if(ppb.ge.etabl(itypex,1,1)) k1=1
           do kl=1,32
              if(ppb.ge.etabl(itypex,kl+1,1).and.ppb.le.etabl(itypex,kl,1)) k1=kl
           end do
           if(ppb.le.etabl(itypex,33,1)) k1=5
           k2=k1+1
           ediff = etabl(itypex,k2,1)-etabl(itypex,k1,1)
           if (abs(ediff) > tiny_r_kind) then
              del = (ppb-etabl(itypex,k1,1))/ediff
           else
              del = huge_r_kind
           endif
           del=max(zero,min(del,one))
           obserr(3,k)=(one-del)*etabl(itypex,k1,2)+del*etabl(itypex,k2,2)
           obserr(2,k)=(one-del)*etabl(itypex,k1,3)+del*etabl(itypex,k2,3)
           obserr(5,k)=(one-del)*etabl(itypex,k1,4)+del*etabl(itypex,k2,4)
           obserr(1,k)=(one-del)*etabl(itypex,k1,5)+del*etabl(itypex,k2,5)
           obserr(7,k)=(one-del)*etabl(itypex,k1,6)+del*etabl(itypex,k2,6)
           
           obserr(3,k)=max(obserr(3,k),terrmin)
           obserr(2,k)=max(obserr(2,k),qerrmin)
           obserr(5,k)=max(obserr(5,k),werrmin)
           obserr(1,k)=max(obserr(1,k),perrmin)
           obserr(7,k)=max(obserr(7,k),pwerrmin)
        enddo
     endif
     

!    If data with drift position, get drift information
     if(driftl)call ufbint(lunin,drfdat,8,255,iret,drift)
!    Loop over levels       
     do k=1,levs
        do i=1,8
           qcmark(i,k) = min(qcmark(i,k),huge_i_kind)
        end do
        

	  if (kx == id_bias_ps) then
		  plevs(k)=one_tenth*obsdat(1,k)+conv_bias_ps   ! convert mb to cb
	  else
		  plevs(k)=one_tenth*obsdat(1,k)   ! convert mb to cb
	  endif
      pqm(k)=nint(qcmark(1,k))
      qqm(k)=nint(qcmark(2,k))
      tqm(k)=nint(qcmark(3,k))
      wqm(k)=nint(qcmark(5,k))
     end do

!    If temperature ob, extract information regarding virtual
!    versus sensible temperature
     if(tob) then
        call ufbevn(lunin,tpc,1,255,20,levs,'TPC')
        do k=1,levs
           tvflg(k)=one                               ! initialize as sensible
           do j=1,20
              if (tpc(k,j)==vtcd) tvflg(k)=zero       ! reset flag if virtual
              if (tpc(k,j)>=bmiss) exit               ! end of stack
           end do
        end do
     end if

     LOOP_K_LEVS: do k=1,levs

!       Extract quality marks
        if(tob)then
           qm=tqm(k)
        else if(uvob) then 
           qm=wqm(k)
        else if(spdob) then
           qm=wqm(k)
        else if(psob) then
           qm=pqm(k)
        else if(qob) then
           if(obsdat(2,k) > 1.0e9)cycle loop_k_levs
           qm=qqm(k)
        else if(pwob) then
           pwq=nint(qcmark(7,k))
           qm=pwq
        else if(sstob) then
           sstq=100
           if (k==1) sstq=nint(min(sstdat(4,k),huge_i_kind))
           qm=sstq
        else if(metarcldobs) then
           qm=0
        else if(geosctpobs) then
           qm=0
        end if

!       Set inflate_error logical and qc limits based on noiqc flag
        inflate_error=.false.
        if (noiqc) then
           lim_qm=8
           if (qm==3 .or. qm==7) inflate_error=.true.
           if (psob) lim_zqm=7
           if (qob)  lim_tqm=7
           if (tob)  lim_qqm=8
        else
           lim_qm=4
           if (qm==3) inflate_error=.true.
           if (psob) lim_zqm=4
           if (qob)  lim_tqm=4
           if (tob)  lim_qqm=4
        endif

!       Check qc marks to see if obs should be processed or skipped
        if (psob) then
           zqm=nint(qcmark(4,k))
           cat=nint(min(obsdat(8,k),huge_i_kind))
           if( (zqm>=lim_zqm .and. zqm/=15 .and. zqm/=9) .or. cat /=0 &
                .or. obsdat(1,k)< r500)cycle loop_k_levs
        endif

        if(convobs .and. pqm(k) >=lim_qm .and. qm/=15 .and. qm/=9 )cycle loop_k_levs
        if(qm >=lim_qm .and. qm /=15 .and. qm /=9)cycle loop_k_levs
 
!       Set usage variable              
        usage = 0.
        if(icuse(ikx) < 0)usage=100.
        if(qm == 15 .or. qm == 9)usage=100.
        if(ncnumgrp(ikx) > 0 )then                     ! cross validation on
          if(mod(ndata+1,ncnumgrp(ikx))== ncgroup(ikx)-1)usage=ncmiter(ikx)
        end if


!       If needed, extract drift information.   
        if(driftl)then
           if(drfdat(1,k) >= r360)drfdat(1,k)=drfdat(1,k)-r360
           if(drfdat(1,k) <  zero)drfdat(1,k)=drfdat(1,k)+r360
           if(abs(drfdat(2,k)) > r90 .or. drfdat(1,k) > r360 .or. drfdat(1,k) < zero)then
              drfdat(2,k)=hdr(3)
              drfdat(1,k)=hdr(2)
           end if

!          Check to ensure header lat and drift lat similar
           if(abs(drfdat(2,k)-hdr(3)) > r10 .and.  &
              abs(drfdat(1,k)-hdr(2)) > r10)then
              drfdat(2,k)=hdr(3)
              drfdat(1,k)=hdr(2)
           end if

!          Check to see if the time is outrageous if so set to header value
           time_drift = drfdat(3,k) + time_correction
           if (abs(time_drift-time)>four) time_drift = time

!          Check to see if the time is outside range
           if(abs(time_drift) > ctwind(ikx) .or. abs(time_drift) > twindin)then
              time_drift=timex
              if(abs(timex) > ctwind(ikx) .or. abs(timex) > twindin) CYCLE LOOP_K_LEVS
           end if
           time=time_drift
           
           dlat_earth = drfdat(2,k) * deg2rad
           dlon_earth = drfdat(1,k) * deg2rad

           if(regional)then
              call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
              if(outside) cycle LOOP_K_LEVS 
           else
              dlat = dlat_earth
              dlon = dlon_earth
              call grdcrd(dlat,1,rlats,nlat,1)
              call grdcrd(dlon,1,rlons,nlon,1)
           endif
        end if

!       Interpolate guess pressure profile to observation location
        klon1= int(dlon);  klat1= int(dlat)
        dx   = dlon-klon1; dy   = dlat-klat1
        dx1  = one-dx;     dy1  = one-dy
        w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy

        klat1=min(max(1,klat1),nlat); klon1=min(max(0,klon1),nlon)
        if (klon1==0) klon1=nlon
        klatp1=min(nlat,klat1+1); klonp1=klon1+1
        if (klonp1==nlon+1) klonp1=1

        do kk=1,nsig
           presl(kk)=w00*prsl_full(klat1,klon1,kk) + w10*prsl_full(klatp1,klon1,kk) + &
                w01*prsl_full(klat1,klonp1,kk) + w11*prsl_full(klatp1,klonp1,kk)
        end do

!       Compute depth of guess pressure layersat observation location
        if (.not.twodvar_regional) then
           do kk=1,nsig-1
              dpres(kk)=presl(kk)-presl(kk+1)
           end do
        endif

!       Extract pressure level and quality marks
        dlnpob=log(plevs(k))  ! ln(pressure in cb)
		ntmp=ndata  ! counting moved to map3gridS
  
		crit1 = timedif
		! obs thinned have luse set to .false. 
		! obs replacing existing selected obs have approprite index iobsout set
		call map3grids(presl,nsig,dlat_earth,dlon_earth,plevs(k),crit1,ithin,ndata,itx,itt,iobsout,ip,luse)
		if (ndata > ntmp) then
			nodata=nodata+1
			if(uvob)nodata=nodata+1
		endif
		if(ndata > maxobs) then
			 write(6,*)'READ_PREPBUFR:  ***WARNING*** ndata > maxobs for ',obstype
			 call stop2(50)
			 ndata = maxobs
		end if

		if (.not. luse) cycle loop_readsb

!       Temperature
        if(tob) then
           ppb=obsdat(1,k)
           call errormod(pqm,tqm,levs,plevs,errout,k,presl,dpres,nsig)
           toe=obserr(3,k)*errout
           qtflg=tvflg(k) 
           if (inflate_error) toe=toe*r1_2
           if(ppb < r100)toe=toe*r1_2
           cdata_all(1,iobsout)=toe                     ! temperature error
           cdata_all(2,iobsout)=dlon                    ! grid relative longitude
           cdata_all(3,iobsout)=dlat                    ! grid relative latitude
           cdata_all(4,iobsout)=dlnpob                  ! ln(pressure in cb)

		   if (kx == id_bias_t) then
           cdata_all(5,iobsout)=obsdat(3,k)+t0c+conv_bias_t         ! temperature ob.+bias
		   iprint=iprint+1
		   if (iprint < 10) print*,'obsdat(3,k)+t0c+conv_bias_t',obsdat(3,k),t0c,conv_bias_t
		   else
           cdata_all(5,iobsout)=obsdat(3,k)+t0c         ! temperature ob.
		   endif
           cdata_all(6,iobsout)=rstation_id             ! station id
           cdata_all(7,iobsout)=time                    ! time
           cdata_all(8,iobsout)=ikx                     ! type
           cdata_all(9,iobsout)=qtflg                   ! qtflg (virtual temperature flag)
           cdata_all(10,iobsout)=tqm(k)                 ! quality mark
           cdata_all(11,iobsout)=obserr(3,k)            ! original obs error
           cdata_all(12,iobsout)=usage                  ! usage parameter
           cdata_all(13,iobsout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(14,iobsout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(15,iobsout)=stnelev                ! station elevation (m)
           cdata_all(16,iobsout)=obsdat(4,k)            ! observation height (m)

!       Winds 
        else if(uvob) then 
           call errormod(pqm,wqm,levs,plevs,errout,k,presl,dpres,nsig)
           woe=obserr(5,k)*errout
           if (inflate_error) woe=woe*r1_2
           if(obsdat(1,k) < r50)woe=woe*r1_2
           selev=stnelev
           oelev=obsdat(4,k)
           if(selev == oelev)oelev=r10+selev
           if(kx >= 280 .and. kx < 290 )then
             if (kx == 280) oelev=r20+selev
             if (kx == 281) oelev=r10+selev
             if (kx == 282) oelev=r20+selev
             if (kx == 284) oelev=r10+selev
             if (kx == 285) oelev=selev
             if (kx == 285) selev=zero
             if (kx == 286) oelev=r10+selev
             if (kx == 287) oelev=r10+selev
             if (kx == 288) oelev=r10+selev
             if (kx == 289) oelev=r10+selev
           end if

!          Rotate winds to rotated coordinate
           uob=obsdat(5,k)
           vob=obsdat(6,k)
           if(regional)then
              u0=uob
              v0=vob
              call rotate_wind_ll2xy(u0,v0,uob,vob,dlon_earth,dlat_earth,dlon,dlat)
              if(diagnostic_reg) then
                 call rotate_wind_xy2ll(uob,vob,u00,v00,dlon_earth,dlat_earth,dlon,dlat)
                 nvtest=nvtest+1
                 disterr=sqrt((u0-u00)**2+(v0-v00)**2)
                 vdisterrmax=max(vdisterrmax,disterr)
              end if
           endif
           
           cdata_all(1,iobsout)=woe                     ! wind error
           cdata_all(2,iobsout)=dlon                    ! grid relative longitude
           cdata_all(3,iobsout)=dlat                    ! grid relative latitude
           cdata_all(4,iobsout)=dlnpob                  ! ln(pressure in cb)
           cdata_all(5,iobsout)=oelev                   ! height of observation
           cdata_all(6,iobsout)=uob                     ! u obs
           cdata_all(7,iobsout)=vob                     ! v obs
           cdata_all(8,iobsout)=rstation_id             ! station id
           cdata_all(9,iobsout)=time                    ! time
           cdata_all(10,iobsout)=ikx                    ! type
           cdata_all(11,iobsout)=selev                  ! station elevation
           cdata_all(12,iobsout)=wqm(k)                 ! quality mark
           cdata_all(13,iobsout)=obserr(5,k)            ! original obs error
           cdata_all(14,iobsout)=usage                  ! usage parameter
           cdata_all(15,iobsout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(16,iobsout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)

        else if(spdob) then 
           woe=obserr(5,k)
           if (inflate_error) woe=woe*r1_2
           elev=r20

           cdata_all(1,iobsout)=woe                     ! wind error
           cdata_all(2,iobsout)=dlon                    ! grid relative longitude
           cdata_all(3,iobsout)=dlat                    ! grid relative latitude
           cdata_all(4,iobsout)=dlnpob                  ! ln(pressure in cb)
           cdata_all(5,iobsout)=obsdat(5,k)             ! u obs
           cdata_all(6,iobsout)=obsdat(6,k)             ! v obs
           cdata_all(7,iobsout)=rstation_id             ! station id
           cdata_all(8,iobsout)=time                    ! time
           cdata_all(9,iobsout)=ikx                     ! type
           cdata_all(10,iobsout)=elev                   ! elevation of observation
           cdata_all(11,iobsout)=wqm(k)                 ! quality mark
           cdata_all(12,iobsout)=obserr(5,k)            ! original obs error
           cdata_all(13,iobsout)=usage                  ! usage parameter
           cdata_all(14,iobsout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(15,iobsout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(16,iobsout)=stnelev                ! station elevation (m)

!       Surface pressure 
        else if(psob) then
 
           poe=obserr(1,k)*one_tenth                  ! convert from mb to cb
           if (inflate_error) poe=poe*r1_2
           cdata_all(1,iobsout)=poe                     ! surface pressure error (cb)
           cdata_all(2,iobsout)=dlon                    ! grid relative longitude
           cdata_all(3,iobsout)=dlat                    ! grid relative latitude

           cdata_all(4,iobsout)=exp(dlnpob)             ! pressure (in cb)
		   
           cdata_all(5,iobsout)=obsdat(4,k)             ! surface height
           cdata_all(6,iobsout)=obsdat(3,k)+t0c         ! surface temperature
           cdata_all(7,iobsout)=rstation_id             ! station id
           cdata_all(8,iobsout)=time                    ! time
           cdata_all(9,iobsout)=ikx                     ! type
           cdata_all(10,iobsout)=pqm(k)                 ! quality mark
           cdata_all(11,iobsout)=obserr(1,k)*one_tenth  ! original obs error (cb)
           cdata_all(12,iobsout)=usage                  ! usage parameter
           cdata_all(13,iobsout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(14,iobsout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(15,iobsout)=stnelev                ! station elevation (m)

!       Specific humidity 
        else if(qob) then
           qmaxerr=emerr
           call errormod(pqm,qqm,levs,plevs,errout,k,presl,dpres,nsig)
           qoe=obserr(2,k)*one_tenth*errout
           if (inflate_error) then
              qmaxerr=emerr*r0_7; qoe=qoe*r1_2
           end if
           qobcon=obsdat(2,k)*convert
           tdry=r999
           if (tqm(k)<lim_tqm) tdry=(obsdat(3,k)+t0c)/(one+fv*qobcon)
           cdata_all(1,iobsout)=qoe                     ! q error   
           cdata_all(2,iobsout)=dlon                    ! grid relative longitude
           cdata_all(3,iobsout)=dlat                    ! grid relative latitude
           cdata_all(4,iobsout)=dlnpob                  ! ln(pressure in cb)
           cdata_all(5,iobsout)=qobcon                  ! q ob
           cdata_all(6,iobsout)=rstation_id             ! station id
           cdata_all(7,iobsout)=time                    ! time
           cdata_all(8,iobsout)=ikx                     ! type
           cdata_all(9,iobsout)=qmaxerr                 ! q max error
           cdata_all(10,iobsout)=tdry                   ! dry temperature (obs is tv)
           cdata_all(11,iobsout)=qqm(k)                 ! quality mark
           cdata_all(12,iobsout)=obserr(2,k)*one_tenth  ! original obs error
           cdata_all(13,iobsout)=usage                  ! usage parameter
           cdata_all(14,iobsout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(15,iobsout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(16,iobsout)=stnelev                ! station elevation (m)
           cdata_all(17,iobsout)=obsdat(4,k)            ! observation height (m)

!       Total precipitable water (ssm/i)
        else if(pwob) then

           pwoe=obserr(7,k)
           pwmerr=pwoe*three
           cdata_all(1,iobsout)=pwoe                    ! pw error
           cdata_all(2,iobsout)=dlon                    ! grid relative longitude
           cdata_all(3,iobsout)=dlat                    ! grid relative latitude
           cdata_all(4,iobsout)=obsdat(7,k)             ! pw obs
           cdata_all(5,iobsout)=rstation_id             ! station id
           cdata_all(6,iobsout)=time                    ! time
           cdata_all(7,iobsout)=ikx                     ! type
           cdata_all(8,iobsout)=pwmerr                  ! pw max error
           cdata_all(9,iobsout)=pwq                     ! quality mark
           cdata_all(10,iobsout)=obserr(7,k)            ! original obs error
           cdata_all(11,iobsout)=usage                  ! usage parameter
           cdata_all(12,iobsout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(13,iobsout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(14,iobsout)=stnelev                ! station elevation (m)
           cdata_all(15,iobsout)=obsdat(1,k)            ! observation pressure (hPa)
           cdata_all(16,iobsout)=obsdat(4,k)            ! observation height (m)

!       Conventional sst observations
        else if(sstob) then

!          Locate the observation on the analysis grid.  Get land/sea/ice
!          mask at nearest analysis grid points.

           sstoe=r0_75
!          sstoe=sstdat(5,k)

           cdata_all(1,iobsout)=sstoe                   ! sst error
           cdata_all(2,iobsout)=dlon                    ! grid relative longitude
           cdata_all(3,iobsout)=dlat                    ! grid relative latitude
           cdata_all(4,iobsout)=sstdat(3,k)             ! sst obs
           cdata_all(5,iobsout)=rstation_id             ! station id
           cdata_all(6,iobsout)=time                    ! time
           cdata_all(7,iobsout)=ikx                     ! type
           cdata_all(8,iobsout)=sstoe*three             ! pw max error
           cdata_all(9,iobsout)=sstdat(2,k)             ! depth of measurement
           cdata_all(10,iobsout)=sstdat(1,k)            ! measurement type
           cdata_all(11,iobsout)=sstq                   ! quality mark
           cdata_all(12,iobsout)=sstdat(5,k)            ! original obs error
           cdata_all(13,iobsout)=usage                  ! usage parameter
           cdata_all(14,iobsout)=dlon_earth*rad2deg     ! earth relative longitude (degrees)
           cdata_all(15,iobsout)=dlat_earth*rad2deg     ! earth relative latitude (degrees)
           cdata_all(16,iobsout)=stnelev                ! station elevation (m)

!          if(kx == 120 .or. kx == 282)then
!             write(6,*)'READ_PREPBUFR:  kx,rstation_id,sstq=',&
!                  kx,rstation_id,sstq
!             do i=1,10
!                write(6,*)'READ_PREPBUFR:  i,cdata_all=',i,cdata_all(i,ndata)
!             end do
!          end if

!          Measurement types
!             0       Ship intake
!             1       Bucket
!             2       Hull contact sensor
!             3       Reversing Thermometer
!             4       STD/CTD sensor
!             5       Mechanical BT
!             6       Expendable BT
!             7       Digital BT
!             8       Thermistor chain
!             9       Infra-red scanner
!             10      Micro-wave scanner
!             11-14   Reserved

        else if(metarcldobs) then
          cdata_all(1,iobsout)=rstation_id    !  station ID
          cdata_all(2,iobsout)=dlon           !  grid relative longitude
          cdata_all(3,iobsout)=dlat           !  grid relative latitude
          cdata_all(4,iobsout)=stnelev        !  station  elevation
          cdata_all(5,iobsout)=metarvis(1,1)  !  visibility (m)
          do kk=1, min(metarcldlevs,6)
            cdata_all(5+kk,iobsout) =metarcld(1,kk)  !  cloud amount
            cdata_all(11+kk,iobsout)=metarcld(2,kk)  !  cloud bottom height (m)
          enddo
          do kk=1, min(metarwthlevs,3)
            cdata_all(17+kk,iobsout)=metarwth(1,kk)  !  weather
          enddo
        else if(geosctpobs) then
          cdata_all(1,iobsout)=rstation_id    !  station ID
          cdata_all(2,iobsout)=dlon                 !  grid relative longitude
          cdata_all(3,iobsout)=dlat                 !  grid relative latitude
          cdata_all(4,iobsout)=geoscld(1,k)/100.0_r_kind   !  cloud top pressure (pa)
          cdata_all(5,iobsout)=geoscld(2,k)         !  cloud cover
          cdata_all(6,iobsout)=geoscld(3,k)         !  Cloud top temperature (K)
          cdata_all(7,iobsout)=time                 !  time
        end if

!
!    End k loop over levs
     end do  LOOP_K_LEVS

!
!   End of bufr read loop
  enddo loop_readsb
! Close unit to bufr file
  call closbf(lunin)
  !print*,'closbf(',lunin,')'

  if (ithin > 0 ) then
    call thin_count(ithin)
    do k=nsig,1,-1
        if (kount(1,k) > 0) write(6,*)'READ_PREPBUFR: thinning ',kx,presl(k),kount(1,k),kount(2,k)
    enddo
    print*,'call del3grids'
    call del3grids
  endif

! Normal exit

enddo loop_convinfo! loops over convinfo entry matches

! Write header record and data to output file for further processing
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
  write(lunout) ((cdata_all(k,i),k=1,nreal),i=1,ndata)

  deallocate(cdata_all)


  if(diagnostic_reg .and. ntest>0) write(6,*)'READ_PREPBUFR:  ',&
       'ntest,disterrmax=',ntest,disterrmax
  if(diagnostic_reg .and. nvtest>0) write(6,*)'READ_PREPBUFR:  ',&
       'nvtest,vdisterrmax=',ntest,vdisterrmax

  if (ndata == 0) then 
	call closbf(lunin)
	print*,'closbf(',lunin,')'
  endif

  close(lunin)

! End of routine
  return
end subroutine read_prepbufr
