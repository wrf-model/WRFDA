subroutine read_prepbufr_cld(nread,ndata,infile,obstype,lunout)
!
!   PRGMMR: Ming Hu          ORG: NP22        DATE: 2006-03-27
!
! ABSTRACT: 
!     This routine uses RUC getbufr reader for the prep bufr
!     files used to initialize models at NMC. This code will deBUFR
!     the cloud data and write out separate files of data for each data type.
!
! PROGRAM HISTORY LOG:
!
!
!   input argument list:
!     infile   - unit from which to read BUFR data
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!
! USAGE:
!   INPUT FILES:  prepqc_cld
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  GUST, EJET
!
!$$$
!
!      PROGRAM read_prepbufr_cld
!     Program getbufr.f
!
!_____________________________________________________________________
!
      use kinds, only: r_kind,r_double,i_kind
      use constants, only: zero,one_tenth,one,deg2rad
      use gridmod, only: regional,nlon,nlat,nsig,         &
                         tll2xy,txy2ll,                   &
                         regional_time,nhr_assimilation,  &
                         regional_fhr

      implicit none
!
      real(r_kind),parameter:: r360 = 360.0_r_kind

      character(10),intent(in):: infile,obstype
      integer(i_kind),intent(in):: lunout
      integer(i_kind),intent(inout):: nread,ndata
!
      integer obtime
      character*8 prev
      character*12 adate
      character*256 output_file
 
!     stuff for bufr reading follows...
      integer    maxsta
      real       spval_p
      parameter (maxsta=2500000)
      parameter (spval_p = 99999.)
!
!
      integer    MAXDTA
      PARAMETER	(MAXDTA = 800)
      real*8 data(maxdta)
      character*8 sta_id
      equivalence(data(2), sta_id)
      real(r_kind) :: latitude
      real(r_kind) :: longitude
 
!     40km stuff for w3fb11
!      parameter (alat1=16.281,elon1=233.862) !!lon = -126.1378
!      parameter(dx=40635.25) !grid mesh
!      parameter(elonv=265.00) !this is -95W + 360
!      parameter(alatan=25.00) !true
!
!    ** universal parameters (headers, ids, lat/lon etc) **
!      
      integer    nuniv_p
      parameter(nuniv_p = 7 )
      character*72 univ_p(nuniv_p)
      character*72 univp
      character*72 latlonp
      data univ_p/'TYP','SID','ITP','YOB','XOB','ELV','DHR'/
      data univp/'TYP SID ITP YOB XOB ELV DHR'/
      data latlonp/'YOB XOB'/
!
!     ** parameters for GOES 1x1 cloud top data (TYP=151 new 7/11/01)
!
      integer    max_goest
      integer    ngoest_v
      parameter(max_goest = 200000)
      parameter(ngoest_v = 5)
      real goest_md(max_goest,nuniv_p)
      real goest_data(max_goest,ngoest_v)
      character*60 goestv
      character*8 goest_id(max_goest)
!     data goest_v/'CDTP','TOCC','GCDTT','CDTP_QM','SAID'/
      data goestv/'CDTP TOCC GCDTT CDTP_QM'/
      character*72 saidv
      data saidv /'SAID'/

      real,allocatable:: w_pcld(:,:)    ! cloud top pressure  in grid
      real,allocatable:: w_tcld(:,:)    ! cloud top temperature in grid
      real,allocatable:: w_frac(:,:)    ! effective fractional cloud coverage
!
!     ** parameters for surface data (TYP= 181,183,187,287,284,281)
!
      integer    max_sao
      integer    nsao_cld
      integer    maxsaolev
      parameter(max_sao = 8000)
      parameter(nsao_cld = 4 )
      parameter(maxsaolev = 3)
      real sao_data(max_sao,nsao_cld,maxsaolev)
      real sao_md(max_sao,nuniv_p)
      character*8 sao_cld(nsao_cld)
      character*8 sao_id(max_sao)

!     4-10-2001 added SOB and SQM for variable wind dir good speed obs
!     10-20-04  added PRWE HOVI CLAM HOCB [present weather, vis, clouds]

      data sao_cld/'PRWE','HOVI','CLAM','HOCB'/

      character*9,allocatable:: STANAM(:)  ! stattion name
      integer(i_kind),allocatable:: STANUM(:)  ! station number
      real(r_kind),allocatable:: LAT(:)   ! latitude
      real(r_kind),allocatable:: LON(:)   ! longitude
      real(r_kind),allocatable:: ELEV(:)  ! elevation

      real(r_kind),allocatable:: VIS(:)   ! horizontal visibility
      real(r_kind),allocatable:: CLD(:,:)   ! cloud base height
      character*10,allocatable:: WX(:)      ! weather
      character*8, allocatable:: sky(:,:)   ! cloud cover or amount

      integer(i_kind):: NSTA_NEW
      character*3 :: msky,mwx
      logical     ::  samesta

      real ::  cldamt,awx,cldhgt
      integer :: start, end

!
      real(r_kind),allocatable,dimension(:,:):: cdata_all
!
!  ** misc
      
    real(r_kind)::rix  ! x-grid coordinate (grid units)
    real(r_kind)::riy  ! y-grid coordinate (grid units)
    logical     ::outside     ! .false., then point is inside x-y domain
                              ! .true.,  then point is outside x-y domain

        integer istations
        integer numsao
        integer numgoest
        integer i,j,k,itype,iymdh,ier,jret,ifn,itime
        integer iz,n,nlv,isao,nflag,np,ilen,iflag,iostat
        integer(i_kind) nreal,nchanl,ilat,ilon,ithin

        integer typ_count(20)
        integer         iret
        CHARACTER       subset*8
        character(20):: sis

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
      sis=''
      ifn = 10

      write(*,*) "check =", trim(obstype),regional_time
      write(*,*) nhr_assimilation,regional_fhr
      itime=regional_time(4)*100+regional_time(5)
! here for debug the code  ming hu
      itime=1800
      write(adate,'(i4.4,4i2.2)') (regional_time(i),i=1,5)
!      write (6,117) adate
!117   format ('GETBUFR started - DATE = ',a12)
!
!      write(lunout) nread,ndata
      istations = 0
      numsao=0
      numgoest=0
      do k = 1,maxsaolev
      do j = 1,nsao_cld
      do i = 1,max_sao
        sao_data(i,j,k) = spval_p
      enddo
      enddo
      enddo
      do j = 1,ngoest_v
      do i = 1,max_goest
        goest_data(i,j) = spval_p
      enddo
      enddo
!
!     will have to get BUFR file name and adate differently for oper use
!
!_____________________________________________________________
!
!*
      IF ( infile .eq. ' ' ) THEN
        print *, ' Error opening file, IRET = ', infile
        STOP 123
      END IF
!*
      OPEN ( UNIT = ifn, FILE = infile, STATUS = 'OLD',          &
              FORM = 'UNFORMATTED', IOSTAT = iostat )
      IF ( iostat .ne. 0 ) THEN
            WRITE (6,*) infile, ' must be blocked for unformatted ',  &
                       'I/O.'
            STOP 123
      END IF
      CALL OPENBF ( ifn, 'IN', ifn )
!*
!  ======================================================
!     main loop of program
!  ======================================================
!

!      print *,' BUFR file opened'

      Do 1160 iz = 1,maxsta 
 
        istations = istations + 1
!        CALL JB_NEXT ( ifn, iymdh, ier ) !!goes to (sets) next station
        iret = 0
        ier=0
        CALL READSB ( ifn, ier )
        IF ( ier .ne. 0 ) THEN
            CALL READMG ( ifn, subset, iymdh, iret )
            IF ( iret .eq. -1 ) THEN
                print *, ' No next station, IRET = ', ier
                go to 1175  !output data
            ELSE IF ( iret .ne. 0 ) THEN
                print *, ' No next station, IRET = ', ier
                go to 1175  !output data
            END IF
            CALL READSB ( ifn, ier )
            IF ( ier .ne. 0 ) THEN
                print *, ' No next station, IRET = ', ier
                go to 1175  !output data
            END IF
        ENDIF
!
!
!     first see if this station data is within the domain
!
        call ufbint (ifn,data,2,1,jret,latlonp)

        if(data(2) .ge. r360 ) data(2) = data(2)-r360
        if(data(2) < zero ) data(2) = data(2)+r360
        latitude = data(1)*deg2rad
        longitude= data(2)*deg2rad
!
!     check to see if this lat/lon is within the GSI domain
        call tll2xy(longitude,latitude,rix,riy,outside)

        if(rix.lt.1.or.rix.gt.nlon) go to 1160
        if(riy.lt.1.or.riy.gt.nlat) go to 1160

 
!     now find out what data type this station entry is
 
        call ufbint (ifn,data,1,1,jret,'TYP')

        itype = data(1)
!      write(*,*) itype
!
!        if (itype .eq. 151 ) typ_count(1) = typ_count(1) + 1
!        if (itype .eq. 181 ) typ_count(2) = typ_count(2) + 1
!        if (itype .eq. 281 ) typ_count(3) = typ_count(3) + 1
!        if (itype .eq. 187 ) typ_count(4) = typ_count(4) + 1
!        if (itype .eq. 287 ) typ_count(5) = typ_count(5) + 1
!        if (itype .eq. 183 ) typ_count(6) = typ_count(6) + 1
!        if (itype .eq. 284 ) typ_count(7) = typ_count(7) + 1
!
!************* GOES cloud top data ***********************************
!
        if(obstype.eq.'ctp' .and. itype.eq.151) then !7a
!
           numgoest = numgoest + 1
           if(numgoest.gt.max_goest) then
              write(*,*) 'Error: exceeded GOES cloud top max sta !'
              stop 123 
           endif

           call ufbint (ifn,data,nuniv_p,1,jret,univp)

           do i=1,nuniv_p
             if(i.eq.2) then
               goest_id(numgoest) = sta_id
               goest_md(numgoest,i) = spval_p
             else
               if(data(i).gt.0.and.data(i).lt.0.00001) data(i) = spval_p
               goest_md(numgoest,i) = data(i)
             endif
           enddo

!     convert longitude to deg WEST
           goest_md(numgoest,5) = goest_md(numgoest,5) - 360.
!     convert DHR to ob time (obtime = DHR + itime)...stick in DHR spot
           call comptime_RUC(itime,goest_md(numgoest,7),obtime)
           goest_md(numgoest,7) = obtime*1.

           call ufbint (ifn,data,ngoest_v-1, 1, nlv, goestv)

           do i=1,ngoest_v-1
             if(data(i).gt.999999) data(i) = spval_p
             goest_data(numgoest,i) = data(i)
           enddo

! -- convert GOES CTP from Pa to mb
           goest_data(numgoest,1) = goest_data(numgoest,1)/100.

           call ufbint (ifn,data,1, 1, nlv, saidv)
           goest_data(numgoest,ngoest_v) = data(1)

!
!************************ surface data (sao) *************************
!
!            .or. (itype.eq.183.or.itype.eq.284)                      &
!     surface mass.wind separate
        elseif(  obstype .eq. 'cld' ) then
          if(  (itype.eq.181.or.itype.eq.281)                     &
            .or. (itype.eq.187.or.itype.eq.287) ) then
          isao=1
!
          nflag = 0
!
          if(numsao+1.gt.max_sao) then
            write(*,*) 'Error: max number of saos exceeded !'
            stop 123
          endif
          numsao = numsao + 1
                       
          call ufbint (ifn,data,nuniv_p,1,jret,univp)

          do i= 1,nuniv_p
            if(i.eq.2) then
              sao_id(numsao) = sta_id
              sao_md(numsao,i) = spval_p
            else
              if(data(i).gt.0.and.data(i).lt.0.000001) data(i) = spval_p
              if(data(i).ge.spval_p) data(i) = spval_p  
              sao_md(numsao,i) = data(i)
            endif
          enddo

!     convert longitude to deg WEST
          sao_md(numsao,5) = sao_md(numsao,5) - 360.
!
!     convert DHR to ob time (obtime = DHR + itime)...stick in DHR spot
          call comptime_RUC(itime,sao_md(numsao,7),obtime)
          sao_md(numsao,7) = obtime*1.

!
!     check to see if the current stn is the 2nd half of the last stn
!     if it has the same id and the same time then it is....
!
          if(numsao.Gt.1) then
            prev = sao_id(numsao-1)
            if(prev.eq.sao_id(numsao)) nflag = 1
            if(sao_md(numsao,7).eq.sao_md(numsao-1,7)       &
              .and.nflag.eq.1) numsao = numsao - 1
          endif

!
!     as of Oct 04 we must treat sao data as multi-level
! 
           do i = 1,4
             call jb_read(ifn,sao_cld(i),maxdta,data,np,nlv,ier)
             if(nlv.gt.3) nlv = 3
             do k = 1,nlv
               if(data(k).gt.0.and.data(k).lt.0.00001) data(k) = spval_p
               if(data(k).gt.999999) data(k) = spval_p
               sao_data(numsao,i,k) = data(k)
             enddo
           enddo
!
           isao = 0
          endif !9
        endif !9
!
1160  continue !main station loop

      write(*,*) ' End of data processing'
!******************* end of data processing ****************************
 
1175  CONTINUE
      CALL CLOSBF ( ifn )

      print *,' Close BUFR file'
!
!     let's write the data out to disk for use in the OI....
!
!***********************************************************************
!  ======================================================
!     sao data
!  ======================================================
  nchanl=0
  ilon=2
  ilat=3
      IF( obstype .eq. 'cld') then
      IF( numsao  > 0) then
         nreal=14
         allocate(cdata_all(nreal,numsao))
         allocate(STANAM(numsao))

        NSTA_NEW=0
        do i= 1,numsao
         IF(ABS(sao_md(i,5)) .lt. spval_p .and.          &
            ABS(sao_md(i,4)) .lt. spval_p .and.          &
            ABS(sao_md(i,6)) .lt. spval_p) THEN

!
!*** Only take one ob per station
!
         samesta=.false.
         IF(NSTA_NEW.GT.0) THEN
           DO J=1,NSTA_NEW
             IF(sao_id(i)(1:4).EQ.STANAM(J)(1:4)) samesta=.true.
           ENDDO
         ENDIF
         IF( .not.samesta ) THEN
            NSTA_NEW = NSTA_NEW + 1
            STANAM(NSTA_NEW)=sao_id(i)(1:4)

            latitude = sao_md(i,4)
            longitude= sao_md(i,5)
            if(longitude .ge. r360 ) longitude = longitude-r360
            if(longitude  < zero ) longitude = longitude+r360
            latitude = latitude*deg2rad
            longitude= longitude*deg2rad
            call tll2xy(longitude,latitude,rix,riy,outside)


            cdata_all(1, NSTA_NEW) = 9999              ! should be station ID
            cdata_all(2, NSTA_NEW) = rix               ! index of x 
            cdata_all(3, NSTA_NEW) = riy               ! index of y
            cdata_all(4, NSTA_NEW) = sao_md(i,6)       ! elevation
            If(sao_data(i,2,1).gt.spval_p) then
               cdata_all(5, NSTA_NEW) =  spval_p ! visibility
            Else
               cdata_all(5, NSTA_NEW) =  sao_data(i,2,1) ! visibility
            Endif
!*** processing prepbufr obs to look like raw obs
            Do j=1,3
              cdata_all(5+j, NSTA_NEW) = sao_data(i,3,j)  ! cloud amonut
              cdata_all(8+j, NSTA_NEW) = sao_data(i,4,j)  ! cloud bottom height
              cdata_all(11+j, NSTA_NEW) = sao_data(i,1,j)  ! weather
            EndDo  ! j=1,3
         ENDIF  ! only station
        ENDIF   ! inside
       ENDDO  ! i

! Write header record and data to output file for further processing
       ndata=NSTA_NEW
       write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
       write(lunout) ((cdata_all(k,i),k=1,nreal),i=1,ndata)

       deallocate(cdata_all)
       deallocate(STANAM)
    else  ! no cld observation
      ndata=0
    endif   ! number of  observation
    endif   ! cloud observation
!
!     second output file for present weather, visibility and clouds
!     added Oct 04

if(1==2) then
      IF( obstype .eq. 'cld') then
      output_file = adate // '.sao2'
      ilen = index(output_file,'   ') - 1
      open(unit=20,file=output_file(1:ilen),status= 'unknown')
!
!      write(20,'(i4)') numsao     
!
      allocate(STANAM(numsao))
      allocate(STANUM(numsao))  
      allocate( LAT(numsao) ) 
      allocate( LON(numsao) ) 
      allocate( ELEV(numsao) )

      allocate( VIS(numsao) ) 
      allocate( CLD(6,numsao))
      allocate( WX(numsao)) 
      allocate( sky(6,numsao))

      NSTA_NEW=0
      do i= 1,numsao
       IF(ABS(sao_md(i,5)) .lt. spval_p .and.          &
          ABS(sao_md(i,4)) .lt. spval_p .and.          &
          ABS(sao_md(i,6)) .lt. spval_p) THEN

!
!*** Only take one ob per station
!
         samesta=.false.
         IF(NSTA_NEW.GT.0) THEN
           DO J=1,NSTA_NEW
             IF(sao_id(i)(1:4).EQ.STANAM(J)(1:4)) samesta=.true.
           ENDDO
         ENDIF
         IF( .not.samesta ) THEN
            NSTA_NEW = NSTA_NEW + 1
            STANAM(NSTA_NEW)=sao_id(i)(1:4)
            LAT(NSTA_NEW)=sao_md(i,4)
            LON(NSTA_NEW)=sao_md(i,5)
            ELEV(NSTA_NEW)=sao_md(i,6)

            If(sao_data(i,2,1).gt.spval_p) then
               VIS(nsta_new)=spval_p
            Else
               VIS(nsta_new)=sao_data(i,2,1)
            Endif
!*** processing prepbufr obs to look like raw obs
            Do j=1,3
              msky='   '
              mwx='   '
              cldamt=sao_data(i,3,j)
              awx=sao_data(i,1,j)
              cldhgt=sao_data(i,4,j)
              if(cldamt.eq.0.) msky='CLR'
              if(cldamt.eq.13.) msky='FEW'
              if(cldamt.eq.11.) msky='SCT'
              if(cldamt.eq.12.) msky='BKN'
              if(cldamt.eq.8.) msky='OVC'
              if(cldamt.eq.9.) msky='VV '
              if(awx.ge.10..and.awx.le.12.) mwx='BR '
              if(awx.ge.110..and.awx.le.112.) mwx='BR '
              if(awx.eq.5..or.awx.eq.105.) mwx='HZ '
              if(awx.ge.40..and.awx.le.49.) mwx='FG '
              if(awx.ge.130..and.awx.le.135.) mwx='FG '
              if(awx.ge.50..and.awx.le.59.) mwx='DZ '
              if(awx.ge.150..and.awx.le.159.) mwx='DZ '
              if(awx.ge.60..and.awx.le.69.) mwx='RA '
              if(awx.ge.160..and.awx.le.169.) mwx='RA '
              if(awx.ge.70..and.awx.le.78.) mwx='SN '
              if(awx.ge.170..and.awx.le.178.) mwx='SN '
              if(awx.eq.79..or.awx.eq.179.) mwx='PE '

              if(awx.ge.80..and.awx.le.90.) mwx='SH '
              if(awx.ge.180..and.awx.le.187.) mwx='SH '
              if(awx.ge.91..and.awx.le.99.) mwx='TH '
              if(awx.ge.190..and.awx.le.196.) mwx='TH '
!*** Now do cloud sorting
              If(cldhgt.ge.spval_p .and.msky(1:3).ne.'CLR') then
                CLD(j,nsta_new)=spval_p
                SKY(j,nsta_new)='        '
              Else
                CLD(j,nsta_new)=cldhgt
                SKY(j,nsta_new)=mSKY(1:3)//'     '
              EndIf
              if (j.eq.1) start=1
              if (j.eq.2) start=4
              if (j.eq.3) start=7
              end=start+2
              WX(nsta_new)(start:end)=mwx
            EndDo  ! j=1,3

         ENDIF  ! only station
       ENDIF   ! inside
!
!     writing out SID,lat,lon,elv,p,t,q,ddd,fff
!     now write out present weather, vis, cloud amount and cloud ht
!     new as of Oct 04
!
!        if(sao_data(i,3,2) < 9999.0 ) then
!        write(20,1460) sao_id(i),sao_md(i,7),sao_md(i,4),    &
!                 sao_md(i,5),sao_md(i,6),sao_data(i,2,1),    &
!                 (sao_data(i,1,k),k=1,maxsaolev),    &
!!                 (sao_data(i,3,k),k=1,maxsaolev),    &
!                 (sao_data(i,4,k),k=1,maxsaolev)
!        endif
! 1460   format(a8,1x,f5.0,1x,f6.3,1x,f8.3,1x,f6.1,1x,   &
!        f7.1,1x,3(<maxsaolev>(f7.1,1x)))

      enddo  ! next sao
      write(*,*) NSTA_NEW
      write(20,'(i8)') NSTA_NEW
      DO i=1, NSTA_NEW

        if(lon(i) .ge. r360 ) lon(i) = lon(i)-r360
        if(lon(i)  < zero ) lon(i) = lon(i)+r360
        latitude = lat(i)*deg2rad
        longitude= lon(i)*deg2rad
        call tll2xy(longitude,latitude,rix,riy,outside)

        write(20,'(a8,1x,2f7.1,3f8.2,f8.1,1x,3a8,3f8.1,1x,a9)')   &
              stanam(i),rix,riy,lon(i),lat(i),elev(i),            &
              VIS(i),(SKY(j,i),j=1,3),(CLD(j,i),j=1,3),       &
              WX(i)
      ENDDO
!
      close(20)

      deallocate(STANAM)
      deallocate(STANUM)
      deallocate( LAT )
      deallocate( LON )
      deallocate( ELEV )

      deallocate( VIS )
      deallocate( CLD)
      deallocate( WX)
      deallocate( sky)

      ndata=0
      ENDIF
endif  ! (1==2)
!
!  ======================================================
!     GOES 1x1 cloud top data 
!  ======================================================
!
      IF(obstype.eq.'ctp') then
      IF(numgoest.gt.0) then
         allocate( w_pcld(nlon,nlat) ) 
         allocate( w_tcld(nlon,nlat) ) 
         allocate( w_frac(nlon,nlat) ) 
         call mape_ctp (nlon,nlat,max_goest,numgoest,            &
                goest_md(:,7),goest_md(:,4),goest_md(:,5),       &
                goest_data(:,1),goest_data(:,2),goest_data(:,3), &
                w_pcld,w_tcld,w_frac)

       ndata=nlon*nlat
       nreal=1
       nchanl=1
       write(lunout) obstype,sis,nreal,nchanl,nlon,nlat
       write(lunout) w_pcld,w_tcld,w_frac

if(1==2) then
         output_file = adate//'.ctp'
         open(unit=10,file=output_file,status='unknown')
           k=0
           write(10,'(2I10)') nlon,nlat
           DO j=1,nlat
           DO i=1,nlon
              write(10,'(2i4,2f10.2,f10.3)') i,j,            &
                   w_pcld(i,j),w_tcld(i,j),w_frac(i,j)
!             if( w_pcld(i,j) < 99999.0 .or. w_tcld(i,j) < 99999.0 &
!                 .or. w_frac(i,j) < 99999.0 ) then
!                 k=k+1
!                 write(10,'(2i4,2f10.2,f10.3)') i,j,        &
!                      w_pcld(i,j),w_tcld(i,j),w_frac(i,j)
!             endif
           ENDDO
           ENDDO
           write(*,*) ' The number of cloud product in grid:', k
!
!         write(10,'(i6)') numgoest
!         do i = 1,numgoest
!            write(10,1910) goest_id(i),goest_md(i,7),goest_md(i,4),  &
!                         goest_md(i,5),(goest_data(i,j),j=1,5)
!         enddo
! 1910 format(a8,1x,f5.0,1x,f6.3,1x,f8.3,1x,5(f8.1,1x))
!
         close(10)
         ndata=0
endif   ! 1==2

         deallocate( w_pcld ) 
         deallocate( w_tcld ) 
         deallocate( w_frac ) 

      else
        ndata=0
      ENDIF
      ENDIF
!
!**************************************************************
!
      write(6,2300) istations
 2300 format(' processed ',i7,' stations')
      IF(obstype.eq.'cld') write (6,2320) numsao, ndata
 2320 format(' No. of surface / buoys  ',i6,3x,i6)
      IF(obstype.eq.'ctp') write (6,2370) numgoest, ndata
 2370 format(' No. of GOES / cloud top 1x1s    ',i6,3x,i6)
!
end subroutine read_prepbufr_cld
!**********************************************************************
!
!     subroutine comptime(itime,offset,obtime)
!
!     this routine will compute the observation time
!     input to the routine is the cycle time(itime) and the offset
!     which is the obtime - the cycle time in hours
!
!
subroutine comptime_RUC(itime,offset,obtime)
!    
      integer itime,obtime
      integer ihr, imin, noffset
      real offset
!
      ihr = itime/100
      imin = itime - (ihr)*100

!     get offset into min
!
      if(abs(offset).gt.9) then
                      obtime = itime
                      return
                      endif
!     reduce the offset to mins (get rid of the hours part)
      noffset = offset
      if(noffset.eq.0) go to 50
      ihr = ihr + noffset
      offset = offset -  noffset
!                       
 50   offset = offset*60.
!
      if(offset.lt.0) offset = offset - 0.5 !round to nearst min
      if(offset.gt.0) offset = offset + 0.5 !round
!
      imin = imin + offset
      if(imin.gt.60) then
                     ihr = ihr + 1
                     imin = imin - 60
                     endif
!
      if(imin.lt.0) then
                    imin = 60 + imin
                    ihr = ihr - 1
                    endif
!
      if(ihr.eq.24) ihr = 0
      if(ihr.eq.25) ihr = 1
      if(ihr.eq.-1) ihr = 23
      if(ihr.eq.-2) ihr = 22
!
      obtime = (ihr*100) + imin
!
      return
end subroutine comptime_RUC

SUBROUTINE JB_READ  ( ifn, parms, maxdta, dta, nprm, nlvl, iret )
!************************************************************************
!* JB_READ                                                              *
!*                                                                      * 
!* This subroutine reads a Jack Woollen BUFR data file.                 *
!*                                                                      *
!* To use this routine, first open a BUFR file using JB_OPEN.  Then,    *
!* call JB_NEXT to locate the next BUFR report.  Next, call JB_READ.    *
!* Refer to the BUFR Table associated with the file to specify a list   *
!* of parameters.  This list is stored in the string PARMS with the     *
!* individual names separated by spaces.  The names must be uppercase.  *
!* PARMS must be no more than 80 characters long.                       *
!*                                                                      *
!* This subroutine returns the data values in the REAL*8 array DTA.     *
!* The values in DTA are arranged by parameter, in the order specified  *
!* in PARMS.  If NLVL > 1, then the parameter sequence repeats itself   *
!* NLVL times, giving a profile of data for all of the parameters.      *
!* Therefore, it is not allowed to mix profile and non-profile          *
!* parameter names in the string PARMS.  In this subroutine, DTA is     *
!* treated as a single-dimension array of length MAXDTA, specified by   *
!* the calling program.  The returned array may be treated as a two     *
!* dimensional array of dimensions (NPRM, NLVL) in any subsequent       *
!* subroutine calls.                                                    *
!*                                                                      *
!* If the requested value is missing, the value in DTA is 1.0E+11.      *
!*                                                                      *
!* JB_READ  ( IFN, PARMS, MAXDTA, DTA, NPRM, NLVL, IRET )               *
!*                                                                      *
!* Input parameters:                                                    *
!* Input parameters:                                                    *
!*      IFN             INTEGER         File unit number                *
!*      PARMS           CHAR*           String of parameter names       *
!*      MAXDTA          INTEGER         Number of elements in DTA       *
!*                                                                      *
!* Output parameters:                                                   *
!*      DTA             REAL*8          Output data array               *
!*      NPRM            INTEGER         Number of parameters            *
!*      NLVL            INTEGER         Number of levels                *
!*      IRET            INTEGER         Return code                     *
!*                                       0 = normal return              *
!*                                      -1 = no data read               *
!*                                      -2 = no parms requested         *
!**                                                                     *
!* Log:                                                                 *
!* K. Brill/EMC         10/96                                           *
!************************************************************************
        REAL*8          dta (*)
        CHARACTER*(*)   parms
!*
        CHARACTER       ctst*1
        LOGICAL         count
!*-----------------------------------------------------------------------
        iret = 0
!
!*      Count the number of parameters in the input string.
!
        nprm = 0
        count = .true.
        istop = LEN ( parms )
        DO i = 1, istop
            ctst = parms ( i:i )
            IF ( ctst .ne. ' ' .and. count ) THEN
                nprm = nprm + 1
                count = .false.
            ELSE IF ( ctst .eq. ' ' .and. .not. count ) THEN
                count = .true.
            END IF
        END DO
        IF ( nprm .le. 0 ) then
            iret = -2
            RETURN
        END IF
!
!*      Compute maximum second dimenstion of DTA.
!
        ny = maxdta / nprm
!*
        CALL UFBINT ( ifn, dta, nprm, ny, nlvl, parms )
        IF ( nlvl .le. 0 ) THEN
            iret = -1
        END IF
!*
        RETURN
end subroutine  JB_READ

subroutine mape_ctp (nx,ny,mxline,ncount2,             &
                     otime,xlat,xlon,pcld,xfrac,tcld,  &
                     w_pcld,w_tcld,w_frac)
!subroutine mape_ctp (atime,w_pcld,w_tcld,w_frac,w_eca
!     &                     ,nlev_cld,ios)
!
!  Ming Hu,  March 2006
!     adapted according to RUC subroutine rd_cld
! *
! * This routine reads NESDIS (Madison, WI) cloud product produced
! *  from GOES sounder data. The original product is reprocessed onto
! *   MAPS40 grid boxes. There could be more than one cloud product
! *    in a grid-box, so we use the nearest one that falls in the
! *     grid. The routine combines GOES-8 and 10 products.
!
! Reference:
!   Kim and Benjamin, 2000: 9th AMS Conf. Satellite and Oceanography
!   Kim and Benjamin, 2000: 10th AMS Conf. Aviation and Range
!   Kim and Benjamin, 2001: 14th AMS Conf. Numerical Weather Prediction
!
! ===== History =====
! * Dongsoo Kim, 15 October 1998,  NOAA/FSL
! * Stan Benjamin - modifications - 1999
! * Dongsoo Kim - Feb. 2000;
! *     adopted search of nearest sounder data to the grid points
! * Dongsoo Kim - 22 Mar. 2000;
! *     include cloud-top temp argument to detect multi-cloud
! * Dongsoo Kim - 18 Jan. 2001; PACJET domain
! * Stan Benjamin- 1 Jun  2001  New faster method to map to RUC grid
! * Dongsoo Kim - 12 June 2001  Single FOV version,
!                    Adopt faster algorithm with selection option,
!                    replaces earlier code read_wiscld
!
! * Internal variables:
!     CTP_E, CTP_W           Soft-linked filename for ascii GOES Clouds
!
! * Working variables:
!     xlat, xlon (R4)       Latitude, longitude of ascii GOES Clouds
!     Pcld, Tcld (R4)       Could-top pressure and temperature
!     nfrac      (I4)       Effective fractional cloud coverage (%)
!
! * Working variables used for sorting max size of 10:
!     Pxx, Txx, xdist,xxxdist     (R4)
!     Fxx, Nxx, index, jndex      (I4)
!     ioption              (I4)  = 1  if selection is nearest neighbor
!                                = 2  if selection is median of samples
!
!
! * Output variables on gridpoint (Nx,Ny):
!     w_pcld, w_tcld (R4)   Cloud-top pressure and temperature
!     w_frac         (R4)   Effective fractional cloud coverage, option=1
!                           fractional coverage within RUC grid, option=2
!     w_eca          (R4)   Effective fractional cloud regardless option
!                             (effective cloud amount - eca)
!     nlev_cld       (I4)   Number of cloud levels. TO BE USED LATER
!                            to incorporate multi-level cloud
!
! * Calling routines
!     sorting
!     sortmed
!     txy2ll
!
! *
      use kinds, only: r_kind,r_double,i_kind
      use constants, only: zero,one_tenth,one,deg2rad
      use gridmod, only: regional,nsig,tll2xy,txy2ll
                         
      implicit none

      real(r_kind),parameter:: r360 = 360.0_r_kind

      integer Nx, Ny, mxline, nfov
      parameter (nfov=60)

      character header*80
! input-file variables:

      integer  iyr,iday,ihm,itime
      integer  nfrac(mxline)
      real     xlat(mxline),xlon(mxline)
      real     Pcld(mxline),Tcld(mxline)
      real     xfrac(mxline)
      real     otime(mxline)

! Working
      real     Pxx(Nx,Ny,nfov),Txx(Nx,Ny,nfov)  
      real     xdist(Nx,Ny,nfov), xxxdist(nfov)
      real     fr,sqrt, qc, type
      integer  Nxx(Nx,Ny,nfov),index(Nx,Ny), jndex(nfov)
      integer  ioption 
      integer  ipt,ixx,ii,jj,i,med_pt,igrid,jgrid  &
               ,ncount,ncount1,ncount2,ii1,jj1,nobs,n

      logical     ::outside     ! .false., then point is inside x-y domain
                              ! .true.,  then point is outside x-y domain
      real(r_kind) :: latitude, xc
      real(r_kind) :: longitude, yc

! Output
      integer  ios8, ios10
      real     w_pcld(Nx,Ny), w_tcld(Nx,Ny)
      real     w_frac(Nx,Ny), w_eca(Nx,Ny)
      integer  nlev_cld(Nx,Ny), ios
      character y4d3hh*9

!
! * Initialize outputs since GOES sounder do not scan all MAPS domain
!
      do jj=1,Ny
      do ii=1,Nx
         w_pcld(ii,jj) = 99999.
         w_tcld(ii,jj) = 99999.
         w_frac(ii,jj) = 99999.
         w_eca (ii,jj) =-99999.
         index(ii,jj) = 0
         nlev_cld(ii,jj) = -999
      enddo
      enddo

!
! * Read and process GOES sounder based clouds
!

!*** Start program
!      do 10 n=1,nobs
!        read(4,1750) oid   ,otime(n),xlat(n),xlon(n),
!     +               pcld(n),xfrac(n),tcld(n),qc,type
!        nfrac(n) = xfrac(n)
!   10 continue
      if(ncount2 <=0 ) return
      DO n=1,ncount2
        nfrac(n) = xfrac(n)
      ENDDO
! -- set ios as failed unless valid data points are found below
      ios = 0

! -----------------------------------------------------------
! -----------------------------------------------------------
!     Map each FOV onto RUC grid points 
! -----------------------------------------------------------
! -----------------------------------------------------------
      do 30 ipt=1,ncount2
 
! * Compute RUC grid x/y at lat/lon of cloud data
! -----------------------------------------------------------
          if(xlon(ipt) .ge. r360 ) xlon(ipt) = xlon(ipt)-r360
          if(xlon(ipt)  < zero ) xlon(ipt) = xlon(ipt)+r360
          latitude = xlat(ipt)*deg2rad
          longitude= xlon(ipt)*deg2rad
!
!     check to see if this lat/lon is within the GSI domain
          call tll2xy(longitude,latitude,xc,yc,outside)
 
! * XC,YC should be within RUC boundary, i.e., XC,YC >0
 
          if(XC .ge. 1. .and. XC .lt. Nx .and.        &
               YC .ge. 1. .and. YC .lt. Ny) then
             ii1 = int(xc+0.5)
             jj1 = int(yc+0.5)

             do jj = max(1,jj1-2), min(ny,jj1+2)
             if (jj1-1.ge.1 .and. jj1+1.le.ny) then
               do ii = max(1,ii1-2), min(nx,ii1+2)
               if (ii1-1.ge.1 .and. ii1+1.le.nx) then
             
! * We check multiple data within gridbox

                 if (index(ii,jj).lt.nfov) then
                   index(ii,jj) = index(ii,jj) + 1
 
                   Pxx(ii,jj,index(ii,jj)) = Pcld(ipt)
                   Txx(ii,jj,index(ii,jj)) = Tcld(ipt)
                   Nxx(ii,jj,index(ii,jj)) = nfrac(ipt)
                   nlev_cld(ii,jj) = 1
                   xdist(ii,jj,index(ii,jj)) = sqrt(      &
                      (XC+1-ii)**2 + (YC+1-jj)**2)
                 end if
               endif
               enddo ! ii
             endif
             enddo  ! jj
          endif  ! observation is in the domain
   30 continue
!
! * ioption = 1 is nearest neighrhood
! * ioption = 2 is median of cloudy fov
      ioption = 2
!
      do jj = 1,Ny
      do ii = 1,Nx
         if (index(ii,jj) .lt. 3 ) then
!           w_pcld(ii,jj) = Pxx(ii,jj,1)
!           w_tcld(ii,jj) = Txx(ii,jj,1)
!           w_frac(ii,jj) = float(Nxx(ii,jj,1))/100.
!           w_eca(ii,jj) =  float(Nxx(ii,jj,1))/100.

         elseif(index(ii,jj) .ge. 3) then

! * We decided to use nearest neighborhood for ECA values,
! *     a kind of convective signal from GOES platform...

             do i=1,index(ii,jj)
               jndex(i) = i
               xxxdist(i) = xdist(ii,jj,i)
             enddo
             call sorting(xxxdist,index(ii,jj),jndex)
            w_eca(ii,jj) = float(Nxx(ii,jj,jndex(1)))/100.
! * Sort to find closest distance if more than one sample
             if(ioption .eq. 1) then    !nearest neighborhood
                do i=1,index(ii,jj)
                  jndex(i) = i
                  xxxdist(i) = xdist(ii,jj,i)
                enddo
                call sorting(xxxdist,index(ii,jj),jndex)
                w_pcld(ii,jj) = Pxx(ii,jj,jndex(1))
                w_tcld(ii,jj) = Txx(ii,jj,jndex(1))
                w_frac(ii,jj) = float(Nxx(ii,jj,jndex(1)))/100.
             endif
! * Sort to find median value 
             if(ioption .eq. 2) then    !pick median 
                do i=1,index(ii,jj)
                  jndex(i) = i
                  xxxdist(i) = Pxx(ii,jj,i)
                enddo
                call sortmed(xxxdist,index(ii,jj),jndex,fr)
                med_pt = index(ii,jj)/2  + 1
                w_pcld(ii,jj) = Pxx(ii,jj,jndex(med_pt))
                w_tcld(ii,jj) = Txx(ii,jj,jndex(med_pt))
                w_frac(ii,jj) = fr
             endif
         endif
      enddo  !ii
      enddo  !jj
 
      return
end subroutine mape_ctp
 
subroutine sorting(d,n,is) 
      dimension d(n),is(n)
      nm1 = n-1 
      do 10 i=1,nm1 
      ip1 = i+1 
        do 10 j=ip1,n 
        if(d(i).le.d(j)) goto 10 
          temp = d(i) 
          d(i) = d(j) 
          d(j) = temp 
          iold  = is(i) 
          is(i) = is(j) 
          is(j) = iold 
   10 continue 
      return 
end subroutine  sorting

subroutine sortmed(p,n,is,f) 
      real p(n)
      integer is(n)
! * count cloudy fov
      real    f
      integer cfov
      cfov = 0
      do i=1,n
         if(p(i) .lt. 999.) cfov = cfov + 1
      enddo
      f = float(cfov)/(max(1,n))
! cloud-top pressure is sorted high cld to clear
      nm1 = n-1 
      do 10 i=1,nm1 
      ip1 = i+1 
        do 10 j=ip1,n 
        if(p(i).le.p(j)) goto 10 
          temp = p(i) 
          p(i) = p(j) 
          p(j) = temp 
          iold  = is(i) 
          is(i) = is(j) 
          is(j) = iold 
   10 continue 
      return 
end subroutine sortmed
