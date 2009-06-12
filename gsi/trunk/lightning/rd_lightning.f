      subroutine rd_lightning(atime,lightning,istat)
c 
c * Subroutine to read National Lightning Detection Network (NLDN)
c *    data and process onto RUC grid.
c *
c *   Stan Benjamin, Sept 2002, based on read-radar code
c*       by Dongsoo Kim

      INCLUDE 'IMPLICIT'
      INCLUDE 'MAPSCON'

      integer  nmax
      parameter (nmax   = 20000)

C    - strokes / hr / km**2
      real   lightning(nx_p,ny_p)

      integer min_before, min_after,ibef,ib,status,ibefa,iaft,iafta

      character*9  atime
      character*9  btime

      character  ATYPE*5, FILENAME*80
      character*2 min_a

      integer    i, j, istat, in,ig,jg
      real       xc,yc

      real  xlat(nmax),xlon(nmax)
      integer num_strikes
 
      min_before = 30
      min_after  = 10

C ************************************************************
C -- Pre-fill lightning data with zero
C ************************************************************
      do j=1,Ny_p
      do i=1,Nx_p
         lightning(i,j) = 0.
      enddo
      enddo
 
      ATYPE = '0005r'

      call chngtm (atime,-3600,btime,status)

      ibef = min_before/5
      iaft = min_after/5

C ************************************************************
C --  First, process 5-min periods before analysis time
C ************************************************************
      do 200 ib = 1,ibef

C     -- 12 5-min periods per hour
        ibefa = (12-ib)*5
        write (min_a,'(I2.2)') ibefa

        FILENAME ='NLDN_DAT'//'/'//btime(1:7)//min_a//ATYPE

        write(6,*) 'filename in rd_l',filename

        call  get_nldn_netcdf(FILENAME,
     1       xlat,xlon,nmax,num_strikes,istat)

        write(6,*) ' ISTAT / num. of strokes  =',ISTAT,num_strikes

        if (istat.eq.-1) go to 200

        do in = 1,num_strikes

          call W3FB11(xlat(in),xlon(in),
     &            LAT_LL_P,360.+LON_LL_P,DX_P,
     &            360.+LON_XX_P,LAT_TAN_P,XC,YC)

          ig = int(xc-0.5)
          jg = int(yc-0.5)

          if (ig.ge.1 .and. ig.le.nx_p .and.
     1       jg.ge.1 .and. jg.le.ny_p) 
     1      lightning(ig,jg) = lightning(ig,jg) + 1
        end do

200   continue


C ************************************************************
C --  First, process 5-min periods after analysis time
C ************************************************************
      do 300 ib = 1,iaft

C     -- 12 5-min periods per hour
        iafta = (ib-1)*5
        write (min_a,'(I2.2)') iafta

        write (6,*) 'filename in rd_l',filename
        FILENAME ='NLDN_DAT'//'/'//atime(1:7)//min_a//ATYPE

        call  get_nldn_netcdf(FILENAME,
     1       xlat,xlon,nmax,num_strikes,istat)

        write(6,*) ' ISTAT / num. of strokes  =',ISTAT,num_strikes

        if (istat.eq.-1) go to 300

        do in = 1,num_strikes

          call W3FB11(xlat(in),xlon(in),
     &            LAT_LL_P,360.+LON_LL_P,DX_P,
     &            360.+LON_XX_P,LAT_TAN_P,XC,YC)

          ig = int(xc-0.5)
          jg = int(yc-0.5)

          if (ig.ge.1 .and. ig.le.nx_p .and.
     1       jg.ge.1 .and. jg.le.ny_p) then

            lightning(ig,jg) = lightning(ig,jg) + 1
          end if
        end do
300   continue

      write(6,*) ' lightning - max/min'
      call outqv (lightning, nx_p, ny_p, 1)

      open (unit=14,file='light.dat',form='unformatted',
     1  convert='big_endian',status='unknown')
      write (14) lightning
      close (14)

      return
      end

c *************************************

      subroutine get_nldn_netcdf(FILENAME,
     1       xlat,xlon,nmax,num_strikes,istat)
 
C ************************************************************
c    * A subroutine to read NLDN lightning data
c    * Stan Benjamin, NOAA/FSL, 9 Sept 02
C ************************************************************

      include 'IMPLICIT'
      include 'NETCDFI'

      integer  ISTAT
      integer    nmax, status, nf_status
      integer nf_vid, ncid, nf_fid
 
c * output
      real    xlat (nmax),xlon(nmax)
      integer num_strikes

      CHARACTER FILENAME*80
 
      ISTAT = 0

      STATUS=NF_OPEN(FILENAME,NF_NOWRITE,NCID)
      write (6,*) 'filename',filename

c     STATUS=NF_OPEN ('NLDN_DAT',NF_NOWRITE,NCID)
      if (STATUS .ne. NF_NOERR) then
           ISTAT = -1
           print *, 'NLDN lightning is not available'
           return
      endif

C -- get number of strikes in time period

      nf_status = NF_INQ_DIMID(ncid,'strikeNum',nf_vid)
      if(nf_status.ne.NF_NOERR) then
        print *, NF_STRERROR(nf_status)
        print *,'dim strikeNum'
      endif
      nf_status = NF_INQ_DIMLEN(ncid,nf_vid, num_strikes)

      print *,'num_strikes =',num_strikes

C -- Get lat/lon for each strike

      nf_status = NF_INQ_VARID(NCID,'lat',nf_vid)
      nf_status = NF_GET_VAR_REAL(NCID,nf_vid, xlat)
      nf_status = NF_INQ_VARID(NCID,'lon',nf_vid)
      nf_status = NF_GET_VAR_REAL(NCID,nf_vid, xlon)
      nf_status = NF_CLOSE(ncid)

      write (6,*) ' lat/lon of report #1',xlat(1),xlon(1)

      RETURN
      END
