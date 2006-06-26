c********************************************************************
c   Copyright 1993, UCAR/Unidata
c   See netcdf/COPYRIGHT file for copying and redistribution conditions.
c   $Id: ftest-fujivp.f,v 1.3 2005/02/09 03:04:04 epourmal Exp $
c********************************************************************/



c
c     program to test the FUJIVP Fortran jacket interface to the netCDF
c
      program ftest
      
      include 'netcdf.inc'

c     name of first test cdf
      character*31 name
c     name of second test cdf
      character*31 name2
      
c     Returned error code.
      integer iret 
c     netCDF ID
      integer ncid
c     ID of dimension lat
      integer  latdim
c     ID of dimension lon
      integer londim
c     ID of dimension level
      integer leveldim
c     ID of dimension time
      integer timedim
c     ID of dimension len
      integer lendim

c     variable used to control error-handling behavior
      integer ncopts
      integer dimsiz(MAXNCDIM)
C      allowable roundoff 
      real epsilon
      common /dims/timedim, latdim, londim, leveldim, lendim,
     + dimsiz
      data name/'test.nc'/
      data name2/'copy.nc'/
      data epsilon /.000001/
      
100   format('*** Testing ', a, ' ...')
c     set error-handling to verbose and non-fatal
      ncopts = NCVERBOS
      call ncpopt(ncopts)

c     create a netCDF named 'test.nc'
      write(*,100) 'nccre'
      ncid = nccre(name, NCCLOB, iret)

c     test ncddef
      write(*,100) 'ncddef'
      call tncddef(ncid)

c     test ncvdef
      write(*,100) 'ncvdef'
      call tncvdef(ncid)

c     test ncapt
      write(*, 100) 'ncapt, ncaptc'
      call tncapt(ncid)

c     close 'test.nc'
      write(*, 100) 'ncclos'
      call ncclos(ncid, iret)

c     test ncvpt1
      write(*, 100) 'ncvpt1'
      call tncvpt1(name)

c     test ncvgt1
      write(*, 100) 'ncvgt1'
      call tncvgt1(name)

c     test ncvpt
      write(*, 100) 'ncvpt'
      call tncvpt(name)

c     test ncinq
      write(*, 100) 'ncopn, ncinq, ncdinq, ncvinq, ncanam, ncainq'
      call tncinq(name)

c     test ncvgt
      write(*, 100) 'ncvgt, ncvgtc'
      call tncvgt(name)

c     test ncagt
      write(*, 100) 'ncagt, ncagtc'
      call tncagt(name)

c     test ncredf
      write(*, 100) 'ncredf, ncdren, ncvren, ncaren, ncendf'
      call tncredf(name)

      call tncinq(name)

c     test ncacpy
      write(*, 100) 'ncacpy'
      call tncacpy(name, name2)

c     test ncadel
      write(*, 100) 'ncadel'
      call tncadel(name2)
      end
c
c     subroutine to test ncacpy
c
      subroutine tncacpy(iname, oname)
      character*31 iname, oname
      include 'netcdf.inc'
      integer ndims, nvars, natts, recdim, iret
      character*31 vname, attnam
      integer attype, attlen
      integer vartyp, nvdims, vdims(MAXVDIMS), nvatts
      integer lenstr
c     existing netCDF id
      integer incdf
c     netCDF id of the output netCDF file to which the attribute
c     will be copied
      integer outcdf

      integer mattlen
      parameter (mattlen = 80)
      character*80 charval
      double precision doubval(2)
      real flval(2)
      integer lngval(2)
      integer*2 shval(2)
      integer i, j, k
      character*31 varnam, attname(2,7), gattnam(2)
      integer*1 bytval(2)
      common /atts/attname, gattnam
      integer*2 svalidrg(2)
      real rvalidrg(2)
      integer lvalidrg(2)
      double precision dvalidrg(2)
      integer*1 bvalidrg(2)
      character*31 gavalue(2), cavalue(2)
      real epsilon

      data bvalidrg/1,110/
      data svalidrg/-100,100/
      data lvalidrg/0,360/
      data rvalidrg/0.0, 5000.0/
      data dvalidrg/0D0,500D0/
      data gavalue/'NWS', '88/10/25 12:00:00'/
      data cavalue/'test string', 'a'/
      data lenstr/80/	
      data epsilon /.000001/

      incdf = ncopn(iname, NCNOWRIT, iret)
      outcdf = nccre(oname, NCCLOB, iret)
      call tncddef(outcdf)
      call tncvdef(outcdf)
      call ncinq (incdf, ndims, nvars, natts, recdim, iret)
      do 5 j = 1, natts
         call ncanam (incdf, NCGLOBAL, j, attnam, iret)
         call ncacpy (incdf, NCGLOBAL, attnam, outcdf, NCGLOBAL, iret)
 5    continue
      do 10 i = 1, nvars
         call ncvinq (incdf, i, vname, vartyp, nvdims,
     +        vdims, nvatts, iret)
         do 20 k = 1, nvatts
            call ncanam (incdf, i, k, attnam, iret)
            call ncacpy (incdf, i, attnam, outcdf, i, iret)
 20      continue
 10   continue
c     
c     get global attributes first
c     
      do 100 i = 1, natts
         call ncanam (outcdf, NCGLOBAL, i, attnam, iret)
         call ncainq (outcdf, NCGLOBAL, attnam, attype, attlen,
     +        iret)
         if (attlen .gt. mattlen) then
            write (*,*) 'global attribute too big!', attlen, mattlen
            stop 'Stopped'
         else if (attype .eq. NCBYTE) then
            call ncagt (outcdf, NCBYTE, attnam, bytval, iret)
         else if (attype .eq. NCCHAR) then
            call ncagtc (outcdf, NCGLOBAL, attnam, charval, 
     +           lenstr, iret)
            if (attnam .ne. gattnam(i)) write(*,*) 'error in ncagt G'
            if (charval .ne. gavalue(i))
     + write(*,*) 'error in ncagt G2', lenstr, charval, gavalue(i)
                  charval = ' '                                   
         else if (attype .eq. NCSHORT) then
            call ncagt (outcdf, NCGLOBAL, attnam, shval, iret) 
         else if (attype .eq. NCLONG) then
            call ncagt (outcdf, NCGLOBAL, attnam, lngval, iret)            
         else if (attype .eq. NCFLOAT) then
            call ncagt (outcdf, NCGLOBAL, attnam, flval, iret)
         else 
            call ncagt (outcdf, NCGLOBAL, attnam, doubval,iret)          
         end if
 100   continue
c
c     get variable attributes
c
      do 200 i = 1, nvars
         call ncvinq (outcdf, i, varnam, vartyp, nvdims, vdims,
     +                nvatts, iret)
         do 250 j = 1, nvatts
            call ncanam (outcdf, i, j, attnam, iret)
            call ncainq (outcdf, i, attnam, attype, attlen,
     +                   iret)
            if (attlen .gt. mattlen) then
               write (*,*) 'variable ', i,  'attribute too big !'
               stop 'Stopped'
            else 
               if (attype .eq. NCBYTE) then
                  call ncagt (outcdf, i, attnam, bytval, 
     +                 iret)
                  if (attnam .ne. attname(j,i)) write(*,*)
     + 'error in ncagt BYTE N'
                  if (bytval(j) .ne. bvalidrg(j)) write(*,*)
     + 'ncacpy: byte ', bytval(j), ' .ne. ', bvalidrg(j)
               else if (attype .eq. NCCHAR) then
                  call ncagtc (outcdf, i, attnam, charval, 
     +                 lenstr, iret)
                  if (attnam .ne. attname(j,i)) write(*,*)
     + 'error in ncagt CHAR N'
                  if (charval .ne. cavalue(j)) write(*,*)
     + 'error in  ncagt'
                  charval = ' '                                    
               else if (attype .eq. NCSHORT) then
                  call ncagt (outcdf, i, attnam, shval, 
     +                 iret)  
                  if (attnam .ne. attname(j,i)) write(*,*)
     + 'error in ncagt SHORT N'
                  if (shval(j) .ne. svalidrg(j)) then
                     write(*,*) 'error in ncagt SHORT'
                  end if
               else if (attype .eq. NCLONG) then
                  call ncagt (outcdf, i, attnam, lngval, 
     +                 iret)
                  if (attnam .ne. attname(j,i)) write(*,*)
     + 'error in ncagt LONG N'
                  if (lngval(j) .ne. lvalidrg(j)) write(*,*)
     + 'error in ncagt LONG'
               else if (attype .eq. NCFLOAT) then
                  call ncagt (outcdf, i, attnam, flval, 
     +                 iret)            
                  if (attnam .ne. attname(j,i)) write(*,*)
     + 'error in ncagt FLOAT N'
                  if (flval(j) .ne. rvalidrg(j)) write(*,*)
     + 'error in ncagt FLOAT'
               else if (attype .eq. NCDOUBLE) then
                  call ncagt (outcdf, i, attnam, doubval,
     +                 iret)          
                  if (attnam .ne. attname(j,i)) write(*,*)
     + 'error in ncagt DOUBLE N'
                  if ( abs(doubval(j) - dvalidrg(j)) .gt. epsilon)
     + write(*,*) 'error in ncagt DOUBLE'
               end if
            end if
 250     continue
 200   continue
      call ncclos(incdf, iret)
      call ncclos(outcdf, iret)
      return
      end


      
c     
c     subroutine to test ncadel
c
      subroutine tncadel (cdfname)
      character*31 cdfname
      include 'netcdf.inc'
      
      integer  bid, sid, lid, fid, did, cid, chid
      common /vars/bid, sid, lid, fid, did, cid, chid
      integer ncid, iret, i, j
      integer ndims, nvars, natts, recdim
      integer vartyp, nvdims, vdims(MAXVDIMS), nvatts
      character*31 varnam, attnam

      ncid = ncopn(cdfname, NCWRITE, iret)
c     put cdf in define mode
      call ncredf (ncid,iret)
c     get number of global attributes
      call ncinq (ncid, ndims, nvars, natts, recdim, iret)
      do 10 i = natts, 1, -1
c     get name of global attribute
         call ncanam (ncid, NCGLOBAL, i, attnam, iret)
c     delete global attribute
         call ncadel (ncid, NCGLOBAL, attnam, iret)
 10   continue

      do 100 i = 1, nvars
c     get number of variable attributes
         call ncvinq (ncid, i, varnam, vartyp, nvdims, vdims,
     +        nvatts, iret)
         do 200 j = nvatts, 1, -1
            call ncanam (ncid, i, j, attnam, iret)
            call ncadel (ncid, i, attnam, iret)
 200     continue
 100  continue
      call ncinq (ncid, ndims, nvars, natts, recdim, iret)
      if (natts .ne. 0) write(*,*) 'error in ncadel'
c     put netCDF into data mode
      call ncendf (ncid, iret)
      call ncclos (ncid, iret)
      return
      end

c
c     subroutine to test ncagt and ncagtc

      subroutine tncagt(cdfname)
      include 'netcdf.inc'
      character*31 cdfname
            
c     maximum length of an attribute
      integer mattlen
      parameter (mattlen = 80)
      integer ncid, ndims, nvars, natts, recdim
      integer bid, sid, lid, fid, did, cid, chid
      common /vars/bid, sid, lid, fid, did, cid, chid
      integer i, j
      integer attype, attlen, lenstr, iret
      character*31 attnam
      character*80 charval
      double precision doubval(2)
      real flval(2)
      integer lngval(2)
      integer*2 shval(2)
      integer*1 bytval(2)
      integer vartyp, nvdims, vdims(MAXVDIMS), nvatts

      character*31 varnam, attname(2,7), gattnam(2)
      common /atts/attname, gattnam
      integer*2 svalidrg(2)
      real rvalidrg(2)
      integer lvalidrg(2)
      double precision dvalidrg(2)
      integer*1 bvalidrg(2)
      character*31 gavalue(2), cavalue(2)
      real epsilon

      data bvalidrg/1,110/
      data svalidrg/-100,100/
      data lvalidrg/0,360/
      data rvalidrg/0.0, 5000.0/
      data dvalidrg/0D0,500D0/
      data gavalue/'NWS', '88/10/25 12:00:00'/
      data cavalue/'test string', 'a'/
      data lenstr/80/	
      data epsilon /.000001/
      
      ncid = ncopn (cdfname, NCNOWRIT, iret)
      call ncinq (ncid, ndims, nvars, natts, recdim, iret)
c     
c     get global attributes first
c     
      do 10 i = 1, natts
c     get name of attribute
         call ncanam (ncid, NCGLOBAL, i, attnam, iret)
c     get attribute type and length
         call ncainq (ncid, NCGLOBAL, attnam, attype, attlen,
     +        iret)
         if (attlen .gt. mattlen) then
            write (*,*) 'global attribute too big!'
            stop 'Stopped'
         else if (attype .eq. NCBYTE) then
            call ncagt (ncid, NCBYTE, attnam, bytval, iret)
         else if (attype .eq. NCCHAR) then
            call ncagtc (ncid, NCGLOBAL, attnam, charval, 
     +           lenstr, iret)
            if (attnam .ne. gattnam(i)) write(*,*) 'error in ncagt'
            if (charval .ne. gavalue(i)) write(*,*) 'error in ncagt'
            charval = '                                        '
         else if (attype .eq. NCSHORT) then
            call ncagt (ncid, NCGLOBAL, attnam, shval, iret) 
         else if (attype .eq. NCLONG) then
            call ncagt (ncid, NCGLOBAL, attnam, lngval, iret)            
         else if (attype .eq. NCFLOAT) then
            call ncagt (ncid, NCGLOBAL, attnam, flval, iret)
         else 
            call ncagt (ncid, NCGLOBAL, attnam, doubval,iret)          
         end if
 10   continue

c
c     get variable attributes
c
      do 20 i = 1, nvars
         call ncvinq (ncid, i, varnam, vartyp, nvdims, vdims,
     +                nvatts, iret)
         do 25 j = 1, nvatts
            call ncanam (ncid, i, j, attnam, iret)
            call ncainq (ncid, i, attnam, attype, attlen,
     +                   iret)
            if (attlen .gt. mattlen) then
               write (*,*) 'variable ', i,  'attribute too big !'
               stop 'Stopped'
            else 
               if (attype .eq. NCBYTE) then
                  call ncagt (ncid, i, attnam, bytval, 
     +                 iret)
                  if (attnam .ne. attname(j,i)) write(*,*)
     + 'error in ncagt BYTE name'
                  if (bytval(j) .ne. bvalidrg(j)) write(*,*)
     + 'ncacpy: byte ', bytval(j), ' .ne. ', bvalidrg(j)
               else if (attype .eq. NCCHAR) then
                  call ncagtc (ncid, i, attnam, charval, 
     +                 lenstr, iret)
                  if (attnam .ne. attname(j,i)) write(*,*)
     + 'error in ncagt CHAR name'
                  if (charval .ne. cavalue(j)) write(*,*)
     + 'error in ncagt CHAR name'
	         charval = '                                        '
               else if (attype .eq. NCSHORT) then
                  call ncagt (ncid, i, attnam, shval, 
     +                 iret)  
                  if (attnam .ne. attname(j,i)) write(*,*)
     + 'error in ncagt SHORT name'
                  if (shval(j) .ne. svalidrg(j)) then
                     write(*,*) 'error in ncagt SHORT'
                  end if
               else if (attype .eq. NCLONG) then
                  call ncagt (ncid, i, attnam, lngval, 
     +                 iret)
                  if (attnam .ne. attname(j,i)) write(*,*)
     + 'error in ncagt LONG name'
                  if (lngval(j) .ne. lvalidrg(j)) write(*,*)
     + 'error in ncagt LONG'
               else if (attype .eq. NCFLOAT) then
                  call ncagt (ncid, i, attnam, flval, 
     +                 iret)            
                  if (attnam .ne. attname(j,i)) write(*,*)
     + 'error in ncagt FLOAT name'
                  if (flval(j) .ne. rvalidrg(j)) write(*,*)
     + 'error in ncagt FLOAT'
               else if (attype .eq. NCDOUBLE) then
                  call ncagt (ncid, i, attnam, doubval,
     +                 iret)          
                  if (attnam .ne. attname(j,i)) write(*,*)
     + 'error in ncagt DOUBLE name'
                  if ( abs(doubval(j) - dvalidrg(j)) .gt. epsilon)
     + write(*,*) 'error in ncagt DOUBLE'
               end if
            end if
 25      continue
 20   continue
      call ncclos(ncid, iret)
      return
      end
c
c     subroutine to test ncapt
c
      subroutine tncapt (ncid)
      include 'netcdf.inc'
      integer ncid, iret

c attribute vectors
      integer*2 svalidrg(2)
      real rvalidrg(2)
      integer lvalidrg(2)
      double precision dvalidrg(2)
      integer*1 bvalidrg(2)

c     variable ids
      integer  bid, sid, lid, fid, did, cid, chid
      common /vars/bid, sid, lid, fid, did, cid, chid

c assign attributes
      
c
c     byte
c
      
      bvalidrg(1) = 1
      bvalidrg(2) = 250
      call ncapt (ncid, bid, 'valid range', NCBYTE, 2,
     +bvalidrg, iret)

c
c     short
c

      svalidrg(1) = -100
      svalidrg(2) = 100
      call ncapt (ncid, sid, 'valid range', NCSHORT, 2, 
     +svalidrg, iret)

c
c     long
c

      lvalidrg(1) = 0
      lvalidrg(2) = 360
      call ncapt (ncid, lid, 'valid range', NCLONG, 2,
     +lvalidrg, iret)
      
c
c     float
c

      rvalidrg(1) = 0.0
      rvalidrg(2) = 5000.0
      call ncapt (ncid, fid, 'valid range', NCFLOAT, 2,
     +rvalidrg, iret)

c
c     double
c

      dvalidrg(1) = 0D0
      dvalidrg(2) = 500D0
      call ncapt (ncid, did, 'valid range', NCDOUBLE, 2,
     +dvalidrg, iret)

c
c     global
c

      call ncaptc (ncid, NCGLOBAL, 'source', NCCHAR, 3, 
     +'NWS', iret)
      call ncaptc (ncid, NCGLOBAL, 'basetime', NCCHAR, 17, 
     +'88/10/25 12:00:00', iret)

c
c     char
c

      call ncaptc (ncid, chid, 'longname', NCCHAR, 11,
     +'test string', iret)

      call ncaptc (ncid, chid, 'id', NCCHAR, 1,
     +'a', iret)

      return
      end

c
c     initialize variables in labelled common blocks
c
      block data
      common /cdims/ dimnam
      common /dims/timedim, latdim, londim, leveldim, lendim,
     + dimsiz
      common /varn/varnam
      common /atts/attname, gattnam
      integer  latdim, londim, leveldim, timedim, lendim

c     should include 'netcdf.inc' for MAXNCDIM, but it has EXTERNAL
c     declaration, which is not permitted in a BLOCK DATA unit.

c      integer dimsiz(MAXNCDIM)
      integer dimsiz(32)
c      character*31 dimnam(MAXNCDIM)
      character*31 dimnam(32)
      character*31 varnam(7)
      character*31 attname(2,7)
      character*31 gattnam(2)

      data dimnam /'time', 'lat', 'lon', 'level',
     + 'length', 27*'0'/
      data dimsiz /4, 5, 5, 4, 80, 27*0/
      data varnam/'bytev', 'short v', 'longv', 'floatv', 'doublev', 
     + 'chv', 'cv'/
      
      data attname/'valid range', '0', 'valid range',
     + '0', 'valid range',
     + '0', 'valid range', '0', 'valid range', '0', 'longname', 'id',
     + '0', '0'/
      data gattnam/'source','basetime'/
      end


c
c     subroutine to test ncddef
c

      subroutine tncddef(ncid)
      include 'netcdf.inc'
      integer ncid

c     sizes of dimensions of 'test.nc' and 'copy.nc'
      integer  ndims
      parameter(ndims=5)
c dimension ids
      integer  latdim, londim, leveldim, timedim, lendim
      integer iret
c     function to define a netCDF dimension
      integer dimsiz(MAXNCDIM)
      character*31 dimnam(MAXNCDIM)
      
      common /dims/timedim, latdim, londim, leveldim, lendim,
     + dimsiz
      common /cdims/ dimnam

c define dimensions
      timedim = ncddef(ncid, dimnam(1), NCUNLIM, iret)
      latdim = ncddef(ncid, dimnam(2), dimsiz(2), iret)
      londim = ncddef(ncid, dimnam(3), dimsiz(3), iret)
      leveldim = ncddef(ncid, dimnam(4), dimsiz(4), iret)
      lendim = ncddef(ncid, dimnam(5), dimsiz(5), iret)
      return
      end
c
c     subroutine to test ncinq, ncdinq, ncdid, ncvinq, ncanam
c     and ncainq
c
      subroutine tncinq(cdfname)
      include 'netcdf.inc'
      character*31 cdfname

c     netCDF id
      integer ncid
c     returned number of dimensions
      integer ndims
c     returned number of variables
      integer nvars
c     returned number of global attributes
      integer natts
c     returned id of the unlimited dimension
      integer recdim
c     returned error code
      integer iret
c     returned name of record dimension
      character*31 recnam
c     returned size of record dimension
      integer recsiz
c     loop control variables
      integer i, j, k
c     returned size of dimension
      integer dsize
c     returned dimension ID
      integer dimid
c     returned dimension name
      character*31 dname
c     returned variable name
      character*31 vname
c     returned attribute name
      character*31 attnam
c     returned netCDF datatype of variable
      integer vartyp
c     returned number of variable dimensions
      integer nvdims
c     returned number of variable attributes
      integer nvatts
c     returned vector of nvdims dimension IDS corresponding to the
c     variable dimensions
      integer vdims(MAXNCDIM)
c     returned attribute length
      integer attlen
c     returned attribute type
      integer attype
      character*31 dimnam(MAXNCDIM)
      character*31 varnam(7)
      character*31 attname(2,7)
      character*31 gattnam(2)
      integer vdlist(5,7), vtyp(7), vndims(7), vnatts(7)
      integer attyp(2,7),atlen(2,7),gattyp(2),gatlen(2)
      integer timedim,latdim,londim,leveldim,lendim
      integer dimsiz(MAXNCDIM)
      common /dims/timedim, latdim, londim, leveldim, lendim,
     + dimsiz
      common /varn/varnam
      common /atts/attname, gattnam
      common /cdims/ dimnam

      data vdlist/1,0,0,0,0,1,0,0,0,0,2,0,0,0,0,4,3,2,1,0,4,3,2,1,0,
     + 5,1,0,0,0,1,0,0,0,0/
      data vtyp/NCBYTE, NCSHORT, NCLONG, NCFLOAT, NCDOUBLE, NCCHAR,
     + NCCHAR/
      data vndims/1,1,1,4,4,2,1/
      data vnatts/1,1,1,1,1,2,0/
      data attyp/NCBYTE, 0, NCSHORT, 0, NCLONG, 0, NCFLOAT, 0,
     + NCDOUBLE, 0, NCCHAR, NCCHAR, 0, 0/
      data atlen/2,0,2,0,2,0,2,0,2,0,11,1, 0, 0/
      data gattyp/NCCHAR,NCCHAR/
      data gatlen/3,17/

      ncid = ncopn (cdfname, NCNOWRIT, iret)
      call ncinq (ncid, ndims, nvars, natts, recdim, iret)
      if (ndims .ne. 5) write(*,*) 'error in ncinq or ncddef'
      if (nvars .ne. 7) write(*,*) 'error in ncinq or ncvdef'
      if (natts .ne. 2) write(*,*) 'error in ncinq or ncapt'
      call ncdinq (ncid, recdim, recnam, recsiz, iret)
      if (recnam .ne. 'time') write(*,*) 'error: bad recdim from ncinq'
c
c     dimensions
c
      do 10 i = 1, ndims
         call ncdinq (ncid, i, dname, dsize, iret)
         if (dname .ne. dimnam(i)) write(*,*)
     + 'error in ncdinq or  ncddef, dname=', dname
         if (dsize .ne. dimsiz(i)) write(*,*)
     + 'error in ncdinq or ncddef, dsize=',dsize
         dimid = ncdid (ncid, dname, iret)
         if (dimid .ne. i) write(*,*)
     +      'error in ncdinq or ncddef, dimid=', dimid
 10   continue
c
c     variables
c
      do 30 i = 1, nvars
         call ncvinq (ncid, i, vname, vartyp, nvdims,
     +        vdims, nvatts, iret)
         if (vname .ne. varnam(i)) write(*,*)
     + 'error: from ncvinq, wrong name returned: ', vname,
     + ' .ne. ', varnam(i)
         if (vartyp .ne. vtyp(i)) write(*,*)
     + 'error: from ncvinq, wrong type returned: ', vartyp,
     + ' .ne. ', vtyp(i)
         if (nvdims .ne. vndims(i)) write(*,*)
     + 'error: from ncvinq, wrong num dims returned: ', vdims,
     + ' .ne. ', vndims(i)
         do 35 j = 1, nvdims
            if (vdims(j) .ne. vdlist(j,i)) write(*,*)
     + 'error: from ncvinq wrong dimids: ', vdims(j),
     + ' .ne. ', vdlist(j,i)
 35      continue
         if (nvatts .ne. vnatts(i)) write(*,*)
     + 'error in ncvinq or ncvdef'
c
c     attributes
c
         do 45 k = 1, nvatts
            call ncanam (ncid, i, k, attnam, iret)
            call ncainq (ncid, i, attnam, attype, attlen, iret)
            if (attnam .ne. attname(k,i)) write(*,*)
     + 'error in ncanam or ncapt'
            if (attype .ne. attyp(k,i)) write(*,*)
     + 'error in ncainq or ncapt'
            if (attlen .ne. atlen(k,i)) write(*,*)
     + 'error in ncainq or ncapt'
 45      continue
 30   continue
      do 40 i = 1, natts
         call ncanam (ncid, NCGLOBAL, i, attnam, iret)
         call ncainq (ncid, NCGLOBAL, attnam, attype, attlen, iret)
         if (attnam .ne. gattnam(i)) write(*,*)
     + 'error in ncanam or ncapt'
         if (attype .ne. gattyp(i)) write(*,*)
     + 'error in ncainq or ncapt'
         if (attlen .ne. gatlen(i)) write(*,*)
     + 'error in ncainq or ncapt'
 40   continue
      call ncclos(ncid, iret)
      return
      end
      
      
      
c     subroutine to test ncredf, ncdren, ncvren, ncaren, and 
c     ncendf

      subroutine tncredf(cdfname)
      include 'netcdf.inc'
      character*31 cdfname
      character*31 attname(2,7)
      character*31 gattnam(2)
      common /atts/attname, gattnam
      common /cdims/ dimnam
      character*31 dimnam(MAXNCDIM)
      character*31 varnam(7)
      common /varn/varnam
      integer ncid, iret, latid, varid

      dimnam(2) = 'latitude'
      varnam(4) = 'realv'
      attname(1,6) = 'stringname'
      gattnam(1) = 'agency'
      ncid = ncopn(cdfname, NCWRITE, iret)
      call ncredf(ncid, iret)
      latid = ncdid(ncid, 'lat', iret)
      call ncdren(ncid, latid, 'latitude', iret)
      varid = ncvid(ncid, 'floatv', iret)
      call ncvren(ncid, varid, 'realv', iret)
      varid = ncvid(ncid, 'chv', iret)
      call ncaren(ncid, varid, 'longname', 'stringname', iret)
      call ncaren(ncid, NCGLOBAL, 'source', 'agency', iret)
      call ncendf(ncid, iret)
      call ncclos(ncid, iret)
      return
      end
c     
c     subroutine to test ncvdef
c

      subroutine tncvdef(ncid)
      include 'netcdf.inc'
      integer ncid

c     function to define a netCDF variable
      integer dimsiz(MAXNCDIM)
      integer  latdim, londim, leveldim, timedim, lendim
      common /dims/timedim, latdim, londim, leveldim, lendim, 
     + dimsiz

c variable ids
      integer  bid, sid, lid, fid, did, cid, chid
      common /vars/bid, sid, lid, fid, did, cid, chid

c variable shapes
      integer  bdims(1), fdims(4), ddims(4), ldims(1), sdims(1) 
      integer chdims(2), cdims(1)

      integer iret
c
c define variables
c
c     byte
c 
      bdims(1) = timedim
      bid = ncvdef(ncid, 'bytev', NCBYTE, 1, bdims, iret)
c
c     short
c
      sdims(1) = timedim
      sid = ncvdef (ncid, 'short v', NCSHORT, 1, sdims, iret)
c
c     long
c
      ldims(1) = latdim
      lid = ncvdef (ncid, 'longv', NCLONG, 1, ldims, iret)
c
c     float
c
      fdims(4) = timedim
      fdims(1) = leveldim
      fdims(2) = londim
      fdims(3) = latdim
      fid = ncvdef (ncid, 'floatv', NCFLOAT, 4, fdims, iret)
c
c     double
c
      ddims(4) = timedim
      ddims(1) = leveldim
      ddims(2) = londim
      ddims(3) = latdim
      did = ncvdef (ncid, 'doublev', NCDOUBLE, 4, ddims, iret)
c
c     char
c
      chdims(2) = timedim
      chdims(1) = lendim
      chid = ncvdef (ncid, 'chv', NCCHAR, 2, chdims, iret)

      cdims(1) = timedim
      cid = ncvdef (ncid, 'cv', NCCHAR, 1, cdims, iret)


      return
      end


c    
c     subroutine to test ncvgt and ncvgtc
c
      subroutine tncvgt(cdfname)
      include 'netcdf.inc'
      character*31 cdfname

      integer ndims, times, lats, lons, levels, lenstr
      parameter (times=4, lats=5, lons=5, levels=4)

      integer start(MAXNCDIM), count(MAXNCDIM)
      integer ncid, iret, i, m
      integer  latdim, londim, leveldim, timedim, lendim
      integer dimsiz(MAXNCDIM)
      common /dims/timedim, latdim, londim, leveldim, lendim,
     + dimsiz

      integer bid, sid, lid, fid, did, cid, chid
      common /vars/bid, sid, lid, fid, did, cid, chid
      integer itime, ilev, ilat, ilon

c     arrays of data values to be read
      integer*1 barray(times), byval(times)
      integer*2 sarray(times), shval(times)
      integer larray(lats)
      real farray(levels, lats, lons, times)
      double precision darray(levels, lats, lons, times)
c     character array of data values to be read
      character*31 string
      character*31 varnam
      integer nvars, natts, recdim
      integer vartyp, nvdims, vdims(MAXVDIMS), nvatts

      data start/1,1,1,1, 28*0/, count/levels, lats, lons, times, 28*0/
      data byval /97, 98, 99, 100/
      data shval /10, 11, 12, 13/

      ncid = ncopn (cdfname, NCWRITE, iret)
c     get number of variables in netCDF
      call ncinq (ncid, ndims, nvars, natts, recdim, iret)
      do 5 m = 1, nvars-1
c     get variable name, datatype, number of dimensions
c     vector of dimension ids, and number of variable attributes
         call ncvinq (ncid, m, varnam, vartyp, nvdims, vdims,
     +                nvatts, iret)
         if (vartyp .eq. NCBYTE) then
c
c     byte
c
            count(1) = times
            call ncvgt (ncid, m, start, count, barray, iret)
            do 10 i = 1, times
               if (barray(i) .ne. byval(i)) then 
                  write(*,*) 'ncvgt of bytes, got ', barray(i), ' .ne. '
     +			     , byval(i)
               end if
 10         continue
         else if (vartyp .eq. NCSHORT) then
c
c     short
c
            count(1) = times
            call ncvgt (ncid, m, start, count, sarray, iret)
            do 20 i = 1, times
               if (sarray(i) .ne. shval(i)) then 
                  write(*,*) 'ncvgt of short, got ', sarray(i), ' .ne. '
     +			     , shval(i)
               end if
 20         continue
         else if (vartyp .eq. NCLONG) then
c     
c     long
c
            count(1) = lats
            call ncvgt (ncid, m, start, count, larray, iret)
            do 30 i = 1, lats
               if (larray(i) .ne. 1000) then 
                  write(*,*) 'long error in ncvgt'
               end if
 30         continue
         else if (vartyp .eq. NCFLOAT) then
c     
c     float
c
            count(1) = levels
            call ncvgt (ncid, m, start, count, farray, iret)
            i = 0
            do 40 itime = 1,times
               do 40 ilon = 1, lons
                  do 40 ilat = 1, lats
                     do 40 ilev = 1, levels
                        i = i + 1
                        if (farray(ilev, ilat, ilon, itime) .ne.
     + real(i)) then
                           write (*,*) 'float error in ncvgt'
                        end if
 40         continue
         else if (vartyp .eq. NCDOUBLE) then
c
c     double
c
            count(1) = levels
            call ncvgt (ncid, m, start, count, darray, iret)
            i = 0
            do 50 itime = 1, times
               do 50 ilon = 1, lons
                  do 50 ilat = 1, lats
                     do 50 ilev = 1, levels
                        i = i + 1
                        if (darray(ilev, ilat, ilon, itime) .ne.
     +                       real(i)) then
                           write(*,*) 'double error in ncvgt:', i,
     +              darray(ilev, ilat, ilon, itime), '.ne.', real(i)
                        end if
 50         continue
         else 
c     
c     char
c
	    count(1) = 3
	    count(2) = 4
	    lenstr = 31
            call ncvgtc (ncid, m, start, count, string, lenstr, iret)
            if (string .ne. 'testhikin of') then 
               write(*,*) 'error in ncvgt, returned string =', string
            end if
         end if
 5    continue
      call ncclos(ncid, iret)
      return
      end

      
      subroutine tncvgt1(cdfname)
      include 'netcdf.inc'
      character*31 cdfname

      integer ncid, iret
      integer  latdim, londim, leveldim, timedim, lendim
      integer dimsiz(MAXNCDIM)
      common /dims/timedim, latdim, londim, leveldim, lendim,
     + dimsiz

      integer bindx, sindx, lindx, findx(4), dindx(4), cindx

      integer bid, sid, lid, fid, did, cid, chid
      common /vars/bid, sid, lid, fid, did, cid, chid

      integer*1 bvalue
      integer*2 svalue
      integer lvalue
      real fvalue
      double precision dvalue
      character*1 c
      real epsilon
      double precision onethird

      data epsilon /.000001/
      data lindx/1/, bindx/1/, sindx/1/, findx/1,1,1,1/
     +dindx/1,1,1,1/, cindx/1/
      data onethird/0.3333333333D0/
      
      ncid = ncopn (cdfname, NCNOWRIT, iret)
c
c     test ncvgt1 for byte
c
      call ncvgt1 (ncid, bid, bindx, bvalue, iret)
      if (bvalue .ne. ichar('z')) write(*,*) 'error in ncvgt1 byte:',
     + bvalue, ' .ne.', ichar('z')
c
c     test ncvgt1 for short
c
      call ncvgt1 (ncid, sid, sindx, svalue, iret)
      if (svalue .ne. 10) write(*,*) 'error in ncvgt1 short:',
     + svalue, ' .ne.', 10
c     
c     test ncvgt1 for long
c
      call ncvgt1 (ncid, lid, lindx, lvalue, iret)
      if (lvalue .ne. 1000) write(*,*) 'error in ncvgt1 long:',
     + lvalue,  ' .ne.', 1000
c
c     test ncvgt1 for float
c
      call ncvgt1 (ncid, fid, findx, fvalue, iret)
      if (abs(fvalue - 3.14159) .gt. epsilon) write(*,*)
     + 'error in ncvgt1 float:', fvalue, ' not close to', 3.14159
c
c     test ncvgt1 for double
c
      call ncvgt1 (ncid, did, dindx, dvalue, iret)
      if (abs(dvalue - onethird) .gt. epsilon) write(*,*)
     + 'error in ncvgt1 double:', dvalue, ' not close to',
     +     onethird
c
c     test ncvg1c for char
c
      call ncvg1c (ncid, cid, cindx, c, iret)
      if (c .ne. 'a') write(*,*) 'error in ncvg1c'
      call ncclos(ncid, iret)
      return
      end

      
      
c
c     subroutine to test ncvpt and ncvptc
c
      subroutine tncvpt(cdfname)
      include 'netcdf.inc'
      character*31 cdfname

c     size of dimensions
      integer times, lats, lons, levels
      parameter (times=4, lats=5, lons=5, levels=4)

      integer ncid, iret
c     loop control variables
      integer itime, ilev, ilon, ilat, i
      integer  latdim, londim, leveldim, timedim, lendim
      integer dimsiz(MAXNCDIM)
      common /dims/timedim, latdim, londim, leveldim, lendim,
     + dimsiz
      integer lenstr
      integer bid, sid, lid, fid, did, cid, chid
      common /vars/bid, sid, lid, fid, did, cid, chid

c     vector of integers specifying the corner of the  hypercube
c     where the first of the data values will be written
      integer start(MAXNCDIM)
c     vector of integers specifying the edge lengths from the
c     corner of the hypercube where the first of the data values
c     will be written
      integer count(MAXNCDIM)

c     arrays of data values to be written
      integer*1 barray(times)
      integer*2 sarray(times)
      integer larray(lats)
      real farray(levels, lats, lons, times)
      double precision darray(levels, lats, lons, times)
      character*31 string

      data start/1,1,1,1, 28*0/, count/levels, lats, lons, times, 28*0/
      data barray /97, 98, 99, 100/
      data sarray /10, 11, 12, 13/

      ncid = ncopn (cdfname, NCWRITE, iret)

c
c     byte
c
      count(1) = times
      call ncvpt (ncid, bid, start, count, barray, iret)
c
c     short
c
      count(1) = times
      call ncvpt (ncid, sid, start, count, sarray, iret)
c
c     long
c
      do 30 i = 1,lats
         larray(i) = 1000
 30   continue
      count(1) = lats
      call ncvpt (ncid, lid, start, count, larray, iret)
c
c     float
c
      i = 0
      do 40 itime = 1,times
         do 40 ilon = 1, lons
            do 40 ilat = 1, lats
               do 40 ilev = 1, levels
                  i = i + 1
                  farray(ilev, ilat, ilon, itime) = real(i)
 40   continue
      count(1) = levels
      call ncvpt (ncid, fid, start, count, farray, iret)
c
c     double
c
      i = 0
      do 50 itime = 1, times
         do 50 ilon = 1, lons
            do 50 ilat = 1, lats
               do 50 ilev = 1, levels
                  i = i + 1
                  darray(ilev, ilat, ilon, itime) = real(i)
 50   continue
      count(1) = levels
      call ncvpt (ncid, did, start, count, darray, iret)
c
c     char
c
      start(1) = 1
      start(2) = 1
      count(1) = 4
      count(2) = 4
      lenstr = 31	
      string = 'testthiskind of '
      call ncvptc (ncid, chid,start, count, string, lenstr, iret)
      call ncclos(ncid, iret)
      return
      end

      
      subroutine tncvpt1(cdfname)
      include 'netcdf.inc'
      character*31 cdfname


      integer iret, ncid
      integer  latdim, londim, leveldim, timedim, lendim
      integer dimsiz(MAXNCDIM)
      common /dims/timedim, latdim, londim, leveldim, lendim, 
     + dimsiz

      integer bindx, sindx, lindx, findx(4), dindx(4), cindx

      integer lvalue
      integer*2 svalue
      integer*1 bvalue
      double precision onethird
      integer bid, sid, lid, fid, did, cid, chid
      common /vars/bid, sid, lid, fid, did, cid, chid
      data lindx/1/, bindx/1/, sindx/1/, findx/1,1,1,1/
     +dindx/1,1,1,1/, cindx/1/
      data lvalue /1000/
      data svalue/10/
      data onethird/0.3333333333D0/

      bvalue = ichar('z')
      
      ncid = ncopn (cdfname, NCWRITE, iret)
c
c     test ncvpt1 for byte
c
      call ncvpt1 (ncid, bid, bindx, bvalue, iret)
c
c     test ncvpt1 for short
c
      call ncvpt1 (ncid, sid, sindx, svalue, iret)
c     
c     test ncvpt1 for long
c
      call ncvpt1 (ncid, lid, lindx, lvalue, iret)
c
c     test ncvpt1 for float
c
      call ncvpt1 (ncid, fid, findx, 3.14159, iret)
c
c     test ncvpt1 for double
c
      call ncvpt1 (ncid, did, dindx, onethird, iret)
c
c     test ncvp1c for char
c
      call ncvp1c (ncid, cid, cindx, 'a', iret)
      call ncclos (ncid, iret)
      return
      end
