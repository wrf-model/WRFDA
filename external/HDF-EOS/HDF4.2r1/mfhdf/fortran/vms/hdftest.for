C
C     Testing the Fortran interface for the multiple SD routines
C
      program hdftest

      implicit none

      integer fid1, fid2

      integer sds1, sds2, sds3, sds4

      integer dim1, dim2

      integer access, nt, rank, stat, i, err
      integer err_chunk, err_char_chunk, err_compress
      integer*4 ival, ivals(1000)
      integer dims(10), start(10), end(10), stride(10), count, nattr
      integer idims(10)
      integer num, ref, j
      real*4 fval

      real*8  cal, cale, ioff, ioffe
      real*8  eps
      character*50  name, l, u, f, c
      character cdata(6,4), icdata(6,4), cfill, icfill
      character catt(2), icatt(2)
      integer*4   i32(2), ii32(2), max32, min32
      integer*4 natt(2), inatt(2)

      integer sfstart,  sfcreate,  sfendacc, sfend,    sfsfill
      integer sfrdata,  sfwdata,   sfdimid,  sfsdmname
      integer sffinfo,  sfn2index, sfsdmstr, sfsdtstr, sfsdscale
      integer sfscal,   sfselect,  sfginfo,  sfgdinfo, sfgainfo
      integer sffattr,  sfsrange,  sfgrange, sfgfill,  sfsflmd
      integer sfgcal,   sfgdscale, sfgdtstr, sfgdmstr
      integer sfid2ref, sfref2index, sfsdmvc, sfisdmvc
      integer sfsextf,  hxsdir,    hxscdir
      integer sfwcdata, sfrcdata,  sfscfill, sfgcfill
      integer sfscatt,  sfrcatt,   sfsnatt,  sfrnatt
      integer SD_UNLIMITED, SD_DIMVAL_BW_INCOMP, DFNT_INT32
      integer DFNT_FLOAT32, DFNT_CHAR8
      integer SD_DIMVAL_BW_COMP, SD_FILL, SD_NOFILL
      parameter (SD_UNLIMITED = 0,
     +            SD_DIMVAL_BW_INCOMP = 0,
     +            SD_DIMVAL_BW_COMP = 1,
     +            DFNT_INT32 = 24,
     +            DFNT_FLOAT32 = 5,
     +            DFNT_CHAR8 = 4,
     +            SD_FILL = 0,
     +            SD_NOFILL = 256)
      DATA cfill/'@'/, icfill/' '/
      DATA catt/'U','S'/, icatt/' ',' '/
      DATA natt/10,20/, inatt/0,0/
      DATA i32/15,25/, ii32/0,0/

C     create a new file
      err = 0
      eps = 0.0001
      access = 4
      fid1 = sfstart('test1.hdf', access)
      if(fid1.eq.-1) then
         print *, 'Hopen returned bad ID'
         err = err + 1
      endif

      dims(1) = 4
      dims(2) = 9
      nt = DFNT_INT32
      rank = 2
      sds1 = sfcreate(fid1, 'Alpha', nt, rank, dims)
      if(sds1.eq.-1) then
         print *, 'SDcreate #1 returned bad ID', sds1
         err = err + 1
      endif

      dims(1) = 2
      dims(2) = 5
      dims(3) = 15
      nt = DFNT_FLOAT32
      rank = 3
      sds2 = sfcreate(fid1, 'Beta[float32]', nt, rank, dims)
      if(sds2.eq.-1) then
         print *, 'SDcreate #2 returned bad ID', sds2
         err = err + 1
      endif

      ival = 14
      stat = sfsfill(sds1, ival)
      if(stat.ne.0) then
         print *, 'Set fill returned', stat
         err = err + 1
      endif

      max32 = 10
      min32 = 1
      stat = sfsrange(sds1, max32, min32)
      if(stat.ne.0) then
         print *, 'Set range returned', stat
         err = err + 1
      endif
      max32 = 0
      min32 = 0

      do 100 i = 1, 10
         ivals(i) = i
 100  continue

      start(1)  = 0
      start(2)  = 0
      stride(1) = 1
      stride(2) = 1
      end(1)    = 3
      end(2)    = 3
      stat = sfwdata(sds1, start, stride, end, ivals)
      if(stat.ne.0) then
         print *, 'Write data returned', stat
         err = err + 1
      endif
      
      fval = -14.25
      stat = sfsfill(sds2, fval)
      if(stat.ne.0) then
         print *, 'Set fill returned', stat
         err = err + 1
      endif

      start(1)  = 1
      start(2)  = 1
      stride(1) = 1
      stride(2) = 1
      end(1)    = 3
      end(2)    = 3
      stat = sfrdata(sds1, start, stride, end, ivals)
      if(stat.ne.0) then
         print *, 'Read data returned', stat
         err = err + 1
      endif

      if (ivals(1).ne.5)  then
         err = err + 1
         print *, 'was expecting 5 got', ivals(1)
      endif
      
      if(err.ne.0) print *, 'Before ReadVerify err = ', err
      if (ivals(2).ne.6)  then
        err = err + 1
        print *, 'was expecting 6 got', ivals(2)
      endif
      if (ivals(3).ne.14) then
        err = err + 1
        print *, 'was expecting 14 got', ivals(3)
        endif
      if (ivals(4).ne.8)  then
        err = err + 1
        print *, 'was expecting 8 got', ivals(4)
        endif


      if (ivals(5).ne.9)  then
        err = err + 1
        print *, 'was expecting 9 got', ivals(5)
        endif

      if (ivals(6).ne.14) then
        err = err + 1
        print *, 'was expecting 14 got', ivals(6)
        endif

      if (ivals(7).ne.14) then
        err = err + 1
        print *, 'was expecting 14 got', ivals(7)
        endif

      if (ivals(8).ne.14) then
        err = err + 1
        print *, 'was expecting 14 got', ivals(8)
        endif

      if (ivals(9).ne.14) then
        err = err + 1
        print *, 'was expecting 14 got', ivals(9)
        endif

      if(err.ne.0) print *, 'After ReadVerify err = ', err

      nt = DFNT_INT32
      stat = sfsnatt(sds2, 'TestAttr', nt, 3, ivals)
      if(stat.ne.0) then
         print *, 'Set numeric attr returned', stat
         err = err + 1
      endif

      dim1 = sfdimid(sds2, 1)
      if(dim1.ne.327683) then
         print *, 'Dim id returned', dim1
         err = err + 1
      endif

      stat = sfsdmname(dim1, 'TestDim')
      if(stat.ne.0) then
         print *, 'Set dim name returned', stat
         err = err + 1
      endif

      stat = sfsdmstr(dim1, 'dA', 'dBB', 'dCCC')
      if(stat.ne.0) then
         print *, 'Set dim strs returned', stat
         err = err + 1
      endif

      do 110 i = 1, 10
         ivals(i) = 10 * i + i
 110  continue

      nt = DFNT_INT32
      count = 5
      stat = sfsdscale(dim1, count, nt, ivals)
      if(stat.ne.0) then
         print *, 'Set dims scales returned', stat
         err = err + 1
      endif

      stat = sfsdtstr(sds1, 'lxxx', 'uyyy', 'fzzz', 'caaa')
      if(stat.ne.0) then
         print *, 'Set data strings returned', stat
         err = err + 1
      endif

      stat = sffinfo(fid1, num, nattr)
      if(stat.ne.0.or.num.ne.3) then
         print *, 'File info returned wrong values', stat, num
         err = err + 1
      endif

      cal   = 10.1
      cale  = 20.1
      ioff  = 40.1
      ioffe = 50.1
C     why 16?  16 is not a legal HDF NType value.
      nt    = 16
      stat = sfscal(sds2, cal, cale, ioff, ioffe, nt)
      if(stat.ne.0) then
         print *, 'Set calibration returned', stat
         err = err + 1
      endif

      stat = sfn2index(fid1, 'Alpha')
      if(stat.ne.0) then
         print *, 'Index of Alpha data set is wrong', stat
         err = err + 1
      endif

      ref = sfid2ref(sds1)
      if(ref.eq.0) then
         print *, 'sfidtoref failed'
         err = err + 1
      endif

      stat = sfref2index(fid1, ref)
      if(stat.ne.0) then
         print *, 'mapping from ref to index failed', stat
         err = err + 1
      endif

      stat = sfn2index(fid1, 'Bogus')
      if(stat.ne.(-1)) then
         print *, 'Found a bogus data set with index', stat
         err = err + 1
      endif

      nt = DFNT_CHAR8
      stat = sfscatt(fid1, 'Globulator', nt, 12, 'Howdy Sailor')
      if(stat.ne.0) then
         print *, 'Set attr returned', stat
         err = err + 1
      endif
      nt = DFNT_INT32
      stat = sfsnatt(fid1, 'Numeric', nt, 2, i32)
      if(stat.ne.0) then
         print *, 'Set attr returned', stat
         err = err + 1
      endif

      stat = sfendacc(sds1)
      if(stat.ne.0) then
         print *, 'SDendaccess returned', stat
         err = err + 1
      endif
      stat = sfend(fid1)
      if(stat.ne.0) then
         print *, 'SDend returned', stat
         err = err + 1
      endif

C
C     OK, let's open it back up and take a look at what we've done
C
      fid2 = sfstart('test1.hdf', 3)
      if(fid2.ne.393216) then
          print *, 'Reopen returned', fid2
          err = err + 1
      endif
 
      sds3 = sfselect(fid2, 0)
      if(sds3.ne.262144) then
         print *, 'Select returned', sds3
         err = err + 1
      endif

      stat = sfginfo(sds3, name, rank, idims, nt, nattr)
      if(stat.ne.0) then
         print *, 'Get info returned ', stat
         err = err + 1
      endif

      if(nt.ne.DFNT_INT32) then
         print *, 'Incorrect number type ', nt
         err = err + 1
      endif

      if(rank.ne.2) then
         print *, 'Incorrect rank ', rank
         err = err + 1
      endif

      if(idims(1).ne.4) then
         print *, 'Incorrect Dim(1) = ', idims(1)
         err = err + 1
      endif

      if(idims(2).ne.9) then
         print *, 'Incorrect Dim(2) = ', idims(2)
         err = err + 1
      endif

      if(nattr.ne.6) then
         print *, 'Wrong number of attributes returned', nattr
         err = err + 1
      endif

      print *, 'name = ',   name

      stat = sfgrange(sds3, max32, min32)
      if(stat.ne.0) then
         print *, 'Get range returned', stat
         err = err + 1
      endif

      if(max32.ne.10) then
         print *, 'Max from GetRange ', max32
         err = err + 1
      endif

      if(min32.ne.1) then
         print *, 'Min from GetRange ', min32
         err = err + 1
      endif

      if(err.ne.0) print *, 'Current error count ', err

      stat = sfgfill(sds3, max32)
      if(stat.ne.0) then
         print *, 'Get fillvalue returned', stat
         err = err + 1
      endif

      if(max32.ne.14) then
         print *, 'Incorrect FillValue ', max32
         err = err + 1
      endif

      sds4 = sfselect(fid2, 1)
      if(sds4.ne.262145) then
         print *, 'Select #4  returned', sds4
         err = err + 1
      endif

      dim2 = sfdimid(sds4, 1)
      if(dim2.ne.327683) then
         print *, 'Get dim id #2 returned', dim2
         err = err + 1
      endif

      stat = sfgdinfo(dim2, name, rank, nt, nattr)
      if(stat.ne.0) then
         print *, 'Get dim info returned', stat
         err = err + 1
      endif

      if(nt.ne.DFNT_INT32) err = err + 1
      if(rank.ne.5) err = err + 1
      print *, 'name = ',   name

      stat = sfgainfo(fid2, 0, name, nt, rank)
      if(stat.ne.0) then
         print *, 'Attr info returned', stat
         err = err + 1
      endif
      
      if(nt.ne.4) err = err + 1
      if(rank.ne.12) err = err + 1
      print *, 'name = ',   name

      cal   = 0
      cale  = 0
      ioff  = 0 
      ioffe = 0
      nt    = 0
      stat = sfgcal(sds4, cal, cale, ioff, ioffe, nt)
      if(stat.ne.0) then
         print *, 'Get cal returned', stat
         err = err + 1
      endif

      if(abs(cal - 10.1) .gt. eps) err = err + 1
      if(abs(cale - 20.1) .gt. eps) err = err + 1
      if(abs(ioff - 40.1) .gt. eps) err = err + 1
      if(abs(ioffe - 50.1) .gt. eps) err = err + 1
      if(nt.ne.16) err = err + 1


      do 120 i = 1, 10
         ivals(i) = 0
 120  continue

      stat = sfgdscale(dim2, ivals)
      if(stat.ne.0) then
         print *, 'Get scales returned', stat
         err = err + 1
      endif

      if (ivals(1).ne.11) err = err + 1
      if (ivals(2).ne.22) err = err + 1
      if (ivals(3).ne.33) err = err + 1
      if (ivals(4).ne.44) err = err + 1
      if (ivals(5).ne.55) err = err + 1

      stat = sfgdtstr(sds3, l, u, f, c, 50)
      if(stat.ne.0) then
         print *, 'Get data strs returned', stat
         err = err + 1
      endif
      
      print *, 'label    = ', l
      print *, 'unit     = ', u
      print *, 'format   = ', f
      print *, 'coordsys = ', c

      stat = sfgdmstr(dim2, l, u, f, 50)
      if(stat.ne.0) then
         print *, 'Get dim strs returned', stat
         err = err + 1
      endif
      
      print *, 'label    = ', l
      print *, 'unit     = ', u
      print *, 'format   = ', f

      stat = sfrcatt(fid2, 0, name)
      if(stat.ne.0) then
         print *, 'Attr read returned', stat
         err = err + 1
      endif
      print *, 'values = ', name
      stat = sfrnatt(fid2, 1, ii32)
      if(stat.ne.0) then
         print *, 'Attr read returned', stat
         err = err + 1
      endif
      if ((ii32(1) .ne. 15) .or. (ii32(2) .ne. 25)) then
         print *, 'Numeirc attr read erro: '
         print *, ' should be 15 25, get ',ii32(1), ii32(2)
         err = err + 1
      endif

C
C     Testing External Element functions: sfsextf, hxsdir, hxscdir.
C     First set the external create directory to "testdir".
C     Set dataset sds3 to store in external file.
C     Try read it back (should fail the first time).
C     Set locating directory to "nosuch:testdir".
C     Read again.  Should succeed this time.
C
      stat = hxscdir('testdir')
      if(stat.ne.0) then
	 print *, 'HX set create dir (hxscdir) returned', stat
	 err = err + 1
      endif

      stat = sfsextf(sds3, 'testext.hdf', 0)
      if(stat.ne.0) then
	 print *, 'set external file (sfsextf) returned', stat
	 err = err + 1
      endif

C
C     Close and reopen sds3 so that data is flushed to the ext. file
C
      stat = sfendacc(sds3)
      if(stat.ne.0) then
         print *, 'sfendacc returned', stat
         err = err + 1
      endif

      sds3 = sfselect(fid2, 0)
      if(sds3.eq.-1) then
         print *, 'Select returned', sds3
         err = err + 1
      endif

      start(1)  = 1
      start(2)  = 1
      stride(1) = 1
      stride(2) = 1
      end(1)    = 3
      end(2)    = 3
      stat = sfrdata(sds3, start, stride, end, ivals)
C
C     Should fail first time.
C
      if(stat.ne.-1) then
         print *, 'Read data (sfrdata) returned', stat
         err = err + 1
      endif

      stat = hxsdir('nosuch|testdir')
      if(stat.ne.0) then
	 print *, 'HX set dir (hxscdir) returned', stat
	 err = err + 1
      endif

      stat = sfrdata(sds3, start, stride, end, ivals)
C
C     Should succeed this time.
C
      if(stat.ne.0) then
         print *, 'Read data (sfrdata) returned', stat
         err = err + 1
      endif

      if (ivals(1).ne.5)  then
         err = err + 1
         print *, 'was expecting 5 got', ivals(1)
      endif

      stat = sfendacc(sds3)
      if(stat.ne.0) then
         print *, 'sfendacc returned', stat
         err = err + 1
      endif

      stat = sfendacc(sds4)
      if(stat.ne.0) then
         print *, 'sfendacc returned', stat
         err = err + 1
      endif

      stat = sfend(fid2)
      if(stat.ne.0) then
         print *, 'SDend returned', stat
         err = err + 1
      endif

C     test sfsdmvc and sfisdmvc -- dimval backward compatible 
      fid1 = sfstart('test2.hdf', 4)
      if(fid1 .lt. 1) then
         print *, 'sfstart returned', fid1
         err = err + 1
      endif

      dims(1) = 6
      dims(2) = 0
      nt = DFNT_INT32
      rank = 2
      sds1 = sfcreate(fid1, 'ncomp', nt, rank, dims)
      if (sds1 .eq. -1) then
         print *, 'sfcreate returned', sds1
         err = err + 1
      endif

      dim1 = sfdimid(sds1, 0)
      if (dim1 .eq. -1) then
         print *, 'sfdimid returned', dim1
         err = err + 1
      endif

      stat = sfsdmvc(dim1, 0)
      if(stat .ne. 0) then
         print *, 'sfsdmvc returned', stat
         err = err + 1
      endif
      dim2 = sfdimid(sds1, 1)
      stat = sfsdmvc(dim2, 0)
      if(stat .ne. 0) then
         print *, 'sfsdmvc returned', stat
         err = err + 1
      endif
      do 140 i=1, 6
         ivals(i) = i*5
140   continue
      stat = sfsdscale(dim1, 6, DFNT_INT32, ivals)
      if(stat .ne. 0) then
          print *, 'sfsdscale returned', stat
          err = err + 1
      endif
      start(1)=0
      start(2)=0
      stride(1) = 1
      stride(2) = 1
      end(1)=6
      end(2)=4
      do 160 i=1, 24
        ivals(i) = i
160   continue
      stat = sfwdata(sds1, start, stride, end, ivals)
      if (stat .ne. 0) then
          print *, 'sfwdata returned', stat
          err = err + 1
      endif
      stat = sfendacc(sds1)
      if(stat .ne. 0) then
           print *, 'sfendacc returned', stat
           err = err + 1
      endif

      stat = sfend(fid1)
      if(stat .ne. 0) then
         print *, 'SDend returned', stat
         err = err + 1
      endif

C     let's open it back up and take a look at what we've done
C

      fid2 = sfstart('test2.hdf', 3)
      if(fid2 .lt.  0) then
         print *, 'Reopen returned', fid2
         err = err + 1
      endif

      stat = sfn2index(fid2, 'ncomp')
      if (stat .lt. 0) then
         print *, 'sfn2index returned', stat
         err = err + 1
      endif

      sds3 = sfselect(fid2, stat)
      if (sds3 .eq. -1) then
         print *, 'sfselect returned', sds3
         err = err + 1
      endif
      stat = sfginfo(sds3, name, rank, idims, nt, nattr)
      if (stat .ne. 0) then
          print *, 'sfginfo returned', stat
          err = err + 1
      endif
      if ((rank .ne. 2) .or. (idims(1) .ne. 6) .or.
     +    (idims(2) .ne. 4) .or. (nt .ne. DFNT_INT32)) then
          print *, 'error in sfginfo'
          err = err + 1
      endif
      dim2=sfdimid(sds3,1)
      stat = sfgdinfo(dim2, name, dims(2), nt, nattr)
      if ((dims(2) .ne. SD_UNLIMITED) .or. (nt .ne.  0 ))  then
          print *, '1st sfgdinfo error', stat, dims(2), nt
          err = err + 1
      endif
      dim1=sfdimid(sds3,0)
      stat = sfgdinfo(dim1, name, dims(1), nt, nattr)
      if ((dims(1) .ne. 6) .or. (nt .ne. DFNT_INT32 ))  then
         print *, '2nd sfgdinfo error', stat, dims(1), nt
         err = err + 1
      endif
      stat = sfrdata(sds3, start, stride, end, ivals)
      if (stat .ne. 0) then
           print *, 'sfrdata returned', stat
           err = err + 1
      endif
      do 180 i=1, 24
          if (ivals(i) .ne. i)  then
              print *,  'wrong value: should be ',i,'  got ',ivals(i)
              err = err + 1
          endif
180    continue
      stat = sfisdmvc(dim1)
      if (stat .ne. 0)  then
          print *, 'sfisdmvc returned', stat
          err = err + 1
      endif
      stat = sfsdmvc(dim1, 1)
      stat = sfendacc(sds3)
      if (stat .ne. 0) then
          print *, 'sfendacc returned', stat
          err = err + 1
      endif
      stat = sfend(fid2)
      if (stat .ne. 0) then
           print *, 'sfend returned', stat
           err = err + 1
      endif

C     open one last time to check that NDG ref has been constant
C     check SDsetdimval_compat
      fid1 = sfstart('test2.hdf', 3)
      if (fid1 .eq. -1) then
           print *, 'sfstart returned', stat
           err = err + 1
      endif
C     read back dimval_non_compat
      stat = sfn2index(fid1, 'ncomp')
      if (stat .lt. 0) then
         print *, 'sfn2index returned', stat
         err = err + 1
      endif

      sds2 = sfselect(fid1, stat)
      if (sds2 .eq. -1) then
         print *, 'sfselect returned', sds2
         err = err + 1
      endif
      stat = sfginfo(sds2, name, rank, idims, nt, nattr)

      if (stat .ne. 0) then
          print *, 'sfginfo returned', stat
          err = err + 1
      endif
      if ((rank .ne. 2) .or. (idims(2) .ne. 4) .or.
     +    (idims(1) .ne. 6) .or. (nt .ne. DFNT_INT32)) then
          print *, 'error in sfginfo'
          err = err + 1
      endif
      dim1=sfdimid(sds2,0)
      stat = sfgdinfo(dim1, name, dims(1), nt, nattr)
      if ((dims(1) .ne. 6) .or. (nt .ne. DFNT_INT32 ))  then
         print *, '3rd sfgdinfo error', stat, dims(1), nt 
         err = err + 1
      endif
      stat = sfisdmvc(dim1)
      if (stat .ne. 1)  then
          print *, 'sfisdmvc returned', stat
          err = err + 1
      endif
      stat = sfendacc(sds2)
      if (stat .lt. 0) then
          print *, 'sfendacc returned', stat
          err = err + 1
      endif
      stat = sfend(fid1)
      if (stat .lt. 0) then
           print *, 'sfend returned', stat
           err = err + 1
      endif

C Test char attr, char fill value and char data routines
C sfscatt,sfrnatt,sfsnatt, sfrnatt,sfwcdata,sfrcdata
C sfscfill, sfgcfill
      fid1 = sfstart('test2.hdf', 4)
      if(fid1 .lt. 1) then
         print *, 'sfstart returned', fid1
         err = err + 1
      endif

      dims(1) = 6
      dims(2) = 0
      nt = DFNT_CHAR8
      rank = 2
      sds1 = sfcreate(fid1, 'char_type', nt, rank, dims)
      if (sds1 .eq. -1) then
         print *, 'sfcreate returned', sds1
         err = err + 1
      endif
C Set char fill value
      stat = sfscfill(sds1, cfill)
      if (stat .ne. 0) then
         print *, 'sfscfill returned', stat
         err = err + 1
      endif
      start(1) = 0
      start(2) = 1
      stride(1) = 1
      stride(2) = 1
      end(1) = 6
      end(2) = 2
C create the char data
      do 195 i=1,4
         do 190 j=1,6
             cdata(j,i) = 'C'
             icdata(j,i) = ' '
190      continue
195   continue
C Write a slab of char data
      stat = sfwcdata(sds1, start, stride, end, cdata)
      if (stat .ne. 0) then
          print *, 'sfwdata returned', stat
          err = err + 1
      endif
C Set char attr
      stat = sfscatt(sds1, 'CharAttr',nt, 2, catt)
      if(stat.ne.0) then
         print *, 'sfscatt returned', stat
         err = err + 1
      endif
C Set numeric attr
      nt = DFNT_INT32
      stat = sfsnatt(sds1, 'NumericAttr',nt, 2, natt)
      if(stat.ne.0) then
         print *, 'sfsnatt returned', stat
         err = err + 1
      endif
      stat = sfendacc(sds1)
      if(stat .ne. 0) then
           print *, 'sfendacc returned', stat
           err = err + 1
      endif
C Close file
      stat = sfend(fid1)
      if(stat .ne. 0) then
         print *, 'SDend returned', stat
         err = err + 1
      endif

C read back
      fid1 = sfstart('test2.hdf', 3)
      if(fid1 .lt. 1) then
         print *, 'sfstart returned', fid1
         err = err + 1
      endif
      stat = sfn2index(fid1, 'char_type')
      if (stat .lt. 0) then
         print *, 'sfn2index returned', stat
         err = err + 1
      endif
      sds2 = sfselect(fid1, stat)
      if (sds2 .eq. -1) then
         print *, 'sfselect returned', sds2
         err = err + 1
      endif
      stat = sfginfo(sds2, name, rank, idims, nt, nattr)
      if (stat .ne. 0) then
          print *, 'sfginfo returned', stat
          err = err + 1
      endif
      start(1) = 0
      start(2) = 0
      stride(1) = 1
      stride(2) = 1
      end(1) = 6
      end(2) = 3
C read char data and char fill
      stat = sfrcdata(sds2, start, stride, end, icdata)
      do 200 i=1,6
         if (icdata(i,1) .ne. cfill) then 
          print *, 'error in read c_fill'
          err = err + 1
         endif
200   continue
      do 250 i=2,3
          do 230 j=1,6
             if (icdata(j,i) .ne. 'C') then
                 print *, 'error in sfrcdata'
                 err = err + 1
             endif
230       continue
250   continue
C read char fillvalue
      stat = sfgcfill(sds2, icfill)
      if ((stat .eq. -1) .or. (icfill .ne. cfill)) then
         print *, 'sfgcfill returned', sds2
         err = err + 1
      endif
 
C read char attr
      stat = sffattr(sds2, 'CharAttr')
      if (stat .eq. -1) then
         print *, 'sffattr returned', sds2
         err = err + 1
      endif
      stat = sfrcatt(sds2, stat, icatt)
      if ((icatt(1) .ne. catt(1)) .or. (icatt(2) .ne. catt(2))) then
         print *, 'sfrcatt returned', sds2
         err = err + 1
      endif
C read numeric attr
      stat = sffattr(sds2, 'NumericAttr')
      if (stat .eq. -1) then
         print *, 'sffattr returned', sds2
         err = err + 1
      endif
      stat = sfrnatt(sds2, stat, inatt)
      if ((inatt(1) .ne. natt(1)) .or. (inatt(2) .ne. natt(2))) then
         print *, 'sfrnatt returned', inatt(1), inatt(2)
         err = err + 1
      endif

      stat = sfendacc(sds2)
      if(stat .ne. 0) then
           print *, 'sfendacc returned', stat
           err = err + 1
      endif
C Close file
      stat = sfend(fid1)
      if(stat .ne. 0) then
         print *, 'SDend returned', stat
         err = err + 1
      endif
C Test set fill mode
      fid1 = sfstart('test1.hdf', 3)
      nt = DFNT_INT32
      rank = 2
      dims(1) = 6
      dims(2) = 5
      sds1 = sfcreate(fid1, 'FIXED1', nt,rank,dims)
      ival = -300
      do  400 i = 1, 30
          ivals(i) = i + 100
400   continue
      stat = sfsfill(sds1, ival)
      if(stat .ne. 0) then
           print *, 'sfsnatt returned', stat
           err = err + 1
      endif
      stat = sfsflmd(fid1, SD_NOFILL)
      if(stat .ne. SD_FILL) then
           print *, 'sfsflmd returned', stat
           err = err + 1
      endif
      stat = sfendacc(sds1)
      if(stat .ne. 0) then
           print *, 'sfendacc returned', stat
           err = err + 1
      endif
      i = sfn2index(fid1,'FIXED1')
      sds1 = sfselect(fid1, i)
      if(sds1 .lt. 0) then
           print *, 'sfselect returned', sds1
           err = err + 1
      endif
      start(1) = 0
      start(2) = 2
      stride(1) = 1
      stride(2) = 1
      end(1) = 6
      end(2) = 1
      stat = sfsflmd(fid1, SD_FILL)
      if (stat .ne. SD_NOFILL) then
           print *, 'sfsflmd returned', stat
           err = err + 1
      endif
      stat = sfwdata(sds1,start, stride, end, ivals)
      if (stat .eq. -1) then
           print *, 'sfwdata returned', stat
           err = err + 1
      endif
      stat = sfendacc(sds1)
C create a new fixed size SDS, srite the 3rd rec NOFILL.
C then set to SD_FILL and write the 5th rec.
      sds1 = sfcreate(fid1, 'FIXED_SDS', nt,rank,dims)
      stat = sfsfill(sds1, ival)
      stat = sfsflmd(fid1, SD_NOFILL)
      if(stat .ne. SD_FILL) then
           print *, 'sfsflmd returned', stat
           err = err + 1
      endif
      stat = sfwdata(sds1,start, stride, end, ivals)
      if (stat .eq. -1) then
           print *, 'sfwdata returned', stat
           err = err + 1
      endif
      stat = sfendacc(sds1)
      stat = sfend(fid1)
C open again, change fillmode and write the 5th rec 
      fid1 = sfstart('test1.hdf', 3)
      i = sfn2index(fid1, 'FIXED_SDS')
      sds1 = sfselect(fid1, i)
      stat = sfsflmd(fid1, SD_FILL)
      start(2) = 4
      stat = sfwdata(sds1,start,stride,end,ivals)
      stat = sfendacc(sds1)
      stat = sfend(fid1) 
C read back FIXED_SDS
      fid1 = sfstart('test1.hdf', 3)
      i = sfn2index(fid1, 'FIXED_SDS')
      sds1 = sfselect(fid1, i)
      start(1) = 0
      start(2) = 0
      end(1) = 6
      end(2) = 5
      stat = sfrdata(sds1,start,stride,end,ivals)
      stat = sfendacc(sds1)
      do 450 i=13,18
         if (ivals(i) .ne. (100+(i-12))) then
             print *,'wrong value: should be ', 100+(i-12)
             print *,' get ', ivals(i)
             err = err+1
         endif
         if (ivals(i+12) .ne. (100+(i-12))) then
             print *,'wrong value: should be ', 100+(i-12)
             print *,' get ', ivals(i+12)
             err = err+1
         endif
450   continue
      do 500 i=19,24
         if (ivals(i) .eq. ival) then
             print *,'Should not be ',ival, ' got ', ivals(i)
             err = err+1
         endif
500   continue
C read FIXED1
      i = sfn2index(fid1, 'FIXED1')
      sds1 = sfselect(fid1, i)
      stat = sfrdata(sds1,start,stride,end,ivals)
      stat = sfendacc(sds1)
      do 510 i=13,18
         if (ivals(i) .ne. (100+(i-12))) then
             print *,'wrong value: should be ', 100+(i-12)
             print *,' get ', ivals(i)
             err = err+1
         endif
510   continue
      do 520 i=19,24
         if (ivals(i) .ne. ival) then
            print *,'Should be ',ival, ' got ', ivals(i)
             err = err+1
         endif
520   continue
      stat = sfend(fid1)
C test unlimited sds 
      fid1 = sfstart('test1.hdf', 3)
      if (fid1 .eq. -1) then
            print *,'Open test1.hdf failed.'
             err = err+1
      endif
      dims(1) = 6
      dims(2) = SD_UNLIMITED
      sds1=sfcreate(fid1,'UNLIMITED_SDS',DFNT_INT32,rank,dims)
      if (sds1 .eq. -1) then
          print *,'create UNLIMITED_SDS failed. '
          err = err+1
      endif
      ival = -300
      do 550 i=1,24
         ivals(i) = i
550   continue
      stat = sfsfill(sds1, ival)
      stat = sfsflmd(fid1, SD_NOFILL)
      if (stat .ne. SD_FILL) then
          print *,'Should be ',SD_FILL, ' got ',  stat
          err = err+1
      endif
      start(1) = 0
      start(2) = 2
      end(1) = 6
      end(2) = 1
      stat = sfwdata(sds1,start, stride, end, ivals)
      if (stat .eq. -1) then
          print *,'write UNLIMITED_SDS failed. '
          err = err+1
      endif
      stat = sfendacc(sds1)
      stat = sfend(fid1)
C open again, write the 5th rec
      fid1 = sfstart('test1.hdf', 3)
      i = sfn2index(fid1, 'UNLIMITED_SDS')
      sds1 = sfselect(fid1, i)
      stat = sfsflmd(fid1, SD_FILL)
      start(2) = 4
      stat = sfwdata(sds1,start,stride,end, ivals)
      stat = sfendacc(sds1)
      stat = sfend(fid1)
C read back 
      fid1 = sfstart('test1.hdf', 3)
      i = sfn2index(fid1, 'UNLIMITED_SDS')
      sds1 = sfselect(fid1, i)
      start(1) = 0
      start(2) = 0
      end(1) = 6
      end(2) = 5
      stat = sfrdata(sds1,start,stride,end,ivals)
      stat = sfendacc(sds1)
      do 600 i=13,18
         if (ivals(i) .ne. (i-12)) then
             print *,'wrong value: should be ', (i-12)
             print *,' get ', ivals(i)
             err = err+1
         endif
         if (ivals(i+12) .ne. (i-12)) then
             print *,'wrong value: should be ', (i-12)
             print *,' get ', ivals(i+12)
             err = err+1
         endif
600   continue
      do 650 i=19,24
         if (ivals(i) .ne. ival) then
             print *,'Should be ',ival, ' got ', ivals(i)
             err = err+1
         endif
650   continue
      stat = sfend(fid1) 
C
C
C     Call three subroutines:
C
C     test_chunk (err_chunk)   - tests fortran chunking functions
C     test_char_chunk (err_char_chunk) - tests fortran char chunking
C                                        functions
C     test_compress(err_compress) - tests compression function with
C                                   different compression types
C
C     EIP 1/6/98
C
C
      err_chunk = 0
      call test_chunk ( err_chunk )
      err = err + err_chunk
C
      err_char_chunk = 0
      call test_char_chunk( err_char_chunk )
      err = err + err_char_chunk
C
      err_compress = 0
      call test_compress( err_compress )
C
      err = err + err_compress
      print *, 'Total errors : ', err

      end


         subroutine test_chunk( err_chunk ) 
         implicit none
C
C------- Begin Variables declarations -----------------------------------
C
         integer   N_COMP_TYPES, N_COMP_ARG
         parameter (N_COMP_TYPES = 5, N_COMP_ARG = 4)
         integer   sd_id(N_COMP_TYPES),
     .             sds_id(N_COMP_TYPES),
     .             sds_index(N_COMP_TYPES)
         integer   RANK, comp_type, c_out
         integer   comp_arg(N_COMP_ARG)
         integer   comp_type_out(N_COMP_TYPES)
         integer   d_dims(2)
         integer   ch_dims(2),ch_dims_out(2), start_dims(2)
         integer   start(2), stride(2), edges(2)
         integer   status, fill_value
         integer   maxcache, flags
         integer   err_chunk
         integer   n,m, n_part, m_part
         integer   n_start, m_start, n_stride, m_stride
         integer   nc, mc, n_nc, n_mc
         integer   i, j, k, l, lb, kb, kl, kj
         integer   i_comp
         integer mod1, mod2

         character*12 file(N_COMP_TYPES)
         character*12 name(N_COMP_TYPES)
C
C  SDS functions declarations
C
         integer   sfstart, sfcreate, sfendacc, sfend,
     .             sfn2index, sfselect,
     .             sfsfill, sfschnk, sfscchnk, sfwchnk, 
     .             sfgichnk, sfrchnk, sfwdata, sfrdata
C
C  Initial data declarations( change if you which to test larger arrays )
C
C  Data array dimensions n and m and RANK
C
         parameter (n = 9, m = 4, RANK = 2)

C
C  Part data dimensions n_part, m_part
C
         parameter (n_part = 5, m_part = 2)
C
C  Start coordinates of part_data
C
         parameter (n_start = 2, m_start = 1)
C
C  Stride in each dimension
C
         parameter (n_stride = 1, m_stride = 1)
C
C  Chunk dimensions nc and mc
C
         parameter (nc = 3, mc = 2)
C
C  Dimensions of "chunk matrix" n_nc and n_mc.
C  Note if n is multiple of nc or m is multiple
C  of mc we need smaller dimensions ( by one)
         parameter (n_nc = n/nc + 1, n_mc = m/mc + 1)
C
C  Actual size of chunk matrix ( will be calculated latera )
C
         integer n_nc_a, n_mc_a
C
C  Data declaration
C
         integer   data(n,m),
     .             chunk(nc,mc),
     .             chunk_out(nc,mc),
     .             chunk_data(nc,mc,n_nc,n_mc)
C
C  Buffers to hold part of the data when we read data back
C
         integer    part_data(n_part,m_part)
C
C  HDF parameters initialization
C
C
C  Read/Write parameters
C
         integer   DFACC_CREATE,
     .             DFACC_WRITE,
     .             DFACC_READ
         parameter (DFACC_CREATE = 4,
     .             DFACC_READ   =  1,
     .             DFACC_WRITE   = 2)
C
C  Data type parameters
C
C         integer   DATA_TYPE
         integer   DFNT_CHAR,
     .             DFNT_INT16,
     .             DFNT_INT32,
     .             DFNT_FLOAT32,
     .             DFNT_FLOAT64
         parameter (DFNT_CHAR   = 4,
     .             DFNT_INT16   = 22,
     .             DFNT_INT32   = 24,
     .             DFNT_FLOAT32 = 5,
     .             DFNT_FLOAT64 = 6)
C
C  Compression parametes
C
         integer   COMP_CODE_NONE,
     .             COMP_CODE_RLE,
     .             COMP_CODE_NBIT,
     .             COMP_CODE_SKPHUFF,
     .             COMP_CODE_DEFLATE

         parameter (COMP_CODE_NONE   = 0,
     .             COMP_CODE_RLE     = 1,
     .             COMP_CODE_NBIT    = 2,
     .             COMP_CODE_SKPHUFF = 3,
     .             COMP_CODE_DEFLATE = 4)
C
C  Compression arguments ( defined for clarity, actual values
C  will be passed to SFSCHUNK function via comp_arg parameter)
C
         integer deflate_level,
     .           skphuff_skp_size,
     .           nbit_sign_ext,
     .           nbit_fill_one,
     .           nbit_start_bit,
     .           nbit_bit_len
          parameter ( deflate_level    = 1,
     .                skphuff_skp_size = 2,
     .                nbit_sign_ext    = 0,
     .                nbit_fill_one    = 0,
     .                nbit_start_bit   = 0,
     .                nbit_bit_len     = 31 )


C
C--------------------End of declarations------------------------------
C
C
C  We will write to five different files corresponding to the 
C  different compression types
C
C  NO compression
C 
         file(1) = 'chunk_no.hdf'
         name(1) = 'Nocomp_data'
         comp_type_out(1) = 0
C
C  RLE compression
C
         file(2) = 'chunk_rl.hdf'
         name(2) = 'Rlcomp_data'
         comp_type_out(2) = 1 
C
C  Nbit compression
C
         file(3) = 'chunk_nb.hdf'
         name(3) = 'Nbcomp_data'
         comp_type_out(3) = 2 
C
C  Addaptive Huffman compression
C
         file(4) = 'chunk_sk.hdf'
         name(4) = 'Hucomp_data'
         comp_type_out(4) = 1 
C
C  Gzip compression
C
         file(5) = 'chunk_gz.hdf'
         name(5) = 'Gzcomp_data'
         comp_type_out(5) = 1 
C
C   Dimension sizes array initialization
C
         d_dims(1) = n
         d_dims(2) = m
C
C   Chunk dimension sizes array initialization
C
         ch_dims(1) = nc
         ch_dims(2) = mc
C
C   Find actual size of chunk matrix
C
         mod1 = mod (n,nc)
         mod2 = mod (m,mc)
         if (mod1 .eq. 0) n_nc_a = n_nc - 1
         if (mod2 .eq. 0) n_mc_a = n_mc - 1
C        
C   Initilize original array
C
         
         do 20 j = 1, m 
            do 10 i = 1, n
               data(i,j) = 10*j + i
10          continue
20       continue
C
C        Initialize chunks
C
         lb = mc
         kb = nc
         do 60 j = 1, n_mc_a
            do 50 i = 1, n_nc_a
               do 40 l = 1, lb
                  do 30 k = 1, kb
                   chunk_data(k,l,i,j) = 0. 
30                continue 
40              continue
50            continue
60        continue          
C
C  Assign actual data to the chunks
C
         do 100 j = 1, n_mc_a
            do 90 i = 1, n_nc_a
               if (j .eq. n_mc_a .and. mod2 .ne. 0 ) lb = mod(m, mc)  
               if (i .eq. n_nc_a .and. mod1 .ne. 0 ) kb = mod(n, nc)
               do 80 l = 1, lb
                  do 70 k = 1, kb
                   chunk_data(k,l,i,j) = data ((i-1)*nc +k, (j-1)*mc +l)
70                continue 
80              continue
90            continue
100        continue 
C
C  Initialize SD interfaces
C
       do 101 i = 1, N_COMP_TYPES
          sd_id(i) = sfstart (file(i), DFACC_CREATE)
101    continue          
C
C  Main loop through different compression types
C
         do 1000 i_comp = 1, N_COMP_TYPES

C
C        Create the data set
C
         sds_id(i_comp) = sfcreate(sd_id(i_comp), name(i_comp), 
     .                     DFNT_INT32, RANK, d_dims)
            if( sds_id(i_comp) .eq. -1 ) then
                print *, 'sfcreate failed for', i_comp, ' -th dataset'
                err_chunk = err_chunk + 1
            endif
C
C        Fill the data set with fill_value
C
         fill_value = 0
         status = sfsfill (sds_id(i_comp), fill_value)
            if( status .ne. 0 ) then
                print *, 'sfsfill failed for', i_comp, ' -th dataset'
                err_chunk = err_chunk + 1
            endif
C
C  Set compression type and compression arguments
C
         comp_type  = i_comp - 1
C
C   Initialize compression arguments array
C
         do 1 i = 1, n_comp_arg
            comp_arg(i) = 0
1        continue

         if( comp_type .eq. COMP_CODE_NBIT) then
             comp_arg(1) = nbit_sign_ext
             comp_arg(2) = nbit_fill_one
             comp_arg(3) = nbit_start_bit
             comp_arg(4) = nbit_bit_len
         endif

         if( comp_type .eq. COMP_CODE_SKPHUFF ) then
             comp_arg(1) = skphuff_skp_size
         endif

         if (comp_type .eq. COMP_CODE_DEFLATE ) then
             comp_arg(1) = deflate_level
         endif
C
C        Create chunked SDS 
C
         status = sfschnk (sds_id(i_comp), ch_dims, comp_type,
     .                     comp_arg)
            if( status .ne. 0 ) then
                print *, 'sfschnk failed for', i_comp, ' -th dataset'
                err_chunk = err_chunk + 1
            endif
C
C        Set chunk cache to hold maximum of nc chunks
C
         maxcache =  n_nc_a 
         flags = 0 
         status = sfscchnk (sds_id(i_comp), maxcache, flags)
            if( status .ne. maxcache ) then
                print *, 'sfscchnk failed for', i_comp, ' -th dataset'
                err_chunk = err_chunk + 1
            endif
C
C        Write the data chunks. First chunk is written by sfwdata function
C
         do 150 j = 1, n_mc_a
            do 140 i = 1, n_nc_a

               start_dims(1) = i 
               start_dims(2) = j 
            
               do 130 l = 1, mc
                  do 120 k = 1, nc
                     chunk(k,l) = chunk_data(k,l,i,j)
120               continue
130            continue
            if (i .eq. 1 .and. j .eq. 1) then
                start(1) = 0
                start(2) = 0
                stride(1) = 1
                stride(2) = 1
                edges(1)  = nc
                edges(2)  = mc
                status = sfwdata(sds_id(i_comp), start, stride,
     .                           edges, chunk)
            if( status .ne. 0 ) then
                print *, 'sfwdata failed for', i_comp, ' -th dataset'
                print *, ' first chunk'
                err_chunk = err_chunk + 1
            endif
            else 
            status = sfwchnk(sds_id(i_comp), start_dims, chunk)
            if( status .ne. 0 ) then
                print *, 'sfwchnk failed for', i_comp, ' -th dataset'
                print *, i,'-th',j,'-th', 'chunk'
                err_chunk = err_chunk + 1
            endif
            endif
140         continue
150       continue
         
         status = sfendacc(sds_id(i_comp))
            if( status .ne. 0 ) then
                print *, 'sfendacc failed for', i_comp, ' -th dataset'
                err_chunk = err_chunk + 1
            endif
         status = sfend (sd_id(i_comp))
            if( status .ne. 0 ) then
                print *, 'sfend failed for', i_comp, ' -th dataset'
                err_chunk = err_chunk + 1
            endif

1000      continue 

C
C   Let's check what we have written 
C   We will skip NBIT until things are clarified with QAK.
          
C
C   Open files and restart SD interfaces
C
         do 200 i = 1, N_COMP_TYPES
C
            sd_id(i) = sfstart (file(i), DFACC_READ)
            if( sd_id(i) .eq. -1 ) then
                print *, 'sfstart failed for', i, ' -th dataset'
                err_chunk = err_chunk + 1
            endif
200      continue 

C
C  Find written dataset in each file using its name and index
C

         do 201 i = 1, N_COMP_TYPES
C
            sds_index(i) = sfn2index (sd_id(i), name(i))
            if( sds_index(i) .eq. -1 ) then
                print *, 'sfn2index failed for', i, ' -th dataset'
                err_chunk = err_chunk + 1
            endif
            sds_id(i)    = sfselect (sd_id(i), sds_index(i))
            if( sds_id(i) .eq. -1 ) then
                print *, 'sfselect failed for', i, ' -th dataset'
                err_chunk = err_chunk + 1
            endif
201      continue

C
C  Get and check chunking and compression information about each dataset
C 
         do 202 i = 1, N_COMP_TYPES
C
            status = sfgichnk(sds_id(i),ch_dims_out,c_out)
            if( status .ne. 0 ) then
                print *, 'sfgichnk failed for', i, ' -th dataset'
                err_chunk = err_chunk + 1
            endif
            if(  c_out .ne. comp_type_out(i)) then
                print *, 'sfgichnk returned incorrect comptype info'
                err_chunk = err_chunk + 1
            endif
            if ( (ch_dims(1) .ne. ch_dims_out(1)) .or.
     .            (ch_dims(2) .ne. ch_dims_out(2)) ) then
                print *, 'sfgichnk returned incorrect chunk dimensions'
                err_chunk = err_chunk + 1
            endif
202      continue  

C
C   Read part of the data back using sfrdata function
C
         start(1) = n_start
         start(2) = m_start
         edges(1) = n_part
         edges(2) = m_part
         stride(1) = n_stride 
         stride(2) = m_stride 
         do  205 i = 1, N_COMP_TYPES
C
C   Skip NBIT until we know how to read nbit data back
C
         if (i .eq. 3) goto 205
             status = sfrdata (sds_id(i), start, stride, edges,
     .                         part_data)
             if (status .ne. 0) then
                 print *, 'sfrdata failed for reading part data for ',
     .           i, '-th dataset'
             err_chunk = err_chunk + 1
             endif
C
C   Compare output with aqtual data
C
         do 204 j = 1, m_part
            do 203 l = 1, n_part
               kl = n_start + 1 + (l-1)*n_stride
               kj = m_start + 1 + (j-1)*m_stride
               if (data(kl, kj) .ne. part_data(l,j)) then
                  print *, 'sfrdata read wrong data for ', 
     .            i ,'-th dataset'
               err_chunk = err_chunk +1
               endif 
203         continue
204      continue
 
205      continue


C
C    Read chunks back and compare with the actual data for each compression
C    type
C

      do 2000 i_comp = 1, N_COMP_TYPES
C
C    Skip NBIT
C
         if(i_comp. eq. 3) goto 2000
         comp_type = i_comp - 1 
         do 250 j = 1, n_mc_a
            do 240 i = 1, n_nc_a

               start_dims(1) = i 
               start_dims(2) = j 
            
            status = sfrchnk(sds_id(i_comp), start_dims, chunk_out)
            if (status .ne. 0) then
                print *, 'sfrchnk failed for ', i, ',', j,
     .         '-th chunk, compression type is ', comp_type  
                err_chunk = err_chunk + 1
            endif
C
C  Compare with actual data
C
               lb = mc
               kb = nc 
               if (j .eq. n_mc_a .and. mod2 .ne. 0 ) lb = mod(m, mc)  
               if (i .eq. n_nc_a .and. mod1 .ne. 0 ) kb = mod(n, nc)
               do 280 l = 1, lb
                  do 270 k = 1, kb
               if(chunk_out(k,l) .ne. data ((i-1)*nc +k, (j-1)*mc +l))
     . then
               print *, 'Data is incorrest'
                  err_chunk = err_chunk + 1
               endif
270                continue 
280              continue
240            continue

250       continue
         
C 
C  Terminate access to SDS, shutdown interfaces and close the files
C
           status = sfendacc(sds_id(i_comp))
            if( status .ne. 0 ) then
                print *, 'sfendacc failed for', i_comp, ' -th dataset'
                err_chunk = err_chunk + 1
            endif
           status = sfend(sd_id(i_comp))
            if( status .ne. 0 ) then
                print *, 'sfend failed for', i_comp, ' -th dataset'
                err_chunk = err_chunk + 1
            endif

2000   continue
         return
         end

         subroutine test_char_chunk( err_char_chunk ) 
         implicit none
C
C------- Begin Variables declarations -----------------------------------
C
         integer   N_COMP_TYPES, N_COMP_ARG
         parameter (N_COMP_TYPES = 5, N_COMP_ARG = 4)
         integer   sd_id(N_COMP_TYPES),
     .             sds_id(N_COMP_TYPES),
     .             sds_index(N_COMP_TYPES)
         integer   RANK, comp_type, c_out
         integer   comp_arg(N_COMP_ARG)
         integer   comp_type_out(N_COMP_TYPES)
         integer   d_dims(2)
         integer   ch_dims(2),ch_dims_out(2), start_dims(2)
         integer   start(2), stride(2), edges(2)
         integer   status, fill_value
         integer   maxcache, flags
         integer   err_char_chunk
         integer   n,m, n_part, m_part
         integer   n_start, m_start, n_stride, m_stride
         integer   nc, mc, n_nc, n_mc
         integer   i, j, k, l, lb, kb, kl, kj
         integer   i_comp
         integer mod1, mod2

         character*13 file(N_COMP_TYPES)
         character*12 name(N_COMP_TYPES)
C
C  SDS functions declarations
C
         integer   sfstart, sfcreate, sfendacc, sfend,
     .             sfn2index, sfselect,
     .             sfsfill, sfschnk, sfscchnk, sfwcchnk, 
     .             sfgichnk, sfrcchnk, sfwcdata, sfrcdata
C
C  Initial data declarations( change if you which to test larger arrays )
C
C  Data array dimensions n and m and RANK
C
         parameter (n = 9, m = 4, RANK = 2)

C
C  Part data dimensions n_part, m_part
C
         parameter (n_part = 5, m_part = 2)
C
C  Start coordinates of part_data
C
         parameter (n_start = 2, m_start = 1)
C
C  Stride in each dimension
C
         parameter (n_stride = 1, m_stride = 1)
C
C  Chunk dimensions nc and mc
C
         parameter (nc = 3, mc = 2)
C
C  Dimensions of "chunk matrix" n_nc and n_mc.
C  Note if n is multiple of nc or m is multiple
C  of mc we need smaller dimensions ( by one)
         parameter (n_nc = n/nc + 1, n_mc = m/mc + 1)
C
C  Actual size of chunk matrix ( will be calculated latera )
C
         integer n_nc_a, n_mc_a
C
C  Data declaration
C
         character data(n,m),
     .             chunk(nc,mc),
     .             chunk_out(nc,mc),
     .             chunk_data(nc,mc,n_nc,n_mc)
C
C  Buffers to hold part of the data when we read data back
C
         character part_data(n_part,m_part)
C
C  HDF parameters initialization
C
C
C  Read/Write parameters
C
         integer   DFACC_CREATE,
     .             DFACC_WRITE,
     .             DFACC_READ
         parameter (DFACC_CREATE = 4,
     .             DFACC_READ   =  1,
     .             DFACC_WRITE   = 2)
C
C  Data type parameters
C
C         integer   DATA_TYPE
         integer   DFNT_CHAR,
     .             DFNT_INT16,
     .             DFNT_INT32,
     .             DFNT_FLOAT32,
     .             DFNT_FLOAT64
         parameter (DFNT_CHAR   = 4,
     .             DFNT_INT16   = 22,
     .             DFNT_INT32   = 24,
     .             DFNT_FLOAT32 = 5,
     .             DFNT_FLOAT64 = 6)
C
C  Compression parametes
C
         integer   COMP_CODE_NONE,
     .             COMP_CODE_RLE,
     .             COMP_CODE_NBIT,
     .             COMP_CODE_SKPHUFF,
     .             COMP_CODE_DEFLATE

         parameter (COMP_CODE_NONE   = 0,
     .             COMP_CODE_RLE     = 1,
     .             COMP_CODE_NBIT    = 2,
     .             COMP_CODE_SKPHUFF = 3,
     .             COMP_CODE_DEFLATE = 4)
C
C  Compression arguments ( defined for clarity, actual values
C  will be passed to SFSCHUNK function via comp_arg parameter)
C
         integer deflate_level,
     .           skphuff_skp_size,
     .           nbit_sign_ext,
     .           nbit_fill_one,
     .           nbit_start_bit,
     .           nbit_bit_len
          parameter ( deflate_level    = 1,
     .                skphuff_skp_size = 2,
     .                nbit_sign_ext    = 0,
     .                nbit_fill_one    = 0,
     .                nbit_start_bit   = 0,
     .                nbit_bit_len     = 31 )


C
C--------------------End of declarations------------------------------
C
C
C  We will write to five different files corresponding to the 
C  different compression types
C
C  NO compression
C 
         file(1) = 'cchunk_no.hdf'
         name(1) = 'Nocomp_data'
         comp_type_out(1) = 0
C
C  RLE compression
C
         file(2) = 'cchunk_rl.hdf'
         name(2) = 'Rlcomp_data'
         comp_type_out(2) = 1 
C
C  Nbit compression
C
         file(3) = 'cchunk_nb.hdf'
         name(3) = 'Nbcomp_data'
         comp_type_out(3) = 2 
C
C  Addaptive Huffman compression
C
         file(4) = 'cchunk_sk.hdf'
         name(4) = 'Hucomp_data'
         comp_type_out(4) = 1 
C
C  Gzip compression
C
         file(5) = 'cchunk_gz.hdf'
         name(5) = 'Gzcomp_data'
         comp_type_out(5) = 1 
C
C   Dimension sizes array initialization
C
         d_dims(1) = n
         d_dims(2) = m
C
C   Chunk dimension sizes array initialization
C
         ch_dims(1) = nc
         ch_dims(2) = mc
C
C   Find actual size of chunk matrix
C
         mod1 = mod (n,nc)
         mod2 = mod (m,mc)
         if (mod1 .eq. 0) n_nc_a = n_nc - 1
         if (mod2 .eq. 0) n_mc_a = n_mc - 1
C        
C   Initilize original array
C
         
         do 20 j = 1, m 
            do 10 i = 1, n
               data(i,j) = char(10*j + i)
10          continue
20       continue
C
C        Initialize chunks
C
         lb = mc
         kb = nc
         do 60 j = 1, n_mc_a
            do 50 i = 1, n_nc_a
               do 40 l = 1, lb
                  do 30 k = 1, kb
                   chunk_data(k,l,i,j) = char(32) 
30                continue 
40              continue
50            continue
60        continue          
C
C  Assign actual data to the chunks
C
         do 100 j = 1, n_mc_a
            do 90 i = 1, n_nc_a
               if (j .eq. n_mc_a .and. mod2 .ne. 0 ) lb = mod(m, mc)  
               if (i .eq. n_nc_a .and. mod1 .ne. 0 ) kb = mod(n, nc)
               do 80 l = 1, lb
                  do 70 k = 1, kb
                   chunk_data(k,l,i,j) = data ((i-1)*nc +k, (j-1)*mc +l)
70                continue 
80              continue
90            continue
100        continue 
C
C  Initialize SD interfaces
C
       do 101 i = 1, N_COMP_TYPES
          sd_id(i) = sfstart (file(i), DFACC_CREATE)
101    continue          
C
C  Main loop through different compression types
C
         do 1000 i_comp = 1, N_COMP_TYPES

C
C        Create the data set
C
         sds_id(i_comp) = sfcreate(sd_id(i_comp), name(i_comp), 
     .                     DFNT_CHAR, RANK, d_dims)
            if( sds_id(i_comp) .eq. -1 ) then
                print *, 'sfcreate failed for', i_comp, ' -th dataset'
                err_char_chunk = err_char_chunk + 1
            endif
C
C        Fill the data set with fill_value
C
         fill_value = 0
         status = sfsfill (sds_id(i_comp), fill_value)
            if( status .ne. 0 ) then
                print *, 'sfsfill failed for', i_comp, ' -th dataset'
                err_char_chunk = err_char_chunk + 1
            endif
C
C  Set compression type and compression arguments
C
         comp_type  = i_comp - 1
C
C   Initialize compression arguments array
C
         do 1 i = 1, n_comp_arg
            comp_arg(i) = 0
1        continue

         if( comp_type .eq. COMP_CODE_NBIT) then
             comp_arg(1) = nbit_sign_ext
             comp_arg(2) = nbit_fill_one
             comp_arg(3) = nbit_start_bit
             comp_arg(4) = nbit_bit_len
         endif

         if( comp_type .eq. COMP_CODE_SKPHUFF ) then
             comp_arg(1) = skphuff_skp_size
         endif

         if (comp_type .eq. COMP_CODE_DEFLATE ) then
             comp_arg(1) = deflate_level
         endif
C
C        Create chunked SDS 
C
         status = sfschnk (sds_id(i_comp), ch_dims, comp_type,
     .                     comp_arg)
            if( status .ne. 0 ) then
                print *, 'sfschnk failed for', i_comp, ' -th dataset'
                err_char_chunk = err_char_chunk + 1
            endif
C
C        Set chunk cache to hold maximum of nc chunks
C
         maxcache =  n_nc_a 
         flags = 0 
         status = sfscchnk (sds_id(i_comp), maxcache, flags)
            if( status .ne. maxcache ) then
                print *, 'sfscchnk failed for', i_comp, ' -th dataset'
                err_char_chunk = err_char_chunk + 1
            endif
C
C        Write the data chunks. First chunk is written by sfwdata function
C
         do 150 j = 1, n_mc_a
            do 140 i = 1, n_nc_a

               start_dims(1) = i 
               start_dims(2) = j 
            
               do 130 l = 1, mc
                  do 120 k = 1, nc
                     chunk(k,l) = chunk_data(k,l,i,j)
120               continue
130            continue
            if (i .eq. 1 .and. j .eq. 1) then
                start(1) = 0
                start(2) = 0
                stride(1) = 1
                stride(2) = 1
                edges(1)  = nc
                edges(2)  = mc
                status = sfwcdata(sds_id(i_comp), start, stride,
     .                           edges, chunk)
            if( status .ne. 0 ) then
                print *, 'sfwdata failed for', i_comp, ' -th dataset'
                print *, ' first chunk'
                err_char_chunk = err_char_chunk + 1
            endif
            else 
            status = sfwcchnk(sds_id(i_comp), start_dims, chunk)
            if( status .ne. 0 ) then
                print *, 'sfwcchnk failed for', i_comp, ' -th dataset'
                print *, i,'-th',j,'-th', 'chunk'
                err_char_chunk = err_char_chunk + 1
            endif
            endif
140         continue
150       continue
         
         status = sfendacc(sds_id(i_comp))
            if( status .ne. 0 ) then
                print *, 'sfendacc failed for', i_comp, ' -th dataset'
                err_char_chunk = err_char_chunk + 1
            endif
         status = sfend (sd_id(i_comp))
            if( status .ne. 0 ) then
                print *, 'sfend failed for', i_comp, ' -th dataset'
                err_char_chunk = err_char_chunk + 1
            endif

1000      continue 

C
C   Let's check what we have written 
C   We will skip NBIT until things are clarified with QAK.
          
C
C   Open files and restart SD interfaces
C
         do 200 i = 1, N_COMP_TYPES
C
            sd_id(i) = sfstart (file(i), DFACC_READ)
            if( sd_id(i) .eq. -1 ) then
                print *, 'sfstart failed for', i, ' -th dataset'
                err_char_chunk = err_char_chunk + 1
            endif
200      continue 

C
C  Find written dataset in each file using its name and index
C

         do 201 i = 1, N_COMP_TYPES
C
            sds_index(i) = sfn2index (sd_id(i), name(i))
            if( sds_index(i) .eq. -1 ) then
                print *, 'sfn2index failed for', i, ' -th dataset'
                err_char_chunk = err_char_chunk + 1
            endif
            sds_id(i)    = sfselect (sd_id(i), sds_index(i))
            if( sds_id(i) .eq. -1 ) then
                print *, 'sfselect failed for', i, ' -th dataset'
                err_char_chunk = err_char_chunk + 1
            endif
201      continue

C
C  Get and check chunking and compression information about each dataset
C 
         do 202 i = 1, N_COMP_TYPES
C
            status = sfgichnk(sds_id(i),ch_dims_out,c_out)
            if( status .ne. 0 ) then
                print *, 'sfgichnk failed for', i, ' -th dataset'
                err_char_chunk = err_char_chunk + 1
            endif
            if(  c_out .ne. comp_type_out(i)) then
                print *, 'sfgichnk returned incorrect comptype info'
                err_char_chunk = err_char_chunk + 1
            endif
            if ( (ch_dims(1) .ne. ch_dims_out(1)) .or.
     .            (ch_dims(2) .ne. ch_dims_out(2)) ) then
                print *, 'sfgichnk returned incorrect chunk dimensions'
                err_char_chunk = err_char_chunk + 1
            endif
202      continue  

C
C   Read part of the data back using sfrdata function
C
         start(1) = n_start
         start(2) = m_start
         edges(1) = n_part
         edges(2) = m_part
         stride(1) = n_stride 
         stride(2) = m_stride 
         do  205 i = 1, N_COMP_TYPES
C
C   Skip NBIT
C
         if (i .eq. 3) goto 205
             status = sfrcdata (sds_id(i), start, stride, edges,
     .                         part_data)
             if (status .ne. 0) then
                 print *, 'sfrdata failed for reading part data for ',
     .           i, '-th dataset'
             err_char_chunk = err_char_chunk + 1
             endif
C
C   Compare output with aqtual data
C
         do 204 j = 1, m_part
            do 203 l = 1, n_part
               kl = n_start + 1 + (l-1)*n_stride
               kj = m_start + 1 + (j-1)*m_stride
               if (data(kl, kj) .ne. part_data(l,j)) then
                  print *, 'sfrdata read wrong data for ', 
     .            i ,'-th dataset'
               err_char_chunk = err_char_chunk +1
               endif 
203         continue
204      continue
 
205      continue


C
C    Read chunks back and compare with the actual data for each compression
C    type
C

      do 2000 i_comp = 1, N_COMP_TYPES
C
C   Skip NBIT
C 
         if(i_comp .eq. 3) goto 2000 

         comp_type = i_comp - 1 
         do 250 j = 1, n_mc_a
            do 240 i = 1, n_nc_a

               start_dims(1) = i 
               start_dims(2) = j 
            
            status = sfrcchnk(sds_id(i_comp), start_dims, chunk_out)
            if (status .ne. 0) then
                print *, 'sfrcchnk failed for ', i, ',', j,
     .         '-th chunk, compression type is ', comp_type  
                err_char_chunk = err_char_chunk + 1
            endif
C
C  Compare with actual data
C
               lb = mc
               kb = nc 
               if (j .eq. n_mc_a .and. mod2 .ne. 0 ) lb = mod(m, mc)  
               if (i .eq. n_nc_a .and. mod1 .ne. 0 ) kb = mod(n, nc)
               do 280 l = 1, lb
                  do 270 k = 1, kb
               if(chunk_out(k,l) .ne. data ((i-1)*nc +k, (j-1)*mc +l))
     . then
               print *, 'Data is incorrest'
                  err_char_chunk = err_char_chunk + 1
               endif
270                continue 
280              continue
240            continue

250       continue
         
C 
C  Terminate access to SDS, shutdown interfaces and close the files
C
           status = sfendacc(sds_id(i_comp))
            if( status .ne. 0 ) then
                print *, 'sfendacc failed for', i_comp, ' -th dataset'
                err_char_chunk = err_char_chunk + 1
            endif
           status = sfend(sd_id(i_comp))
            if( status .ne. 0 ) then
                print *, 'sfend failed for', i_comp, ' -th dataset'
                err_char_chunk = err_char_chunk + 1
            endif

2000   continue
         return
         end


         subroutine test_compress( err_compress ) 
         implicit none
C
C------- Begin Variables declarations -----------------------------------
C
         integer   N_COMP_TYPES, N_COMP_ARG
         parameter (N_COMP_TYPES = 4, N_COMP_ARG = 1)
         integer   sd_id(N_COMP_TYPES),
     .             sds_id(N_COMP_TYPES),
     .             sds_index(N_COMP_TYPES)
         integer   RANK, comp_type
         integer   comp_arg(N_COMP_ARG)
         integer   d_dims(2)
         integer   start(2), stride(2), edges(2)
         integer   status, fill_value
         integer   err_compress
         character*12 file(N_COMP_TYPES)
         character*12 name(N_COMP_TYPES)
         integer   n, m, n_part, m_part
         integer   n_part_stride, m_part_stride
         integer   n_part_start, m_part_start
         integer   n_stride, m_stride
         integer   n_start, m_start
         integer   n_edges, m_edges
         integer   i, j, l, kl, kj, i_comp
C
C  SDS functions declarations
C
         integer   sfstart, sfcreate, sfendacc, sfend,
     .             sfn2index, sfselect,
     .             sfsfill, sfrdata,
     .             sfwdata, sfscompress
C
C  Initial data declarations( change if you which to test larger arrays )
C
C  Data array dimensions n and m and RANK
C
         parameter (n = 9, m = 4, RANK = 2)

C
C  Part data dimensions n_part, m_part
C
         parameter (n_part = 5, m_part = 2)
C
C  Stride and start coordinates of part_data
C
         parameter (n_part_stride = 1, m_part_stride = 1)
         parameter (n_part_start = 2, m_part_start = 1)
C
C  Sart, stride  and edges parameters in each dimension
C
         parameter (n_start = 0, m_start = 0)
         parameter (n_stride = 1, m_stride = 1)
         parameter (n_edges = n, m_edges = m)
C
C  Data declaration
C
         integer   data(n,m)
C
C  Buffer to hold part of the data when we read data back
C
         integer    part_data(n_part,m_part)
C
C  HDF parameters initialization
C
C
C  Read/Write parameters
C
         integer   DFACC_CREATE,
     .             DFACC_WRITE,
     .             DFACC_READ
         parameter (DFACC_CREATE = 4,
     .             DFACC_READ   =  1,
     .             DFACC_WRITE   = 2)
C
C  Data type parameters
C
C         integer   DATA_TYPE
         integer   DFNT_CHAR,
     .             DFNT_INT16,
     .             DFNT_INT32,
     .             DFNT_FLOAT32,
     .             DFNT_FLOAT64
         parameter (DFNT_CHAR   = 4,
     .             DFNT_INT16   = 22,
     .             DFNT_INT32   = 24,
     .             DFNT_FLOAT32 = 5,
     .             DFNT_FLOAT64 = 6)
C
C  Compression parametes
C
         integer   COMP_CODE_NONE,
     .             COMP_CODE_RLE,
     .             COMP_CODE_SKPHUFF,
     .             COMP_CODE_DEFLATE

         parameter (COMP_CODE_NONE   = 0,
     .             COMP_CODE_RLE     = 1,
     .             COMP_CODE_SKPHUFF = 3,
     .             COMP_CODE_DEFLATE = 4)
C
C  Compression arguments ( defined for clarity, actual values
C  will be passed to SFSCHUNK function via comp_arg parameter)
C
         integer deflate_level,
     .           skphuff_skp_size
          parameter ( deflate_level    = 1,
     .                skphuff_skp_size = 2 )


C
C--------------------End of declarations------------------------------
C
C
C  We will write to five different files corresponding to the 
C  different compression types
C
C  NO compression
C 
         file(1) = 'comp_no.hdf'
         name(1) = 'Nocomp_data'
C
C  RLE compression
C
         file(2) = 'comp_rl.hdf'
         name(2) = 'Rlcomp_data'
C
C  Addaptive Huffman compression
C
         file(3) = 'comp_sk.hdf'
         name(3) = 'Hucomp_data'
C
C  Gzip compression
C
         file(4) = 'comp_gz.hdf'
         name(4) = 'Gzcomp_data'
C
C   Dimension sizes array initialization
C
         d_dims(1) = n
         d_dims(2) = m
C        
C   Initilize original array
C
         
         do 20 j = 1, m 
            do 10 i = 1, n
               data(i,j) = 10*j + i
10          continue
20       continue
C
C  Initialize SD interfaces
C
       do 101 i = 1, N_COMP_TYPES
          sd_id(i) = sfstart (file(i), DFACC_CREATE)
101    continue          
C
C  Main loop through different compression types
C
         do 1000 i_comp = 1, N_COMP_TYPES

C
C        Create the data set
C
         sds_id(i_comp) = sfcreate(sd_id(i_comp), name(i_comp), 
     .                     DFNT_INT32, RANK, d_dims)
            if( sds_id(i_comp) .eq. -1 ) then
                print *, 'sfcreate failed for', i_comp, ' -th dataset'
                err_compress = err_compress + 1
            endif
C
C        Fill the data set with fill_value
C
         fill_value = 0
         status = sfsfill (sds_id(i_comp), fill_value)
            if( status .ne. 0 ) then
                print *, 'sfsfill failed for', i_comp, ' -th dataset'
                err_compress = err_compress + 1
            endif
C
C  Set compression type and compression arguments
C
C
C   Initialize compression arguments array
C
         do 1 i = 1, n_comp_arg
            comp_arg(i) = 0
1        continue

         if( i_comp .eq. 1 ) then 
             comp_type = COMP_CODE_ NONE
         endif
         if( i_comp .eq. 2 ) then 
             comp_type = COMP_CODE_ RLE
         endif
         if( i_comp .eq. 3 ) then 
             comp_type = COMP_CODE_SKPHUFF 
             comp_arg(1) = skphuff_skp_size
         endif
         if( i_comp .eq. 4 ) then 
             comp_type =    COMP_CODE_DEFLATE 
             comp_arg(1) = deflate_level
         endif
         status = sfscompress(sds_id(i_comp), comp_type, comp_arg)
          if( status .ne. 0 ) then
            print *, 'sfscompress failed for', i_comp, ' -th dataset'
            err_compress = err_compress + 1
          endif
C
C   Write data to the file
C
                start(1) = n_start
                start(2) = m_start
                stride(1) = n_stride
                stride(2) = m_stride
                edges(1)  = n_edges
                edges(2)  = m_edges
                status = sfwdata(sds_id(i_comp), start, stride,
     .                           edges, data)
            if( status .ne. 0 ) then
                print *, 'sfwdata failed for', i_comp, ' -th dataset'
                err_compress = err_compress + 1
            endif
         
         status = sfendacc(sds_id(i_comp))
            if( status .ne. 0 ) then
                print *, 'sfendacc failed for', i_comp, ' -th dataset'
                err_compress = err_compress + 1
            endif
         status = sfend (sd_id(i_comp))
            if( status .ne. 0 ) then
                print *, 'sfend failed for', i_comp, ' -th dataset'
                err_compress = err_compress + 1
            endif

1000      continue 

C
C   Let's check what we have written 
C
C   Open files and restart SD interfaces
C
         do 2000 i = 1, N_COMP_TYPES
C
            sd_id(i) = sfstart (file(i), DFACC_READ)
            if( sd_id(i) .eq. -1 ) then
                print *, 'sfstart failed for', i, ' -th dataset'
                err_compress = err_compress + 1
            endif

C
C  Find written dataset in each file using its name and index
C

            sds_index(i) = sfn2index (sd_id(i), name(i))
            if( sds_index(i) .eq. -1 ) then
                print *, 'sfn2index failed for', i, ' -th dataset'
                err_compress = err_compress + 1
            endif
            sds_id(i)    = sfselect (sd_id(i), sds_index(i))
            if( sds_id(i) .eq. -1 ) then
                print *, 'sfselect failed for', i, ' -th dataset'
                err_compress = err_compress + 1
            endif


C
C   Read part of the data back using sfrdata function
C
         start(1) = n_part_start
         start(2) = m_part_start
         edges(1) = n_part
         edges(2) = m_part
         stride(1) = n_part_stride 
         stride(2) = m_part_stride 
         status = sfrdata (sds_id(i), start, stride, edges,
     .                         part_data)
             if (status .ne. 0) then
                 print *, 'sfrdata failed for reading part data for ',
     .           i, '-th dataset'
             err_compress = err_compress + 1
             endif
C
C   Compare output with aqtual data
C
         do 204 j = 1, m_part
            do 203 l = 1, n_part
               kl = n_part_start + 1 + (l-1)*n_part_stride
               kj = m_part_start + 1 + (j-1)*m_part_stride
               if (data(kl, kj) .ne. part_data(l,j)) then
                  print *, 'sfrdata read wrong data for ', 
     .            i ,'-th dataset'
               err_compress = err_compress +1
               endif 
203         continue
204      continue
 
         
C 
C  Terminate access to SDS, shutdown interfaces and close the files
C
           status = sfendacc(sds_id(i))
            if( status .ne. 0 ) then
                print *, 'sfendacc failed for', i, ' -th dataset'
                err_compress = err_compress + 1
            endif
           status = sfend(sd_id(i))
            if( status .ne. 0 ) then
                print *, 'sfend failed for', i, ' -th dataset'
                err_compress = err_compress + 1
            endif

2000   continue
         return
         end

