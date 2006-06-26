C****************************************************************************
C* NCSA HDF                                                                 *
C* Software Development Group                                               *
C* National Center for Supercomputing Applications                          *
C* University of Illinois at Urbana-Champaign                               *
C* 605 E. Springfield, Champaign IL 61820                                   *
C*                                                                          *
C* For conditions of distribution and use, see the accompanying             *
C* hdf/COPYING file.                                                        *
C*                                                                          *
C****************************************************************************
C
C $Id: tsdmmsf.f,v 1.17 1999/03/27 20:00:33 epourmal Exp $
C
      subroutine tsdmmsf (number_failed)
C
C
C  Program to test writing SDSs with different types of data and
C  scales and max/min values.
C
C  Input file:  none
C  Output files: o0, o1, ... o6
C
C
C  **** VMS users ****
C
C  VMS has a special way of handling the passsing of character
C   strings between C and FORTRAN.  For these tests to work 
C   correctly, you must change the definition of i8 and ti8
C   to be 'byte' not 'character'  You will also need to remove
C   a couple of calls to char().  If you search on the string 
C   VMS you should be able to find all of the necessary changes.
C
      implicit none
      include 'fortest.inc'
   
      integer number_failed
      character*20 myname
      parameter (myname = 'sdmms')
      
      integer dsgdata, dsadata, dssdims, dssrang, dsgrang, dssnt
      integer dssdisc, dsgdisc, dsscal, dsgcal

      real*8 f64(10,10), tf64(10,10)
      real*8 f64scale(10), tf64scale(10)
      real*8 f64max, f64min, tf64max, tf64min

      real*8 cal,  cale,  ioff,  ioffe
      real*8 ical, icale, iioff, iioffe
      integer*4 ctype, ictype

      real*4 f32(10,10), tf32(10,10)
      real*4 f32scale(10), tf32scale(10)
      real*4 f32max, f32min, tf32max, tf32min

C Change these to be of type 'byte' for VMS      
C      byte i8(10,10), ti8(10,10)
C      byte i8scale(10), ti8scale(10), i8max, i8min
C      byte ti8max, ti8min   
      character i8(10,10), ti8(10,10)
      character i8scale(10), ti8scale(10), i8max, i8min
      character ti8max, ti8min
C Align the Character variables with the surrogate names.
C Need to do this because HDF always assume int8 is a packed 8 bits
C quantities of precisely 1 byte big.  Integer*1 may have memory size
C as large as a normal integer (e.g. Cray).
C Cannot just use the character variables as for some compilers,
C the argument address of a character argument is not compatible with
C that of a numerial argument.

      integer surri8, surri8max, surri8min, surri8scale
      integer surrti8, surrti8max, surrti8min, surrti8scale
      equivalence (i8, surri8)
      equivalence (i8scale, surri8scale)
      equivalence (i8min, surri8min)
      equivalence (i8max, surri8max)
      equivalence (ti8, surrti8)
      equivalence (ti8scale, surrti8scale)
      equivalence (ti8min, surrti8min)
      equivalence (ti8max, surrti8max)

      integer*2 i16(10,10), ti16(10,10)
      integer*2 i16scale(10), ti16scale(10), i16max, i16min
      integer*2 ti16max, ti16min

      integer*4 i32(10,10), ti32(10,10)
      integer*4 i32scale(10), ti32scale(10), i32max, i32min
      integer*4 ti32max, ti32min

      integer i, j, err, err1, err2, err3, err4
      integer rank, dims(2)
      integer DFNT_FLOAT64, DFNT_FLOAT32, DFNT_INT8, DFNT_INT16
      integer DFNT_INT32

      call ptestban('Testing', myname)
      f64max = 40.0
      f64min = 0.0
      f32max = 40.0
      f32min = 0.0
C Use the following lines for VMS
C      i8min = -128
C      i8max = 127
      i8max = char(127)
C NOTE: If you get a compile error on the "char(-128)" line, substitute
C       the "char(0)" line.  Its not quite as thorough a test, but...
      i8min = char(0)
C      i8min = char(-128)
      i16max = 1200
      i16min = -1200
      i32max = 99999999
      i32min = -999999999
      
      rank = 2
      dims(1) = 10
      dims(2) = 10
      number_failed = 0
      DFNT_FLOAT64 = 6
      DFNT_FLOAT32 = 5
      DFNT_INT8 = 20
      DFNT_INT16 = 22
      DFNT_INT32 = 24
      
C
C Set up some calibration info
C
      cal   = 10.0
      cale  = 35.235
      ioff  = 16.75
      ioffe = 47.8
      ctype = DFNT_INT16

      call MESSAGE(5, 'Creating arrays...')
      
      do 110 i=1,10
          do 100 j=1,10
            f64(i,j) = (i * 40) + j
            f32(i,j) = (i * 40) + j
C  Use the following line for VMS
C            i8(i,j) =  (i * 10) + j      
            i8(i,j) = char( (i * 10) + j )
            i16(i,j) = (i * 3000) + j
            i32(i,j) = (i * 20) + j
  100     continue
          f64scale(i) = (i * 40) + j
          f32scale(i) = (i * 40) + j
C  Use the following line for VMS
C          i8scale(i) = (i * 10) + j
          i8scale(i) = char((i * 10) + j)
      	  i16scale(i) = (i * 3000) + j
      	  i32scale(i) = (i * 20) + j
  110 continue

      err1 = dssdims(rank, dims)
      
C
C  Writing dimscale, max/min, and arrays to a single file 
C
      call MESSAGE(5, 'Writing arrays to single file...')

      err  = dssnt(DFNT_FLOAT64)
      err1 = dssdisc(1, 10, f64scale)
      err2 = dssrang(f64max, f64min)
      err4 = dsscal(cal, cale, ioff, ioffe, ctype)
      err3 = dsadata('of.hdf', rank, dims, f64)
      call errchkio(err1, err2, err3, number_failed, 'float64 write')

      if(err4.eq.(-1)) then
         number_failed = number_failed + 1
         print *, '>>> Setting calibration failed'
      endif

      err  = dssnt(DFNT_FLOAT32)
      err1 = dssdisc(1, 10, f32scale)
      err2 = dssrang(f32max, f32min)
      err3 = dsadata('of.hdf', rank, dims, f32)
      call errchkio(err1, err2, err3, number_failed, 'float32 write')

      err  = dssnt(DFNT_INT8)
      err1 = dssdisc(1, 10, surri8scale)
      err2 = dssrang(surri8max, surri8min)
      err3 = dsadata('of.hdf', rank, dims, surri8)
      call errchkio(err1, err2, err3, number_failed, 'int8 write')
      
      
      err  = dssnt(DFNT_INT16)
      err1 = dssdisc(1, 10, i16scale)
      err2 = dssrang(i16max, i16min)
      err3 = dsadata('of.hdf', rank, dims, i16)
      call errchkio(err1, err2, err3, number_failed, 'int16 write')
      
      err  = dssnt(DFNT_INT32)
      err1 = dssdisc(1, 10, i32scale)
      err2 = dssrang(i32max, i32min)
      err3 = dsadata('of.hdf', rank, dims, i32)
      call errchkio(err1, err2, err3, number_failed, 'int32 write')
      
C
C  Reading back dimscales, max/min, and arrays from single file
C
      err1 = dsgdata('of.hdf', rank, dims, tf64)
      err2 = dsgdisc(1, 10, tf64scale)
      err3 = dsgrang(tf64max, tf64min)
      err4 = dsgcal(ical, icale, iioff, iioffe, ictype) 
      call errchkio(err1, err2, err3, number_failed, 'float64 read')

      if(err4.eq.(-1)) then
         number_failed = number_failed + 1
         print *, '>>> Reading calibration failed'
      endif
      
      if((cal.ne.ical).or.(cale.ne.icale)) then
         if((ioff.ne.iioff).or.(ioff.ne.iioffe)) then
            if(ctype.ne.ictype) then
               print *, '>>>Returned calibration values are wrong'
               print *, ical, icale
               print *, iioff, iioffe
               print *, ictype 
               print *, cal, cale
               print *, ioff, ioffe
               print *, ctype
               number_failed = number_failed + 1
            endif
         endif
      endif

      err1 = dsgdata('of.hdf', rank, dims, tf32)
      err2 = dsgdisc(1, 10, tf32scale)
      err3 = dsgrang(tf32max, tf32min)
      err4 = dsgcal(ical, icale, iioff, iioffe, ictype) 
      call errchkio(err1, err2, err3, number_failed, 'float32 read')

      if(err4.ne.(-1)) then
         number_failed = number_failed + 1
         print *, '>>> Read calibration where none stored'
      endif
      
      err1 = dsgdata('of.hdf', rank, dims, surrti8)
      err2 = dsgdisc(1, 10, surrti8scale)
      err3 = dsgrang(surrti8max, surrti8min)
      call errchkio(err1, err2, err3, number_failed, 'int8 read')
      
      err1 = dsgdata('of.hdf', rank, dims, ti16)
      err2 = dsgdisc(1, 10, ti16scale)
      err3 = dsgrang(ti16max, ti16min)
      call errchkio(err1, err2, err3, number_failed, 'int16 read')
      
      err1 = dsgdata('of.hdf', rank, dims, ti32)
      err2 = dsgdisc(1, 10, ti32scale)
      err3 = dsgrang(ti32max, ti32min)
      call errchkio(err1, err2, err3, number_failed, 'int32 read')
      
C
C  Checking dimscales, max/min and arrays from single file
C
      call MESSAGE(5,
     +    'Checking dimscales, max/min & arrays from single file')

C  float64
      err1 = 0
      err2 = 0
      err3 = 0
      do 1010 i=1,10
         do 1000 j=1,10
           if (f64(i,j) .ne. tf64(i,j)) err1 = 1
 1000    continue

         if (f64scale(i) .ne. tf64scale(i)) err2 = 1
 1010 continue

      if ((f64max .ne. tf64max) .or. (f64min .ne. tf64min)) err3 = 1
      call errchkarr(err1, err2, err3, number_failed, 'float64')

C  float32
      err1 = 0
      err2 = 0
      err3 = 0
      do 1030 i=1,10
         do 1020 j=1,10
           if (f32(i,j) .ne. tf32(i,j)) err1 = 1
 1020    continue
         if (f32scale(i) .ne. tf32scale(i)) err2 = 1
 1030 continue

      if ((f32max .ne. tf32max) .or. (f32min .ne. tf32min)) err3 = 1
      call errchkarr(err1, err2, err3, number_failed, 'float32')

C  int8
      err1 = 0
      err2 = 0
      err3 = 0
      do 1110 i=1,10
         do 1100 j=1,10
           if (i8(i,j) .ne. ti8(i,j)) err1 = 1
 1100    continue
         if (i8scale(i) .ne. ti8scale(i)) err2 = 1
 1110 continue

      if ((i8max .ne. ti8max) .or. (i8min .ne. ti8min)) err3 = 1
      call errchkarr(err1, err2, err3, number_failed, 'int8')

C  int16
      err1 = 0
      err2 = 0
      err3 = 0
       do 1210 i=1,10
         do 1200 j=1,10
           if (i16(i,j) .ne. ti16(i,j)) err1 = 1
 1200    continue
         if (i16scale(i) .ne. ti16scale(i)) err2 = 1
 1210 continue

      if ((i16max .ne. ti16max) .or. (i16min .ne. ti16min)) err3 = 1
      call errchkarr(err1, err2, err3, number_failed, 'int16')

C  int32
      err1 = 0
      err2 = 0
      err3 = 0
       do 1310 i=1,10
         do 1300 j=1,10
           if (i32(i,j) .ne. ti32(i,j)) err1 = 1
 1300    continue
      	 if (i32scale(i) .ne. ti32scale(i)) err2 = 1
 1310 continue

      if ((i32max .ne. ti32max) .or. (i32min .ne. ti32min)) err3 = 1
      call errchkarr(err1, err2, err3, number_failed, 'int32')
C
C  Sum up
C
      
      if (number_failed .gt. 0 ) then
          print *, '        >>> ', number_failed, ' TESTS FAILED <<<'
      else
          if (verbosity .ge. VERBO_HI) then
	      print *, '        >>> ALL TESTS PASSED <<<'
	  endif
      endif

      return
      end
