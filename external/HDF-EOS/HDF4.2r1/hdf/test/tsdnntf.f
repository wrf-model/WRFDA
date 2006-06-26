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
C $Id: tsdnntf.f,v 1.14 1997/10/24 17:28:05 acheng Exp $
C
      subroutine tsdnntf (number_failed)
C
C
C  Program to test writing SDSs with different types of data.
C
C  Input file:  none
C  Output files:  fo1.hdf, fo2.hdf, ...fo5.hdf, fo.hdf
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
      character*20  myname
      parameter (myname = 'sdnnt')

      integer dspdata, dsgdata, dsadata, dssdims, dssnt
      
      real*8 f64(10,10), tf64(10,10)
      real*4 f32(10,10), tf32(10,10)
      integer*2 i16(10,10), ti16(10,10)
      integer*4 i32(10,10), ti32(10,10)
      
C  Change these to be of type 'byte' for VMS
C      byte      i8(10,10), ti8(10,10)
      character i8(10,10), ti8(10,10)
      
      integer i, j, err
      integer rank
      integer dims(2)
      integer DFNT_FLOAT64, DFNT_FLOAT32, DFNT_INT8, DFNT_INT16
      integer DFNT_INT32
      integer DFNT_NFLOAT64, DFNT_NFLOAT32, DFNT_NINT8
      integer DFNT_NINT16, DFNT_NINT32, DFNT_NATIVE
      
      call ptestban('Testing', myname)
      DFNT_FLOAT64 = 6
      DFNT_FLOAT32 = 5
      DFNT_INT8 = 20
      DFNT_INT16 = 22
      DFNT_INT32 = 24
      DFNT_NATIVE = 4096
      
C These should really use a logical OR to compute these values
C However, OR() is not really that portable

      DFNT_NFLOAT64 = DFNT_NATIVE + DFNT_FLOAT64
      DFNT_NFLOAT32 = DFNT_NATIVE + DFNT_FLOAT32
      DFNT_NINT8 =    DFNT_NATIVE + DFNT_INT8
      DFNT_NINT16 =   DFNT_NATIVE + DFNT_INT16
      DFNT_NINT32 =   DFNT_NATIVE + DFNT_INT32

      rank = 2
      dims(1) = 10
      dims(2) = 10
      number_failed = 0
 
      call MESSAGE(5, 'Creating arrays...')
  
      do 110 i=1,10
          do 100 j=1,10
            f64(i,j) = (i * 10) + j
  	    f32(i,j) = (i * 10) + j
C  Use the following line for VMS
C            i8(i,j) =  (i * 10) + j
  	     i8(i,j) = char( (i * 10) + j )
  	    i16(i,j) = (i * 10) + j
  	    i32(i,j) = (i * 10) + j
  100     continue
  110 continue
  
      err = dssdims(rank, dims)
  
C  individual files 
      call MESSAGE(5, 'Testing arrays in individual files...')
  
      err = dssnt(DFNT_NFLOAT64)
      call VRFY(err, 'dssnt (float64)', number_failed)
      err = dspdata('fo1.hdf', rank, dims, f64)
      call VRFY(err, 'dspdata (float64)', number_failed)
      err = dsgdata('fo1.hdf', rank, dims, tf64)
      call VRFY(err, 'dsgdata (float64)', number_failed)
      err = 0
      do 160 i=1,10
          do 150 j=1,10
  	    if (f64(i,j).ne.tf64(i,j)) err = 1
  	    tf64(i,j) = 0.0
  150     continue
  160 continue

      call err_check(err, number_failed, 'float64')

      err = dssnt(DFNT_NFLOAT32)
      call VRFY(err, 'dssnt (float32)', number_failed)
      err = dspdata('fo2.hdf', rank, dims, f32)
      call VRFY(err, 'dspdata (float32)', number_failed)
      err = dsgdata('fo2.hdf', rank, dims, tf32)
      call VRFY(err, 'dsgdata (float32)', number_failed)
      err = 0
      do 210 i=1,10
         do 200 j=1,10
             if (f32(i,j).ne.tf32(i,j)) err = 1
             tf32(i,j) = 0.0
 200      continue
 210   continue
       
       call err_check(err, number_failed, 'float32')
       
      err = dssnt(DFNT_NINT8)
      call VRFY(err, 'dssnt (int8)', number_failed)
      err = dspdata('fo3.hdf', rank, dims, i8)
      call VRFY(err, 'dspdata (int8)', number_failed)
      err = dsgdata('fo3.hdf', rank, dims, ti8)
      call VRFY(err, 'dsgdata (int8)', number_failed)
      err = 0
      do 310 i=1,10
         do 300 j=1,10
             if (i8(i,j).ne.ti8(i,j)) err = 1
C     Use the following line for VMS
C     ti8(i,j) = 0
             ti8(i,j) = char(0)
 300      continue
 310  continue
       
      call err_check(err, number_failed, 'int8')
       
      err = dssnt(DFNT_NINT16)
      call VRFY(err, 'dssnt (int16)', number_failed)
      err = dspdata('fo4.hdf', rank, dims, i16)
      call VRFY(err, 'dspdata (int16)', number_failed)
      err = dsgdata('fo4.hdf', rank, dims, ti16)
      call VRFY(err, 'dsgdata (int16)', number_failed)
      err = 0
      do 410 i=1,10
          do 400 j=1,10
             if (i16(i,j).ne.ti16(i,j)) err = 1
             ti16(i,j) = 0
 400      continue
 410  continue
       
      call err_check(err, number_failed, 'int16')
       
      err = dssnt(DFNT_NINT32)
      call VRFY(err, 'dssnt (int32)', number_failed)
      err = dspdata('fo5.hdf', rank, dims, i32)
      call VRFY(err, 'dspdata (int32)', number_failed)
      err = dsgdata('fo5.hdf', rank, dims, ti32)
      call VRFY(err, 'dsgdata (int32)', number_failed)
      err = 0
      do 510 i=1,10
          do 500 j=1,10
             if (i32(i,j).ne.ti32(i,j)) err = 1
             ti32(i,j) = 0
 500      continue
 510  continue
       
      call err_check(err, number_failed, 'int32')
       
       
C     
      call MESSAGE(5, 'Writing arrays to single file.')
C     
      err = dssnt(DFNT_NFLOAT64)
      err = dsadata('fo.hdf', rank, dims, f64)
      call VRFY(err, 'dsadata (f64)', number_failed)
       
      err = dssnt(DFNT_NFLOAT32)
      err = dsadata('fo.hdf', rank, dims, f32)
      call VRFY(err, 'dsadata (f32)', number_failed)
       
      err = dssnt(DFNT_NINT8)
      err = dsadata('fo.hdf', rank, dims, i8)
      call VRFY(err, 'dsadata (i8)', number_failed)
       
      err = dssnt(DFNT_NINT16)
      err = dsadata('fo.hdf', rank, dims, i16)
      call VRFY(err, 'dsadata (i16)', number_failed)
       
      err = dssnt(DFNT_NINT32)
      err = dsadata('fo.hdf', rank, dims, i32)
      call VRFY(err, 'dsadata (i32)', number_failed)
       
C     
      call MESSAGE(5, 'Reading arrays from single file... ')
C     
      err = dsgdata('fo.hdf', rank, dims, tf64)
      call VRFY(err, 'dsgdata (tf64)', number_failed)
      err = dsgdata('fo.hdf', rank, dims, tf32)
      call VRFY(err, 'dsgdata (tf32)', number_failed)
      err = dsgdata('fo.hdf', rank, dims, ti8)
      call VRFY(err, 'dsgdata (ti8)', number_failed)
      err = dsgdata('fo.hdf', rank, dims, ti16)
      call VRFY(err, 'dsgdata (ti16)', number_failed)
      err = dsgdata('fo.hdf', rank, dims, ti32)
      call VRFY(err, 'dsgdata (ti32)', number_failed)
       
C     
      call MESSAGE(5, 'Checking arrays from single file...\n\n')
       
      err = 0
      do 910 i=1,10
          do 900 j=1,10
             if (f64(i,j) .ne. tf64(i,j)) err = 1
 900     continue
 910  continue
       
      call err_check(err, number_failed, 'float64')
C     
       
      err = 0
      do 1010 i=1,10
          do 1000 j=1,10
             if (f32(i,j) .ne. tf32(i,j)) err = 1
 1000     continue
 1010 continue
       
      call err_check(err, number_failed, 'float32')
C     
      err = 0
      do 1110 i=1,10
          do 1100 j=1,10
             if (i8(i,j) .ne. ti8(i,j)) err = 1
 1100     continue
 1110 continue
       
      call err_check(err, number_failed, 'int8')
C     
      err = 0
      do 1210 i=1,10
          do 1200 j=1,10
             if (i16(i,j) .ne. ti16(i,j)) err = 1
 1200     continue
 1210 continue
       
      call err_check(err, number_failed, 'int16')
C     
      err = 0
      do 1310 i=1,10
          do 1300 j=1,10
             if (i32(i,j) .ne. ti32(i,j)) err = 1
 1300     continue
 1310 continue
       
      call err_check(err, number_failed, 'int32')
C     
      if (number_failed .gt. 0 ) then
          print *,'        >>> ', number_failed, ' TESTS FAILED <<<'
      else
          call MESSAGE(VERBO_HI, '        >>> ALL TESTS PASSED <<<')
      endif
       
      return
      end
