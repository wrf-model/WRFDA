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
C $Id: tpf.f,v 1.16 1997/10/24 17:28:03 acheng Exp $
C
      subroutine tpf (number_failed)
C      program tpff 
      implicit none
      include 'fortest.inc'
C
C
C Test program: Writes palettes in a file.
C               Reads palettes from the file.
C               Writes palette with specified reference number.
C               Reads palette with specified reference number.
C
C Input file: none
C
C Output file: tpalf.hdf
C
C

      integer number_failed
      character*20 myname
      parameter (myname = 'p')

      integer dpppal, dpapal, dprest, dpgpal, dpnpals
      integer dprref, dpwref
      integer dplref

      character*64 TESTFILE
      character*1 CR
      character pal1(768), pal2(768), ipal(768)
      integer ret, ref 
      integer ref1, ref2, newref1, newref2
      integer i


      call ptestban('Testing', myname)
      TESTFILE = 'tpalf.hdf'
      CR = char(10)
      number_failed = 0
      ref = 0
C
C Initialize pal1 as {1, 2, 3, 4, 5, ...}
C Initialize pal2 as {1, 1, 1, 2, 2, 2, ...}
      do 100 i = 0, 255
          pal1(3*i + 1) = char(i)
          pal1(3*i + 2) = char(i) 
          pal1(3*i + 3) = char(i)
          pal2(i + 1) = char(i) 
          pal2(i + 256 + 1) = char(i) 
          pal2(i + 512 + 1) = char(i) 
100   continue

C
C Write out pal1, then pal2.
C Keep their ref number in ref1 and ref2.
      call MESSAGE(VERBO_HI, 'Putting pal1 in new file.')
      ret = dpppal(TESTFILE, pal1, 0, 'w')
      call VRFY(ret, 'dpppal', number_failed)

      call MESSAGE(VERBO_HI, 'Getting ref1')
      ref1 = dplref()
      ref = ref1*1
      call VRFY(ref, 'dplref', number_failed)
C VRFY expects an integer, but ref1 is only integer*2.  The
C expression promotes it to an integer expression.

      call MESSAGE(VERBO_HI, 'Putting pal2 in file')
      ret = dpapal(TESTFILE, pal2)
      call VRFY(ret, 'dpapal', number_failed)

      call MESSAGE(VERBO_HI, 'Getting ref2')
      ref2 = dplref()
      ref = ref2*1
      call VRFY(ref, 'dplref', number_failed)
     
C
C Reset the palettes for reading
      call MESSAGE(VERBO_HI, 'Restarting palette interface')
      ret = dprest()
      call VRFY(ret, 'dprest', number_failed)

C
C Get palette 1 and match it with pal1
      call MESSAGE(VERBO_HI, 'Reading pal1')
      ret = dpgpal(TESTFILE, ipal)
      call VRFY(ret, 'dpgpal', number_failed)
      do 200 i=1, 768
          if (ipal(i) .ne. pal1(i))  then
              print *, 'Error at ', i, ', ipal:', ipal(i), 
     *                 '      pal1(i):', pal1(i)
          endif
200   continue
      
C
C verify the ref number is updated correctly too
      call MESSAGE(VERBO_HI, 'Getting newref1')
      newref1 =  dplref()
      if (newref1 .ne. ref1) then
	print *, 'Error: newref1 is ', newref1, ', should be ', ref1
	number_failed = number_failed + 1
      endif

C
C Get palette 2 and match it with pal2
      call MESSAGE(VERBO_HI, 'Reading pal2.')
      ret = dpgpal(TESTFILE, ipal)
      call VRFY(ret, 'dpgpal', number_failed)
      do 300 i=1, 768
          if (ipal(i) .ne. pal2(i)) then
              print *, 'Error at ', i, ', ipal:', ipal(i),
     *                 '      pal2:', pal2(i)
          endif
300   continue

C
C Again verify the ref number
      call MESSAGE(VERBO_HI, 'Getting ref2')
      newref2 =  dplref()
      if (newref2 .ne. ref2) then
	print *, 'Error: newref2 is ', newref2, ', should be ', ref2
	number_failed = number_failed + 1
      endif

C
C Check number of palettes
      call MESSAGE(VERBO_HI, 'Getting number of palettes')
      ret = dpnpals(TESTFILE)
      if (ret .ne. 2) then
	print *, 'Error: number of palette is ', ret, ', should be 2'
	number_failed = number_failed + 1
      endif

C
C Explicitly set to palette of ref2 for reading
      call MESSAGE(VERBO_HI, 'Setting read ref to ref2.')
      ret = dprref(TESTFILE, ref2)
      call VRFY(ret, 'dprref', number_failed)
      
      call MESSAGE(VERBO_HI, 'Reading pal2')
      ret = dpgpal(TESTFILE, ipal)
      call VRFY(ret, 'dpgpal', number_failed)

      newref2 =  dplref()
      if (newref2 .ne. ref2) then
	print *, 'Error: newref2 is ', newref2, ', should be ', ref2
	number_failed = number_failed + 1
      endif

C
C match it with pal2
      do 400 i=1, 768
          if (ipal(i) .ne. pal2(i)) then
              print *,  'Error at ', i, ', ipal:', ipal(i),
     *                 '      pal2:', pal2(i)
          endif
400   continue

C
C Explicitly set to palette of ref1 for reading
      call MESSAGE(VERBO_HI, 'Setting read ref to ref1.')
      ret = dprref(TESTFILE, ref1)
      call VRFY(ret, 'dprref', number_failed)

      call MESSAGE(VERBO_HI, 'Reading pal1')
      ret = dpgpal(TESTFILE, ipal)
      call VRFY(ret, 'dpgpal', number_failed)

      newref1 =  dplref()
      if (newref1 .ne. ref1) then
	print *, 'Error: newref1 is ', newref1, ', should be ', ref1
	number_failed = number_failed + 1
      endif
      
C
C match it with pal1
      do 500 i=1, 768
          if (ipal(i) .ne. pal1(i)) then
              print *,  'Error at ', i, ', ipal:', ipal(i),
     *                 '      pal1:', pal1(i)
          endif
500   continue

C
C Modify the middle chunk of pal1 and replace its file copy.
      call MESSAGE(VERBO_HI, 'Modifying pal1')
      do 600 i=1,256
          pal1(i+256) = char(256-i)
600   continue

      call MESSAGE(VERBO_HI, 'Setting write ref to ref1')
      ret = dpwref(TESTFILE, ref1)
      call VRFY(ret, 'dpwref', number_failed)
      call MESSAGE(VERBO_HI, 'Writing pal1')
      ret = dpppal(TESTFILE, pal1, 1, 'a')
      call VRFY(ret, 'dpppal', number_failed)
      ret=dplref()
C     print *,'last ref is: ', ret
      call MESSAGE(VERBO_HI, 'setting read ref to ref1')
      ret = dprref(TESTFILE, ref1)
      call VRFY(ret, 'dprref', number_failed)
      call MESSAGE(VERBO_HI, 'Reading pal1')
      ret = dpgpal(TESTFILE, ipal)
      call VRFY(ret, 'dpgpal', number_failed)
      do 700 i=1, 768
          if (ipal(i) .ne. pal1(i)) then
              print *,  'Error at ', i, ', ipal:', ipal(i),
     *                 '      pal1:', pal1(i)
          endif
700   continue

      if (number_failed .eq. 0) then 
          call MESSAGE(VERBO_DEF + 1,
     +		'****** ALL TESTS SUCCESSFUL ******')
      else
          print *, '****** ', number_failed, ' TESTS FAILES  ******'
      endif

      return
      end


