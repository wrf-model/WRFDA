C****************************************************************************
C* $Id: tstubsf.f,v 1.18 1997/01/17 19:08:16 sxu Exp $			    *
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
C
      subroutine tstubsf (nerrors)
      implicit none
      include 'fortest.inc'

      integer nerrors
      character*20  myname
      parameter (myname = 'stubs')

      character ar0*10
      character ar1*9
      character ar2*30
      character ar3*8

      character in*20

      integer dfile
      integer dfinfo(16)
      integer dlist(128)
      integer a0size, a1size, a2size, a3size
      integer ret, err, i, nd, dfenum, tag, ref, length

      integer t255
      integer t127
      integer r1
      integer r3
      integer r7

      integer dfaccess, dfopen, dfclose, dfdesc, dfdup, dfdel, dfread,
     +     dfwrite, dfupdate, dfget, dfput, dfsfind, dffind,
     +     dferrno, dfishdf, dfnewref, dfnumber, dfstat

      data t255 /255/
      data t127 /127/
      data r1   /1/
      data r3   /3/
      data r7   /7/

      data ar0 /'Testing...'/
      data ar1 /'...one...'/
      data ar2 /'...two...'/
      data ar3 /'...three'/


      call ptestban('Testing', myname)
      nerrors = 0
      a0size = len(ar0)
      a1size = len(ar1)
      a2size = len(ar2)
      a3size = len(ar3)

C      print *, 'This program will test the DF emulation layer'
C      print *, 'of HDF 3.2 and beyond (FORTRAN version).  Many'
C      print *, 'routines will be tested individually.  Each test'
C      print *, 'will report its name and results.  If all goes'
C      print *, 'well, all of the results will begin with "Success".'
C      print *, 'If a test fails, the results line will begin with'
C      print *, '">>>Failure".  An error count is kept and printed'
C      print *, 'out at the end.'
C      print *, 'Hit <return> to continue.'
C      read(5,100) in
C 100  format(a)
C      in(1:20) = '                    '

      call MESSAGE(VERBO_MED, 'Testing dferrno...')
      ret = dferrno()
      if (ret .ne. 0) then
         print *, '>>>Failure:  Returned ', ret, ' rather than 0.'
         nerrors = nerrors + 1
      else
         call MESSAGE(VERBO_HI, 'Success!')
      endif

      call MESSAGE(VERBO_MED, 'Testing dfishdf... (should fail)')
      ret = dfishdf('tstubsF.hdf')
      dfenum = dferrno()
      if (ret .eq. -1) then
C       print *, 'Success:  dfishdf failed with DFerror = ', dfenum
        call MESSAGE(VERBO_HI, 'Success:  dfishdf did fail')
      else
         print *, '>>>Failure:  Non-existent file looks like HDF file.'
         print *, '   Maybe was a pre-existing file named "tstubsF.hdf"'
         print *, '   DFerror = ', dfenum
         nerrors = nerrors + 1
      endif

      call MESSAGE(VERBO_MED, 'Testing dfopen... (new file)')
      dfile = dfopen('tstubsF.hdf', 6, 0)
      dfenum = dferrno()
      if (dfile .eq. 0) then
         print *, '>>>Failure:  Error ', dfenum, ' opening file.'
         print *, '   Quiting.'
         return
      else
         call MESSAGE(VERBO_HI, 'Success!')
      endif

      call MESSAGE(VERBO_MED, 'Testing dfclose...')
      ret = dfclose(dfile)
      dfenum = dferrno()
      if (ret .eq. -1) then
         print *, '>>>Failure:  could not close file.'
         print *, '   DFerror = ', dfenum
         nerrors = nerrors + 1
      else
         call MESSAGE(VERBO_HI, 'Success!')
      endif

      call MESSAGE(VERBO_MED,
     +    'Testing dfclose... (invalid file; should fail)')
      ret = dfclose(dfile)
      dfenum = dferrno()
      if (ret .eq. -1) then
         call MESSAGE(VERBO_HI, 'Success: dfclose did fail')
      else
         print *, '>>>Failure:  Close allowed on unopened file.'
         nerrors = nerrors + 1
      endif

      call MESSAGE(VERBO_MED, 'Testing dfopen... (existing file)')
      dfile = dfopen('tstubsF.hdf', 2, 0)
      dfenum = dferrno()
      if (dfile .eq. 0) then
         print *, '>>>Failure:  Error ', dfenum, ' opening file.'
         print *, '   Quiting.'
         return
      else
         call MESSAGE(VERBO_HI, 'Success!')
      endif

      call MESSAGE(VERBO_MED, 'Testing dfput...')
      ret = dfput(dfile, t255, r1, ar0, a0size)
      dfenum = dferrno()
      if (ret .ne. a0size) then
         print *, '>>>Failure:  DFerror = ', dfenum
         nerrors = nerrors + 1
      else
         call MESSAGE(VERBO_HI, 'Success!')
      endif

      call MESSAGE(VERBO_MED, 'Testing dfget...')
      ret = dfget(dfile, t255, r1, in)
      dfenum = dferrno()
      if (ret .ne. a0size) then
         print *, '>>>Failure:  read ', ret, ' of ', a0size, ' bytes.'
         print *, '   String read:  ', in
         print *, '   DFerror = ', dfenum
         nerrors = nerrors + 1
      else
         err = 0
         do 10 i=1, a0size
	    if (in(i:i) .ne. ar0(i:i)) err = 1
 10      continue
         if (err .eq. 1) then
            print *, '>>>Failure:  strings differ.'
            print *, '   String written:  ', ar0
            print *, '   String read:     ', in
            nerrors = nerrors + 1
         else
            call MESSAGE(VERBO_HI,
     +		'Success:  string read is the same as written.')
         endif
      endif

      in(1:20) = '                    '

      call MESSAGE(VERBO_MED, 'Testing dfaccess (write)...')
      ret = dfaccess(dfile, t255, r3, 'w')
      dfenum = dferrno()
      if (ret .eq. -1) then
         print *, '>>>Failure:'
         print *, '   DFerror = ', dfenum
         nerrors = nerrors + 1
      else
         call MESSAGE(VERBO_HI, 'Success!')
      endif

      call MESSAGE(VERBO_MED, 'Testing dfread... (should fail)')
      ret = dfread(dfile, in, 5)
      dfenum = dferrno()
      if (ret .eq. -1) then
         call MESSAGE(VERBO_HI, 'Success:  dfread did fail')
      else
         print *, '>>>Failure:  Read allowed on write element.'
         nerrors = nerrors + 1
      endif
      in(1:20) = '                    '

      call MESSAGE(VERBO_MED, 'Testing dfwrite...')
      ret = dfwrite(dfile, ar1, a1size)
      dfenum = dferrno()
      if (ret .ne. a1size) then
         print *, '>>>Failure:  wrote ', ret, ' of ', a1size, ' bytes.'
         print *, '   DFerror = ', dfenum
         nerrors = nerrors + 1
      else
         call MESSAGE(VERBO_HI, 'Success!')
      endif

      call MESSAGE(VERBO_MED, 'Testing dfaccess (read)...')
      ret = dfaccess(dfile, t255, r3, 'r')
      dfenum = dferrno()
      if (ret .eq. -1) then
         print *, '>>>Failure:'
         print *, '   DFerror = ', dfenum
         nerrors = nerrors + 1
      else
         call MESSAGE(VERBO_HI, 'Success!')
      endif

      call MESSAGE(VERBO_MED, 'Testing dfwrite... (should fail)')
      ret = dfwrite(dfile, in, 5)
      dfenum = dferrno()
      if (ret .eq. -1) then
         call MESSAGE(VERBO_HI, 'Success:  dfwrite did fail')
      else
         print *, '>>>Failure:  write allowed on read element.'
         nerrors = nerrors + 1
      endif

      call MESSAGE(VERBO_MED, 'Testing dfread...')
      ret = dfread(dfile, in, a1size)
      dfenum = dferrno()
      if (ret .ne. a1size) then
         print *, '>>>Failure:  read ', ret, ' of ', a1size, ' bytes.'
         print *, '   String read:  ', in
         print *, '   DFerror = ', dfenum
         nerrors = nerrors + 1
      else
	err = 0
	do 40 i=1, a1size
           if (in(i:i) .ne. ar1(i:i)) err = 1
 40	continue
	if (err .eq. 1) then
           print *, '>>>Failure:  strings differ.'
           print *, '   String written:  ', ar1
           print *, '   String read:     ', in
           nerrors = nerrors + 1
	else
           call MESSAGE(VERBO_HI,
     +		'Success:  string read is the same as written.')
        endif
      endif
      in(1:20) = '                    '

      call MESSAGE(VERBO_MED, 'Testing dfnumber...')
      nd = dfnumber(dfile, t255)
      dfenum = dferrno()
      if (nd .ne. 2) then
         print *, '>>>Failure:'
         print *, '   Saw ', nd, ' occurrances of tag 255 not than 2.'
         print *, '   DFerror = ', dfenum
         nerrors = nerrors + 1
      else
         call MESSAGE(VERBO_HI, 'Success!')
      endif

      call MESSAGE(VERBO_MED, 'Testing dfdesc...')
      ret = dfdesc(dfile, dlist, 0, 5)
      dfenum = dferrno()
C
C  add one for version tag
C
      if (ret .ne. (nd + 1)) then
         print *, '>>>Failure:  Returned ', ret, ' rather than ', nd+1
         print *, '   DFerror = ', dfenum
         nerrors = nerrors + 1
      else
         call MESSAGE(VERBO_HI, 'Success!')
      endif

      call MESSAGE(VERBO_MED, 'Testing dfupdate')
      ret = dfupdate(dfile)
      dfenum = dferrno()
      if (ret .ne. 0) then
         print *, '>>>Failure:'
         print *, '   DFerror = ', dfenum
         nerrors = nerrors + 1
      else
         call MESSAGE(VERBO_HI, 'Success!')
      endif

      call MESSAGE(VERBO_MED, 'Testing dfstat')
      ret = dfstat(dfile, dfinfo)
      dfenum = dferrno()
      if (ret .ne. 0) then
         print *, '>>>Failure:'
         print *, '   DFerror = ', dfenum
         nerrors = nerrors + 1
      else
         call MESSAGE(VERBO_HI, 'Success!')
      endif

      call MESSAGE(VERBO_MED, 'Testing dfnewref...')
      ret = dfnewref(dfile)
      dfenum = dferrno()
      if (ret .ne. 4) then
         print *, '>>>Failure:  Returned ref. ', ret, ' instead of 4'
         print *, '   DFerror = ', dfenum
         nerrors = nerrors + 1
      else
         call MESSAGE(VERBO_HI, 'Success!')
      endif

      call MESSAGE(VERBO_MED, 'Testing dfdup...')
      ret = dfdup(dfile, 127, r7, t255, r3)
      dfenum = dferrno()
      if (ret .eq. -1) then
         print *, '>>>Failure:'
         print *, '   DFerror = ', dfenum
         nerrors = nerrors + 1
      else
         ret = dfnumber(dfile, t127)
         dfenum = dferrno()
	if (ret .ne. 1) then
           print *, '>>>Failure:  duplicated tag not found.'
           print *, '   DFerror = ', dfenum
           nerrors = nerrors + 1
	else
           call MESSAGE(VERBO_HI, 'Success!')
        endif
      endif

      call MESSAGE(VERBO_MED, 'Testing dfdel...')
      ret = dfdel(dfile, t127, r7)
      dfenum = dferrno()
      if (ret .eq. -1) then
         print *, '>>>Failure:'
         print *, '   DFerror = ', dfenum
         nerrors = nerrors + 1
      else
         ret = dfnumber(dfile, t127)
         dfenum = dferrno()
         if (ret .ne. 0) then
            print *, '>>>Failure:  found ', ret, ' deleted tags.'
            print *, '   DFerror = ', dfenum
            nerrors = nerrors + 1
         else
	  call MESSAGE(VERBO_HI, 'Success!')
       endif
      endif

      call MESSAGE(VERBO_MED, 'Testing dfsfind...')
      ret = dfsfind(dfile, 254, 0)
      dfenum = dferrno()
      if (ret .eq. -1) then
         print *, '>>>Failure:'
         print *, '   DFerror = ', dfenum
         nerrors = nerrors + 1
      else
         call MESSAGE(VERBO_HI, 'Success!')
      endif

      call MESSAGE(VERBO_MED, 'Testing dffind...')
      ret = dfdup(dfile, 254, 4, 255, 3)
      if (ret .ne. 0) then
         print *, '>>>DFdup 1 failed.'
      endif
      ret = dfdup(dfile, 254, 5, 255, 3)
      if (ret .ne. 0) then
         print *, '>>>DFdup 2 failed.'
      endif
      ret = dfdup(dfile, 254, 6, 255, 3)
      if (ret .ne. 0) then
         print *, '>>>DFdup 3 failed.'
      endif
      do 200 i=4,6
         ret = dffind(dfile, tag, ref, length)
         dfenum = dferrno()
         if (ret .eq. -1) then
            print *, '>>>Failure on find #', i
            print *, '   DFerror = ', dfenum
            nerrors = nerrors + 1
         else
            if ((tag .ne. 254) .or. (ref .ne. i) .or.
     +           (length .ne. a1size)) then
               print *, '>>>Failure:  tag/ref found is not correct.'
               print *, '   Looking for:'
               print *, '      tag:      254'
               print *, '      ref:    ', i
               print *, '      length: ', a1size
               print *, '   Found:'
               print *, '      tag:    ', tag
               print *, '      ref:    ', ref
               print *, '      length: ', length
               nerrors = nerrors + 1
            else
               call MESSAGE(VERBO_HI, 'Success!')
            endif
         endif
 200  continue

      ret = dfclose(dfile)
      dfenum = dferrno()
      if (ret .ne. 0) then
         print *, '>>>Failure:  dfclose failed (probably from open aid)'
         print *, '   DFerror = ', dfenum
         nerrors = nerrors + 1
      endif

      if (nerrors .ne. 0) then
	  print *, '   ', nerrors, ' errors were encountered.'
      else
	  if (verbosity .ge. VERBO_HI) then
	      print *, '        >>> ALL TESTS PASSED <<<'
	  endif
      endif

      return
      end
