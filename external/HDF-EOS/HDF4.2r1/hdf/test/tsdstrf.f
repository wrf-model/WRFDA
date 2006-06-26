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
C $Id: tsdstrf.f,v 1.10 1997/01/17 19:08:14 sxu Exp $
C
      subroutine tsdstrf (num_failed)
      implicit none
C
C This program tests correctness of writing and read datastrings
C and dimension strings.
C To avoid the '\0' inserted by HDstrncpy, compare the first 14
C characters of output and input strings in subroutine compare()
      integer num_failed
      character*20 myname
      parameter (myname = 'sdstr')

      integer rank, i, j, ret, err
      integer dims(2)
      integer dssnt, dssdims, dssdast, dssdist, dspdata
      integer dsgdast, dsgdist, dsgdata, DFNT_NFLOAT32
      real    f32(10,10), inf32(10,10)
      character*15 datalabel, dataunit, datafmt, coordsys
      character*15 dimlabels(2), dimunits(2), dimfmts(2)
      character*16 indatalabel, indataunit, indatafmt, incoordsys
      character*16 indimlabels(2), indimunits(2), indimfmts(2)
      character*15 fn

      call ptestban('Testing', myname)
      DFNT_NFLOAT32 = 4096+5
      rank = 2
      dims(1) = 10
      dims(2) = 10
      datalabel = 'Datalabel'
      dataunit = 'Dataunit'
      datafmt = 'Datafmt'
      coordsys = 'Coordsys'
      dimlabels(1) = 'f_dim1_label_b'
      dimunits(1) =  'f_dim1_unit_b '
      dimfmts(1) =   'f_dim1_fmt_b  '
      dimlabels(2) = 'f_dim2_label_a'
      dimunits(2) =  'f_dim2_unit_a '
      dimfmts(2) =   'f_dim2_fmt_a  '
      fn = 'sdstrsf.hdf'

      err = 0
      num_failed = 0

      call MESSAGE(5, 'Creating arrays...')

      do 110 i=1,dims(2)
          do 100 j=1,dims(1) 
             f32(j,i) = (i*10) + j
100       continue
110   continue

      ret = dssdims(rank, dims)
      err = err + ret
      ret = dssnt(DFNT_NFLOAT32)
      err = err + ret
      ret = dssdast(datalabel, dataunit, datafmt, coordsys)
      err = err + ret
      ret = dssdist(1, dimlabels(1), dimunits(1), dimfmts(1))
      err = err + ret
      ret = dssdist(2, dimlabels(2), dimunits(2), dimfmts(2))
      err = err + ret
      ret = dspdata(fn, rank,dims, f32)
      err = err + ret

      call MESSAGE(5, 'Test strings written so far')

      ret = dsgdata(fn, rank, dims, inf32)
      err = err + ret
      ret = dsgdast(indatalabel, indataunit, indatafmt, incoordsys)
      err = err + ret
      ret = dsgdist(1, indimlabels(1), indimunits(1), indimfmts(1))
      err = err + ret
      ret = dsgdist(2, indimlabels(2), indimunits(2), indimfmts(2))
      err = err + ret

      call compare(datalabel, indatalabel, num_failed)
      call compare(dataunit, indataunit, num_failed)
      call compare(datafmt, indatafmt, num_failed)
      call compare(coordsys, incoordsys, num_failed)
      do 150 i=1,2
          call compare(dimlabels(i), indimlabels(i), num_failed)
          call compare(dimunits(i), indimunits(i), num_failed)
          call compare(dimfmts(i), indimfmts(i), num_failed)
150   continue

      if ((err .eq. 0) .and. (num_failed .eq. 0)) then
          call MESSAGE(5, '>>>> All Tests Passed.  >>>>')
      else
          print *, abs(err),' calls returned -1'
          print *, num_failed, ' values incorrect.'
      endif

      return
      end

     

      subroutine compare(outstring, instring, num)
      implicit none
      include 'fortest.inc'

      character*14 outstring, instring
      integer      num
C
C Note, outstring and instring are of length 14 instead of 15.
C
 
      if (outstring .ne. instring) then
          print *, 'Test failed for <', outstring,'>'
          print *, '      HDF says= <', instring,'>'
          num= num+ 1
      else
	  if (Verbosity .ge. VERBO_HI) then
	      print *, 'Test passed for ', outstring
	  endif
      endif
      return
      end
