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
C $Id: tanfilef.f,v 1.17 1997/10/24 17:28:02 acheng Exp $
C
      subroutine tanfilef (number_failed)
C
C
C  Test program:
C                Writes file labels and descriptions in a file.
C                Reads the  labels and descriptions from the file
C
C  Input file:  none
C  Output files: tdfanflF.hdf
C
C  Possible bug:  When reading in a label, we have to give it a
C                 length that is one greater than MAXLEN_LAB. This
C                 may be due to a bug in dfan.c in DFANIgetann().
C

      implicit none
      include 'fortest.inc'

      integer number_failed
      character*20 myname
      parameter (myname = 'anfile')

      integer daafid, daafds,dagfidl,dagfid
      integer dagfdsl, dagfds, hopen, hclose
      integer ret
      integer ISFIRST, NOFIRST, MAXLEN_LAB, MAXLEN_DESC
      integer fid, DFACC_CREATE, DFACC_READ

      character*35 lab1, lab2
      character*35 templab
      character*100 desc1, desc2, tempstr
      character*64 TESTFILE
      character*1 CR

      call ptestban('Testing', myname)
      ISFIRST = 1
      NOFIRST = 0
      number_failed = 0
      TESTFILE = 'tdfanflF.hdf'
      CR = char(10)
      MAXLEN_LAB = 35
      MAXLEN_DESC = 100
      DFACC_CREATE = 4
      DFACC_READ = 1

      lab1 = 'File label #1: aaa'
      lab2 = 'File label #2: bbbbbb'
      desc1 = 'File descr #1: This is a test file annotation'
      desc2 = 'File descr #2: One more test ...'

      call MESSAGE(VERBO_HI, '****** Write file labels *******')
      fid = hopen(TESTFILE, DFACC_CREATE, 0)
      call VRFY(fid, 'hopen', number_failed)
      ret = daafid(fid, lab1)
      call VRFY(ret, 'daafid', number_failed)

      ret = daafid(fid, lab2)
      call VRFY(ret, 'daafid', number_failed)

      call MESSAGE(VERBO_HI, '****** Write file descriptions *******')
      ret = daafds(fid, desc1, len(desc1))
      call VRFY(ret, 'daafds', number_failed)

      ret = daafds(fid, desc2, len(desc2))
      call VRFY(ret, 'daafds', number_failed)

      ret = hclose(fid)
      call VRFY(ret, 'hclose', number_failed)

      call MESSAGE(VERBO_HI,
     +    '****** Read length of the first file label ****')
      fid = hopen(TESTFILE, DFACC_READ, 0)
      call VRFY(fid, 'hopen-read', number_failed)
      ret = dagfidl(fid, ISFIRST)
      call VRFY(ret, 'dagfidl', number_failed)
      call checklen(ret, lab1,  'label'  )

      call MESSAGE(VERBO_HI, '******...followed by the label *****')
      ret = dagfid(fid, templab, MAXLEN_LAB, ISFIRST)

      call VRFY(ret, 'dagfid', number_failed)
      call checklab(lab1, templab, ret, 'label')

      call MESSAGE(VERBO_HI,
     +    '****** Read length of the second file label ****')
      ret = dagfidl(fid, NOFIRST)
      call VRFY(ret, 'dagfidl', number_failed)
      call checklen(ret, lab2, 'label')

      call MESSAGE(VERBO_HI, '******...followed by the label *****')
      ret = dagfid(fid, templab, MAXLEN_LAB, NOFIRST)
      call VRFY(ret, 'dagfid', number_failed)
      call checklab(lab2, templab, ret, 'label')

      call MESSAGE(VERBO_HI,
     +    '****** Read length of the first file description ****')
      ret = dagfdsl(fid, ISFIRST)
      call VRFY(ret, 'dagfdsl', number_failed)
      call checklen(ret, desc1, 'description' )

      call MESSAGE(VERBO_HI,
     +    '******...followed by the description *****')
      ret = dagfds(fid, tempstr, MAXLEN_DESC, ISFIRST)
      call VRFY(ret, 'dagfds', number_failed)
      call checkann(desc1, tempstr, ret, 'description')

      call MESSAGE(VERBO_HI,
     +    '****** Read length of the second file description ****')
      ret = dagfdsl(fid, NOFIRST)
      call VRFY(ret, 'dagfdsl', number_failed)
      call checklen(ret, desc2, 'description' )

      call MESSAGE(VERBO_HI,
     +    '******...followed by the description *****')
      ret = dagfds(fid, tempstr, MAXLEN_DESC, NOFIRST)
      call VRFY(ret, 'dagfds', number_failed)
      call checkann(desc2, tempstr, ret, 'description')

      ret = hclose(fid)
      call VRFY(ret, 'hclose', number_failed)

      if (number_failed .eq. 0) then
         call MESSAGE(VERBO_HI,
     +    '***** ALL DFANFILE TESTS SUCCESSFUL ******')
      else
         print *, '********', number_failed, ' TESTS FAILED'
      endif


      return
      end


C*********************************************
C
C  checklen
C
C*********************************************

      subroutine checklen(ret, oldstr, type)
      implicit none
      character*(*) type, oldstr
      integer ret

      integer oldlen

      oldlen = len(oldstr)
      if (ret .ge. 0 .and.  ret .ne. oldlen) then
          print *, 'Length of ', type, ' is ', len(oldstr),
     *             ' instead of ', ret
      endif
      return
      end

C***********************************************
C
C  checkann
C
C***********************************************

      subroutine checkann(oldstr, newstr, ret, type)
      implicit none
      character*90  oldstr, newstr
      character*(*) type
      integer ret


      if (ret .ge. 0 .and. oldstr .ne. newstr) then
          print *, type, ' is incorrect.'
          print *, ' It should be <', oldstr, '>'
          print *, ' instead of   <', newstr, '>'
      endif
      return
      end

C***********************************************
C
C  checklab
C
C***********************************************

      subroutine checklab(oldstr, newstr, ret, type)
      implicit none
      character*30  oldstr, newstr
      character*(*) type
      integer ret


      if (ret .ge. 0 .and. oldstr .ne. newstr) then
          print *, type, ' is incorrect.'
          print *, ' It should be <', oldstr, '>'
          print *, ' instead of   <', newstr, '>'
      endif
      return
      end
