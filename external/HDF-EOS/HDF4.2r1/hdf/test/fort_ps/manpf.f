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
C $Id: manpf.f,v 1.6 2001/10/02 21:31:33 epourmal Exp $
C
      subroutine manf (number_failed)
C
C
C  Test program: stores annotations in a file using fortran multi-file
C                annotation interface.
C                Writes several SDSs and corresponding RISs to a file.
C                Writes labels and descriptions for all 2 out of 3 SDS
C                Writes labels and descriptions for all RISs.
C
C  Input file:   none
C  Output files: manf.hdf
C
      implicit none
      include 'fortest.inc'

      integer number_failed
      character*20 myname
      parameter (myname = 'manf')

      integer afstart, afend, affcreate
      integer afwriteann, afendaccess, hopen, hclose
c	integer afcreate

C      integer dssdims, dsadata, dslref, dsgdims
C      integer d8aimg, DFR8lastref, d8gimg
C
      integer hishdf, hestringf, heprntf
      character*80 error_message
      integer fileh
      character*15 ERR_FILE
      parameter (ERR_FILE = 'Fortran_err.dat')

      integer numberfailed, ISFIRST, NOTFIRST, MAXLENLAB
      integer MAXLEN_DESC, ROWS, COLS, REPS
      integer DFTAG_SDG, DFTAG_RIG

      parameter ( ISFIRST =        1, 
     *            NOTFIRST =       0, 
     *            MAXLENLAB =    30,
     *            MAXLEN_DESC =  500, 
     *            DFTAG_SDG   =  700,
     *            DFTAG_RIG   =  306,
     *            ROWS =          10, 
     *            COLS =          10,
     *            REPS =           2 )

c      integer refnum
      integer ret
c      integer rank, j, dimsizes(2)
      integer fhandle, anhandle, ahandle
      integer  DFACC_CREATE, DFACC_READ
      integer AN_DATA_LABEL, AN_DATA_DESC, AN_FILE_LABEL, AN_FILE_DESC

      character*30 labsds, labsds2 
      character*30 labris, labris2
      character*500 descsds, descsds2
      character*500 descris, descris2
      character*35 lab1, lab2
      character*100 desc1, desc2
c      character pal(768)
      character*64 TESTFILE

      character*1 CR
c      character image(ROWS, COLS), newimage(ROWS, COLS)
c      real      data(ROWS, COLS)


      call ptestban('Testing', myname)
      number_failed = 0
      numberfailed =  0
      CR = char(10)
      DFACC_CREATE = 4
      DFACC_READ   = 1
      AN_DATA_LABEL = 0
      AN_DATA_DESC  = 1
      AN_FILE_LABEL = 2
      AN_FILE_DESC  = 3
      TESTFILE = 'manf.hdf'

C *** set up file labels and descriptions ***
      lab1 = 'File label #1: aaa'
      lab2 = 'File label #2: bbbbbb'
      desc1 = 'File descr #1: This is a test file annotation'
      desc2 = 'File descr #2: One more test ...'

C *** set up object labels and descriptions ***

      labsds  = 'Object label #1: sds'
      labsds2 = 'Object label #1: sds2'
      labris  = 'Object label #2: image'
      labris2 = 'Object label #2: image2'
      descsds = 'Object Descr #1: 1  2  3  4  5  6  7  8  9 10 11 12 ' 
     *          // CR // '                13 14 15 16 17 18 19 20 '
     *          // ' **END SDS DESCR**'
      descsds2 = 'Object Descr #1: 1  2  3  4  5  6  7  8  9 10 11 12 ' 
     *          // CR // '                13 14 15 16 17 18 19 20 '
     *          // ' **END SDS2 DESCR**'
      descris = 'Object Descr #2: A B C D E F G H I J K L '
     *          // CR // '                M N O **END IMAGE DESCR **'
      descris2 = 'Object Descr #2: A B C D E F G H I J K L '
     *          // CR // '                M N O **END IMAGE2 DESCR **'

C  *** generate float array and image ***
 
C      rank = 2
C      dimsizes(1)=ROWS 
C      dimsizes(2)=COLS

C      call gen2Dfloat(ROWS, COLS, data)
C      call genimage(ROWS, COLS, data, image)

C      ret = dssdims(rank,dimsizes)
C      call VERIFY(ret,'dssdims',number_failed)

C  *** start annotation on file ***
      fhandle = hopen(TESTFILE,DFACC_CREATE, 0)
      ret = fhandle
      call VERIFY(ret,'fhanlde',number_failed)
      ahandle = afstart(fhandle)
      ret = ahandle
      call VERIFY(ret,'afstart',number_failed)

C  *** write file 2 labels/ 2 descriptions ***
      anhandle = affcreate(ahandle, AN_FILE_LABEL)
      ret = anhandle
      call VERIFY(ret, 'affcreate', number_failed)
      ret = afwriteann(anhandle,lab2,len(lab2))
      call VERIFY(ret, 'afwriteann', number_failed)
      ret = afendaccess(anhandle)
      call VERIFY(ret, 'afendaccess', number_failed)

      anhandle = affcreate(ahandle, AN_FILE_LABEL)
      ret = anhandle
      call VERIFY(ret, 'affcreate', number_failed)
      ret = afwriteann(anhandle,lab1,len(lab1))
      call VERIFY(ret, 'afwriteann', number_failed)
      ret = afendaccess(anhandle)
      call VERIFY(ret, 'afendaccess', number_failed)

      anhandle = affcreate(ahandle, AN_FILE_DESC)
      ret = anhandle
      call VERIFY(ret, 'affcreate', number_failed)
      ret = afwriteann(anhandle,desc2,len(desc2))
      call VERIFY(ret, 'afwriteann', number_failed)
      ret = afendaccess(anhandle)
      call VERIFY(ret, 'afendaccess', number_failed)

      anhandle = affcreate(ahandle, AN_FILE_DESC)
      ret = anhandle
      call VERIFY(ret, 'affcreate', number_failed)
      ret = afwriteann(anhandle,desc1,len(desc1))
      call VERIFY(ret, 'afwriteann', number_failed)
      ret = afendaccess(anhandle)
      call VERIFY(ret, 'afendaccess', number_failed)

C  ***  Write data labels and descriptions ***

C      call MESSAGE(VERBO_HI,
C     +    '***  Writing labels & descriptions with SDS and RIS ***')

C      do 100 j=1,REPS
C  ***  write out scientific data set 
C         ret = dsadata(TESTFILE, rank,dimsizes, data)
C         call VERIFY(ret, 'dsadata', number_failed)

C ****    write out annotations for 2 out of every 3 
C         if (mod(j,3) .ne. 0) then 
C            refnum = dslref()

C ********** Write out 2 labels for each SDS *****************
C            anhandle = afcreate(ahandle,DFTAG_SDG,refnum,AN_DATA_LABEL)
C            ret = anhandle
C            call VERIFY(ret, 'afcreate', number_failed)
C            ret = afwriteann(anhandle,labsds2,len(labsds2))
C            call VERIFY(ret, 'afwriteann', number_failed)
C            ret = afendaccess(anhandle)
C            call VERIFY(ret, 'afendaccess', number_failed)

C            anhandle = afcreate(ahandle,DFTAG_SDG,refnum,AN_DATA_LABEL)
C            ret = anhandle
C            call VERIFY(ret, 'afcreate', number_failed)
C            ret = afwriteann(anhandle,labsds,len(labsds))
C            call VERIFY(ret, 'afwriteann', number_failed)
C            ret = afendaccess(anhandle)
C            call VERIFY(ret, 'afendaccess', number_failed)

C *********** Write out 2 descritptions for each SDS ***********
C            anhandle = afcreate(ahandle,DFTAG_SDG,refnum,AN_DATA_DESC)
C            ret = anhandle
C            call VERIFY(ret, 'afcreate', number_failed)
C            ret = afwriteann(anhandle,descsds2,len(descsds2))
C            call VERIFY(ret, 'afwriteann', number_failed)
C            ret = afendaccess(anhandle)
C            call VERIFY(ret, 'afendaccess', number_failed)

C            anhandle = afcreate(ahandle,DFTAG_SDG,refnum,AN_DATA_DESC)
C            ret = anhandle
C            call VERIFY(ret, 'afcreate', number_failed)
C            ret = afwriteann(anhandle,descsds,len(descsds))
C            call VERIFY(ret, 'afwriteann', number_failed)
C            ret = afendaccess(anhandle)
C            call VERIFY(ret, 'afendaccess', number_failed)

C         endif

C         ret = d8aimg(TESTFILE, image, COLS, ROWS, 0)
C         call VERIFY(ret, 'd8aimg', number_failed)
C         refnum = DFR8lastref()

C ********** Write out 2 labels for each Image *****************
C          anhandle = afcreate(ahandle, DFTAG_RIG, refnum, AN_DATA_LABEL)
C          ret = anhandle
C          call VERIFY(ret, 'afcreate', number_failed)
C          ret = afwriteann(anhandle,labris2,len(labris2))
C          call VERIFY(ret, 'afwriteann', number_failed)
C          ret = afendaccess(anhandle)
C          call VERIFY(ret, 'afendaccess', number_failed)

C          anhandle = afcreate(ahandle, DFTAG_RIG, refnum, AN_DATA_LABEL)
C          ret = anhandle
c          call VERIFY(ret, 'afcreate', number_failed)
c          ret = afwriteann(anhandle,labris,len(labris))
c          call VERIFY(ret, 'afwriteann', number_failed)
c          ret = afendaccess(anhandle)
c          call VERIFY(ret, 'afendaccess', number_failed)

C *********** Write out 2 descritptions for each Image ***********
c          anhandle = afcreate(ahandle, DFTAG_RIG, refnum, AN_DATA_DESC)
c          ret = anhandle
c          call VERIFY(ret, 'afcreate', number_failed)
c          ret = afwriteann(anhandle,descris2,len(descris2))
c          call VERIFY(ret, 'afwriteann', number_failed)
c          ret = afendaccess(anhandle)
c          call VERIFY(ret, 'afendaccess', number_failed)

c          anhandle = afcreate(ahandle, DFTAG_RIG, refnum, AN_DATA_DESC)
c          ret = anhandle
c          call VERIFY(ret, 'afcreate', number_failed)
c          ret = afwriteann(anhandle,descris,len(descris))
c          call VERIFY(ret, 'afwriteann', number_failed)
c          ret = afendaccess(anhandle)
c          call VERIFY(ret, 'afendaccess', number_failed)

c  100 continue

C ******* End writing annotatons **********
      ret = afend(ahandle)
      call VERIFY(ret, 'afend', number_failed)
      ret = hclose(fhandle)
      call VERIFY(ret, 'hclose', number_failed)


C********  Read data labels and descriptions *********
c      call MESSAGE(VERBO_HI,
c     +    '*** Reading labels and descriptions for SDS and RIS ***')

c      do 200 j=1,REPS
c          ret = dsgdims(TESTFILE, rank,dimsizes,3)
c          call VERIFY(ret, 'dsgdims', number_failed)
c          refnum = dslref()

C ******  read in annotations for 2 out of every 3 
c          if (mod(j,3) .ne. 0) then
c              call man_check_lab_desc(TESTFILE, DFTAG_SDG, refnum, 
c     *                            labsds, descsds, numberfailed)
c
c              call man_check_lab_desc(TESTFILE, DFTAG_SDG, refnum, 
c     *                            labsds2, descsds2, numberfailed)
c          endif

C ****    read annotations for images
c          ret = d8gimg(TESTFILE, newimage, COLS, ROWS, pal)
c          call VERIFY(ret, 'd8gimg', number_failed)
c          refnum = DFR8lastref()
c          call man_check_lab_desc(TESTFILE, DFTAG_RIG, refnum, 
c     *                        labris, descris, numberfailed)
c
c          call man_check_lab_desc(TESTFILE, DFTAG_RIG, refnum, 
c     *                        labris2, descris2, numberfailed)
      
c  200 continue

C ****** Check file labels/descriptions *******
      call MESSAGE(VERBO_HI,
     +    '*** Reading file labels and descriptions ***')

      call check_fan(TESTFILE, 0, lab1, desc1, numberfailed)
      call check_fan(TESTFILE, 1, lab2, desc2, numberfailed)

      if ( numberfailed .eq. 0 ) then
          call MESSAGE(VERBO_HI,
     +        '***** ALL ANxxx TESTS SUCCESSFUL ***** ')
      else
          print *,'***** ',numberfailed,' TESTS FAILED ***** '
      endif

C ***** Test if the file fname is an HDF file
C
C
      ret = hishdf(TESTFILE)
      if (ret .ne. 1) then
          numberfailed = numberfailed + 1
          write(*,*) 'HISHDF function failed'
      endif
      ret = hestringf(0, error_message)
       if (ret .ne. 0) then
          numberfailed = numberfailed + 1
          write(*,*) 'HESTRINGF function failed'
      endif

C
C     Call hishdf with  file not being an hdf file. Call should return
C     0 
C
      ret = hishdf('manf.f')
      if (ret .ne. 0) then
          numberfailed = numberfailed + 1
          write(*,*) 'HISHDF function failed'
      endif
C
C *****  end of hishdf test
C
      fileh = hopen('nonexist', DFACC_READ,0)
      ret = heprntf(ERR_FILE, 0)
      ret = hclose(fileh)
      ret = heprntf(ERR_FILE, 0)
      return
      end


C**************************************************************
C
C  man_check_lab_desc:  read and compare label and description
C                   with expected ones
C
C**************************************************************
      subroutine man_check_lab_desc(fname, tag, ref, label, desc, 
     *                          num_failed)
      implicit none
      include 'fortest.inc'

      character*(*) fname, label, desc
      integer tag, ref, num_failed

      integer MAXLENLAB, MAXLEN_DESC
      parameter ( MAXLENLAB =    30,
     *            MAXLEN_DESC =  500 )


      integer  inlablen, indesclen, ret

      integer affileinfo, afnumann, afannlist, afannlen
      integer afreadann, afstart, afend, afendaccess, hopen, hclose

      integer fileh, anh
      integer nflabs, nfdescs, nolabs, nodescs
      integer numdlabels, numddescs
      integer annlen, j, found, fannlen
      integer dlabels(2), ddescs(2)
      character*30   inlabel, fannlabel
      character*500 indesc, fanndesc
      integer AN_DATA_LABEL, AN_DATA_DESC, AN_FILE_LABEL, AN_FILE_DESC
      integer DFACC_READ
      DFACC_READ = 1
      AN_DATA_LABEL = 0
      AN_DATA_DESC  = 1
      AN_FILE_LABEL = 2
      AN_FILE_DESC  = 3

C *****start annotation access on file *****
      fileh = hopen(fname, DFACC_READ,0)
      ret = fileh
      call VERIFY(ret, 'hopen', num_failed)
      anh = afstart(fileh)
      ret = anh
      call VERIFY(ret, 'afstart', num_failed)

      ret = affileinfo(anh,nflabs,nfdescs,nolabs,nodescs)
      call VERIFY(ret, 'affileinfo', num_failed)

      numdlabels = afnumann(anh, AN_DATA_LABEL, tag, ref)
      call VERIFY(numdlabels, 'afnumann', num_failed)

      numddescs = afnumann(anh, AN_DATA_DESC, tag, ref)
      call VERIFY(numddescs, 'afnumann', num_failed)

      ret = afannlist(anh, AN_DATA_LABEL, tag, ref, dlabels)
      call VERIFY(ret, 'afannlist', num_failed)

      ret = afannlist(anh, AN_DATA_DESC, tag, ref, ddescs)
      call VERIFY(ret, 'afannlist', num_failed)

C ***** Look for label in list ******
      found = 0
      fannlen = 0
      fannlabel = ' '
      do 300 j=1, numdlabels
         annlen = afannlen(dlabels(j))
         call VERIFY(annlen, 'afannlen', num_failed)

         ret = afreadann(dlabels(j), inlabel, MAXLENLAB)
         call VERIFY(ret, 'afreadann', num_failed)
         ret = afendaccess(dlabels(j))
         call VERIFY(ret, 'afendaccess', num_failed)

         if (inlabel .eq. label) then
            found = 1
            inlablen = annlen
            fannlabel = inlabel
         endif

  300 continue

C ****** Check if we found label in list *****
      if (inlablen .ne. len(label)) then
         print *,'   >>>BAD LABEL LENGTH.'
         print *,'                        IS: ', inlablen
         print *,'                 SHOULD BE: ', len(label)
         num_failed = num_failed + 1
      endif

      if (fannlabel .ne. label) then
         print *,'   >>>BAD LABEL.'
         print *,'                        IS: ', fannlabel
         print *,'                 SHOULD BE: ', label
         num_failed = num_failed + 1
      endif

C ***** look for description in list 
      found = 0
      fannlen = 0
      fanndesc = ' '
      do 400 j=1, numddescs
         annlen = afannlen(ddescs(j))
         call VERIFY(annlen, 'afannlen', num_failed)

         ret = afreadann(ddescs(j), indesc, MAXLEN_DESC)
         call VERIFY(ret, 'afreadann', num_failed)
         ret = afendaccess(ddescs(j))
         call VERIFY(ret, 'afendaccess', num_failed)

         if (indesc .eq. desc) then
            found = 1
            indesclen = annlen
            fanndesc = indesc
         endif

  400 continue

      if (indesclen .ne. len(desc)) then
          print *,'   >>>BAD DESCRIPTION LENGTH.' 
          print *,'                        IS: ', indesclen 
          print *,'                 SHOULD BE: ', len(desc) 
          num_failed = num_failed + 1 
       endif

       if (fanndesc .ne. desc) then
          print *,'   >>>BAD DESCRIPTION.' 
          print *,'                        IS: ', fanndesc 
          print *,'                 SHOULD BE: ', desc 
          num_failed = num_failed + 1 
       endif

C ****** close file *******
      ret = afend(anh)
      call VERIFY(ret, 'afend', num_failed)
      ret = hclose(fileh)
      call VERIFY(ret, 'hclose', num_failed)


      return
      end


C************************************************************
C
C     SUBROUTINE check_fan
C
C************************************************************
      subroutine check_fan(fname, index, label, desc, num_failed)
      implicit none
      include 'fortest.inc'

      character*(*) fname, label, desc
      integer index, num_failed

      integer MAXLENFLAB, MAXLEN_FDESC
      parameter ( MAXLENFLAB =    35,
     *            MAXLEN_FDESC =  100 )

      integer affileinfo, afselect, afannlen, afreadann
      integer afstart, afend, afendaccess, hopen, hclose

      integer ret
      integer fileh, annh, anh
      integer nflabs, nfdescs, nolabs, nodescs
      integer fannlen
      character*35 flabel
      character*100 fdesc
      integer AN_DATA_LABEL, AN_DATA_DESC, AN_FILE_LABEL, AN_FILE_DESC
      integer DFACC_READ
      DFACC_READ = 1
      AN_DATA_LABEL = 0
      AN_DATA_DESC  = 1
      AN_FILE_LABEL = 2
      AN_FILE_DESC  = 3

C **** We check both file label/description
      fileh = hopen(fname, DFACC_READ,0)
      ret = fileh
      call VERIFY(ret, 'hopen', num_failed)
      anh = afstart(fileh)
      ret = anh
      call VERIFY(ret, 'afstart', num_failed)

      ret = affileinfo(anh,nflabs,nfdescs,nolabs,nodescs)
      call VERIFY(ret, 'affileinfo', num_failed)

C ***** Read file label **********
      annh = afselect(anh, index, AN_FILE_LABEL)
      call VERIFY(ret, 'afselect', num_failed)

      fannlen = afannlen(annh)
      call VERIFY(fannlen, 'afannlen', num_failed)

      ret = afreadann(annh, flabel, fannlen)
      call VERIFY(ret, 'afreadann', num_failed)
      ret = afendaccess(annh)
      call VERIFY(ret, 'afendaccess', num_failed)

      if (fannlen .ne. len(label)) then
         print *,'   >>>BAD LABEL LENGTH.'
         print *,'                        IS: ', fannlen
         print *,'                 SHOULD BE: ', len(label)
         num_failed = num_failed + 1
      endif

      if (flabel .ne. label) then
         print *,'   >>>BAD LABEL.'
         print *,'                        IS: ', flabel
         print *,'                 SHOULD BE: ', label
         num_failed = num_failed + 1
      endif

C **** Read file description *****
      annh = afselect(anh, index, AN_FILE_DESC)
      call VERIFY(ret, 'afselect', num_failed)

      fannlen = afannlen(annh)
      call VERIFY(fannlen, 'afannlen', num_failed)

      ret = afreadann(annh, fdesc, fannlen)
      call VERIFY(ret, 'afreadann', num_failed)
      ret = afendaccess(annh)
      call VERIFY(ret, 'afendaccess', num_failed)

      if (fannlen .ne. len(desc)) then
          print *,'   >>>BAD DESCRIPTION LENGTH.' 
          print *,'                        IS: ', fannlen 
          print *,'                 SHOULD BE: ', len(desc) 
          num_failed = num_failed + 1 
       endif

       if (fdesc .ne. desc) then
          print *,'   >>>BAD DESCRIPTION.' 
          print *,'                        IS: ', fdesc 
          print *,'                 SHOULD BE: ', desc 
          num_failed = num_failed + 1 
       endif

C ****** close file *******
      ret = afend(anh)
      call VERIFY(ret, 'afend', num_failed)
      ret = hclose(fileh)
      call VERIFY(ret, 'hclose', num_failed)

      return
      end

