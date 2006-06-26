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
C $Id: tr8f.f,v 1.12 1997/10/24 17:28:04 acheng Exp $
C
      subroutine tr8f (number_failed)
      implicit none
      include 'fortest.inc'
C
C Test program:
C     		Writes images together with pals to a file.
C		Reads images from the file.
C		Writes and reads images with speicfied ref's.
C Input file: none
C Output file: tdfr8f.hdf
C
      integer number_failed
      character*20 myname
      parameter (myname = 'r8')

      integer d8spal, d8pimg, d8aimg, d8gdims, d8nims
      integer d8gimg, d8rref, d8wref, d8first, d8lref
      integer DFTAG_RLE, DFTAG_IMCOMP

      character im1(100,100), im2(321, 111)
      character ii1(100, 100), ii2(321, 111)
      character pal1(768), pal2(768), ipal(768)
      character*64 TESTFILE
      character*1 CR

      integer x, y, ret, num_images
      integer d1, d2, ispal
      integer ref1, ref2, ref3

      call ptestban('Testing', myname)
      DFTAG_RLE = 11
      DFTAG_IMCOMP = 12
      TESTFILE = 'tdfr8f.hdf' 
      number_failed = 0
      num_images = 0
      CR = char(10)  

      do 120 x=1, 100
          do 100 y=1, 100
              im1(y,x) = char(x+y)
100       continue
120   continue

      do 180 x=1, 111
          do 150 y=1, 321
              im2(y,x) = char(y-x)
150       continue
180   continue

      do 200 x=1, 256
          pal1(3*x - 2) = char(x)
          pal1(3*x - 1) = char(x)
          pal1(3*x ) = char(x)
          pal2(x) = char(x)
          pal2(x+256) = char(x)
          pal2(x+512) = char(x)
200   continue

C Start here

      call MESSAGE(VERBO_HI, 'Setting palette 1')
      ret = d8spal(pal1)
      call VRFY(ret, 'd8spal',number_failed)
      call MESSAGE(VERBO_HI,
     +		'Putting image 1 with pal 1, no compression')
      ret=d8pimg(TESTFILE, im1, 100, 100, 0)
      call VRFY(ret, 'd8pimg',number_failed)
      num_images = num_images + 1
      call MESSAGE(VERBO_HI, 'Getting ref1')
      ref1 = d8lref()
      
      call MESSAGE(VERBO_HI,
     +		'Putting image 2 with pal 1, REL compression')
      ret=d8aimg(TESTFILE, im2, 321, 111, DFTAG_RLE)
      call VRFY(ret, 'd8aimg',number_failed)
      num_images = num_images + 1
      call MESSAGE(VERBO_HI, 'Getting ref2')
      ref2 = d8lref()
      
      call MESSAGE(VERBO_HI, 'Setting palette 2')
      ret = d8spal(pal2)
      call VRFY(ret, 'd8spal',number_failed)
      call MESSAGE(VERBO_HI,
     +		'Putting image 2 with pal 2, IMCOMP  compression')
      ret=d8aimg(TESTFILE, im2, 321, 111, DFTAG_IMCOMP)
      call VRFY(ret, 'd8aimg',number_failed)
      num_images = num_images + 1
      call MESSAGE(VERBO_HI, 'Getting ref3')
      ref3 = d8lref()
      
      call MESSAGE(VERBO_HI, 'Getting number of images')
      ret = d8nims(TESTFILE)
      if (ret .ne. num_images) then 
          print *, '    >>>> WRONG NUMBER OF IMAGES  <<<   '
      else 
	  if (Verbosity .ge. VERBO_HI) then
	      print *, ret, ' images in the file'
	  endif
      endif
      call MESSAGE(VERBO_HI, 'Restarting file')
      ret = d8first()
      call VRFY(ret, 'd8first',number_failed)
      call MESSAGE(VERBO_HI, 'Getting dimensions of first image')
      ret=d8gdims(TESTFILE, d1, d2, ispal)
      call VRFY(ret, 'd8gdims',number_failed)
      call MESSAGE(VERBO_HI, 'Getting image 1')
      ret=d8gimg(TESTFILE, ii1, 100, 100, ipal)
      call VRFY(ret, 'd8gimg',number_failed)
      call check_im1_pal(100, 100, d1, d2, im1, ii1, pal1, ipal)
      call MESSAGE(VERBO_HI, 'Getting dimensions of image2')
      ret=d8gdims(TESTFILE, d1, d2, ispal)
      call VRFY(ret, 'd8gdims',number_failed)
      call MESSAGE(VERBO_HI, 'Getting dimensions of image 3')
      ret=d8gdims(TESTFILE, d1, d2, ispal)
      call VRFY(ret, ' d8gdims',number_failed)
      call MESSAGE(VERBO_HI, 'Getting image 3')
      ret = d8gimg(TESTFILE, ii2, 321, 111, ipal)
      call VRFY(ret, 'd8gimg',number_failed)

      call MESSAGE(VERBO_HI, 'setting read ref2')
      ret = d8rref(TESTFILE, ref2)
      call VRFY(ret, 'd8rref',number_failed)

      call MESSAGE(VERBO_HI, 'Getting image 2')
      ret = d8gimg(TESTFILE, ii2, 321, 111,ipal)
      call VRFY(ret, 'd8gimg',number_failed)
      call check_im2_pal(321,111,321, 111, im2, ii2, pal1, ipal)
      call MESSAGE(VERBO_HI,'Setting write ref1')
      ret = d8wref(TESTFILE, ref1)
      call VRFY(ret, 'd8wref',number_failed)
      call MESSAGE(VERBO_HI, 'Setting palette 2')
      ret = d8spal(pal2)
      call VRFY(ret, 'd8spal',number_failed)
      call MESSAGE(VERBO_HI,'Putting image 1 with pal 2, RLE')
      ret = d8aimg(TESTFILE, im1, 100, 100, DFTAG_RLE)
      call VRFY(ret, 'd8aimg',number_failed)
      call MESSAGE(VERBO_HI, 'Setting read ref1')
      ret = d8rref(TESTFILE, ref1)
      call VRFY(ret, 'd8rref',number_failed)
      call MESSAGE(VERBO_HI, 'Getting dimensions of first image')
      ret = d8gdims(TESTFILE, d1, d2, ispal)
      call VRFY(ret, 'd8gdims',number_failed)
      call MESSAGE(VERBO_HI, 'Getting image 1')
      ret = d8gimg(TESTFILE, ii1, d1, d2, ipal)
      call VRFY(ret, 'd8gimg',number_failed)
      call check_im1_pal(100, 100, d1, d2, im1, ii1, pal2, ipal)

      if (number_failed .eq. 0) then
          call MESSAGE(VERBO_HI, '****** ALL TESTS SUCCESSFUL *******')
      else 
          print *, '****' , number_failed, ' TESTS FAILED ****'
      endif

      return
      end

C********************************************************
C
C check_im1_pal
C 
C********************************************************

      subroutine check_im1_pal(od1,od2,nd1,nd2,oim,nim,opal,npal)
      implicit none
      include 'fortest.inc'

      integer od1, od2, nd1, nd2
      character oim(100, 100), nim(100, 100)
      character opal(768), npal(768)


      integer prob, i, j
      
      prob = 0 
      call MESSAGE(VERBO_HI, 'Checking image and palette')

      if (od1 .ne. nd1 .OR. od2 .ne. nd2) then
          print *, '    >>> DIMENSIONS WRONG <<<    '
          prob = 1
      endif
      do 520 j=1, 100
          do 500 i=1,100
              if (oim(i,j) .ne. nim(i,j)) then
	          print *, '     ERROR at ', i, j,' old image: ',
     *                    oim(i,j), ' new image: ', nim(i,j)
                  prob = 1
              endif
500       continue
520   continue
       
      if (prob .eq. 0) then
          call MESSAGE(VERBO_HI, 'Image is correct')
      endif

      prob = 0
      do 550 i=1,768
          if (opal(i) .ne. npal(i)) then
              print *, '    ERROR at ',i, 'old pal: ', opal(i),
     *                  ' new pal: ', npal(i)
          prob = 1
      endif
550   continue
      if (prob .eq. 0) then
          call MESSAGE(VERBO_HI, 'Palette is correct')
      endif
      return
      end

    
C********************************************************
C
C check_im2_pal
C 
C********************************************************

      subroutine check_im2_pal(od1,od2,nd1,nd2,oim,nim,opal,npal)
      implicit none
      include 'fortest.inc'

      integer od1, od2, nd1, nd2
      character oim(321, 111), nim(321, 111)
      character opal(768), npal(768)


      integer prob, i, j
      
      prob = 0
      call MESSAGE(VERBO_HI, 'Checking image and palette')

      if (od1 .ne. nd1 .OR. od2 .ne. nd2) then
          print *, '    >>> DIMENSIONS WRONG <<<    '
          prob = 1
      endif
      do 520 j=1, 111
          do 500 i=1,321
              if (oim(i,j) .ne. nim(i,j)) then
	          print *, '     ERROR at ', i, j,' old image: ',
     *                    oim(i,j), ' new image: ', nim(i,j)
                  prob = 1
              endif
500       continue
520   continue
       
      if (prob .eq. 0) then
          call MESSAGE(VERBO_HI, 'Image is correct')
      endif

      prob = 0
      do 550 i=1,768
          if (opal(i) .ne. npal(i)) then
              print *, '    ERROR at ',i, 'old pal: ', opal(i),
     *                  ' new pal: ', npal(i)
          prob = 1
      endif
550   continue
      if (prob .eq. 0) then
          call MESSAGE(VERBO_HI, 'Palette is correct')
      endif
      return
      end

 
      
      







