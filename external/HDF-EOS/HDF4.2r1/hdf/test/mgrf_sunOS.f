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
C $Id: mgrf_sunOS.f,v 1.1 1999/05/05 14:45:32 epourmal Exp $
C
       subroutine mgrf (num_err)
C
C Test Program: 
C   Tests the multi-file GR interface.
C Input file: none
C Output file: tmgrf.hdf
C
C
      implicit none
      include 'fortest.inc'

      integer num_err
C
C  ---chunking and compression errors ----------
      integer err_grchunk, err_grwrchunk, err_grcompress
C  ---------------------------------------------
C
      character*20 myname
      parameter (myname = 'mgrf')

      integer hopen, hclose
      integer mgstart, mgfinfo, mgend, mgcreat, mgselct 
      integer mgn2ndx, mggiinf, mgwrimg, mgrdimg, mgendac
      integer mgid2rf, mgr2idx, mgrltil, mgrimil, mggltid
c     integer mgwrlut, mgrdlut
      integer mgglinf, mgwclut, mgrclut
c      integer mgsattr
	integer mgatinf, mggattr, mgfndat
      integer mgscatt, mgsnatt, mggcatt, mggnatt
      integer mgwcimg, mgrcimg
      integer MFGR_INTERLACE_PIXEL, MFGR_INTERLACE_LINE,
     *      MFGR_INTERLACE_COMPONENT

      parameter(MFGR_INTERLACE_PIXEL	= 0,
     *		MFGR_INTERLACE_LINE	= 1,
     * 		MFGR_INTERLACE_COMPONENT	= 2)

      integer 	DFACC_READ, DFACC_WRITE, DFACC_CREATE, DFACC_ALL  
      integer	DFACC_RDONLY, DFACC_RDWR, DFACC_CLOBBER

      parameter(DFACC_READ       	 = 1,
     *		DFACC_WRITE      	 = 2,
     *		DFACC_CREATE     	 = 4,
     *		DFACC_ALL        	 = 7,
     *		DFACC_RDONLY		 = 1,
     *		DFACC_RDWR		 = 3,
     *		DFACC_CLOBBER		 = 4)

      integer 	DFNT_INT8,  DFNT_UINT8, 
     *		DFNT_INT16, DFNT_UINT16,
     *		DFNT_INT32, DFNT_UINT32, 
     *          DFNT_INT64, DFNT_UINT64,
     *          DFNT_INT128,DFNT_UINT128,
     *          DFNT_CHAR8, DFNT_UCHAR8
 
      parameter(DFNT_INT8	= 20,
     *		DFNT_UINT8	= 21,
     *		DFNT_INT16	= 22,
     *		DFNT_UINT16	= 23,
     *		DFNT_INT32	= 24,
     *		DFNT_UINT32	= 25,
     *		DFNT_INT64	= 26,
     *		DFNT_UINT64	= 27,
     *		DFNT_INT128	= 28,
     *		DFNT_UINT128	= 29,
     *          DFNT_CHAR8      =  4,
     *          DFNT_UCHAR8     =  3)
      integer il
      character*80 TESTFILE
      character*80 IMAGE1, IMAGE2, IMAGEC, IMAGEC_2
      character*80 ATNAME1, ATNAME_N, ATNAME_C
      character*80 ATNAME2, ATNAME2_N, ATNAME2_C
      character*1 CR
      character buf(3, 2, 2), buf1(2, 3, 2), buf2(2, 2, 3)
      character in(3,2,2), in1(2, 3, 2), in2(2, 2, 3)
      integer   outbuf(4), outbuf1(4), outbuf2(4)
      integer   inbuf(4), inbuf1(4), inbuf2(4)
      equivalence (outbuf, buf), (outbuf1, buf1), (outbuf2,buf2)
      equivalence (inbuf, in), (inbuf1, in1), (inbuf2,in2)
      character pal(3,256), in_pal(3,256), in_pal2(256,3)
      integer   file_id, gr_id, ri_id, pal_id, index, index2
      integer   n_datasets, n_attrs, ref
      integer   n_comp, nt
      integer   dims(2), start(2), stride(2)
      integer*4 attr(5), attr2(5), attr2_n(5)
      character attr_c(6), attr2_c(6)
      character cbuf(2,3,4), icbuf(2,3,4)
      integer i, j, k, ret, number_failed

      DATA attr_c/'A','T','T','R','_','C'/
      DATA cbuf/'A','B','C','D','E','F','G','H','I','J','K','L',
     +          'M','N','O','P','Q','R','S','T','U','V','W','X'/

      call ptestban('Testing', myname)
      num_err = 0
      TESTFILE = 'tmgrf.hdf'
      IMAGE1 = 'Image #1'
      IMAGEC = 'Image_c #1'
      ATNAME1 = 'Attr. #1'
      ATNAME_N = 'Numeric Attr. #1'
      ATNAME_C = 'Character Attr. #1'
      CR = char(10)
      number_failed = 0

C Initialize the arrays
C Initialize the image arrays
      do 150 i=1, 2
          do 2 j=1, 2
              buf(1, j, i) = char(i+j)
              buf(2, j, i) = char(i+j)
              buf(3, j, i) = char(i+j)
              buf1(j, 1, i) = char(i-j)
              buf1(j, 2, i) = char(i-j)
              buf1(j, 3, i) = char(i-j)
              buf2(j, i, 1) = char(2*i - j)
              buf2(j, i, 2) = char(2*i - j)
              buf2(j, i, 3) = char(2*i - j)
2       continue
150   continue
      do 157 i=1,2
          do 156 j=1,3
             do 155 k=1,4
                 icbuf(k, j, i) = ' '
155          continue
156       continue
157   continue

C Initialize the palette array
      do 160 i=1, 256
          do 3 j=1, 3
              pal(j,i)=char(i+j)
3       continue
160   continue
C Initialize the attribute
      do 170 i=1, 5
          attr(i)=i*21
170   continue

C Open the file
      file_id=hopen(TESTFILE, DFACC_ALL, 0)
      call VRFY(file_id,'hopen',number_failed)
      gr_id=mgstart(file_id)
      call VRFY(gr_id,'mgstart',number_failed)

C Create an image
      call MESSAGE(5,'Creating an image')
      dims(1)=2
      dims(2)=2
      ri_id = mgcreat(gr_id,IMAGE1,3,DFNT_UINT8,MFGR_INTERLACE_PIXEL,
     *          dims)
      call VRFY(ri_id,'mgcreat',number_failed)

      start(1)=0
      start(2)=0
      stride(1)=1
      stride(2)=1
      call MESSAGE(5,'Writing image data')
      ret = mgwrimg(ri_id,start,stride,dims,outbuf)
      call VRFY(ret,'mgwrimg',number_failed)

C Store a palette with the image
      call MESSAGE(5,'Writing palette data')
      pal_id = mggltid(ri_id, 0)
      call VRFY(pal_id,'mggltid',number_failed)
      ret = mgwclut(pal_id,3,DFNT_UINT8,MFGR_INTERLACE_PIXEL,256,pal)
      call VRFY(ret,'mgwclut',number_failed)

C Store an attribute with the image
      call MESSAGE(5,'Writing numeric attribute data')
      ret = mgsnatt(ri_id,ATNAME1,DFNT_INT32,5,attr)
      call VRFY(ret,'mgsnatt',number_failed)

C Store a numeric attribute with the image
      call MESSAGE(5,'Writing numeric attribute data')
      ret = mgsnatt(ri_id,ATNAME_N,DFNT_INT32,5,attr)
      call VRFY(ret,'mgsnatt',number_failed)

C Store a character attribute with the image
      call MESSAGE(5,'Writing numeric attribute data')
      ret = mgscatt(ri_id,ATNAME_C,DFNT_CHAR8,6,attr_c)
      call VRFY(ret,'mgscatt',number_failed)

C End access to the image
      ret = mgendac(ri_id)
      call VRFY(ret,'mgendac',number_failed)

C Create a character type image
      call MESSAGE(5,'Creating a character type image')
      dims(1)=2
      dims(2)=2
      ri_id = mgcreat(gr_id,IMAGEC,3,DFNT_CHAR8,MFGR_INTERLACE_PIXEL,
     *          dims)
      call VRFY(ri_id,'mgcreat',number_failed)

      start(1)=0
      start(2)=0
      stride(1)=1
      stride(2)=1
      call MESSAGE(5,'Writing character image data')
      ret = mgwcimg(ri_id,start,stride,dims,cbuf)
      call VRFY(ret,'mgwcimg',number_failed)
      ret = mgendac(ri_id)
      call VRFY(ret,'mgendac',number_failed)

C End access to the GR interface
      ret = mgend(gr_id)
      call VRFY(ret,'mgend',number_failed)

C Close the file
      ret = hclose(file_id)
      call VRFY(ret,'hclose',number_failed)

C Re-open the file
      file_id=hopen(TESTFILE, DFACC_ALL, 0)
      call VRFY(file_id,'hopen',number_failed)
      gr_id=mgstart(file_id)
      call VRFY(gr_id,'mgstart',number_failed)

C Get info about the file
      call MESSAGE(5,'Getting GR file information')
      ret = mgfinfo(gr_id,n_datasets,n_attrs)
      call VRFY(ret,'mgfinfo',number_failed)

C Select an image
      call MESSAGE(5,'Selecting an image')
      index = mgn2ndx(gr_id, IMAGE1)
      call VRFY(index,'mgn2ndx',number_failed)
      ri_id = mgselct(gr_id,index)
      call VRFY(ri_id,'mgselct',number_failed)

C Get info about the image
      call MESSAGE(5,'Getting image information')
      ret = mggiinf(ri_id,IMAGE2,n_comp,nt,il,dims,n_attrs)
      call VRFY(ret,'mggiinf',number_failed)
      ref = mgid2rf(ri_id)
      call VRFY(ref,'mgid2rf',number_failed)
      index2 = mgr2idx(gr_id,ref)
      call VRFY(index2,'mgr2idx',number_failed)

C Check image reading
      start(1)=0
      start(2)=0
      stride(1)=1
      stride(2)=1
      call MESSAGE(5,'Reading image data')
      ret = mgrdimg(ri_id,start,stride,dims,inbuf)
      call VRFY(ret,'mgrdimg',number_failed)
      ret = mgrimil(ri_id,MFGR_INTERLACE_LINE)
      call VRFY(ret,'mgrimil',number_failed)
      ret = mgrdimg(ri_id,start,stride,dims,inbuf1)
      call VRFY(ret,'mgrdimg',number_failed)
      ret = mgrimil(ri_id,MFGR_INTERLACE_COMPONENT)
      call VRFY(ret,'mgrimil',number_failed)
      ret = mgrdimg(ri_id,start,stride,dims,inbuf2)
      call VRFY(ret,'mgrdimg',number_failed)

C Check palette reading
      pal_id = mggltid(ri_id, 0)
      call VRFY(pal_id,'mggltid',number_failed)
      call MESSAGE(5,'Reading palette data')
      ret = mgglinf(pal_id,n_comp,nt,il,i)
      call VRFY(ret,'mgglinf',number_failed)
      ret = mgrclut(pal_id,in_pal)
      call VRFY(ret,'mgrclut',number_failed)
      ret = mgrltil(pal_id,MFGR_INTERLACE_COMPONENT)
      call VRFY(ret,'mgrltil',number_failed)
      ret = mgrclut(pal_id,in_pal2)
      call VRFY(ret,'mgrclut',number_failed)

C Check attribute reading
      index = mgfndat(ri_id,ATNAME1)
      call VRFY(index,'mgfndat',number_failed)
      call MESSAGE(5,'Reading attribute data')
      ret = mgatinf(ri_id,index,ATNAME2,nt,i)
      call VRFY(ret,'mgatinf',number_failed)
      ret = mggattr(ri_id,index,attr2)
      call VRFY(ret,'mggattr',number_failed)

C Check numeric attr
      index = mgfndat(ri_id, ATNAME_N)
      call VRFY(index,'mgfndat',number_failed)
      call MESSAGE(5,'Reading attribute data')
      ret = mgatinf(ri_id,index,ATNAME2_N,nt,i)
      call VRFY(ret,'mgatinf',number_failed)
      ret = mggnatt(ri_id,index,attr2_n)
      call VRFY(ret,'mggnatt',number_failed)

C Check character attr
      index = mgfndat(ri_id, ATNAME_C)
      call VRFY(index,'mgfndat',number_failed)
      call MESSAGE(5,'Reading attribute data')
      ret = mgatinf(ri_id,index,ATNAME2_C,nt,i)
      call VRFY(ret,'mgatinf',number_failed)
      ret = mggcatt(ri_id,index,attr2_c)
      call VRFY(ret,'mggcatt',number_failed)

C End access to the image
      ret = mgendac(ri_id)
      call VRFY(ret,'mgendac',number_failed)

C Select a character image
      call MESSAGE(5,'Selecting a char type image')
      index = mgn2ndx(gr_id, IMAGEC)
      call VRFY(index,'mgn2ndx',number_failed)
      ri_id = mgselct(gr_id,index)
      call VRFY(ri_id,'mgselct',number_failed)

C Get info about the image
      call MESSAGE(5,'Getting image information')
      ret = mggiinf(ri_id,IMAGEC_2,n_comp,nt,il,dims,n_attrs)
      call VRFY(ret,'mggiinf',number_failed)
      ref = mgid2rf(ri_id)
      call VRFY(ref,'mgid2rf',number_failed)
      index2 = mgr2idx(gr_id,ref)
      call VRFY(index2,'mgr2idx',number_failed)

C Check image reading
      start(1)=0
      start(2)=0
      stride(1)=1
      stride(2)=1
      call MESSAGE(5,'Reading image data')

      call VRFY(ret,'mgrdimg',number_failed)
      ret = mgrimil(ri_id,MFGR_INTERLACE_LINE)
      call VRFY(ret,'mgrimil',number_failed)
      ret = mgrcimg(ri_id,start,stride,dims,icbuf)
      call VRFY(ret,'mgrcimg',number_failed)
      ret = mgrimil(ri_id,MFGR_INTERLACE_COMPONENT)
      call VRFY(ret,'mgrimil',number_failed)

C End access to the image
      ret = mgendac(ri_id)
      call VRFY(ret,'mgendac',number_failed)

C End access to the GR interface
      ret = mgend(gr_id)
      call VRFY(ret,'mgend',number_failed)

C Close the file
      ret = hclose(file_id)
      call VRFY(ret,'hclose',number_failed)

C
C     GR chunking and compression tests. Added by EIP 1/13/98
C
C     ----Chunking test
C          creates the following files:
C                       grch_no.hdf
C                       grch_rl.hdf
C                       grch_sk.hdf
C                       grch_gz.hdf
      err_grchunk = 0
      call test_grchunk(err_grchunk)
      if (err_grchunk .ne. 0) then
         number_failed = number_failed + 1
         print *, '*******mgrf: test_grchunk failed********'
      endif


C     ----Chunking write/read test
C          creates the following files:
C                       grchwr_no.hdf
C                       grchwr_rl.hdf
C                       grchwr_sk.hdf
C                       grchwr_gz.hdf
      err_grwrchunk = 0
      call test_grwrchunk(err_grwrchunk)
      if (err_grwrchunk .ne. 0) then
         number_failed = number_failed + 1
         print *, '*******mgrf: test_grwrchunk failed********'
      endif


C     ----Compression test
C
C          creates the following files:
C                       gr_no.hdf
C                       gr_rl.hdf
C                       gr_sk.hdf
C                       gr_gz.hdf
      err_grcompress = 0
      call test_grcompress(err_grcompress)
      if (err_grcompress .ne. 0) then
         number_failed = number_failed + 1
         print *, '*******mgrf: test_grcompress failed*******'
      endif
C
      if (number_failed .eq. 0) then 
	  if (Verbosity .gt. 6) then
	      print *, CR, CR
	      print *, '****** ALL TESTS SUCCESSFUL ******'
	  endif
      else
          print *, '****** ', number_failed, ' TESTS FAILED  ******'
      endif
      return
      end 

      subroutine test_grchunk( err_grchunk )
      implicit none
      integer N_COMP_TYPES, N_COMP_ARG, NCOMP
      integer MFGR_INTERLACE_PIXEL 
      parameter(N_COMP_TYPES = 4, N_COMP_ARG =1)
      parameter(NCOMP = 2, MFGR_INTERLACE_PIXEL = 0)
      integer ri_id(N_COMP_TYPES),
     .        gr_id(N_COMP_TYPES),
     .        file_id(N_COMP_TYPES)
      integer dims(2), start(2), edges(2), stride(2)
      integer err_grchunk
      integer i, j, status, il, k, i_comp, index
      integer flags, maxcache, nc_out
      character*12 file(N_COMP_TYPES)
      character*12 name(N_COMP_TYPES)
C
C---GR interface functions
C
      integer mgstart, mgcreat, mgwrimg, mgsnatt,
     .        mgrdimg, mgselct, mgendac, mgend
C
C---GR chunking functions 
C
      integer mggichnk, 
     .        mgscchnk,
     .        mgschnk
C
      integer hopen, hclose
      integer DFACC_CREATE, 
     .        DFACC_READ,
     .        DFACC_WRITE
      integer DFNT_INT16
      integer X_LENGTH, Y_LENGTH
      integer X_CH_LENGTH, Y_CH_LENGTH
      parameter (DFACC_CREATE = 4,
     .           DFACC_READ   = 1,
     .           DFACC_WRITE  = 2)
      parameter (DFNT_INT16   = 22)
      parameter (X_LENGTH     = 9,
     .           Y_LENGTH     = 4,
     .           X_CH_LENGTH  = 3,
     .           Y_CH_LENGTH  = 2) 
C
C---Compression types and parameters arrays 
C
      integer comp_type(N_COMP_TYPES), comp_type_out(N_COMP_TYPES)
      integer comp_prm(N_COMP_ARG)
C
C---Compression parameters
C
      integer COMP_CODE_NONE,
     .          COMP_CODE_RLE,
     .          COMP_CODE_SKPHUFF,
     .          COMP_CODE_DEFLATE,
     .          SKPHUFF_SKP_SIZE,
     .          DEFLATE_LEVEL

      parameter(COMP_CODE_NONE    = 0,
     .          COMP_CODE_RLE     = 1,
     .          COMP_CODE_SKPHUFF = 3,
     .          COMP_CODE_DEFLATE = 4)
      parameter (DEFLATE_LEVEL = 1,
     .           SKPHUFF_SKP_SIZE = 2)
C
C---Data
C 
      integer*2 image_data(NCOMP, X_LENGTH, Y_LENGTH)
      integer*2 image_data_out(NCOMP,X_LENGTH,Y_LENGTH)
C
C---Default pixel value
C
      integer*2 pixel_value(2)
C
C---Chunking dimension arrays
C
      integer ch_dims(2), ch_dims_out(2)
C
C---We will write/read to four different files corresponding to the
C   different compression types.
C
C   No compresion
C
      file(1) = 'grch_no.hdf'
      name(1) = 'Nocomp_data'    
      comp_type(1) = COMP_CODE_NONE
      comp_type_out(1) = 0
C
C   RLE compresion
C
      file(2) = 'grch_rl.hdf'
      name(2) = 'Rlcomp_data'    
      comp_type(2) = COMP_CODE_RLE
      comp_type_out(2) = 1 
C
C   Adaptive Huffman compresion
C
      file(3) = 'grch_sk.hdf'
      name(3) = 'Hucomp_data'    
      comp_type(3) = COMP_CODE_SKPHUFF
      comp_type_out(3) = 1 
C
C   GZIP compression
C
      file(4) = 'grch_gz.hdf'
      name(4) = 'Gzcomp_data'    
      comp_type(4) = COMP_CODE_DEFLATE
      comp_type_out(4) = 1 
C
C  Data initialization
C 
      do 30 j = 1, Y_LENGTH
         do 20 i = 1, X_LENGTH
           do 10 k = 1, NCOMP
            image_data(k, i, j) = i + j - 1
10         continue
20    continue
30    continue
C
C  Initialize compression argument array
C
      do 35 i = 1, N_COMP_ARG
          comp_prm(i) = 0
35    continue
C
C---Define chunk dimensions
C
         ch_dims(1) = X_CH_LENGTH 
         ch_dims(2) = Y_CH_LENGTH 
C
C   Main loop through different compression types
C

      do 1000 i_comp=1, N_COMP_TYPES
C
C     Create and open the file.
C
      file_id(i_comp) = hopen(file(i_comp), DFACC_CREATE, 0)
C
C     Initiate the GR interface.
C
      gr_id(i_comp) = mgstart(file_id(i_comp))

C     Define the number of components and dimensions of the image.

      il = MFGR_INTERLACE_PIXEL
      dims(1) = X_LENGTH
      dims(2) = Y_LENGTH

C     Create the data set.

      ri_id(i_comp) = mgcreat(gr_id(i_comp), name(i_comp), NCOMP,
     .                        DFNT_INT16, il, dims)

C
C---Set pixel value
C
      pixel_value(1) = 0
      pixel_value(2) = 0
C
C---Fill the image array with the default pixel value
C
      status = mgsnatt(ri_id(i_comp), 'FillValue', DFNT_INT16,
     .                 ncomp, pixel_value) 
      if(status .ne. 0) then
         print *, 'mgsnatt failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 

C
C---Define chunked GR
C
      if (i_comp. eq. 3) comp_prm(1) = SKPHUFF_SKP_SIZE
      if (i_comp. eq. 4) comp_prm(1) =  DEFLATE_LEVEL
      status = mgschnk (ri_id(i_comp), ch_dims,
     .                  comp_type(i_comp),comp_prm)
      if(status .ne. 0) then
         print *, 'mgschnk failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 
C
C---Set chunk cache to hold maximum of 3 chunks
C
      maxcache = 3
      flags = 0
      status = mgscchnk (ri_id(i_comp), maxcache, flags) 
      if(status .ne. 3) then
         print *, 'mgscchnk failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 
C
C     Define the location, pattern, and size of the data set
C     that will be written to.
      start(1) = 0
      start(2) = 0
      edges(1) = X_LENGTH
      edges(2) = Y_LENGTH
      stride(1) = 1
      stride(2) = 1

C     Write the stored data to the image array.
      status = mgwrimg(ri_id(i_comp), start, stride, edges, image_data)
      if(status .ne. 0) then
         print *, 'mgwrimg failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 
C
C     Terminate access to the array.
C
      status = mgendac(ri_id(i_comp))
      if(status .ne. 0) then
         print *, 'mgendac failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 
C
C     Terminate access to the GR interface.
C
      status = mgend(gr_id(i_comp))
      if(status .ne. 0) then
         print *, 'mgend failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 
C
C     Close the file.
C
      status = hclose(file_id(i_comp))
      if(status .ne. 0) then
         print *, 'hclose failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 

1000  continue


      do 2000 i_comp=1, N_COMP_TYPES
C
C     Open the file.
C
      file_id(i_comp) = hopen(file(i_comp), DFACC_READ, 0)
      if(status .eq. -1) then
         print *, 'hopen failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 
C
C     Initiate the GR interface and select first data set.
C
      gr_id(i_comp) = mgstart(file_id(i_comp))
      if(status .eq. -1) then
         print *, 'mgstart failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 
      index = 0
      ri_id(i_comp) = mgselct(gr_id(i_comp), index)
      if(status .eq. -1) then
         print *, 'mgselct failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 
C
C     Read the stored data to the image array.
C
      status = mgrdimg(ri_id(i_comp), start, stride, edges,
     .                  image_data_out)
      if(status .ne. 0) then
         print *, 'mgrdimg failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 
C
C--- Compare the data we read with the original data
C
      do 60 j = 1, Y_LENGTH
         do 50 i = 1, X_LENGTH
           do 40 k = 1, ncomp
            if(image_data(k, i, j).ne.image_data_out(k,i,j)) then
             print *, 'data is wrong'
             err_grchunk = err_grchunk +1
            endif 
40         continue
50    continue
60    continue
C
C   Check chunking info
C
       status = mggichnk(ri_id(i_comp), ch_dims_out,
     .                   nc_out)
      if(status .ne. 0) then
         print *, 'mggichnk failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 
       if (comp_type_out(i_comp) .ne. nc_out) then
          print *, 'mggichnk returns wrong compression type for',
     .              i_comp, '-th data set'
          err_grchunk = err_grchunk + 1
       endif
       if ( (ch_dims(1) .ne. ch_dims_out(1)) .or.
     .      (ch_dims(2) .ne. ch_dims_out(2))) then
          print *, 'mggichnk returns wrong chunk dimensions for',
     .              i_comp, '-th data set'
          err_grchunk = err_grchunk + 1
       endif

  
C
C     Terminate access to the array.
C
      status = mgendac(ri_id(i_comp))
      if(status .ne. 0) then
         print *, 'mgendac failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 
C
C     Terminate access to the GR interface.
C
      status = mgend(gr_id(i_comp))
      if(status .ne. 0) then
         print *, 'mgend failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 
C
C     Close the file.
C
      status = hclose(file_id(i_comp))
      if(status .ne. 0) then
         print *, 'hclose failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 
2000  continue
      return
      end
C
C     GR compression test
C
      subroutine test_grcompress( err_grcompress )
      implicit none
      integer N_COMP_TYPES, N_COMP_ARG, NCOMP
      parameter(N_COMP_TYPES = 4, N_COMP_ARG =1)
      integer MFGR_INTERLACE_PIXEL
      parameter(NCOMP = 2, MFGR_INTERLACE_PIXEL = 0)
      integer ri_id(N_COMP_TYPES),
     .        gr_id(N_COMP_TYPES),
     .        file_id(N_COMP_TYPES)
      integer dims(2), start(2), edges(2), stride(2)
      integer i, j, k, status, il, i_comp, index
      integer err_grcompress
      character*12 file(N_COMP_TYPES)
      character*12 name(N_COMP_TYPES)
C
C---GR interface functions
C
      integer mgstart, mgcreat, mgwrimg, mgn2ndx,
     .        mgsnatt,
     .        mgrdimg, mgselct, mgendac, mgend
C
C---GR compression function 
C
      integer mgscompress
C
      integer hopen, hclose
      integer DFACC_CREATE, 
     .        DFACC_READ,
     .        DFACC_WRITE
      integer DFNT_INT16
      integer X_LENGTH, Y_LENGTH
      integer X_CH_LENGTH, Y_CH_LENGTH
      parameter (DFACC_CREATE = 4,
     .           DFACC_READ   = 1,
     .           DFACC_WRITE  = 2)
      parameter (DFNT_INT16   = 22)
      parameter (X_LENGTH     = 9,
     .           Y_LENGTH     = 4,
     .           X_CH_LENGTH  = 3,
     .           Y_CH_LENGTH  = 2) 
C
C---Compression types and parameters arrays 
C
      integer comp_type(N_COMP_TYPES)
      integer comp_prm(N_COMP_ARG)
C
C---Compression parameters
C
      integer COMP_CODE_NONE,
     .          COMP_CODE_RLE,
     .          COMP_CODE_SKPHUFF,
     .          COMP_CODE_DEFLATE,
     .          SKPHUFF_SKP_SIZE,
     .          DEFLATE_LEVEL

      parameter(COMP_CODE_NONE    = 0,
     .          COMP_CODE_RLE     = 1,
     .          COMP_CODE_SKPHUFF = 3,
     .          COMP_CODE_DEFLATE = 4)
      parameter (DEFLATE_LEVEL = 1,
     .           SKPHUFF_SKP_SIZE = 2)
C
C---Data
C 
      integer*2 image_data(NCOMP, X_LENGTH, Y_LENGTH)
      integer*2 image_data_out(NCOMP,X_LENGTH,Y_LENGTH)
C
C---Default pixel value
C
      integer*2 pixel_value(2)
C
C---We will write/read to four different files corresponding to the
C   different compression types.
C
C   No compresion
C
      file(1) = 'gr_no.hdf'
      name(1) = 'Nocomp_data'    
      comp_type(1) = COMP_CODE_NONE
C
C   RLE compresion
C
      file(2) = 'gr_rl.hdf'
      name(2) = 'Rlcomp_data'    
      comp_type(2) = COMP_CODE_RLE
C
C   Adaptive Huffman compresion
C
      file(3) = 'gr_sk.hdf'
      name(3) = 'Hucomp_data'    
      comp_type(3) = COMP_CODE_SKPHUFF
C
C   GZIP compression
C
      file(4) = 'gr_gz.hdf'
      name(4) = 'Gzcomp_data'    
      comp_type(4) = COMP_CODE_DEFLATE
C
C  Data initialization
C 
      do 30 j = 1, Y_LENGTH
         do 20 i = 1, X_LENGTH
           do 10 k = 1, NCOMP
            image_data(k, i, j) = i + j - 1
10         continue
20    continue
30    continue
C
C  Initialize compression argument array
C
      do 35 i = 1, N_COMP_ARG
          comp_prm(i) = 0
35    continue
C
C   Main loop through different compression types
C

      do 1000 i_comp=1, N_COMP_TYPES
C
C     Create and open the file.
C
      file_id(i_comp) = hopen(file(i_comp), DFACC_CREATE, 0)
      if(file_id(i_comp) .eq. -1) then
         print *, 'hopen failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
C
C     Initiate the GR interface.
C
      gr_id(i_comp) = mgstart(file_id(i_comp))
      if(gr_id(i_comp) .eq. -1) then
         print *, 'mgstart failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 

C     Define the number of components and dimensions of the image.

      il = MFGR_INTERLACE_PIXEL
      dims(1) = X_LENGTH
      dims(2) = Y_LENGTH

C     Create the data set.

      ri_id(i_comp) = mgcreat(gr_id(i_comp), name(i_comp), NCOMP,
     .                        DFNT_INT16, il, dims)
      if(ri_id(i_comp) .eq. -1) then
         print *, 'mgcreat failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 

C
C---Set pixel value
C
      pixel_value(1) = 0
      pixel_value(2) = 0
C
C---Fill the image array with the default pixel value
C
      status = mgsnatt(ri_id(i_comp), 'FillValue', DFNT_INT16,
     .                 ncomp, pixel_value) 
      if(status .ne. 0) then
         print *, 'mgsnatt failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 

C
C---Set compression
C
      if (i_comp. eq. 3) comp_prm(1) = SKPHUFF_SKP_SIZE
      if (i_comp. eq. 4) comp_prm(1) =  DEFLATE_LEVEL
      status = mgscompress (ri_id(i_comp), 
     .                  comp_type(i_comp),comp_prm)
      if(status .ne. 0) then
         print *, 'mgscompress failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
C
C     Define the location, pattern, and size of the data set
C     that will be written to.
      start(1) = 0
      start(2) = 0
      edges(1) = X_LENGTH
      edges(2) = Y_LENGTH
      stride(1) = 1
      stride(2) = 1

C     Write the stored data to the image array..
      status = mgwrimg(ri_id(i_comp), start, stride, edges, image_data)
      if(status .ne. 0) then
         print *, 'mgwrimg failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
C
C     Terminate access to the array.
C
      status = mgendac(ri_id(i_comp))
      if(status .ne. 0) then
         print *, 'mgendac failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
C
C     Terminate access to the GR interface.
C
      status = mgend(gr_id(i_comp))
      if(status .ne. 0) then
         print *, 'mgend failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
C
C     Close the file.
C
      status = hclose(file_id(i_comp))
      if(status .ne. 0) then
         print *, 'hclose failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 

1000  continue


      do 2000 i_comp=1, N_COMP_TYPES
C
C     Open the file.
C
      file_id(i_comp) = hopen(file(i_comp), DFACC_READ, 0)
      if(file_id(i_comp) .eq. -1) then
         print *, 'hopen failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
C
C     Initiate the GR interface and select first data set.
C
      gr_id(i_comp) = mgstart(file_id(i_comp))
      if(gr_id(i_comp) .eq. -1) then
         print *, 'mgstart failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
      index = mgn2ndx(gr_id(i_comp), name(i_comp))
      if(index .eq. -1) then
         print *, 'mgn2ndx failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
      ri_id(i_comp) = mgselct(gr_id(i_comp), index)
      if(ri_id(i_comp) .eq. -1) then
         print *, 'mgselct failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
C
C     Read the stored data to the image array.
C
      start(1) = 0
      start(2) = 0
      edges(1) = X_LENGTH
      edges(2) = Y_LENGTH
      stride(1) = 1
      stride(2) = 1
      status = mgrdimg(ri_id(i_comp), start, stride, edges,
     .                  image_data_out)
      if(status .ne. 0) then
         print *, 'mgrdimg failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
C
C--- Compare the data we read with the original data
C
      do 60 j = 1, Y_LENGTH
         do 50 i = 1, X_LENGTH
           do 40 k = 1, ncomp
            if(image_data(k, i, j).ne.image_data_out(k,i,j)) then
             print *, 'data is wrong'
             err_grcompress = err_grcompress +1
            endif 
40         continue
50    continue
60    continue

  
C
C     Terminate access to the array.
C
      status = mgendac(ri_id(i_comp))
      if(status .ne. 0) then
         print *, 'mgendac failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
C
C     Terminate access to the GR interface.
C
      status = mgend(gr_id(i_comp))
      if(status .ne. 0) then
         print *, 'mgend failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
C
C     Close the file.
C
      status = hclose(file_id(i_comp))
      if(status .ne. 0) then
         print *, 'hclose failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
2000  continue
      return
      end

      subroutine test_grwrchunk( err_grwrchunk )
C
C---This subroutine tests GR write/read functions
C
      implicit none
      integer N_COMP_TYPES, N_COMP_ARG, NCOMP
      integer MFGR_INTERLACE_PIXEL 
      parameter(N_COMP_TYPES = 4, N_COMP_ARG =1)
      parameter(NCOMP = 3, MFGR_INTERLACE_PIXEL = 0)
      integer ri_id(N_COMP_TYPES),
     .        gr_id(N_COMP_TYPES),
     .        file_id(N_COMP_TYPES)
      integer dims(2), start(2), edges(2), stride(2)
      integer err_grwrchunk
      integer i, j, status, il, k, i_comp, index
      character*13 file(N_COMP_TYPES)
      character*12 name(N_COMP_TYPES)
C
C---GR interface functions
C
      integer mgstart, mgcreat, mgsnatt,
     .        mgrdimg, mgselct, mgendac, mgend
C
C---GR chunking functions 
C
      integer mgwchnk, 
     .        mgrchnk,
     .        mgschnk
C
      integer hopen, hclose
      integer DFACC_CREATE, 
     .        DFACC_READ,
     .        DFACC_WRITE
      integer DFNT_INT16
      integer X_LENGTH, Y_LENGTH
      integer X_CH_LENGTH, Y_CH_LENGTH
      parameter (DFACC_CREATE = 4,
     .           DFACC_READ   = 1,
     .           DFACC_WRITE  = 2)
      parameter (DFNT_INT16   = 22)
      parameter (X_LENGTH     = 6,
     .           Y_LENGTH     = 10,
     .           X_CH_LENGTH  = 3,
     .           Y_CH_LENGTH  = 2) 
C
C---Compression types and parameters arrays 
C
      integer comp_type(N_COMP_TYPES), comp_type_out(N_COMP_TYPES)
      integer comp_prm(N_COMP_ARG)
C
C---Compression parameters
C
      integer COMP_CODE_NONE,
     .          COMP_CODE_RLE,
     .          COMP_CODE_SKPHUFF,
     .          COMP_CODE_DEFLATE,
     .          SKPHUFF_SKP_SIZE,
     .          DEFLATE_LEVEL

      parameter(COMP_CODE_NONE    = 0,
     .          COMP_CODE_RLE     = 1,
     .          COMP_CODE_SKPHUFF = 3,
     .          COMP_CODE_DEFLATE = 4)
      parameter (DEFLATE_LEVEL = 1,
     .           SKPHUFF_SKP_SIZE = 2)
C
C---Data
C 
      integer*2 chunk11(NCOMP* X_CH_LENGTH*Y_CH_LENGTH)
      integer*2 chunk21(NCOMP* X_CH_LENGTH*Y_CH_LENGTH)
      integer*2 chunk52(NCOMP* X_CH_LENGTH*Y_CH_LENGTH)
      integer*2 chunk52_out(NCOMP* X_CH_LENGTH*Y_CH_LENGTH)
      integer*2 data_org(NCOMP* X_LENGTH*Y_LENGTH)
      integer*2 data_org1(30)
      integer*2 data_org2(30)
      integer*2 data_org3(30)
      integer*2 data_org4(30)
      integer*2 data_org5(30)
      integer*2 data_org6(30)
       
      integer*2 image_data_out(NCOMP,X_LENGTH,Y_LENGTH)
      integer*2 data_arr(NCOMP,X_LENGTH,Y_LENGTH)
C
C---Those statemnets were created for SunOS since the 
C   compiler does not support multiple continuation lines
C   needed for data initilization in DATA statement.
C   This file should be used only on SunOS and
C   should be deleted from CVS as soon as we drop SunOS support.
C        EP 5/5/99
C
      equivalence (data_org(1), data_arr(1,1,1))
      equivalence (data_org(1), data_org1(1))
      equivalence (data_org(31), data_org2(1))
      equivalence (data_org(61), data_org3(1))
      equivalence (data_org(91), data_org4(1))
      equivalence (data_org(121), data_org5(1))
      equivalence (data_org(151), data_org6(1))
C
C---Default pixel value
C
      integer*2 pixel_value(3)
C
C---Chunking dimension arrays
C
      integer ch_dims(2)
C
C---We will write/read to four different files corresponding to the
C   different compression types.
C
C   No compresion
C
      file(1) = 'grchwr_no.hdf'
      name(1) = 'Nocomp_data'    
      comp_type(1) = COMP_CODE_NONE
      comp_type_out(1) = 0
C
C   RLE compresion
C
      file(2) = 'grchwr_rl.hdf'
      name(2) = 'Rlcomp_data'    
      comp_type(2) = COMP_CODE_RLE
      comp_type_out(2) = 1 
C
C   Adaptive Huffman compresion
C
      file(3) = 'grchwr_sk.hdf'
      name(3) = 'Hucomp_data'    
      comp_type(3) = COMP_CODE_SKPHUFF
      comp_type_out(3) = 1 
C
C   GZIP compression
C
      file(4) = 'grchwr_gz.hdf'
      name(4) = 'Gzcomp_data'    
      comp_type(4) = COMP_CODE_DEFLATE
      comp_type_out(4) = 1 
C
C  Data initialization
C 
      data chunk11 / 110, 111, 112, 120, 121, 122,
     .                130, 131, 132, 140, 141, 142,
     .                150, 151, 152, 160, 161, 162/ 
      data  chunk21 /
     .                210, 211, 212, 220, 221, 222,
     .                230, 231, 232, 240, 241, 242,
     .                250, 251, 252, 260, 261, 262
     .              /
      data  chunk52 /
     .                1010, 1011, 1012, 1020, 1021, 1022,
     .                1030, 1031, 1032, 1040, 1041, 1042,
     .                1050, 1051, 1052, 1060, 1061, 1062
     .              /

      data  data_org1  
     .              / 110, 111, 112, 120, 121, 122, 
     .                210, 211, 212, 220, 221, 222, 0,
     .                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
     .                 0, 0, 0, 0, 0, 0, 0 /

      data  data_org2    
     .              / 130, 131, 132, 140,
     .                141, 142, 230, 231, 232, 240, 241, 242, 
     .                0, 0, 0, 0, 0, 0, 0, 0, 0,
     .                0, 0, 0, 0, 0, 0, 0, 0, 0 /

       data data_org3          
     .              / 150, 151, 152, 160, 161, 162, 250, 251,
     .                252, 260, 261, 262, 0, 0, 0, 0, 0, 0, 0, 
     .                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/

       data data_org4      
     .              / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
     .                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .                1010, 1011, 1012, 1020, 1021, 1022 /

       data data_org5      
     .              / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
     .                1030, 1031, 1032, 1040, 1041, 1042/

       data data_org6    
     .              / 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
     .                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .                0, 1050, 1051, 1052, 1060, 1061, 1062 /

C
C  Initialize compression argument array
C
      do 35 i = 1, N_COMP_ARG
          comp_prm(i) = 0
35    continue
C
C---Define chunk dimensions
C
         ch_dims(1) = Y_CH_LENGTH 
         ch_dims(2) = X_CH_LENGTH 
C
C   Main loop through different compression types
C

      do 1000 i_comp=1, N_COMP_TYPES
C
C     Create and open the file.
C
      file_id(i_comp) = hopen(file(i_comp), DFACC_CREATE, 0)
C
C     Initiate the GR interface.
C
      gr_id(i_comp) = mgstart(file_id(i_comp))

C     Define the number of components and dimensions of the image.

      il = MFGR_INTERLACE_PIXEL
      dims(1) = X_LENGTH
      dims(2) = Y_LENGTH

C     Create the data set.

      ri_id(i_comp) = mgcreat(gr_id(i_comp), name(i_comp), NCOMP,
     .                        DFNT_INT16, il, dims)

C
C---Set pixel value
C
      pixel_value(1) = 0
      pixel_value(2) = 0
      pixel_value(3) = 0
C
C---Fill the image array with the default pixel value
C
      status = mgsnatt(ri_id(i_comp), 'FillValue', DFNT_INT16,
     .                 ncomp, pixel_value) 
      if(status .ne. 0) then
         print *, 'mgsnatt failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 

C
C---Define chunked GR
C
      if (i_comp. eq. 3) comp_prm(1) = SKPHUFF_SKP_SIZE
      if (i_comp. eq. 4) comp_prm(1) =  DEFLATE_LEVEL
      status = mgschnk (ri_id(i_comp), ch_dims,
     .                  comp_type(i_comp),comp_prm)
      if(status .ne. 0) then
         print *, 'mgschnk failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 
C
C     Define the location of the first chunk 
C     that will be written to.
      start(1) = 1 
      start(2) = 1 

C     Write the stored data to the image array.
      status = mgwchnk(ri_id(i_comp), start, chunk11)
      if(status .ne. 0) then
         print *, 'mgwchnk failed for', i_comp, 
     .            '-th data set, first chunk'
         err_grwrchunk = err_grwrchunk +1
      endif 
C
C     Define the location of the first chunk 
C     that will be written to.
      start(1) = 2 
      start(2) = 1 

C     Write the stored data to the image array.
      status = mgwchnk(ri_id(i_comp), start, chunk21)
      if(status .ne. 0) then
         print *, 'mgwchnk failed for', i_comp, 
     .            '-th data set, second chunk'
         err_grwrchunk = err_grwrchunk +1
      endif 
C
C     Define the location of the first chunk 
C     that will be written to.
      start(1) = 5 
      start(2) = 2 

C     Write the stored data to the image array.
      status = mgwchnk(ri_id(i_comp), start, chunk52)
      if(status .ne. 0) then
         print *, 'mgwchnk failed for', i_comp, 
     .            '-th data set, third chunk'
         err_grwrchunk = err_grwrchunk +1
      endif 
C
C     Terminate access to the array.
C
      status = mgendac(ri_id(i_comp))
      if(status .ne. 0) then
         print *, 'mgendac failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 
C
C     Terminate access to the GR interface.
C
      status = mgend(gr_id(i_comp))
      if(status .ne. 0) then
         print *, 'mgend failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 
C
C     Close the file.
C
      status = hclose(file_id(i_comp))
      if(status .ne. 0) then
         print *, 'hclose failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 

1000  continue


      do 2000 i_comp=1, N_COMP_TYPES
C
C     Open the file.
C
      file_id(i_comp) = hopen(file(i_comp), DFACC_READ, 0)
      if(status .eq. -1) then
         print *, 'hopen failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 
C
C     Initiate the GR interface and select first data set.
C
      gr_id(i_comp) = mgstart(file_id(i_comp))
      if(status .eq. -1) then
         print *, 'mgstart failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 
      index = 0
      ri_id(i_comp) = mgselct(gr_id(i_comp), index)
      if(status .eq. -1) then
         print *, 'mgselct failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 
C
C     Read the stored data to the image array.
C
      start(1) = 0
      start(2) = 0
      edges(1) = X_LENGTH
      edges(2) = Y_LENGTH
      stride(1) = 1
      stride(2) = 1
      status = mgrdimg(ri_id(i_comp), start, stride, edges,
     .                  image_data_out)
      if(status .ne. 0) then
         print *, 'mgrdimg failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 
C
C--- Compare the data we read with the original data
C
      do 60 j = 1, Y_LENGTH
         do 50 i = 1, X_LENGTH
           do 40 k = 1, ncomp
            if(data_arr(k, i, j).ne.image_data_out(k,i,j)) then
             print *, 'data is wrong'
             err_grwrchunk = err_grwrchunk +1
            endif 
40         continue
50    continue
60    continue
  
C
C--- Read the third chunk back and compare it with original data.
C

      start(1) = 5 
      start(2) = 2 
      status = mgrchnk(ri_id(i_comp), start, chunk52_out)
      if(status .ne. 0) then
         print *, 'mgrchnk failed for', i_comp, 
     .            '-th data set, third chunk'
         err_grwrchunk = err_grwrchunk +1
      endif 
      do 401 j = 1, NCOMP*X_CH_LENGTH*Y_CH_LENGTH
           if(chunk52(j).ne.chunk52_out(j)) then
             print *, 'read chunk''s data is wrong'
             err_grwrchunk = err_grwrchunk +1
            endif
401         continue

C
C     Terminate access to the array.
C
      status = mgendac(ri_id(i_comp))
      if(status .ne. 0) then
         print *, 'mgendac failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 
C
C     Terminate access to the GR interface.
C
      status = mgend(gr_id(i_comp))
      if(status .ne. 0) then
         print *, 'mgend failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 
C
C     Close the file.
C
      status = hclose(file_id(i_comp))
      if(status .ne. 0) then
         print *, 'hclose failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 
2000  continue
      return
      end


