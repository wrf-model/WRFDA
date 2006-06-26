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
C $Id: mgrf.f,v 1.34 2004/12/21 15:57:05 epourmal Exp $
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
      integer mggnluts
      
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
      integer n_pal

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
                 icbuf(i,j,k) = ' '
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
      n_pal = mggnluts(ri_id)
      call VRFY(ri_id,'mggnluts',number_failed)
      if(n_pal .ne. 1) then
         print *, 'Wrong number of palettes returned for IMAGE1'
      endif
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
      n_pal = mggnluts(ri_id)
      call VRFY(ri_id,'mggnluts',number_failed)
      if(n_pal .ne. 0) then
         print *,'Wrong number of palettes returned for IMAGEC'
      endif

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
C                        GRchunked1.hdf
      err_grchunk = 0
      call test_grchunk(err_grchunk)
      if (err_grchunk .ne. 0) then
         number_failed = number_failed + 1
         print *, '*******mgrf: test_grchunk failed********'
      endif


C     ----Chunking write/read test
C          creates the following files:
C                        GRchunked2.hdf
      err_grwrchunk = 0
      call test_grwrchunk(err_grwrchunk)
      if (err_grwrchunk .ne. 0) then
         number_failed = number_failed + 1
         print *, '*******mgrf: test_grwrchunk failed********'
      endif


C     ----Compression test
C
C          creates the following files:
C                        GRcompressed.hdf
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
CD
CD----All lines started with CD should be deleted after GR bug that prevents
CD----writing multiple images to the file is fixed.
CD 
      subroutine test_grchunk( err_grchunk )
      implicit none
      integer N_COMP_TYPES, N_COMP_ARG, NCOMP
      integer MFGR_INTERLACE_PIXEL 
      parameter(N_COMP_TYPES = 4, N_COMP_ARG =1)
      parameter(NCOMP = 2, MFGR_INTERLACE_PIXEL = 0)
      integer ri_id(N_COMP_TYPES),
     .        gr_id,
     .        file_id
      integer dims(2), start(2), edges(2), stride(2)
      integer err_grchunk
      integer i, j, status, il, k, i_comp, index
      integer flags, maxcache, nc_out
      character*14 file
      character*12 name(N_COMP_TYPES)
      integer n_images, n_file_attrs
C
C---GR interface functions
C
      integer mgstart, mgcreat, mgwrimg, mgsnatt,
     .        mgrdimg, mgfinfo, mgn2ndx, mgselct, mgendac, mgend
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
      integer DFNT_INT32
      integer X_LENGTH, Y_LENGTH
      integer X_CH_LENGTH, Y_CH_LENGTH
      parameter (DFACC_CREATE = 4,
     .           DFACC_READ   = 1,
     .           DFACC_WRITE  = 2)
      parameter (DFNT_INT32   = 24)
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
      integer*4 image_data(NCOMP, X_LENGTH, Y_LENGTH)
      integer*4 image_data_out(NCOMP,X_LENGTH,Y_LENGTH)
C
C---Default pixel value
C
      integer*4 pixel_value(2)
C
C---Chunking dimension arrays
C
      integer ch_dims(2), ch_dims_out(2)
C
C----We will write four images using different compression methods to
C    one file.
C
       file = 'GRchunked1.hdf'
C
C   No compresion
C
      name(1) = 'Nocomp_data'    
      comp_type(1) = COMP_CODE_NONE
      comp_type_out(1) = 0
C
C   RLE compresion
C
      name(2) = 'Rlcomp_data'    
      comp_type(2) = COMP_CODE_RLE
      comp_type_out(2) = 1 
C
C   Adaptive Huffman compresion
C
      name(3) = 'Hucomp_data'    
      comp_type(3) = COMP_CODE_SKPHUFF
      comp_type_out(3) = 1 
C
C   GZIP compression
C
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
C
C     Create and a file and initiate GR interface.
C
      file_id = hopen(file, DFACC_CREATE, 0)
      if(file_id .le. 0) then
         print *, 'hopen failed to create a file'
         err_grchunk = err_grchunk +1
         goto 2223
      endif 
      gr_id = mgstart(file_id)
      if(gr_id .le. 0) then
         print *, 'mgstart failed to initialise GR interface'
         err_grchunk = err_grchunk +1
         goto 2222
      endif 

      do 1000 i_comp=1, N_COMP_TYPES
C

C     Define the number of components and dimensions of the image.

      il = MFGR_INTERLACE_PIXEL
      dims(1) = X_LENGTH
      dims(2) = Y_LENGTH

C     Create the data set.

      ri_id(i_comp) = mgcreat(gr_id, name(i_comp), NCOMP,
     .                        DFNT_INT32, il, dims)
      if(ri_id(i_comp) .le. 0) then
         print *, 'mgcreat failed to create ', i_comp, 'GR dataset'
         err_grchunk = err_grchunk +1
         goto 1000
      endif

C
C---Set pixel value
C
      pixel_value(1) = 0
      pixel_value(2) = 0
C
C---Fill the image array with the default pixel value
C
      status = mgsnatt(ri_id(i_comp), 'FillValue', DFNT_INT32,
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
1000  continue
C
C     Terminate access to the GR interface.
C
      status = mgend(gr_id)
      if(status .ne. 0) then
         print *, 'mgend failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 
C
C     Close the file.
C
      status = hclose(file_id)
      if(status .ne. 0) then
         print *, 'hclose failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 



C
C     Open the file.
C
      file_id = hopen(file, DFACC_READ, 0)
      if(file_id .eq. -1) then
         print *, 'hopen failed to access the file'
         err_grchunk = err_grchunk +1
         goto 2223
      endif 
C
C     Initiate the GR interface and select first data set.
C
      gr_id = mgstart(file_id)
      if(gr_id .eq. -1) then
         print *, 'mgstart failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
         goto 2222
      endif 
C
C     Check that file contains 4 GR datasets and has 0 file attributes.
C
      status = mgfinfo(gr_id, n_images, n_file_attrs)
      if(status .ne. 0) then
         print *, 'mgfinfo failed '
         err_grchunk = err_grchunk +1
      goto 2222
      endif 
      if(n_images .ne. 4) then
         print *, 'Wrong number of images returned '
         err_grchunk = err_grchunk +1
      goto 2222
      endif 
      if(n_file_attrs .ne. 0) then
         print *, 'Wrong number of file attributes returned '
         err_grchunk = err_grchunk +1
      endif 

      do 2000 i_comp=1, n_images
C      ri_id(i_comp) = mgselct(gr_id(i_comp), index)
C
C     Find an index using image's name.
C
      index = mgn2ndx(gr_id, name(i_comp))
      if(index .lt. 0 .or. index .gt. 3) then
         print *, 'Wrong index range '
         err_grchunk = err_grchunk +1
      goto 2222
      endif 

      ri_id(i_comp) = mgselct(gr_id, index)
      if(ri_id(i_comp) .eq. -1) then
         print *, 'mgselct failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
         goto 1999
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
1999  continue
C
2000  continue
C
C     Terminate access to the GR interface.
C
      status = mgend(gr_id)
      if(status .ne. 0) then
         print *, 'mgend failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 
2222  continue
C
C     Close the file.
C
      status = hclose(file_id)
      if(status .ne. 0) then
         print *, 'hclose failed for', i_comp, '-th data set'
         err_grchunk = err_grchunk +1
      endif 
2223  continue
      return
      end
C
C     GR compression test
C
      subroutine test_grcompress( err_grcompress )
      implicit none
      integer N_COMP_TYPES, N_COMP_ARG, NCOMP
C      parameter(N_COMP_TYPES = 4, N_COMP_ARG =1)
      parameter(N_COMP_TYPES = 5, N_COMP_ARG = 2)
      integer MFGR_INTERLACE_PIXEL
      parameter(NCOMP = 3, MFGR_INTERLACE_PIXEL = 0)
      integer ri_id(N_COMP_TYPES),
     .        gr_id,
     .        file_id
      integer dims(2), start(2), edges(2), stride(2)
      integer i, j, k, status, il, i_comp, index
      integer err_grcompress
C      character*12 file(N_COMP_TYPES)
      character*16 file
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
      integer mgscompress, mggcompress
C
      integer hopen, hclose
      integer DFACC_CREATE, 
     .        DFACC_READ,
     .        DFACC_WRITE
      integer DFNT_INT32
      integer X_LENGTH, Y_LENGTH
      integer X_CH_LENGTH, Y_CH_LENGTH
      parameter (DFACC_CREATE = 4,
     .           DFACC_READ   = 1,
     .           DFACC_WRITE  = 2)
      parameter (DFNT_INT32   = 24)
      parameter (X_LENGTH     = 9,
     .           Y_LENGTH     = 4,
     .           X_CH_LENGTH  = 3,
     .           Y_CH_LENGTH  = 2) 
C
C---Compression types and parameters arrays 
C
      integer comp_type(N_COMP_TYPES), comp_type_out
      integer comp_prm(N_COMP_ARG), comp_prm_out(N_COMP_ARG)
C
C---Compression parameters
C
      integer COMP_CODE_NONE,
     .          COMP_CODE_RLE,
     .          COMP_CODE_SKPHUFF,
     .          COMP_CODE_DEFLATE,
     .          SKPHUFF_SKP_SIZE,
     .          DEFLATE_LEVEL,
     .          COMP_CODE_JPEG,
     .          JPEG_QUALITY,
     .          JPEG_COMPATIBILITY

      parameter(COMP_CODE_NONE    = 0,
     .          COMP_CODE_RLE     = 1,
     .          COMP_CODE_SKPHUFF = 3,
     .          COMP_CODE_DEFLATE = 4,
     .          COMP_CODE_JPEG = 7)
      parameter (DEFLATE_LEVEL = 6,
     .           SKPHUFF_SKP_SIZE = 2)
      parameter (JPEG_QUALITY = 100,
     .           JPEG_COMPATIBILITY = 1)
C
C---Data
C 
      integer*4 image_data(NCOMP, X_LENGTH, Y_LENGTH)
      integer*4 image_data_out(NCOMP,X_LENGTH,Y_LENGTH)
C
C---Default pixel value
C
      integer*4 pixel_value(NCOMP)
C
C---We will write/read to four different files corresponding to the
C   different compression types.
      file = 'GRcompressed.hdf'
C
C   No compresion
C
      name(1) = 'Nocomp_data'    
      comp_type(1) = COMP_CODE_NONE
C
C   RLE compresion
C
      name(2) = 'Rlcomp_data'    
      comp_type(2) = COMP_CODE_RLE
C
C   Adaptive Huffman compresion
C
      name(3) = 'Hucomp_data'    
      comp_type(3) = COMP_CODE_SKPHUFF
C
C   GZIP compression
C
      name(4) = 'Gzcomp_data'    
      comp_type(4) = COMP_CODE_DEFLATE
C
C   JPEG compression
C
      name(5) = 'Jpcomp_data'    
      comp_type(5) = COMP_CODE_JPEG
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
C---Set pixel value
C
      do 305 i = 1, NCOMP
           pixel_value(i) = 0
305   continue
C
C   Main loop through different compression types
C

C
C     Create and open the file.
C
      file_id = hopen(file, DFACC_CREATE, 0)
      if(file_id .eq. -1) then
         print *, 'hopen failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
C
C     Initiate the GR interface.
C
C      gr_id(i_comp) = mgstart(file_id(i_comp))
C      if(gr_id(i_comp) .eq. -1) then
      gr_id = mgstart(file_id)
      if(gr_id .eq. -1) then
         print *, 'mgstart failed for', i_comp, '-th dataset' 
         err_grcompress = err_grcompress +1
      endif 

      do 1000 i_comp=1, N_COMP_TYPES
C     Define the number of components and dimensions of the image.

      il = MFGR_INTERLACE_PIXEL
      dims(1) = X_LENGTH
      dims(2) = Y_LENGTH

C     Create the data set.

      ri_id(i_comp) = mgcreat(gr_id, name(i_comp), NCOMP,
     .                        DFNT_INT32, il, dims)
      if(ri_id(i_comp) .eq. -1) then
         print *, 'mgcreat failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 

C
C---Fill the image array with the default pixel value
C
      status = mgsnatt(ri_id(i_comp), 'FillValue', DFNT_INT32,
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
      if (i_comp. eq. 5) then       
          comp_prm(1) = JPEG_QUALITY
          comp_prm(2) = JPEG_COMPATIBILITY
      endif
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

C     Write the stored data to the image array.
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
1000  continue
C
C     Terminate access to the GR interface.
C
      status = mgend(gr_id)
      if(status .ne. 0) then
         print *, 'mgend failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
C
C     Close the file.
C
      status = hclose(file_id)
      if(status .ne. 0) then
         print *, 'hclose failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 



C
C     Open the file.
C
      file_id = hopen(file, DFACC_READ, 0)
      if(file_id .eq. -1) then
         print *, 'hopen failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
C
C     Initiate the GR interface and select first data set.
C
      gr_id = mgstart(file_id)
      if(gr_id .eq. -1) then
         print *, 'mgstart failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
      do 2000 i_comp=1, N_COMP_TYPES 
      index = mgn2ndx(gr_id, name(i_comp))
      if(index .eq. -1 ) then
         print *, 'mgn2ndx failed for',  name(i_comp), ' data set'
         err_grcompress = err_grcompress +1
      endif 
      ri_id(i_comp) = mgselct(gr_id, index)
      if(ri_id(i_comp) .eq. -1) then
         print *, 'mgselct failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
         goto 1999
      endif 
C
C  Find out type of compression used and compression parameters.
C
       status = mggcompress(ri_id(i_comp), comp_type_out, comp_prm_out)
	    if (status .eq. -1) then
            print *, 'mggcompress failed for', i, ' -th dataset'
                err_grcompress = err_grcompress + 1
            endif
            if (name(i_comp) .eq. 'Nocomp_data') then
                if (comp_type_out .ne. COMP_CODE_NONE) then
            print *, 'wrong compression type for Nocomp_data dataset'
                err_grcompress = err_grcompress + 1
                endif
            endif
            if (name(i_comp) .eq. 'Rlcomp_data') then
                if (comp_type_out .ne. COMP_CODE_RLE) then
            print *, 'wrong compression type for Rlcomp_data dataset'
                err_grcompress = err_grcompress + 1
                endif
            endif
            if (name(i_comp) .eq. 'Hucomp_data') then
                if (comp_type_out .ne. COMP_CODE_SKPHUFF) then
            print *, 'wrong compression type for Hucomp_data dataset'
                err_grcompress = err_grcompress + 1
                endif
                if (comp_prm_out(1). ne. skphuff_skp_size) then
         print *, 'wrong compression parameter for Hucomp_data dataset'
                err_grcompress = err_grcompress + 1
                endif

            endif
            if (name(i_comp) .eq. 'Gzcomp_data') then
                if (comp_type_out .ne. COMP_CODE_DEFLATE) then
          print *, 'wrong compression type for Gzcomp_data dataset'
                endif
                if (comp_prm_out(1). ne. deflate_level) then
          print *, 'wrong compression parameter for Gzcomp_data dataset'
                err_grcompress = err_grcompress + 1
                endif
            endif
            if (name(i_comp) .eq. 'Jpcomp_data') then
                if (comp_type_out .ne. COMP_CODE_JPEG) then
            print *, 'wrong compression type for Jpcomp_data dataset'
                err_grcompress = err_grcompress + 1
                endif
            goto 1111
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

1111  continue  
C
C     Terminate access to the array.
C
      status = mgendac(ri_id(i_comp))
      if(status .ne. 0) then
         print *, 'mgendac failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
1999  continue
2000  continue
C
C     Terminate access to the GR interface.
C
      status = mgend(gr_id)
      if(status .ne. 0) then
         print *, 'mgend failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
C
C     Close the file.
C
      status = hclose(file_id)
      if(status .ne. 0) then
         print *, 'hclose failed for', i_comp, '-th data set'
         err_grcompress = err_grcompress +1
      endif 
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
     .        gr_id,
     .        file_id
      integer dims(2), start(2), edges(2), stride(2)
      integer err_grwrchunk
      integer i, j, status, il, k, i_comp, index
C      character*13 file(N_COMP_TYPES)
      character*14 file
      character*12 name(N_COMP_TYPES)
C
C---GR interface functions
C
      integer mgstart, mgcreat, mgsnatt,
     .        mgrdimg, mgn2ndx, mgselct, mgendac, mgend, mgfinfo,
     .        mggcompress
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
      integer DFNT_INT32
      integer X_LENGTH, Y_LENGTH
      integer X_CH_LENGTH, Y_CH_LENGTH
      parameter (DFACC_CREATE = 4,
     .           DFACC_READ   = 1,
     .           DFACC_WRITE  = 2)
      parameter (DFNT_INT32   = 24)
      parameter (X_LENGTH     = 6,
     .           Y_LENGTH     = 10,
     .           X_CH_LENGTH  = 3,
     .           Y_CH_LENGTH  = 2) 
C
C---Compression types and parameters arrays 
C
      integer comp_type(N_COMP_TYPES), comp_type_out(N_COMP_TYPES)
      integer comp_typegr
      integer comp_prm(N_COMP_ARG)
      integer comp_prm_out(N_COMP_ARG)
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
      parameter (DEFLATE_LEVEL = 6,
     .           SKPHUFF_SKP_SIZE = 3)
C
C---Data
C 
      integer*4 chunk11(NCOMP* X_CH_LENGTH*Y_CH_LENGTH)
      integer*4 chunk21(NCOMP* X_CH_LENGTH*Y_CH_LENGTH)
      integer*4 chunk52(NCOMP* X_CH_LENGTH*Y_CH_LENGTH)
      integer*4 chunk52_out(NCOMP* X_CH_LENGTH*Y_CH_LENGTH)
      integer*4 data_org(NCOMP* X_LENGTH*Y_LENGTH)
       
      integer*4 image_data_out(NCOMP,X_LENGTH,Y_LENGTH)
      integer*4 data_arr(NCOMP,X_LENGTH,Y_LENGTH)
      integer n_images, n_file_attrs
      equivalence (data_org(1), data_arr(1,1,1))
C
C---Default pixel value
C
      integer*4 pixel_value(3)
C
C---Chunking dimension arrays
C
      integer ch_dims(2)
C
C---We will write/read to four different files corresponding to the
C   different compression types.
C
C   We will try to write to one file GRchunked2.hdf
       file = 'GRchunked2.hdf'
C   No compresion
C
      name(1) = 'Nocomp_data'    
      comp_type(1) = COMP_CODE_NONE
      comp_type_out(1) = 0
C
C   RLE compresion
C
      name(2) = 'Rlcomp_data'    
      comp_type(2) = COMP_CODE_RLE
      comp_type_out(2) = 1 
C
C   Adaptive Huffman compresion
C
      name(3) = 'Hucomp_data'    
      comp_type(3) = COMP_CODE_SKPHUFF
      comp_type_out(3) = 1 
C
C   GZIP compression
C
      name(4) = 'Gzcomp_data'    
      comp_type(4) = COMP_CODE_DEFLATE
      comp_type_out(4) = 1 
C
C  Data initialization
C 
      data chunk11 / 110, 111, 112, 120, 121, 122,
     .                130, 131, 132, 140, 141, 142,
     .                150, 151, 152, 160, 161, 162/
      data chunk21 /  210, 211, 212, 220, 221, 222,
     .                230, 231, 232, 240, 241, 242,
     .                250, 251, 252, 260, 261, 262/
      data chunk52 / 1010, 1011, 1012, 1020, 1021, 1022,
     .                1030, 1031, 1032, 1040, 1041, 1042,
     .                1050, 1051, 1052, 1060, 1061, 1062/
      data data_org /
     .     110, 111, 112, 120, 121, 122, 210, 211, 212, 220, 221, 222,
     .     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     0, 0, 0, 0, 0, 0, 130, 131, 132, 140, 141, 142,
     .     230, 231, 232, 240, 241, 242, 0, 0, 0, 0, 0, 0,
     .     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
     .     150, 151, 152, 160, 161, 162, 250, 251, 252, 260, 261, 262,
     .     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     0, 0, 0, 0, 0, 0, 1010, 1011, 1012, 1020, 1021, 1022, 
     .     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
     .     1030, 1031, 1032, 1040, 1041, 1042, 0, 0, 0, 0, 0, 0,
     .     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     .     0, 0, 0, 0, 0, 0, 1050, 1051, 1052, 1060, 1061, 1062/
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
C
C     Create and open the file.
C
C
C     Initiate the GR interface.
C
      file_id = hopen(file, DFACC_CREATE, 0)
      gr_id = mgstart(file_id)

      do 1000 i_comp=1, N_COMP_TYPES

C     Define the number of components and dimensions of the image.

      il = MFGR_INTERLACE_PIXEL
      dims(1) = X_LENGTH
      dims(2) = Y_LENGTH

C     Create the data set.

      ri_id(i_comp) = mgcreat(gr_id, name(i_comp), NCOMP,
     .                        DFNT_INT32, il, dims)

C
C---Set pixel value
C
      pixel_value(1) = 0
      pixel_value(2) = 0
      pixel_value(3) = 0
C
C---Fill the image array with the default pixel value
C
      status = mgsnatt(ri_id(i_comp), 'FillValue', DFNT_INT32,
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
1000  continue
C
C     Terminate access to the GR interface.
C
C      status = mgend(gr_id(i_comp))
      status = mgend(gr_id)
      if(status .ne. 0) then
         print *, 'mgend failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 
C
C     Close the file.
C
      status = hclose(file_id)
      if(status .ne. 0) then
         print *, 'hclose failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 

      file_id = hopen(file, DFACC_READ, 0)
      if(file_id .eq. -1) then
         print *, 'hopen failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 
C
C     Initiate the GR interface and select first data set.
C
      gr_id = mgstart(file_id)
      if(gr_id .eq. -1) then
         print *, 'mgstart failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 
C
C     Find number of images in the file ( should be 4)
C
      status = mgfinfo(gr_id, n_images, n_file_attrs)
      if(status .ne. 0) then
         print *, 'mgfinfo failed '
         err_grwrchunk = err_grwrchunk +1
      goto 2222
      endif 
      if(n_images .ne. 4) then
         print *, 'Wrong number of images returned '
         err_grwrchunk = err_grwrchunk +1
      goto 2222
      endif 
      if(n_file_attrs .ne. 0) then
         print *, 'Wrong number of file attributes returned '
         err_grwrchunk = err_grwrchunk +1
      endif 

      do 2000 i_comp=1, n_images 

C
C     Find an index using image's name.
C
      index = mgn2ndx(gr_id, name(i_comp))
      if(index .lt. 0 .or. index .gt. 3) then
         print *, 'Wrong index range '
         err_grwrchunk = err_grwrchunk +1
      goto 2222 
      endif 
      ri_id(i_comp) = mgselct(gr_id, index)
      if( ri_id(i_comp) .eq. -1) then
         print *, 'mgselct failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
         goto 2000 
      endif 
      status = mggcompress(ri_id(i_comp), comp_typegr, comp_prm_out)
      if (status .eq. -1) then
      print *, 'mggcompress failed for', i, ' -th dataset'
         err_grwrchunk = err_grwrchunk +1
      endif
      
            if (name(i_comp) .eq. 'Nocomp_data') then
                if (comp_typegr .ne. COMP_CODE_NONE) then
            print *, 'wrong compression type for Nocomp_data dataset'
                err_grwrchunk = err_grwrchunk +1
                endif
            endif
            if (name(i_comp) .eq. 'Rlcomp_data') then
                if (comp_typegr .ne. COMP_CODE_RLE) then
            print *, 'wrong compression type for Rlcomp_data dataset'
                err_grwrchunk = err_grwrchunk +1
                endif
            endif
            if (name(i_comp) .eq. 'Hucomp_data') then
                if (comp_typegr .ne. COMP_CODE_SKPHUFF) then
            print *, 'wrong compression type for Hucomp_data dataset'
                err_grwrchunk = err_grwrchunk +1
                endif
                if (comp_prm_out(1). ne. skphuff_skp_size) then
         print *, 'wrong compression parameter for Hucomp_data dataset'
                err_grwrchunk = err_grwrchunk +1
                endif

            endif
            if (name(i_comp) .eq. 'Gzcomp_data') then
                if (comp_typegr .ne. COMP_CODE_DEFLATE) then
          print *, 'wrong compression type for Gzcomp_data dataset'
                endif
                if (comp_prm_out(1). ne. deflate_level) then
          print *, 'wrong compression parameter for Gzcomp_data dataset'
                err_grwrchunk = err_grwrchunk +1
                endif
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
1999  continue
2000  continue

2222  continue
C
C     Terminate access to the GR interface.
C
      status = mgend(gr_id)
      if(status .ne. 0) then
         print *, 'mgend failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 
C
C     Close the file.
C
      status = hclose(file_id)
      if(status .ne. 0) then
         print *, 'hclose failed for', i_comp, '-th data set'
         err_grwrchunk = err_grwrchunk +1
      endif 
      return
      end
