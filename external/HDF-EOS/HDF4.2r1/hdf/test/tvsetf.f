C**********************************************************************
C* NCSA HDF
C*
C* Software Development Group
C*
C* National Center for Supercomputing Applications
C*
C* University of Illinois at Urbana-Champaign
C*
C* 605 E. Springfield, Champaign IL 61820
C*
C*
C*
C* For conditions of distribution and use, see the accompanying
C*
C* hdf/COPYING file.
C*
C*
C*
C*********************************************************************
C $Id: tvsetf.f,v 1.24 2005/02/09 22:55:28 epourmal Exp $
C*********************************************************************
C      test vset Fortran data types
C
      subroutine tvsetf (number_failed)
C Test Program:
C   Tests the vdata and vgroup interface
C Input file: none
C Output file: tvsetf1.hdf

      implicit none
      include 'fortest.inc'
      include '../src/hdf.inc'
      include '../src/dffunc.inc'

      integer number_failed
      character*20 myname
      parameter (myname = 'vsetf')

      integer fid1, vgid1, vgid2, vsid1, vsid2
      integer vgref1, vsref1, vsref2, vref, vsref22
      integer vsize, found
      integer ret,ntrs,i,il,nelts
      integer*4 dbuf(320),idbuf(320),ddata4(10),iddata4(10)
      integer*2 ddata1(10), iddata1(10)
      integer*2 ddata2(10),iddata2(10)
      integer*4 tags(10), refs(10)
      real      fdata(10), ifdata(10)
      double precision gdata1(10)
      double precision igdata1(10)
      double precision geps
      real   feps

      character*11 fn1
      character*64 vname
      character*9  fields1
      character*10 fields2
      character*31 fields3
      character*15 cdata, icdata
      character*7  field_name
      parameter (
     +           feps = 1.0E-5,
     +           geps = 1.0D-9
     +          )
C-------------------------------------------------------------------------
C    These definitions are added to test hglibver and hgfilver functions
C
      integer major_v, minor_v, release
      character*80 hdfstring
C-------------------------------------------------------------------------     
      DATA ddata1/10,11,12,13,14,15,16,17,18,19/
      DATA ddata2/20,21,22,23,24,25,26,27,28,29/
      DATA ddata4/40,41,42,43,44,45,46,47,48,49/
      DATA fdata/5.0,5.1,5.2,5.3,5.4,5.5,5.6,5.7,5.8,5.9/
      DATA gdata1/6.0,6.1,6.2,6.3,6.4,6.5,6.6,6.7,6.8,6.9/

      call ptestban('Testing', myname)
      cdata = 'abcdebcdefcdefg'
      fn1 = 'tvsetf1.hdf'

C Open the file
      fid1 = hopen(fn1, DFACC_CREATE, 0)
      call VRFY(fid1,'hopen',number_failed)
      ret = vfstart(fid1)
      call VRFY(ret,'vfstart',number_failed)
C Create a vgroup
      call MESSAGE(5,'Creating a vgroup')
      vgid1 = vfatch(fid1, -1, 'w')
      call VRFY(vgid1, 'vfatch', number_failed)
      ret = vfsnam(vgid1, 'Top Vgroup')
      call VRFY(ret,'vfsnam',number_failed)
      ret = vfscls(vgid1, 'Test Object')
      call VRFY(ret,'vfscls',number_failed)

C Add a vgroup to it
      call MESSAGE(5,'Add a vgroup to the Top vgroup')
      vgid2 = vfatch(fid1, -1, 'w')
      call VRFY(vgid2, 'vfatch', number_failed)
      ret = vfinsrt(vgid1, vgid2)
      call VRFY(ret, 'vfinsrt', number_failed)

      ret = vfdtch(vgid1)
      call VRFY(ret,'vfdtch',number_failed)
      ret = vfdtch(vgid2)
      call VRFY(ret,'vfdtch',number_failed)

      ret = vfend(fid1)
      call VRFY(ret,'vfend',number_failed)
      ret = hclose(fid1)
      call VRFY(ret,'hclose',number_failed)

C      add a vdatas in vgroup1
      call MESSAGE(5,'Creating a char vdata')
      fid1 =  hopen(fn1, DFACC_RDWR, 0)
      call VRFY(fid1,'hopen',number_failed)
      ret = vfstart(fid1)
      call VRFY(ret,'vfstart',number_failed)
      vgref1 = vfgid(fid1, -1)
      call VRFY(vgref1,'vfgid',number_failed)
C
C     Find group with the name 'Top Vgroup'
C
      ret = vfind(fid1, 'Top Vgroup')
      if (ret .le. 0) then
          number_failed = number_failed + 1
          call MESSAGE(3, 'Top Vgroup is not found. ')
      endif 
      vgid1 = vfatch(fid1, vgref1, 'w')
      call VRFY(vgid1,'vfatch',number_failed)
C      create a single field (char) vdata
      vsid1 = vsfatch(fid1, -1, 'w')
      call VRFY(vsid1,'vsfatch',number_failed)
      ret = vsffdef(vsid1, 'char type', DFNT_CHAR, 5)
      call VRFY(ret,'vsffdef',number_failed)
      ret = vsfsfld(vsid1, 'char type')
      call VRFY(ret,'vsfsfld',number_failed)
C      Use vsfwrtc to write the values
      ret = vsfwrtc(vsid1, cdata, 3, FULL_INTERLACE)
      call VRFY(ret,'vsfwrtc',number_failed)
      ret = vsfdtch(vsid1)
      call VRFY(ret,'vsfdtch',number_failed)
C      create a single field int16 vdata
      call MESSAGE(5,'Creating an int16 vdata')
      vsid1 = vsfatch(fid1, -1, 'w')
      call VRFY(vsid1,'vsfatch',number_failed)
      ret = vsffdef(vsid1, 'int16 type', DFNT_INT16, 2)
      call VRFY(ret,'vsffdef',number_failed)
      ret = vsfsfld(vsid1, 'int16 type')
      call VRFY(ret,'vsfsfld',number_failed)
C      Use vsfwrt to write the values
      ret = vsfwrt(vsid1, ddata1, 5, FULL_INTERLACE)
      call VRFY(ret,'vsfwrt',number_failed)
      ret = vsfdtch(vsid1)
      call VRFY(ret,'vsfdtch',number_failed)
C      use vhfscd and vhfscdm to create char vdata
      call MESSAGE(5,'Using vhfscd/vsfscdm to create vdatas')
      ret = vhfscd(fid1,'char1',cdata,5,DFNT_CHAR,'c1','c')
      call VRFY(ret,'vhfscd',number_failed)
      ret = vhfscdm(fid1,'char3',cdata,5,DFNT_CHAR,'c3','s',3)
      call VRFY(ret,'vhfscdm',number_failed)

C      create a five-field vdata, 2*int32,2*double,2*float32,
C             2*int16 and 3*char type 
      call MESSAGE(5,'Creating a five_field vdata')
      vsid2 = vsfatch(fid1, -1, 'w')
      call VRFY(vsid2,'vsfatch',number_failed)
      ret = vsfsnam(vsid2, 'mixed type')
      call VRFY(ret,'vsfsnam',number_failed)
      ret = vsfscls(vsid2, 'test NT')
      call VRFY(ret,'vsfscls',number_failed)
      ret = vsffdef(vsid2, 'int32', DFNT_INT32, 2)
      call VRFY(ret,'vsffdef',number_failed)
      ret = vsffdef(vsid2, 'double', DFNT_FLOAT64, 2)
      call VRFY(ret,'vsffdef',number_failed)
      ret = vsffdef(vsid2, 'float32', DFNT_FLOAT32, 2)
      call VRFY(ret,'vsffdef',number_failed)
      ret = vsffdef(vsid2, 'int16', DFNT_INT16, 2)
      call VRFY(ret,'vsffdef',number_failed)
      ret = vsffdef(vsid2, 'char', DFNT_CHAR, 3)
      call VRFY(ret,'vsffdef',number_failed)
      ret = vsfsfld(vsid2, 'int32,double,float32,int16,char')
      call VRFY(ret,'vsfsfld',number_failed)
C     pack the fields into data buf
      ret = vsfnpak(vsid2, HDF_VSPACK,
     +             'int32,double,float32,int16,char', dbuf,
     +             320*4,5,'int32',ddata4)
      call VRFY(ret,'vsfnpak',number_failed)
      ret = vsfnpak(vsid2, HDF_VSPACK,
     +             ' ', dbuf, 320*4, 5,'double',gdata1)
      call VRFY(ret,'vsfnpak',number_failed)
      ret = vsfnpak(vsid2, HDF_VSPACK,
     +             ' ', dbuf, 320*4, 5,'float32',fdata)
      call VRFY(ret,'vsfnpak',number_failed)
      ret = vsfnpak(vsid2, HDF_VSPACK,
     +             ' ', dbuf, 320*4,5,'int16',ddata2)
      call VRFY(ret,'vsfnpak',number_failed)
      ret = vsfcpak(vsid2, HDF_VSPACK,
C    +             'int32,double,float32,int16,char', dbuf,
     +             ' ', dbuf, 320*4, 5,'char',cdata)
      call VRFY(ret,'vsfcpak',number_failed)
      ret = vsfwrt(vsid2, dbuf, 5, FULL_INTERLACE)
      call VRFY(ret,'vsfwrt',number_failed)
      ret = vsfdtch(vsid2)
      call VRFY(ret,'vsfdtch',number_failed)

C     insert vdata1 into vgroup1
      call MESSAGE(5,'Inserting vdata1 into top vgroup')
      vsref1 = vsfgid(fid1, -1)
      call VRFY(vsref1, 'vsfgid', number_failed)
      vsref2  = vsffnd(fid1, 'mixed type')
      call VRFY(vsref2, 'vsffnd', number_failed)
      ret = vfadtr(vgid1, DFTAG_VH, vsref1)
      call VRFY(ret, 'vfadtr', number_failed)
      ret = vfdtch(vgid1)
      call VRFY(ret,'vfdtch',number_failed)
      ret = vfend(fid1)
      call VRFY(ret,'vfend',number_failed)
      ret = hclose(fid1)
      call VRFY(ret,'hclose',number_failed)

C     read data back
      call MESSAGE(5,'Readng data back')
      fid1 = hopen(fn1, DFACC_RDWR, 0)
      call VRFY(fid1,'hopen',number_failed)
      ret = vfstart(fid1)
      call VRFY(ret,'vfstart',number_failed)
      vgref1 = vfgid(fid1, -1)
      call VRFY(vgref1, 'vfgid', number_failed)
      vgid1 = vfatch(fid1, vgref1,'w')
      call VRFY(vgid1, 'vfatch', number_failed)
      ntrs = vfgttrs(vgid1, tags, refs, 10)
      call VRFY(ntrs, 'vfgttrs', number_failed)
      if (ntrs .ne. 2) then
         number_failed = number_failed + 1
         call MESSAGE(3,'Wrong number of tag/refs. ')
      endif
C     look for the first vdata
      found = FALSE
      vref = -1
      do 20 i = 1, ntrs
         if (found .eq. FALSE) then
             vref = vfgnxt(vgid1,vref)
             call VRFY(vref, 'vref', number_failed)
             found = vfisvs(vgid1, vref)
         endif
20    continue
      if (found .eq. TRUE) then
         vsid1 = vsfatch(fid1, vref, 'w')
         call VRFY(vsid1,'vsfatch',number_failed)
         ret = vsfinq(vsid1, nelts,il,fields1,vsize,vname)
         call VRFY(ret,'vsfinq',number_failed)
         if (nelts .ne. 3) then
             number_failed = number_failed + 1
             call MESSAGE(3,'Wrong number of records. ') 
         endif
         if (il .ne. FULL_INTERLACE) then
             number_failed = number_failed + 1
              call MESSAGE(3,'Wrong interlace ')
         endif
         if (fields1 .ne. 'char type') then
             number_failed = number_failed + 1
             call MESSAGE(3,'Wrong field names. ')
         endif
         if (vsize .ne. 5) then
             number_failed = number_failed + 1
             call MESSAGE(3,'Wrong vsize. ')
         endif
         ret = vsfsfld(vsid1, 'char type')
         call VRFY(ret,'vsfsfld',number_failed)
         ret = vsfrdc(vsid1, icdata, 3, FULL_INTERLACE)
         call VRFY(ret,'vsfrdc',number_failed)
         if (icdata .ne. 'abcdebcdefcdefg') then
             number_failed = number_failed + 1
             call MESSAGE(3,'Wrong icdata. Correct: abcdebcdefcdefg')
         endif
         ret = vsfdtch(vsid1)
         call VRFY(ret,'vsfdtch',number_failed)
C     not found, print error message
      else
         call MESSAGE(3,'Not found char type vdata. ')
      endif

C     read the second vdata
      call MESSAGE(5,'Reading the int16 vdata')
      vsref1 = vsfgid(fid1, vref)
      call VRFY(vsref1, 'vsfgid', number_failed)
      vsid1 = vsfatch(fid1, vsref1, 'w')
      call VRFY(vsid1,'vsfatch',number_failed)
      ret = vsfinq(vsid1, nelts,il, fields2,vsize,vname)
      call VRFY(ret,'vsfinq',number_failed)
      if (nelts .ne. 5) then
          number_failed = number_failed + 1
          call MESSAGE(3,'Wrong number of records. ')
      endif
      if (il .ne. FULL_INTERLACE) then
          number_failed = number_failed + 1
          call MESSAGE(3,'Wrong interlace. ')
      endif
      if (fields2 .ne. 'int16 type') then
          number_failed = number_failed + 1
          call MESSAGE(3,'Wrong fields. ')
      endif
      ret = vsfsfld(vsid1, 'int16 type')
      call VRFY(ret,'vsfsfld',number_failed)
      ret = vsfrd(vsid1, iddata1, 5, FULL_INTERLACE)
      call VRFY(ret,'vsfrd',number_failed)
      do 40 i=1,10 
          if (iddata1(i) .ne. (9+i)) then
             number_failed = number_failed + 1
             call MESSAGE(3,'Wrong data. ')  
          endif
40    continue
      ret = vsfdtch(vsid1)
      call VRFY(ret,'vsfdtch',number_failed)
C     read the 'c1' vdata
      vsref1 = vsffnd(fid1, 'c1')
      call VRFY(vsref1, 'vsffnd', number_failed)
      vsid1 = vsfatch(fid1, vsref1, 'w')
      call VRFY(vsid1, 'vsfatch', number_failed)
      ret = vsfsfld(vsid1, 'char1')
      call VRFY(ret, 'vsfsfld', number_failed)
      ret = vsfrd(vsid1, idbuf, 5,FULL_INTERLACE)
      call VRFY(ret, 'vsfrd', number_failed)
      icdata = '               '
      ret = vsfcpak(vsid1,HDF_VSUNPACK,' ',idbuf,320*4,
     +        5, ' ', icdata)
      call VRFY(ret, 'vsfcpak', number_failed)
      if (icdata .ne. 'abcde          ')  then
          number_failed = number_failed + 1
          call MESSAGE(3,'Wrong icdata. ')
      endif
      ret = vsfdtch(vsid1)
      call VRFY(ret,'vsfdtch',number_failed)
C     read the 'c3' vdata
      vsref1 = vsffnd(fid1, 'c3')
      call VRFY(vsref1, 'vsffnd', number_failed)
      vsid1 = vsfatch(fid1, vsref1, 'w')
      call VRFY(vsid1, 'vsfatch', number_failed)
      ret = vsfsfld(vsid1, 'char3') 
      call VRFY(ret, 'vsfsfld', number_failed)
      ret = vsfrd(vsid1, idbuf, 5,FULL_INTERLACE)
      call VRFY(ret, 'vsfrd', number_failed)
      icdata = '               '
      ret = vsfcpak(vsid1,HDF_VSUNPACK,' ',idbuf,320*4,
     +        5, ' ', icdata)
      call VRFY(ret, 'vsfcpak', number_failed)
      if (icdata .ne. 'abcdebcdefcdefg')  then
          number_failed = number_failed + 1
          call MESSAGE(3,'Wrong data. ')
      endif 
      ret = vsfdtch(vsid1)
      call VRFY(ret,'vsfdtch',number_failed)
 
C     read the 'mixed type' vdata
      vsref2 = vsffnd(fid1, 'mixed type')
      call VRFY(vsref2, 'vsffnd', number_failed)
      vsref22 = vsffcls(fid1, 'test NT')
      call VRFY(vsref22, 'vsffcls', number_failed)
      vsid2 = vsfatch(fid1, vsref2, 'w')
      call VRFY(vsid2, 'vsfatch', number_failed)
C
C     This piece of the code exercises VF interface function
C     Added by E. Pourmal 1/22/99
C
C      
C     Find the total number of the fields in the vdata.
C
      ret = vfnflds (vsid2)
      if (ret .ne. 5) then
          number_failed = number_failed + 1
          call MESSAGE(3, 'Wrong number of the vdata fileds. ')
      endif 
C
C     Find the datatype of the first field (should be DFNT_INT32)
C
      ret = vfftype(vsid2, 0)
      if (ret. ne. DFNT_INT32) then
          number_failed = number_failed + 1
          call MESSAGE(3, 'Wrong field datatype returned. ')
      endif
C
C     Find the order of the second field (should be 2)
C
      ret = vffordr(vsid2, 1)
      if (ret .ne. 2) then
          number_failed = number_failed + 1
          call MESSAGE(3, 'Wrong field order returned. ') 
      endif
C
C     Find the name of the third field (should be 'float32')
C
      ret = vffname(vsid2, 2, field_name)
      if (ret .ne. 0 .or. field_name .ne. 'float32') then
          number_failed = number_failed + 1
          call MESSAGE(3, 'Cannot return name of the field. ') 
      endif
C
C     Find the size as stored in memory of the fourth vdata field.
C
      ret = vffisiz(vsid2, 3)
      call VRFY(ret, 'vffisiz', number_failed)
C
C     Find th esize as stored in file of the fifth vdata field.
C
      ret = vffesiz(vsid2, 4)
      call VRFY(ret, 'vffesiz', number_failed)
C
C     The end of the VF interface test.  Two last calls should be tested
C     more carefully, i.e. what the correct ret values are ??????????
C

      ret = vsfinq(vsid2, nelts,il, fields3,vsize,vname)
      call VRFY(ret, 'vsfinq', number_failed)
      if (nelts .ne. 5) then
          number_failed = number_failed + 1
          call MESSAGE(3,'Wrong number of records. ')
      endif
      if (il .ne. FULL_INTERLACE) then
          number_failed = number_failed + 1
          call MESSAGE(3, 'Wrong interlace. ')
      endif
      if (fields3 .ne. 'int32,double,float32,int16,char') then
          number_failed = number_failed + 1
          call MESSAGE(3,'Wrong fields. ')
      endif
      ret = vsfsfld(vsid2, 'int32,double,float32,int16,char')
      call VRFY(ret, 'vsfsfld', number_failed)
      ret = vsfrd(vsid2, idbuf, 3, FULL_INTERLACE)
      call VRFY(ret, 'vsfrd', number_failed)
      ret = vsfnpak(vsid2, HDF_VSUNPACK,
     +             ' ', idbuf, 320*4,3,'int32',iddata4)
      call VRFY(ret, 'vsfnpak', number_failed)
      ret = vsfnpak(vsid2, HDF_VSUNPACK,
     +             ' ', idbuf, 320*4,3,'double',igdata1)
      call VRFY(ret, 'vsfnpak', number_failed)
      ret = vsfnpak(vsid2, HDF_VSUNPACK,
     +             ' ', idbuf, 320*4,3,'float32',ifdata)
      call VRFY(ret, 'vsfnpak', number_failed)
      ret = vsfnpak(vsid2, HDF_VSUNPACK,
     +             ' ', idbuf, 320*4,3,'int16',iddata2)
      call VRFY(ret, 'vsfnpak', number_failed)
      icdata = '               '
      ret = vsfcpak(vsid2, HDF_VSUNPACK,
     +             'int32,double,float32,int16,char', idbuf,
     +             320*4,3,'char',icdata)
      call VRFY(ret, 'vsfcpak', number_failed)

      do 45 i=1,3
         if (iddata4(i) .ne. (39+i)) then
             number_failed = number_failed + 1
             call MESSAGE(3,'Wrong data. ')
         endif
         if (iddata2(i) .ne. (19+i)) then
             number_failed = number_failed + 1
             call MESSAGE(3,'Wrong data. ')
         endif
45    continue
      do 50 i = 1, 3
         if (abs(gdata1(i) - igdata1(i)) .GE.
     +        gdata1(i)*geps)  then
             number_failed = number_failed + 1
             call MESSAGE(3,'Wrong data. ')
         endif
         if (abs(fdata(i) - ifdata(i)) .GE.
     +        fdata(i)*feps)  then
             number_failed = number_failed + 1
             call MESSAGE(3,'Wrong data. ')
         endif
50    continue
      if (icdata .ne. 'abcdebcde      ') then
          number_failed = number_failed + 1
          call MESSAGE(3,'Wrong data. ')
      endif
C     read field 'char' only, test pckfld ' '
      ret = vsfsfld(vsid2, 'char')
      call VRFY(ret, 'vsfsfld', number_failed)
      ret = vsfrd(vsid2, idbuf, 2, FULL_INTERLACE)
      call VRFY(ret, 'vsfrd', number_failed)
      icdata = '               '
      ret = vsfcpak(vsid2, HDF_VSUNPACK,
     +             'char', idbuf, 320*4,2,' ',icdata)
      call VRFY(ret, 'vsfcpak', number_failed)
      if (icdata .ne. 'fcdefg         ') then
          number_failed = number_failed + 1
          call MESSAGE(3,'Wrong data. ')
      endif

      ret = vsfdtch(vsid2)
      call VRFY(ret,'vsfdtch',number_failed)
      ret = vfdtch(vgid1)
      call VRFY(ret,'vsfdtch',number_failed)
      ret = vfend(fid1)
      call VRFY(ret,'vfend',number_failed)
      ret = hclose(fid1)
      call VRFY(ret, 'hclose', number_failed)
C
C     Testing deleting a tag/ref pair from Vgroups using vfdtr()
C     Not as extensive as the C-version of the similar test because
C     for now some the fortran versions of Vgroup routines are missing.
C
C Open the file for writing
      fid1 = hopen(fn1, DFACC_RDWR, 0)
      call VRFY(fid1,'hopen',number_failed)
C
C-----Test hglibver and hgfilver functions.
C
      ret = hglibver(major_v, minor_v, release, hdfstring)
      call VRFY(ret, 'hglibver', number_failed)
      ret = hgfilver(fid1, major_v, minor_v, release, hdfstring)
      call VRFY(ret, 'hgfilver', number_failed)

      ret = vfstart(fid1)
      call VRFY(ret,'vfstart',number_failed)

C Create a vgroup to add a bogus element to.
      call MESSAGE(5,'Creating a vgroup')
      vgid1 = vfatch(fid1, -1, 'w')
      call VRFY(vgid1, 'vfatch', number_failed)
      ret = vfsnam(vgid1, 'Vgroup to delete from')
      call VRFY(ret,'vfsnam',number_failed)
      ret = vfscls(vgid1, 'Vgroup to delete from')
      call VRFY(ret,'vfscls',number_failed)

C     Add a bogus element to the vgroup
      ret = vfadtr(vgid1, 5000, 1234)
      call VRFY(ret, 'vfadtr', number_failed)

C     Now delete it again just make sure the call does not fail
C     The C-version of the tests does a better job overall.

      ret = vfdtr(vgid1, 5000, 1234)
      call VRFY(ret, 'vfdtr', number_failed)

C     Detach from vgroup
      ret = vfdtch(vgid1)
      call VRFY(ret,'vfdtch',number_failed)

C     Close Vxxx interface down
      ret = vfend(fid1)
      call VRFY(ret,'vfend',number_failed)

C     Close file
      ret = hclose(fid1)
      call VRFY(ret,'hclose',number_failed)

      return
      end
C
C This subroutine tests vsfsetblsz, vsfsetnm, vsfgetinfo functions
C
C It creates and writes multi-component Vdata and single-component Vdata; 
C then it sets sizes and number of blocks
C for multi-component Vdata's link block and appends to the Vdata.
C File is then closed and reponened once more; block information
C is retrieved for the multi-component Vdata.
C
      subroutine tvsetblock(number_failed)
      implicit none
      character*20 myname
      parameter (myname = 'vsetblock')
      integer number_failed
C
C     Parameter declaration
C
      character*22 FILE_NAME
      character*9  CLASS1_NAME
      character*9  CLASS2_NAME
      character*11 VDATA1_NAME
      character*12 VDATA2_NAME
      character*22 FIELD1_NAME
      character*21 FIELD2_NAME
      integer      N_RECORDS_1, N_RECORDS_2
      integer      ORDER_2
      integer      FULL_INTERLACE
C
      parameter (FILE_NAME   = 'Vdatas_blocks_test.hdf',
     +           CLASS1_NAME = '5x1 Array',
     +           CLASS2_NAME = '6x4 Array',
     +           VDATA1_NAME = 'First Vdata',
     +           VDATA2_NAME = 'Second Vdata',
     +           FIELD1_NAME = 'Single-component Field',
     +           FIELD2_NAME = 'Multi-component Field')
      parameter (N_RECORDS_1 = 5,
     +           N_RECORDS_2 = 256,
     +           ORDER_2     = 2,
     +           FULL_INTERLACE = 0)               
   
      integer DFACC_WRITE, DFNT_CHAR8, DFNT_INT32
      parameter (DFACC_WRITE = 2,
     +           DFNT_CHAR8  = 4,
     +           DFNT_INT32  = 24)
      integer BLOCK_SIZE, NUM_BLOCKS
      parameter (BLOCK_SIZE = 256,
     +           NUM_BLOCKS = 3)
      
C
C     Function declaration
C
      integer hopen, hclose
      integer vfstart, vhfscd, vhfsdm, vfend 
      integer vsfsetblsz, vsfsetnmbl, vsfgetblinfo
      integer vsfatch, vsfdtch, vsffnd, vsfwrt, vsfrd, vsfseek

C
C**** Variable declaration *******************************************
C
      integer   status, return_flag
      integer   file_id
      integer   vdata1_ref, vdata2_ref 
      integer   vdata2_id 
      character vdata1_buf(N_RECORDS_1)
      integer   vdata2_buf(ORDER_2, N_RECORDS_2)
      integer   buf(ORDER_2)
      integer   i, j
      integer   block_size_out, num_blocks_out
      integer   n_records
      data vdata1_buf /'V','D','A','T','A'/
C
C**** End of variable declaration ************************************
      call ptestban('Testing', myname)
C
C     Initialize vdata2_buf
C
      do i = 1, N_RECORDS_2
         do j = 1, ORDER_2
            vdata2_buf(j,i) = j + i
         enddo
      enddo
C
C
C     Open the HDF file for writing.
C
      file_id = hopen(FILE_NAME, DFACC_WRITE, 0)
      call VRFY(file_id,'hopen',number_failed)
C
C     Initialize the VS interface.
C
      status = vfstart(file_id) 
      call VRFY(status,'vfstart',number_failed)
C
C     Create multi-component vdata and populate it with data from vdata2_buf array.
C     
      vdata2_ref = vhfsdm(file_id, FIELD2_NAME, vdata2_buf, N_RECORDS_2,
     +                    DFNT_INT32, VDATA2_NAME, CLASS2_NAME,
     +                    ORDER_2)
      call VRFY(vdata2_ref,'vhfsdm',number_failed)
C
C     Create single-component vdata and populate it with data from vdata1_buf array.
C     
      vdata1_ref = vhfscd(file_id, FIELD1_NAME, vdata1_buf, N_RECORDS_1,
     +                    DFNT_CHAR8, VDATA1_NAME, CLASS1_NAME)
      call VRFY(vdata1_ref,'vhfscd',number_failed)
C
C     Terminate access to the VS interface and close the HDF file.
C
      status = vfend(file_id)
      call VRFY(status,'vfend',number_failed)
      status = hclose(file_id)
      call VRFY(status,'hclose',number_failed)
      if (number_failed .gt. 0) goto 1000
C
C     Reopen the HDF file for writing.
C
      file_id = hopen(FILE_NAME, DFACC_WRITE, 0)
      call VRFY(file_id,'hopen',number_failed)
C
C     Initialize the VS interface.
C
      status = vfstart(file_id) 
      call VRFY(status,'vfstart',number_failed)
C
C     Attach to the multi-component Vdata
C
      vdata2_ref = vsffnd(file_id, VDATA2_NAME) 
      call VRFY(vdata2_ref,'vsffnd',number_failed)
      vdata2_id = vsfatch(file_id, vdata2_ref, 'w')
      status = vsfsetblsz(vdata2_id, BLOCK_SIZE) 
      call VRFY(status,'vsfsetblsz',number_failed)
      status = vsfsetnmbl(vdata2_id, NUM_BLOCKS) 
      call VRFY(status,'vsfsetblnm',number_failed)
C
C     Append to the multi-component Vdata 
C
      n_records = vsfseek(vdata2_id, N_RECORDS_2-1)
      call VRFY(n_records,'vsfseek',number_failed)
      n_records = 1
      status = vsfrd(vdata2_id, buf, n_records, FULL_INTERLACE)
      call VRFY(status,'vsfrd',number_failed)
       
      n_records = N_RECORDS_2
      status = vsfwrt(vdata2_id, vdata2_buf, n_records,
     +                FULL_INTERLACE)
      if (status .ne. N_RECORDS_2) then
          number_failed = number_failed + 1
          call MESSAGE(3,'Wrong number of records added ')
          call MESSAGE(3,'Append to multi-component Vdata failed ')
      endif
      call VRFY(vdata2_id,'vsfatch',number_failed)
      status = vsfgetblinfo(vdata2_id, block_size_out, num_blocks_out)
      if (block_size_out .ne. BLOCK_SIZE .or. 
     +    num_blocks_out .ne. NUM_BLOCKS) then
          call MESSAGE(3,'Linked-block info is wrong ')
          number_failed = number_failed + 1
      endif
      status = vsfdtch(vdata2_id)
      call VRFY(status,'vsfdtch',number_failed)
      status = vfend(file_id)
      call VRFY(status,'vfend',number_failed)
      status = hclose(file_id)
      call VRFY(status,'hclose',number_failed)

1000  continue
      return
      end
