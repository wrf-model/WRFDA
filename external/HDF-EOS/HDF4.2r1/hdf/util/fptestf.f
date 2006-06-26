       program fptest

c
c  This program creates six floating point files that can be 
c  used to test the fp2hdf program.  [Note: the 32-bit floating
c  point files are omitted for Cray (UNICOS) systems]
c 
c  May 23, 1990
c  Glen A. Mortensen, gam@inel.gov
c
c  The file names that are created are named:
c      ftxtr2, type 'TEXT', size 3x4
c      ftxtr3, type 'TEXT', size 3x4x5
c      fb32r2, type 'FP32', size 3x4
c      fb32r3, type 'FP32', size 3x4x5
c      fb64r2, type 'FP64', size 3x4
c      fb64r3, type 'FP64', size 3x4x5
c
c   row    values start at 11 and increment by 1 => 11, 12, 13
c   column values start at 21 and increment by 2 => 21, 23, 25, 27
c   plane  values start at 51 and increment by 5 => 51, 56, 61, 66, 71
c
c   data element value = row value + column value [+ plane value, if rank=3]
c

       integer    nrow    , ncol    , npln    , ione
       parameter (nrow = 3, ncol = 4)
       integer    i       , j       , k       , nrec
       integer    funit

       real       b32r2(3,4)    , b32r3(3,4,5)
       real       row4(3)       , col4(4)       , pln4(5)
       real       rowo4         , colo4         , plno4
       real       rowi4         , coli4         , plni4
       parameter (rowo4 = 11.0e0, colo4 = 21.0e0, plno4 = 51.0e0)
       parameter (rowi4 =  1.0e0, coli4 =  2.0e0, plni4 =  5.0e0)
       real       ezero
       parameter (ezero  =  0.0e0)

C#ifndef UNICOS
C       double precision     b64r2(3,4)    , b64r3(3,4,5)
C       double precision    row8(3)       , col8(4)       , pln8(5)
C       double precision    rowo8         , colo8         , plno8
C       double precision    rowi8         , coli8         , plni8
C       parameter       (rowo8 = 11.0d0, colo8 = 21.0d0, plno8 = 51.0d0)
C       parameter       (rowi8 =  1.0d0, coli8 =  2.0d0, plni8 =  5.0d0)
C       double precision    dzero
C       parameter       (dzero =  0.0d0)
C#else
       real       b64r2(3,4)    , b64r3(3,4,5)
       real       row8(3)       , col8(4)       , pln8(5)
       real       rowo8         , colo8         , plno8
       real       rowi8         , coli8         , plni8
       parameter (rowo8 = 11.0e0, colo8 = 21.0e0, plno8 = 51.0e0)
       parameter (rowi8 =  1.0e0, coli8 =  2.0e0, plni8 =  5.0e0)
       real       dzero
       parameter (dzero  =  0.0e0)
C#endif

       integer   text,  fp32,  fp64
       data      text   / 'TEXT' /
       data      fp32   / 'FP32' /
       data      fp64   / 'FP64' /
       data      funit  /    21  /

c
c  setting of these two parameters using assignments gets around the
c  bug in the Absoft MacFortran/MPW compiler.  This bug is fixed
c  in the new version, MacFortran II, gam 7/5/90.
c
       ione = 1
       npln = 5

c
c
c  initialize the row, column, and plane vectors
c
c  row values start at 11 and increment by 1 => 11, 12, 13
c  column values start at 21 and increment by 2 => 21, 23, 25, 27
c  plane values start at 51 and increment by 5 => 51, 56, 61, 66, 71
c
       row4(1) = rowo4
       col4(1) = colo4
       pln4(1) = plno4
       row8(1) = rowo8
       col8(1) = colo8
       pln8(1) = plno8

       do 10 i = 2, nrow
         row4(i) = row4(i-1) + rowi4
         row8(i) = row8(i-1) + rowi8
   10  continue
       do 20 j = 2, ncol
         col4(j) = col4(j-1) + coli4
         col8(j) = col8(j-1) + coli8
   20  continue
       do 30 k = 2, npln
         pln4(k) = pln4(k-1) + plni4
         pln8(k) = pln8(k-1) + plni8
   30  continue

c
c  build array elements - rank 2
c
c  element value = sum of row value and col values
c

       do 110 i = 1, nrow
         do 100 j = 1, ncol
           b32r2(i,j) = row4(i) + col4(j)
           b64r2(i,j) = row8(i) + col8(j)
  100    continue
  110  continue

c
c  build array elements - rank 3
c
c  element value = sum of row value, col, and plane values
c

       do 220 i = 1, nrow
         do 210 j = 1, ncol
           do 200 k = 1, npln
             b32r3(i,j,k) = row4(i) + col4(j) + pln4(k)
             b64r3(i,j,k) = row8(i) + col8(j) + pln8(k)
  200      continue
  210    continue
  220  continue

c 
c  text file - rank 2 & 3
c

       open  (funit,file='ftxtr2')
       write (funit,800) text
       write (funit,801) ione, nrow , ncol
       write (funit,802) ezero, ezero
       write (funit,802) (row4(i), i=1, nrow)
       write (funit,802) (col4(j), j=1, ncol )
       do 310 i = 1, nrow
         write (funit,802) (b32r2(i,j), j=1, ncol)
  310  continue
       close (funit)

       open  (funit,file='ftxtr3')
       write (funit,800) text
       write (funit,801) npln, nrow, ncol
       write (funit,802) ezero, ezero
       write (funit,802) (pln4(k), k=1, npln)
       write (funit,802) (row4(i), i=1, nrow)
       write (funit,802) (col4(j), j=1, ncol)
       do 330 k = 1, npln
         do 320 i = 1, nrow
           write (funit,802) (b32r3(i,j,k), j=1, ncol)
  320    continue
  330  continue
       close (funit)

c
c  binary 32-bit file - rank 2 & 3
c

#ifndef UNICOS
#ifdef	DECSTATION
       open  (funit,file='fb32r2',form='unformatted',recl=1,
     .        access='direct')
#else
       open  (funit,file='fb32r2',form='unformatted',recl=4,
     .        access='direct')
#endif
       write (funit,rec=1) fp32
       write (funit,rec=2) ione
       write (funit,rec=3) nrow
       write (funit,rec=4) ncol
       write (funit,rec=5) ezero
       write (funit,rec=6) ezero
       nrec = 6
       do 410 i = 1, nrow
         nrec = nrec + 1
         write (funit,rec=nrec) row4(i)
  410  continue
       do 420 j = 1, ncol
         nrec = nrec + 1
         write (funit,rec=nrec) col4(j)
  420  continue
       do 440 i = 1, nrow
         do 430 j = 1, ncol
           nrec = nrec + 1
           write (funit,rec=nrec) b32r2(i,j)
  430    continue
  440  continue
       close (funit)

#ifdef	DECSTATION
       open  (funit,file='fb32r3',form='unformatted',recl=1,
     .        access='direct')
#else
       open  (funit,file='fb32r3',form='unformatted',recl=4,
     .        access='direct')
#endif
       write (funit,rec=1) fp32
       write (funit,rec=2) npln
       write (funit,rec=3) nrow
       write (funit,rec=4) ncol
       write (funit,rec=5) ezero
       write (funit,rec=6) ezero
       nrec = 6
       do 450 k = 1, npln
         nrec = nrec + 1
         write (funit,rec=nrec) pln4(k)
  450  continue
       do 460 i = 1, nrow
         nrec = nrec + 1
         write (funit,rec=nrec) row4(i)
  460  continue
       do 470 j = 1, ncol
         nrec = nrec + 1
         write (funit,rec=nrec) col4(j)
  470  continue
       do 500 k = 1, npln
         do 490 i = 1, nrow
           do 480 j = 1, ncol
             nrec = nrec + 1
             write (funit,rec=nrec) b32r3(i,j,k)
  480      continue
  490    continue
  500  continue
       close (funit)
#endif

c
c  binary 64-bit file - rank 2 & 3
c

#ifdef UNICOS
       open  (funit,file='fb64r2',form='unformatted',recl=8,
     .        access='direct')
#else
#ifdef DECSTATION
       open  (funit,file='fb64r2',form='unformatted',recl=1,
     .        access='direct')
#else
       open  (funit,file='fb64r2',form='unformatted',recl=4,
     .        access='direct')
#endif
#endif
       write (funit,rec=1) fp64
       write (funit,rec=2) ione
       write (funit,rec=3) nrow
       write (funit,rec=4) ncol
#ifdef UNICOS
       nrec = 5
#else
       close (funit)
#ifdef	DECSTATION
       open  (funit,file='fb64r2',form='unformatted',recl=2,
     .        access='direct')
#else
       open  (funit,file='fb64r2',form='unformatted',recl=8,
     .        access='direct')
#endif
       nrec = 3
#endif
       write (funit,rec=nrec) dzero
       nrec = nrec + 1
       write (funit,rec=nrec) dzero
       do 510 i = 1, nrow
         nrec = nrec + 1
         write (funit,rec=nrec) row8(i)
  510  continue
       do 520 j = 1, ncol
         nrec = nrec + 1
         write (funit,rec=nrec) col8(j)
  520  continue
       do 540 i = 1, nrow
         do 530 j = 1, ncol
           nrec = nrec + 1
           write (funit,rec=nrec) b64r2(i,j)
  530    continue
  540  continue
       close (funit)

#ifdef UNICOS
       open  (funit,file='fb64r3',form='unformatted',recl=8,
     .        access='direct')
#else
#ifdef DECSTATION
       open  (funit,file='fb64r3',form='unformatted',recl=1,
     .        access='direct')
#else
       open  (funit,file='fb64r3',form='unformatted',recl=4,
     .        access='direct')
#endif
#endif
       write (funit,rec=1) fp64
       write (funit,rec=2) npln
       write (funit,rec=3) nrow
       write (funit,rec=4) ncol
#ifdef UNICOS
       nrec = 5
#else
       close (funit)
#ifdef DECSTATION
       open  (funit,file='fb64r3',form='unformatted',recl=2,
     .        access='direct')
#else
       open  (funit,file='fb64r3',form='unformatted',recl=8,
     .        access='direct')
#endif
       nrec = 3
#endif
       write (funit,rec=nrec) dzero
       nrec = nrec + 1
       write (funit,rec=nrec) dzero
       do 550 k = 1, npln
         nrec = nrec + 1
         write (funit,rec=nrec) pln8(k)
  550  continue
       do 560 i = 1, nrow
         nrec = nrec + 1
         write (funit,rec=nrec) row8(i)
  560  continue
       do 570 j = 1, ncol
         nrec = nrec + 1
         write (funit,rec=nrec) col8(j)
  570  continue
       do 600 k = 1,npln
         do 590 i = 1, nrow
           do 580 j = 1, ncol
             nrec = nrec + 1
             write (funit,rec=nrec) b64r3(i,j,k)
  580      continue
  590    continue
  600  continue
       close (funit)

c
c  formats for the text files
c

  800  format (a4)
  801  format (3i10)
  802  format (1p5e14.6)

       end      
