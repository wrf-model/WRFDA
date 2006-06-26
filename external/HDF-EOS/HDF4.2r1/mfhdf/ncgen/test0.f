      program fgennc
      include 'netcdf.inc'
      integer  iret
* netCDF id
      integer  ncid
* dimension ids
      integer  idim, jdim, kdim, ldim
* variable ids
      integer  bearsid, orderid, shotid, aloanid, crossid, iid, jid, lid
* variable shapes
      integer dims(3)
* corners and edge lengths
      integer corner(3), edges(3)
* data variables
      character*18 bears
      integer*2 order(3,2)
      integer shot(3,2)
      real aloan(3,2)
      double precision cross(3,2)
      integer i(2)
      real j(3)
      integer*2 l(3)
* attribute vectors
      integer*2  shortval(1)
      integer  nclongval(1)
      real  floatval(3)
      double precision  doubleval(2)
* enter define mode
      ncid = nccre ('ftest0.nc', NCCLOB, iret)
* define dimensions
      idim = ncddef(ncid, 'i', 2, iret)
      jdim = ncddef(ncid, 'j', 3, iret)
      kdim = ncddef(ncid, 'k', NCUNLIM, iret)
      ldim = ncddef(ncid, 'l', 3, iret)
* define variables
      dims(3) = idim
      dims(2) = jdim
      dims(1) = ldim
      bearsid = ncvdef (ncid, 'bears', NCCHAR, 3, dims, iret)
      dims(2) = idim
      dims(1) = jdim
      orderid = ncvdef (ncid, 'order', NCSHORT, 2, dims, iret)
      dims(2) = idim
      dims(1) = jdim
      shotid = ncvdef (ncid, 'shot', NCLONG, 2, dims, iret)
      dims(2) = idim
      dims(1) = jdim
      aloanid = ncvdef (ncid, 'aloan', NCFLOAT, 2, dims, iret)
      dims(2) = idim
      dims(1) = jdim
      crossid = ncvdef (ncid, 'cross', NCDOUBLE, 2, dims, iret)
      dims(1) = idim
      iid = ncvdef (ncid, 'i', NCLONG, 1, dims, iret)
      dims(1) = jdim
      jid = ncvdef (ncid, 'j', NCFLOAT, 1, dims, iret)
      dims(1) = ldim
      lid = ncvdef (ncid, 'l', NCSHORT, 1, dims, iret)
* assign attributes
      call ncaptc(ncid, bearsid, 'act', NCCHAR, 16, 'text string'//char(
     110)//char(9)//'123', iret)
      shortval(1) = -40
      call ncapt(ncid, bearsid, 'acs', NCSHORT, 1, shortval, iret)
      nclongval(1) = 17000
      call ncapt(ncid, bearsid, 'acl', NCLONG, 1, nclongval, iret)
      floatval(1) = -2
      floatval(2) = 1
      floatval(3) = 0
      call ncapt(ncid, bearsid, 'acf', NCFLOAT, 3, floatval, iret)
      doubleval(1) = -1
      doubleval(2) = 0.75
      call ncapt(ncid, bearsid, 'acd', NCDOUBLE, 2, doubleval, iret)
      call ncaptc(ncid, NCGLOBAL, 'history', NCCHAR, 136, 'This is an ex
     1ample of a multi-line global'//char(10)//'attribute.  It could be 
     2used for representing the'//char(10)//'processing history of the d
     3ata, for example.', iret)
* leave define mode
      call ncendf(ncid, iret)
* store bears
      corner(1) = 1
      corner(2) = 1
      corner(3) = 1
      edges(1) = 3
      edges(2) = 3
      edges(3) = 2
      bears = 'indistinguishable' // char(0)
      call ncvptc(ncid, bearsid, corner, edges, bears, 18, iret)
* store order
      corner(1) = 1
      corner(2) = 1
      edges(1) = 3
      edges(2) = 2
      data order /1, 2, 3, 4, 5, 6/
      call ncvpt(ncid, orderid, corner, edges, order, iret)
* store shot
      corner(1) = 1
      corner(2) = 1
      edges(1) = 3
      edges(2) = 2
      data shot /2, 3, 4, 5, 6, 7/
      call ncvpt(ncid, shotid, corner, edges, shot, iret)
* store aloan
      corner(1) = 1
      corner(2) = 1
      edges(1) = 3
      edges(2) = 2
      data aloan /3, 4, 5, 6, 7, 1e+12/
      call ncvpt(ncid, aloanid, corner, edges, aloan, iret)
* store cross
      corner(1) = 1
      corner(2) = 1
      edges(1) = 3
      edges(2) = 2
      data cross /4., 5., 0.000244140625, 7., 8., 10000000000./
      call ncvpt(ncid, crossid, corner, edges, cross, iret)
* store i
      corner(1) = 1
      edges(1) = 2
      data i /10, 20/
      call ncvpt(ncid, iid, corner, edges, i, iret)
* store j
      corner(1) = 1
      edges(1) = 3
      data j /2, 4, 6/
      call ncvpt(ncid, jid, corner, edges, j, iret)
* store l
      corner(1) = 1
      edges(1) = 3
      data l /10, 9, 8/
      call ncvpt(ncid, lid, corner, edges, l, iret)
      call ncclos (ncid, iret)
      end
