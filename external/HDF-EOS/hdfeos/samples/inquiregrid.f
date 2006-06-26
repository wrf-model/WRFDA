	program inquiregrid

	integer      status, i, gddetach, gdclose, gdgridinfo
	integer      gdprojinfo
	integer*4    gdfid, gdid1, ndim, nflds, gddiminfo
	integer*4    dims(32), rank(32), ntype(32), gdopen, gdattach
	integer*4    n, strbufsize, gdid2, gdinqdims, gdinqflds
	integer*4    xdimsize, ydimsize, dimsize, projcode,zonecode
	integer*4    spherecode, origincode
    
	real*8       upleftpt(2), lowrightpt(2), projparm(13)
    
	character*72   dimname, fieldlist


	integer DFACC_READ
	parameter (DFACC_READ=1)


	
	gdfid = gdopen('GridFile.hdf', DFACC_READ)

    
	if (gdfid .ne. -1) then

	   gdid1 = gdattach(gdfid, 'UTMGrid')
	   gdid2 = gdattach(gdfid, 'PolarGrid')

	   ndim = gdinqdims(gdid1, dimname, dims)
	   write(*,*) 'Dimension list (UTMGrid): ', dimname
	   do i=1,ndim
	      write(*,*) 'dim size: ', dims(i)
	   enddo
	   
	   ndim = gdinqdims(gdid2, dimname, dims)
	   write(*,*) 'Dimension list (PolarGrid): ', dimname
	   do i=1,ndim
	      write(*,*) 'dim size: ', dims(i)
	   enddo
	   
	   dimsize = gddiminfo(gdid1, 'Time')
	   write(*,*) 'Size of "Time" Array: ', dimsize
	
	   dimsize = gddiminfo(gdid2, 'Bands')
	   write(*,*) 'Size of "Bands" Array: ', dimsize
	
	
	   status = gdgridinfo(gdid1, xdimsize, ydimsize,
     1                  	   upleftpt, lowrightpt)
	   write(*,*) 'X dim size, Y dim size (UTMGrid): ', 
     1 	               xdimsize, ydimsize
	   
	   write(*,*) 'Up left pt (UTMGrid): ', 
     1	               upleftpt(1), upleftpt(2)
	   write(*,*) 'Low right pt (UTMGrid): ', 
     1		       lowrightpt(1), lowrightpt(2)
	   
	   
	   status = gdgridinfo(gdid2, xdimsize, ydimsize,
     1                  	   upleftpt, lowrightpt)
	   write(*,*) 'X dim size, Y dim size (PolarGrid): ', 
     1 	               xdimsize, ydimsize
	   
	   write(*,*) 'Up left pt (PolarGrid): ', 
     1	               upleftpt(1), upleftpt(2)
	   write(*,*) 'Low right pt (PolarGrid): ', 
     1	               lowrightpt(1), lowrightpt(2)

	       
	   status = gdprojinfo(gdid1, projcode, zonecode,
     1	                       spherecode, projparm)
	   write(*,*) 'projcode , zonecode (UTMGrid): ', projcode,
     1	               zonecode
	   write(*,*) 'spherecode (UTMGrid): ', spherecode	

	
	   status = gdprojinfo(gdid2, projcode, zonecode
     1	                       spherecode, projparm)

	   status = gdprojinfo(gdid2, projcode, zonecode,
     1	                       spherecode, projparm)
	   
	   write(*,*) 'projcode (PolarGrid): ', projcode
	   write(*,*) 'spherecode (PolarGrid): ', spherecode
	   do i=1,13
	      write(*,*) 'Projection Parameter: ',i,projparm(i)
	   enddo
	   

	   nflds = gdinqflds(gdid1, fieldlist, rank, ntype)
	   if (nflds .ne. 0) then
	      write(*,*) 'Data fields (UTMGrid): ', fieldlist
	      do i=1,nflds
		 write(*,*) 'rank type: ',rank(i),ntype(i)
	      enddo
	   endif
	
	
	   nflds = gdinqflds(gdid2, fieldlist, rank, ntype)
	   if (nflds .ne. 0) then
	      write(*,*) 'Data fields (PolarGrid): ', fieldlist
	      do i=1,nflds
		 write(*,*) 'rank type: ',rank(i),ntype(i)
	      enddo
	   endif	
	
	
	   status = gdfldinfo(gdid2, 'Spectra', rank, 
     1	                        dims, ntype, dimname)
	   write(*,*) 'Spectra rank dims: ',rank(1)
	   do i=1,rank(1)
	      write(*,*) 'Spectra dims: ',i,dims(i), dimname
	   enddo
	
C	   n = gdnentries(gdid1, HDFE_NENTDIM, strbufsize)
C	   write(*,*) 'Number of dimension entries (UTMGrid): ', n
C	   write(*,*) 'Length of Dimension List (UTMGrid): ', strbufsize
	   
C	   n = gdnentries(gdid1, HDFE_NENTDFLD, strbufsize)
C	   write(*,*) 'Number of data fields (UTMGrid): ', n
C	   write(*,*) 'Length of Field List (UTMGrid): ', strbufsize
	   
	endif
	
	status = gddetach(gdid1)
	status = gddetach(gdid2)
	status = gdclose(gdfid)

	stop
	end
