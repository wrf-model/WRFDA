
c
c	In this example we will retrieve (1) information about the
c	dimensions, (2) the dimension mappings (geolocation relations), 
c	and (3) the swath fields.
c 

	program inquireswath

	integer         i, swmapinfo, swfldinfo, status
	integer         swdetach, swclose
	integer*4       swinqdims, swinqmaps, swinqdflds, swinqgflds
	integer*4       swdiminfo, offset, incr, rk, nt
	integer*4       swimapinfo, swinqimaps
	integer*4       swfid, swid, ndims, nmaps, nflds
	integer*4       swopen, swattach
	integer*4       dims(32), off(32), inc(32), rank(32), ntype(32)
	integer*4       sizes(8), indx(32), dimsize
	character*72    dimname, dimmap, fieldlist, dimlist
	integer         DFACC_READ
	parameter        (DFACC_READ=1)

c
c	Open the Swath File for read only access
c

	swfid = swopen("SwathFile.hdf", DFACC_READ)

    
	if (swfid .NE. -1) then

c	   Attach the swath

	   swid = swattach(swfid, "Swath1")
	
	   if (swid .NE. -1) then


c	    Inquire Dimensions

	      ndims = swinqdims(swid, dimname, dims)

	      write(*,*) 'Dimension list: ', dimname
	      do i = 1,ndims
		 write(*,*) 'dim size: ', dims(i)
	      enddo
	      write(*,*)	      


c           Inquire Dimension Mappings

	      nmaps = swinqmaps(swid, dimmap, off, inc)

	      write(*,*) 'Dimension map: ', dimmap
	      do i = 1,nmaps
		 write(*,*) 'offset increment: ', off(i), inc(i)
	      enddo
	      write(*,*)

	      
	      
c	    Inquire Indexed Dimension Mappings

	      nmaps = swinqimaps(swid, dimmap, sizes)

	      write(*,*) 'Index Dimension map: ', dimmap
	      do i=1,nmaps
		 write(*,*) 'sizes: ', sizes(i)
	      enddo
	      write(*,*)	      

	     
c         Inquire Geolocation Fields
	    
	      nflds = swinqgflds(swid, fieldlist, rank, ntype)
	      write(*,*) 'Geolocation fieldlist: ', fieldlist
	      do i=1,nflds
		 write(*,*) 'field rank & datatype: ', rank(i), ntype(i)
	      enddo
	      write(*,*) 
	    
	      
c         Inquire Data Fields	      

	      nflds = swinqdflds(swid, fieldlist, rank, ntype)
	      write(*,*) 'Data Fieldlist: ', fieldlist
	      do i=1,nflds
		 write(*,*) 'field rank & datatype: ', rank(i), ntype(i)
	      enddo
	      write(*,*) 
	    



c	  Get info on "GeoTrack" dim

	      dimsize = swdiminfo(swid, "GeoTrack")
	      write(*,*) 'Size of GeoTrack: ', dimsize
	      write(*,*)
	      

c	  Get info on "GeoTrack/Res2tr" mapping

	      status = swmapinfo(swid, "GeoTrack", "Res2tr", offset,
     1                           incr)	      
	      write(*,*) 'Mapping Offset: ', offset
	      write(*,*) 'Mapping Increment: ', incr
	      write(*,*)


c	  Get info on "IndxTrack/Res2tr" indexed mapping

	      n = swimapinfo(swid, "IndxTrack", "Res2tr", indx)
	      do i=1,n
		 write(*,*) 'Index Mapping Entry ', i, indx(i)
	      enddo
	      write(*,*)
	      

c	  Get info on "Longitude" Field

	      status = swfldinfo(swid, "Longitude", rk, dims, nt, dimlist)
	      write(*,*) 'Longitude Rank: ', rk
	      write(*,*) 'Longitude NumberType: ', nt
	      write(*,*) 'Longitude Dimlist: ', dimlist
	      do i=1,rk
		 write(*,*) 'Dimension ',i,dims(i)
	      enddo
	     
	  endif
	endif
	
	status = swdetach(swid)
	status = swclose(swfid)
	stop
	end








