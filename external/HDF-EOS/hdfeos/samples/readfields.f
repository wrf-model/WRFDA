
c
c  In this example we will (1) open the "SwathFile" HDF file, (2) attach to
c  the "Swath1" swath, and (3) read data from the "Longitude" field.
c 


	program readfields

	integer            status, swrdfld, swrdattr, swdetach, swclose
	integer            start(2),stride(2),count(2), attr(4)
	integer*4          swfid, swid, swopen, swattach

	real*4         lng(10,20)
	
	integer DFACC_READ
	parameter (DFACC_READ=1)
	

c    
c     Open the HDF swath file, "SwathFile.hdf"
c 

	swfid = swopen("SwathFile.hdf", DFACC_READ)


	if (swfid .NE. -1) then

	   swid = swattach(swfid, "Swath1")

	   if (swid .NE. -1) then

	      
c     Read the entire Longitude field

	      start(1) = 0
	      start(2) = 0
	      stride(1) = 1
	      stride(2) = 1
	      count(1) = 10
	      count(2) = 20

	      status = swrdfld(swid, "Longitude", start, stride, count,
     1                         lng)

	      do i=1,20
		 do j=1,10
		    write(*,*)'i j Longitude ',i,j,lng(j,i)
		 enddo
	      enddo


c      Read Attribute	      
	      status = swrdattr(swid, "TestAttr", attr)
	      do i=1,4
		 write(*,*) 'Attribute Element', i, ':', attr(i)
	      enddo
	      
	      
	   endif
	endif
	
	status = swdetach(swid)
	status = swclose(swfid)

	stop
	end




