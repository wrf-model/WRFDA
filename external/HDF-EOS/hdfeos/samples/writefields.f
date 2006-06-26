
c
c  In this example we will (1) open the "SwathFile" HDF file, (2) attach to
c  the "Swath1" swath, and (3) write data to the "Longitude", 
c  "Longitude" and "Spectra" fields.
c 


	program writefields

	integer            status, swwrfld, swwrattr, swdetach, swclose
	integer            track, start(3),stride(3),count(3)
	integer*4          swfid, SWid, swopen, swattach, attr(4)

	real*4         lng(10), lat(10)
	real*8         plane(800), tme(20)
	
	integer DFACC_RDWR
	parameter (DFACC_RDWR=3)
	integer DFNT_INT32
	parameter (DFNT_INT32=24)
	
c     Define longitude values along the cross track

	do i=1,10
	   lng(i) = i-1.0
	enddo
	

c    
c     Open the HDF swath file, "SwathFile.hdf"
c 

	swfid = swopen("SwathFile.hdf", DFACC_RDWR)


	if (swfid .NE. -1) then

	   SWid = swattach(swfid, "Swath1")

	   if (swid .NE. -1) then


c     Write data starting at the beginning of each cross track

	      start(1) = 0
	      stride(1) = 1
	      stride(2) = 1
	      count(1) = 10
	      count(2) = 1

c
c      Loop through all the tracks, incrementing the track starting
c      position by one each time.
c

	      do track = 1,20
		 start(2) = track - 1
		 status = swwrfld(swid, "Longitude", start, stride,
     1                            count, lng)

		 do xtrack = 1,10
		    lat(xtrack) = track
		 enddo
		 
		 status = swwrfld(swid, "Latitude", start, stride,
     1                            count, lat)
		 
						  
	      enddo


	      do i = 1,20
 		 tme(i) = 34574087.3 + 84893.2*(i-1)
	      enddo

	      start(1) = 0
	      stride(1) = 1
	      count(1) = 20
	      status = swwrfld(swid, "Time", start, stride, count, tme)

c
c	     Write Spectra one plane at a time 
c	     Value is 100 * track index + band index (0-based)
c
	    start(1) = 0
	    start(2) = 0
	    count(1) = 20
	    count(2) = 40
	    count(3) = 1
	    stride(3) = 1
	    
	    do i=1,15
	       start(3) = i - 1
	       do j=1,40
		  do k=1,20
		     plane((j-1)*20+k) = (j-1)*100 + i-1
		  enddo
	       enddo
	       
	       status = swwrfld(swid, "Spectra", start, stride,
     1	                        count, plane)

	    enddo


c      Write User Attribute
	      attr(1) = 3
	      attr(2) = 5
	      attr(3) = 7
	      attr(4) = 11
	      status = swwrattr(swid, "TestAttr", DFNT_INT32, 4, attr)
	      
	   endif
	 endif

	 status =  swdetach(swid)
	 status = swclose(swfid)

	 stop
	 end

