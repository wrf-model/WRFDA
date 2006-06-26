c
c  In this example we will (1) open the "SwathFile" HDF file, (2) attach to
c  the "Swath1" swath, and (3) subset data from the "Spectra" field.
c 


	program subsetswath

	integer            status, swextreg, swreginfo, swdetach
	integer            swextper, swperinfo, swclose
	
	integer*4          swfid, swid, swopen, swattach, dims(8)
	integer*4          rank, ntype, size, swdefboxreg, swdeftmeper
	integer*4          regionid, periodid

	real*8             cornerlon(2), cornerlat(2)
	real*8             datbuf(40,20,15), tmebuf(20)
	
	integer DFACC_READ
	parameter (DFACC_READ=1)
	integer HDFE_MIDPOINT
	parameter (HDFE_MIDPOINT=0)
	integer HDFE_INTERNAL
	parameter (HDFE_INTERNAL=0)


c    
c     Open the HDF swath file, "SwathFile.hdf"
c 

	swfid = swopen("SwathFile.hdf", DFACC_READ)

	if (swfid .NE. -1) then

	   swid = swattach(swfid, "Swath1")

	   if (swid .NE. -1) then
	      cornerlon(1) = 3.
	      cornerlat(1) = 5.
	      cornerlon(2) = 7.
	      cornerlat(2) = 12.

	      regionid = swdefboxreg(swid, cornerlon, cornerlat,
     1	                             HDFE_MIDPOINT)
	      write(*,*) regionid,swid
	      
	      status = swreginfo(swid, regionid, "Spectra", ntype,
     1	                         rank, dims, size)
	      write(*,*) dims(1), dims(2), dims(3), rank, ntype, size
	      
	      status = swextreg(swid, regionid, "Spectra", 
     1	                HDFE_INTERNAL, datbuf)

	      
c             Time Subsetting	      
	      periodid = swdeftmeper(swid, 35232487.2d0, 36609898.1d0,
     1	                             HDFE_MIDPOINT)
	      write(*,*) 'Time Subset: ', periodid,swid
	      
	      status = swperinfo(swid, periodid, "Time", ntype,
     1	                         rank, dims, size)
	      write(*,*) 'Time Subset: ', rank, dims(1), size
	      
	      status = swextper(swid, periodid, "Time", 
     1	                        HDFE_INTERNAL, tmebuf)
	      
	      do 10 i=1,size/8
		 write(*,*) i, tmebuf(i)
 10		 continue

	   endif
	
	   status = swdetach(swid)
	   status = swclose(swfid)

	endif
	stop
	end
	



