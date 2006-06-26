c
c  In this example we will (1) open the "GridFile" HDF file, (2) attach to
c  the "PolarGrid" grid, and (3) subset data from the "Temperature" field.
c 


	program subsetgrid

	integer            status, gdextreg, gdreginfo, gddetach, gdclose
	integer*4          gdfid, gdid, gdopen, gdattach, dims(8)
	integer*4          rank, ntype, gddefboxreg

	real*8             cornerlon(2), cornerlat(2)
	real*8             upleft(2), lowright(2)
	real*4             datbuf(100*100)
	
	integer DFACC_READ
	parameter (DFACC_READ=1)


c    
c     Open the HDF grid file, "GridFile.hdf"
c 

	gdfid = gdopen("GridFile.hdf", DFACC_READ)

	if (gdfid .NE. -1) then

	   gdid = gdattach(gdfid, "PolarGrid")

	   if (gdid .NE. -1) then

	      cornerlon(1) = 0
	      cornerlat(1) = 90.
	      cornerlon(2) = 90
	      cornerlat(2) = 0

	      regionid = gddefboxreg(gdid, cornerlon, cornerlat)
	      
	      status = gdreginfo(gdid, regionid, "Temperature", ntype,
     1	                         rank, dims, size, upleft, lowright)
	      write(*,*) dims(1), dims(2), dims(3), rank, ntype
	      
	      status = gdextreg(gdid, regionid, "Temperature", 
     1	                        datbuf)
	    

	   endif
	
	   status = gddetach(gdid)
	   status = gdclose(gdfid)

	endif
	stop
	end
	
