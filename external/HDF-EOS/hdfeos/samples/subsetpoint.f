c
c 


	program subsetpoint

	integer            status, ptextreg, ptreginfo, ptdetach
	integer            ptextper, ptperinfo, ptclose
	
	integer*4          ptfid, ptid, ptopen, ptattach
	integer*4          size, ptdefboxreg, ptdeftmeper
	integer*4          regionid, periodid

	real*8             cornerlon(2), cornerlat(2)
	real*8             datbuf(128), starttime, stoptime
	
	integer DFACC_READ
	parameter (DFACC_READ=1)

c    
c     Open the HDF point file, "PointFile.hdf"
c 

	ptfid = ptopen("PointFile.hdf", DFACC_READ)

	if (ptfid .NE. -1) then

	   ptid = ptattach(ptfid, "FloatBuoy Point")

	   if (ptid .NE. -1) then
	      cornerlon(1) = -145.
	      cornerlat(1) = -15.
	      cornerlon(2) = -135.
	      cornerlat(2) = 8.



	      regionid = ptdefboxreg(ptid, cornerlon, cornerlat)

	      level = 1
	      status = ptreginfo(ptid, regionid, level, 
     1				 "Longitude,Latitude", size)

	      status = ptextreg(ptid, regionid, level, 
     1	                        "Longitude,Latitude", datbuf)

	      do 100 i=1,size/16
		 write(*,*) i, datbuf(2*i-1),datbuf(2*i)
 100	      continue
	      
c             Time Subsetting	      

	      starttime = 35208757.6d0
	      stoptime = 35984639.2d0
	    
	      periodid = ptdeftmeper(ptid, starttime, stoptime)

	      level = 1
	      status = ptperinfo(ptid, periodid, level, "Time", size)

	      status = ptextper(ptid, periodid, level, "Time", datbuf)
	      
	      do 200 i=1,size/8
		 write(*,*) i, datbuf(i)
 200	      continue
	      
	   endif
	
	   status = ptdetach(ptid)
	   status = ptclose(ptfid)

	endif
	stop
	end



