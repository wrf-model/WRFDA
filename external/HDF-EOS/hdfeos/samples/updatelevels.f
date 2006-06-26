	program updatelevels

	integer            ptuplev, ptdetach, ptclose, ptgetrecnums
	integer*4          ptfid, ptid1, ptid2
	integer*4          recs(32), inlevel, outlevel
	integer*4          ptopen, ptattach
	integer*4          outrecs(32), outnrec
	real*8             f64
	
	character          datbuf*256, c8*8
	equivalence        (f64,c8)
	
	integer DFACC_RDWR
	parameter (DFACC_RDWR=3)

c
c     Open the HDF point file, "PointFile.hdf".
c

	ptfid = ptopen("PointFile.hdf", DFACC_RDWR)

	ptid1 = ptattach(ptfid, "Simple Point")
	ptid2 = ptattach(ptfid, "FixedBuoy Point")

	f64 = 43.2
	datbuf(1:8) = c8
	recs(1) = 1
	
	status = ptuplev(ptid2, 0, "Longitude", 1, recs, datbuf)


	datbuf(1:1) = 'F'
	recs(1) = 0
	status = ptuplev(ptid2, 0, "ID", 1, recs, datbuf)
	    
	inlevel = 0
	outlevel = 1
	nrec = 1
	status = ptgetrecnums(ptid2, inlevel, outlevel, nrec, recs, 
     1			      outnrec, outrecs)

	
	do 10 i=1,outnrec
		datbuf(i:i) = 'F'
 10	continue

	status = ptuplev(ptid2, outlevel, "ID", outnrec, outrecs, datbuf)

	status = ptdetach(ptid1)
	status = ptdetach(ptid2)
	
	status = ptclose(ptfid)
	
	stop
	end

