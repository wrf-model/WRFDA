c
c  In this example we will (1) open the "PointFile" HDF file, (2) attach to
c  the points, and (3) read data from each level of the points.
c 
	program readlevels
	

	integer*4         ptfid, ptid, recs(32)
	integer*4         fldtype(32), fldorder(32)

        integer            status, i, pntr
	integer*2          wgt
	integer*4          n, date
	
	real*4             rain, temp, conc(4)
	real*8             lon, lat, time
				
	character         fldlist*256, buffer*100000, id*2
	character          spc*4, desc*8
        character          ctime*8, cconc*16, clon*8, clat*8
	character          cdate*4, crain*4, ctemp*4, cwgt*2
	equivalence        (time,ctime), (conc,cconc), (lon,clon)
	equivalence        (lat,clat), (rain,crain), (temp,ctemp)
	equivalence        (date,cdate), (wgt,cwgt)
			
	integer           ptrdlev
	integer           ptdetach, ptclose, ptlevinfo
	integer*4         ptopen, ptattach, ptnrecs
	
	
	integer DFACC_READ
	parameter (DFACC_READ=1)

	
c
c     Open the HDF swath file, "PointFile.hdf".
c

	ptfid = ptopen("PointFile.hdf", DFACC_READ)

c
c    Read Simple Point
c
	ptid = ptattach(ptfid, "Simple Point")

	status = ptlevinfo(ptid, 0, fldlist, fldtype, fldorder)
	n = ptnrecs(ptid, 0)

	do 5 i=1,n
	   recs(i) = i - 1
 5	continue
	   
	status = ptrdlev(ptid, 0, fldlist, n, recs, buffer)
	pntr = 1

	do 10 i=1,n
	   ctime = buffer(pntr:pntr+8)
	   pntr = pntr + 8
	   cconc = buffer(pntr:pntr+4*4)
	   pntr = pntr + 4*4
	   spc = buffer(pntr:pntr+4)
	   pntr = pntr + 4
	   write(*,*) time, conc(1), conc(2), conc(3), conc(4), spc
10	continue
    
	status = ptdetach(ptid)
    

c
c    Read Fixed Buoy Point
c
	ptid = ptattach(ptfid, "FixedBuoy Point")

c
c       Read First (0th) Level
c
	status = ptlevinfo(ptid, 0, fldlist, fldtype, fldorder)

	n = 2
	recs(1) = 0
	recs(2) = 2
    
	pntr = 1
	status = ptrdlev(ptid, 0, fldlist, n, recs, buffer)

	do 20 i=1,n
	   desc = buffer(pntr:pntr+8)
	   pntr = pntr + 8
	   clon = buffer(pntr:pntr+8)
	   pntr = pntr + 8
	   clat = buffer(pntr:pntr+8)
	   pntr = pntr + 8
	   cdate = buffer(pntr:pntr+4)
	   pntr = pntr + 4
	   id = buffer(pntr:pntr+1)
	   pntr = pntr + 1
	   write(*,*) desc, lon, lat, date, id(1:1)
 20	continue
	
c
c    Read Second (1th) Level
c
	status = ptlevinfo(ptid, 1, fldlist, fldtype, fldorder)
	n = ptnrecs(ptid, 1)


	do 30 i=1,n

	   recs(1) = i - 1
	   status = ptrdlev(ptid, 1, fldlist, 1, recs, buffer)

	   pntr = 1
	   
	   ctime = buffer(pntr:pntr+8)
	   pntr = pntr + 8
	   crain = buffer(pntr:pntr+4)
	   pntr = pntr + 4
	   ctemp = buffer(pntr:pntr+4)
	   pntr = pntr + 4
	   id = buffer(pntr:pntr+1)
	   pntr = pntr + 1
	   write(*,*) time, rain, temp, '  ', id(1:1)
 30	continue	

	status = ptdetach(ptid)
	status = ptclose(ptfid)

	stop
	end
