c
c  In this example we will (1) open the "PointFile" HDF file, (2) attach to
c  the points, and (3) write data to each level of the points.
c 


	program writelevels
	implicit none
	
	integer            status, i, pntr
	integer*2          wgt
	integer*4          ptfid, ptid, n, date

	real*4             rain, temp, conc(4)       
	real*8             lon, lat, time

	character          buffer*10000, id*2, desc*16, spc*8
	character          ctime*8, cconc*16, clon*8, clat*8
	character          cdate*4, crain*4, ctemp*4, cwgt*2
	equivalence        (time,ctime), (conc,cconc), (lon,clon)
	equivalence        (lat,clat), (rain,crain), (temp,ctemp)
	equivalence        (date,cdate), (wgt,cwgt)
	
	integer            ptwrlev, ptdetach, ptclose
	integer*4          ptopen, ptattach


	integer DFACC_RDWR
	parameter (DFACC_RDWR=3)

c
c     Open the HDF point file, "PointFile.hdf".
c

	ptfid = ptopen("PointFile.hdf", DFACC_RDWR)


c
c    Write to Simple Point
c
	ptid = ptattach(ptfid, "Simple Point")

	open(unit=1, file='simple.txt', status='OLD')

	n = 0
	pntr = 1
	do 10 i=1,1000
	      
	   read(1, *, end=100)  time, conc(1), conc(2), conc(3), 
     1                            conc(4), spc
	   n = n + 1
	
	   buffer(pntr:pntr+8) = ctime
	   pntr = pntr + 8
	   buffer(pntr:pntr+4*4) = cconc
	   pntr = pntr + 4*4
	   buffer(pntr:pntr+4) = spc
	   pntr = pntr + 4
10	continue

100	close(unit=1)
    
	status = ptwrlev(ptid, 0, n, buffer)
	status = ptdetach(ptid)
    

c
c    Write to Fixed Buoy Point 
c

	ptid = ptattach(ptfid, "FixedBuoy Point")
c
c        Write First (0th) Level
c
	open(unit=1, file='fixedBuoy0.txt', status='OLD')

	n = 0
	pntr = 1
	do 20 i=1,1000
		      
	   read(1, *, end=200)  desc, lon, lat, date, id
	      
	   n = n + 1
	
	   buffer(pntr:pntr+8) = desc
	   pntr = pntr + 8
	   buffer(pntr:pntr+8) = clon
	   pntr = pntr + 8
	   buffer(pntr:pntr+8) = clat
	   pntr = pntr + 8
	   buffer(pntr:pntr+4) = cdate
	   pntr = pntr + 4
	   buffer(pntr:pntr+1) = id
	   pntr = pntr + 1
 20	continue

 200	close(unit=1)

    
	status = ptwrlev(ptid, 0, n, buffer)



c
c      Write Second (1th) Level */
c
	open(unit=1, file='fixedBuoy1.txt', status='OLD')

	n = 0
	pntr = 1
	do 30 i=1,1000

	   read(1, *, end=300) time, rain, temp, id
	   
	   n = n + 1
	   
	   buffer(pntr:pntr+8) = ctime
	   pntr = pntr + 8
	   buffer(pntr:pntr+4) = crain
	   pntr = pntr + 4
	   buffer(pntr:pntr+4) = ctemp
	   pntr = pntr + 4
	   buffer(pntr:pntr+1) = id
	   pntr = pntr + 1
 30	continue
	
 300	close(unit=1)
	  
	status = ptwrlev(ptid, 1, n, buffer)
	  
	status = ptdetach(ptid)
    

c
c    Write to Float Buoy Point 
c

	ptid = ptattach(ptfid, "FloatBuoy Point")

c
c        Write First (0th) Level
c
	open(unit=1, file='floatBuoy0.txt', status='OLD')

	n = 0
	pntr = 1
	do 40 i=1,1000
	      
	   read(1, *, end=400)  desc, date, wgt, id
	   write(*,*) desc,date,wgt,id
	   n = n + 1
	
	   buffer(pntr:pntr+8) = desc
	   pntr = pntr + 8
	   buffer(pntr:pntr+4) = cdate
	   pntr = pntr + 4
	   buffer(pntr:pntr+2) = cwgt
	   pntr = pntr + 2
	   buffer(pntr:pntr+1) = id
	   pntr = pntr + 1
 40	continue

 400	close(unit=1)

    
	status = ptwrlev(ptid, 0, n, buffer)


c
c      Write Second (1th) Level */
c
	open(unit=1, file='floatBuoy1.txt', status='OLD')

	n = 0
	pntr = 1
	do 50 i=1,1000

	   read(1, *, end=500) time, lon, lat, rain, temp, id
	      
	   n = n + 1
	
	   buffer(pntr:pntr+8) = ctime
	   pntr = pntr + 8
	   buffer(pntr:pntr+8) = clon
	   pntr = pntr + 8
	   buffer(pntr:pntr+8) = clat
	   pntr = pntr + 8	      
	   buffer(pntr:pntr+4) = crain
	   pntr = pntr + 4
	   buffer(pntr:pntr+4) = ctemp
	   pntr = pntr + 4
	   buffer(pntr:pntr+1) = id
	   pntr = pntr + 1
 50	continue

 500	close(unit=1)

	status = ptwrlev(ptid, 1, n, buffer)

	status = ptdetach(ptid)
	
 2000	continue
	
	status = ptclose(ptfid)

	stop
	end
	

