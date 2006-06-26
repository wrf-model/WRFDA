	program writegdflds

	integer       i, j, status, gdwrfld, gdwrattr, gddetach, gdclose
	integer*4     gdfid, gdid, gdopen, gdattach
	integer*4     start(2), stride(2), count(2)
	real*4        f32, veg(120,200), temp(100,100)
	integer DFACC_RDWR
	parameter (DFACC_RDWR=3)
	integer DFNT_FLOAT32
	parameter (DFNT_FLOAT32=5)


	do i=1,200
	   do j=1,120
	      veg(j,i) = 10 + i
	   enddo
	enddo
	
	do i=1,100
	   do j=1,100
	      temp(j,i) = 100*(i-1) + j
	   enddo
	enddo


	gdfid = gdopen("GridFile.hdf", DFACC_RDWR)

	if (gdfid .ne. -1) then

	   gdid = gdattach(gdfid, "UTMGrid")

	   if (gdid .ne. -1) then
	
	      start(1) = 0
	      start(2) = 0
	      stride(1) = 1
	      stride(2) = 1
	      count(1) = 120
	      count(2) = 200
	      status = gdwrfld(gdid, "Vegetation", 
     1                  	   start, stride, count, veg)
	      
	      f32 = 1
	      status = gdwrattr(gdid, "float32", DFNT_FLOAT32, 1, f32)
	   endif
	endif

	status = gddetach(gdid)

	gdid = gdattach(gdfid, "PolarGrid")
	if (gdid .ne. -1) then
	   start(1) = 0
	   start(2) = 0
	   stride(1) = 1
	   stride(2) = 1
	   count(1) = 100
	   count(2) = 100
	   status = gdwrfld(gdid, "Temperature", 
     1	                    start, stride, count, temp)

	endif
	status = gddetach(gdid)
	
	status = gdclose(gdfid)

	stop
	end

