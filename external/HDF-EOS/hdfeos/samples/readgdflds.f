	program readgdflds

	integer       status, gdrdfld, gdrdattr, gddetach, gdclose
	integer*4     gdfid, gdid, gdopen, gdattach
	integer*4     start(2), stride(2), count(2)
	real*4        f32, veg(120,200)
	integer DFACC_RDWR
	parameter (DFACC_RDWR=3)
	integer DFNT_FLOAT32
	parameter (DFNT_FLOAT32=5)

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
	      status = gdrdfld(gdid, "Vegetation", 
     1    	               start, stride, count, veg)
	      
	      

	      status = gdrdattr(gdid, "float32", f32)
	      write(*,*) 'f32: ', f32
	      
	   endif
	endif

	status = gddetach(gdid)
	status = gdclose(gdfid)

	stop
	end
