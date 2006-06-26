c
c	In this example we will (1) open the "PointFile" HDF file,
c	(2) attach to the three point structures, and (3) define
c	the fields within each point.
c
	program definelevels

	integer         status
	integer*4       ptfid, ptid
	integer*4       fieldtype(8), fieldorder(8)
	character*255   fldlist

	integer*4       ptopen, ptattach
	integer         ptdeflev, ptdeflink, ptdetach, ptclose
	
	integer  DFACC_RDWR
	parameter (DFACC_RDWR=3)

	integer  DFNT_CHAR8, DFNT_INT16, DFNT_INT32
	parameter (DFNT_CHAR8=4)
	parameter (DFNT_INT16=22)
	parameter (DFNT_INT32=24)
	
	integer  DFNT_FLOAT32, DFNT_FLOAT64
	parameter (DFNT_FLOAT32=5)
	parameter (DFNT_FLOAT64=6)
	
c
c	We first open the HDF point file, "PointFile.hdf".  Because this
c	file already exist and we wish to write to it, we use the
c	DFACC_RDWR access code in the open statement.  The ptopen
c	routine returns the point fileid, ptfid, which is used to
c	identify the file in subsequent routines.

	ptfid = ptopen("PointFile.hdf", DFACC_RDWR)


c
c	We next attach to the three point structures within the file
c       using the ptattach routine, identifying each point by its
c       name defined by the ptcreate routine previously.
c

	if (ptfid .ne. -1) then
	   
c
c      Define "Simple" point
c
	   
	   ptid = ptattach(ptfid, "Simple Point")
	   fldlist = "Time,Concentration,Species"


	   fieldtype(1) = DFNT_FLOAT64
	   fieldtype(2) = DFNT_FLOAT32
	   fieldtype(3) = DFNT_CHAR8
	   
	   fieldorder(1) = 1
	   fieldorder(2) = 4
	   fieldorder(3) = 4
	
	   status = ptdeflev(ptid, "Sensor", fldlist, fieldtype,
     1                       fieldorder)
	
	   status = ptdetach(ptid)
	   goto 5000
c
c      Define Fixed Buoy Point 
c	      

	   ptid = ptattach(ptfid, "FixedBuoy Point")


c     Define Description/Location Level

	   fldlist = "Label,Longitude,Latitude,DeployDate,ID"
	
	   fieldtype(1) = DFNT_CHAR8
	   fieldtype(2) = DFNT_FLOAT64
	   fieldtype(3) = DFNT_FLOAT64
	   fieldtype(4) = DFNT_INT32
	   fieldtype(5) = DFNT_CHAR8
	   
c   Note: order = 0 same as order = 1 for numeric scalars

	   fieldorder(1) = 8
	   fieldorder(2) = 0
	   fieldorder(3) = 0
	   fieldorder(4) = 0
	   fieldorder(5) = 1

	   status = ptdeflev(ptid, "Desc-Loc", fldlist, fieldtype, 
     1                       fieldorder)


c     Define Data Level

	   fldlist = "Time,Rainfall,Temperature,ID"
	
	   fieldtype(1) = DFNT_FLOAT64
	   fieldtype(2) = DFNT_FLOAT32
	   fieldtype(3) = DFNT_FLOAT32
	   fieldtype(4) = DFNT_CHAR8
	   
	   fieldorder(1) = 0
	   fieldorder(2) = 0
	   fieldorder(3) = 0
	   fieldorder(4) = 1

	   status = ptdeflev(ptid, "Observations", fldlist, 
     1			     fieldtype, fieldorder)

	   status = ptdeflink(ptid, "Desc-Loc", "Observations", "ID")

	   status = ptdetach(ptid)



c
c
c	Floating Buoy Point
c
	   ptid = ptattach(ptfid, "FloatBuoy Point")


c      Define Description Level */

	   fldlist = "Label,DeployDate,Weight,ID"
	
	   fieldtype(1) = DFNT_CHAR8
	   fieldtype(2) = DFNT_INT32
	   fieldtype(3) = DFNT_INT16
	   fieldtype(4) = DFNT_CHAR8
	   
	   fieldorder(1) = 8
	   fieldorder(2) = 0
	   fieldorder(3) = 0
	   fieldorder(4) = 1


	   status = ptdeflev(ptid, "Description", fldlist, 
     1                       fieldtype, fieldorder)


c	Define Data Level

	   fldlist = "Time,Longitude,Latitude,Rainfall,Temperature,ID"

	   fieldtype(1) = DFNT_FLOAT64
	   fieldtype(2) = DFNT_FLOAT64
	   fieldtype(3) = DFNT_FLOAT64
	   fieldtype(4) = DFNT_FLOAT32
	   fieldtype(5) = DFNT_FLOAT32
	   fieldtype(6) = DFNT_CHAR8
	   
	   fieldorder(1) = 0
	   fieldorder(2) = 0
	   fieldorder(3) = 0
	   fieldorder(4) = 0
	   fieldorder(5) = 0
	   fieldorder(6) = 1	

	   status = ptdeflev(ptid, "Measurements", fldlist, 
     1                       fieldtype, fieldorder)

	   status = ptdeflink(ptid, "Description", "Measurements", "ID")

	   status = ptdetach(ptid)
	
 5000	   continue
c
c      Close HDF file
c	
	   status = ptclose(swfid)

	   write(*,*) 'status close ', status
	endif
		   
	stop
	end
