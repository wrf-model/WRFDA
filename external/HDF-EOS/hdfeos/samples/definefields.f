c
c	In this example we will (1) open the "SwathFile" HDF file, (2)
c	attach to
c	the "Swath1" swath, and (3) define the swath fields.
c
	program definefields

	integer            status, swdefgfld, swdefdfld, swdetach, swclose
	integer*4          swfid, swid, swopen, swattach

	integer  DFACC_RDWR
	parameter (DFACC_RDWR=3)
	integer  DFNT_INT16, DFNT_FLOAT32, DFNT_FLOAT64
	parameter (DFNT_INT16=22)
	parameter (DFNT_FLOAT32=5)	
	parameter (DFNT_FLOAT64=6)
	integer*4 HDFE_NOMERGE, HDFE_AUTOMERGE
	parameter (HDFE_NOMERGE=0)
	parameter (HDFE_AUTOMERGE=1)
c
c	We first open the HDF swath file, "SwathFile.hdf".  Because this
c	file already exist and we wish to write to it, we use the
c	DFACC_RDWR accesscode in the open statement.  The SWopen
c	routine returns the swath fileid, swfid, which is used to
c	identify the file in subsequent routines.

	swfid = swopen("SwathFile.hdf", DFACC_RDWR)


c
c	If the swath file cannot be found, SWopen will return -1 for the
c	filehandle (swfid).  We there check that this is not the
c       case becforeproceeding with the other routines.  The SWattach
c	routine returns the handle to the existing swath "Swath1", swid.
c	If the swath is not found, SWattach returns -1 for
c	the handle.
c

	if (swfid .NE. -1) then

	   swid = swattach(swfid, "Swath1")

	   if (swid .NE. -1) then

c
c	     We define six fields.  The first three, "Time", "Longitude"
c	     and "Latitude" are geolocation fields and thus we use the
c	     geolocation dimensions "GeoTrack" and "GeoXtrack" in the field
c	     definitions.  We also must specify the data type using the
c	     standard HDF data type codes.  In this example the geolocation
c	     are 4-byte (32 bit) floating point numbers.
c	     
c	     The next three fields are data fields.  Note that either
c	     geolocation or data dimensions can be used.  If an error
c            occurs during the definition, such as a dimension that cannot
c	     be found, then the return status will be set to -1.
c

	      status = swdefgfld(swid, "Time", "GeoTrack", DFNT_FLOAT64,
     1                           HDFE_NOMERGE)

	      status = swdefgfld(swid, "Longitude", "GeoXtrack,GeoTrack",
     1	                         DFNT_FLOAT32, HDFE_AUTOMERGE)

	      status = swdefgfld(swid, "Latitude", "GeoXtrack,GeoTrack",
     1                           DFNT_FLOAT32, HDFE_AUTOMERGE)

	      status = swdefdfld(swid, "Density", "GeoTrack",
     1                           DFNT_FLOAT32, HDF_NOMERGE)

	      status = swdefdfld(swid, "Temperature", 
     1	                         "GeoXtrack,GeoTrack", DFNT_FLOAT32,
     2                           HDFE_NOMERGE)	      
	      
	      status = swdefdfld(swid, "Pressure", "Res2xtr,Res2tr",
     1	                         DFNT_FLOAT32, HDFE_NOMERGE)
	      
	      status = swdefdfld(swid, "Spectra", "Res2xtr,Res2tr,Bands",
     1                           DFNT_FLOAT64, HDFE_NOMERGE)	      


c       Define Appendable Field
	      status = swdefdfld(swid, "Count", "Unlim",
     1                           DFNT_INT16, HDFE_NOMERGE)	      

	      endif

	   endif

	   status = swdetach(swid)
	   status = swclose(swfid)

	   stop
	   end

