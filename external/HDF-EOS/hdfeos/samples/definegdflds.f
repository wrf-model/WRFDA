
	program definegdflds

	integer            status, gddeffld, gdsetfill, gdwrmeta
	integer            gddetach, gdclose
	integer*4          gdfid, gdid1, gdid2, gdopen, gdattach
    
	real*4             fillval1, fillval2
	
	integer DFACC_RDWR
	parameter (DFACC_RDWR=3)
	integer DFNT_FLOAT32
	parameter (DFNT_FLOAT32=5)
	integer DFNT_FLOAT64
	parameter (DFNT_FLOAT64=6)
	integer HDFE_NOMERGE
	parameter (HDFE_NOMERGE=0)
	integer HDFE_AUTOMERGE
	parameter (HDFE_AUTOMERGE=1)

	fillval1=-7.0
	fillval2=-9999.0

	gdfid = gdopen("GridFile.hdf", DFACC_RDWR)


	gdid1 = gdattach(gdfid, "UTMGrid")

	status = gddeffld(gdid1, "Pollution", "XDim,YDim,Time",
     1			    DFNT_FLOAT32, HDFE_NOMERGE)
	
	status = gddeffld(gdid1, "Vegetation", "XDim,YDim",
     1			    DFNT_FLOAT32, HDFE_NOMERGE)

	status = gdwrmeta(gdid1, "Extern", "XDim,YDim",
     1				  DFNT_FLOAT32, HDFE_NOMERGE)

	status = gdsetfill(gdid1, "Pollution", fillval1)
     1	

	

	gdid2 = gdattach(gdfid, "PolarGrid")

	status = gddeffld(gdid2, "Temperature", "XDim,YDim",
     1			    DFNT_FLOAT32, HDFE_AUTOMERGE)

	status = gddeffld(gdid2, "Pressure", "XDim,YDim",
     1			    DFNT_FLOAT32, HDFE_AUTOMERGE)

	status = gddeffld(gdid2, "Soil Dryness", "XDim,YDim",
     1				DFNT_FLOAT32, HDFE_NOMERGE)

	status = gddeffld(gdid2, "Spectra", "XDim,YDim,Bands",
     1			    DFNT_FLOAT64, HDFE_AUTOMERGE)

	status = gdsetfill(gdid2, "Pressure", fillval2)

	status = gddetach(gdid1)
	status = gddetach(gdid2)


	status = gdclose(gdfid)

	stop
	end
