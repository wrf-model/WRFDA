#include "hdf.h"
#include "HdfEosDef.h"

/*
 * In this example we will (1) open the "GridFile" HDF file, (2) attach to
 * the "Grid1" grid, and (3) define the grid fields.
 */


main()
{

    intn            status, i, j;
    int32           gdfid, GDid1, GDid2, nflds;
    int32           dims[8], start[8], count[8];
    
    float32         fillval1=-7.0, fillval2=-9999.0, f32;
    float32         datbuf[100000];
    
    char            fieldlist[255];
    

    /*
     * We first open the HDF grid file, "GridFile.hdf".  Because this file
     * already exist and we wish to write to it, we use the DFACC_RDWR access
     * code in the open statement.  The GDopen routine returns the grid file
     * id, gdfid, which is used to identify the file in subsequent routines.
     */

    gdfid = GDopen("GridFile.hdf", DFACC_RDWR);


    /*
     * If the grid file cannot be found, GDopen will return -1 for the file
     * handle (gdfid).  We there check that this is not the case before
     * proceeding with the other routines.
     * 
     * The GDattach routine returns the handle to the existing grid "Grid1",
     * GDid.  If the grid is not found, GDattach returns -1 for the handle.
     */

    if (gdfid != -1)
    {

	GDid1 = GDattach(gdfid, "UTMGrid");

	status = GDdeffield(GDid1, "Pollution", "Time,YDim,XDim",
			    DFNT_FLOAT32, HDFE_NOMERGE);
	
	status = GDdeffield(GDid1, "Vegetation", "YDim,XDim",
			    DFNT_FLOAT32, HDFE_NOMERGE);

	status = GDwritefieldmeta(GDid1, "Extern", "YDim,XDim",
				  DFNT_FLOAT32);

	status = GDsetfillvalue(GDid1, "Pollution", &fillval1);
	

	

	GDid2 = GDattach(gdfid, "PolarGrid");

	status = GDdeffield(GDid2, "Temperature", "YDim,XDim",
			    DFNT_FLOAT32, HDFE_AUTOMERGE);

	status = GDdeffield(GDid2, "Pressure", "YDim,XDim",
			    DFNT_FLOAT32, HDFE_AUTOMERGE);

	status = GDdeffield(GDid2, "Soil Dryness", "YDim,XDim",
				DFNT_FLOAT32, HDFE_NOMERGE);

	status = GDdeffield(GDid2, "Spectra", "Bands,YDim,XDim",
			    DFNT_FLOAT64, HDFE_AUTOMERGE);

	status = GDsetfillvalue(GDid2, "Pressure", &fillval2);
    }

    GDdetach(GDid1);
    GDdetach(GDid2);

    GDid2 = GDattach(gdfid, "PolarGrid");
    status = GDreadfield(GDid2, "Pressure", NULL, NULL, NULL, datbuf);
    GDdetach(GDid2);


    GDclose(gdfid);

    return;
}
