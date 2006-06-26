#include "hdf.h"
#include <math.h>

/*
 * In this example we will (1) open the "GridFile" HDF file, (2) attach to
 * the "PolarGrid", and (3) subset data from the "Temperature" field.
 */


main()
{

    intn            i, j, status;
    
    int32           gdfid, GDid, regionID, size, dims[8], ntype, rank;

    float32        *datbuf32;
    
    float64         cornerlon[2], cornerlat[2];
    float64        *datbuf64, upleft[2], lowright[2];
    


    /*
     * Open the HDF grid file, "GridFile.hdf".
     */

    gdfid = GDopen("GridFile.hdf", DFACC_RDWR);


    if (gdfid != -1)
    {

	GDid = GDattach(gdfid, "PolarGrid");

	if (GDid != -1)
	{
	    cornerlon[0] = 57.;
	    cornerlat[0] = 23.;
	    cornerlon[1] = 59.;
	    cornerlat[1] = 35.;

	    cornerlon[0] = 0.;
	    cornerlat[0] = 90.;
	    cornerlon[1] = 90.;
	    cornerlat[1] = 0.;


	    regionID = GDdefboxregion(GDid, cornerlon, cornerlat);
	    
	    status = GDregioninfo(GDid, regionID, "Temperature", &ntype,
				&rank, dims, &size, upleft, lowright);
	    printf("size: %d\n",size);
	    
	    datbuf32 = (float32 *) calloc(size, 1);
	    
	    status = GDextractregion(GDid, regionID, "Temperature", 
				     datbuf32);
	    
	    free(datbuf32);
	}
    }

    GDdetach(GDid);

    GDclose(gdfid);

    return;
}
