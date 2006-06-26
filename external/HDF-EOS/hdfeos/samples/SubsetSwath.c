#include "hdf.h"
#include "HdfEosDef.h"

/*
 * In this example we will (1) open the "SwathFile" HDF file, (2) attach to
 * the "UTMSwath", and (3) read data from the "Vegetation" field.
 */


main()
{

    intn            i, j, status;
    
    int32           swfid, SWid, regionID, size, periodID;
    int32           firstXtrk, LastXtrk, dims[8], rank, ntype;
    
    float64         cornerlon[2], cornerlat[2];
    float64        *datbuf;
    


    /*
     * Open the HDF swath file, "SwathFile.hdf".
     */

    swfid = SWopen("SwathFile.hdf", DFACC_RDWR);


    if (swfid != -1)
    {

	SWid = SWattach(swfid, "Swath1");

	if (SWid != -1)
	{
	    cornerlon[0] = 3.;
	    cornerlat[0] = 5.;
	    cornerlon[1] = 7.;
	    cornerlat[1] = 12.;

	    regionID = SWdefboxregion(SWid, cornerlon, cornerlat,
				      HDFE_MIDPOINT);


	    status = SWregioninfo(SWid, regionID, "Longitude", &ntype,
				  &rank, dims, &size);

	    status = SWregioninfo(SWid, regionID, "Spectra", &ntype,
				  &rank, dims, &size);

	    datbuf = (float64 *) malloc(size);

	    status = SWextractregion(SWid, regionID, "Spectra", 
				     HDFE_INTERNAL, datbuf);
	    
	    free(datbuf);



	    /* Time Subsetting */
	    periodID = SWdeftimeperiod(SWid, 35232487.2, 36609898.1,
				      HDFE_MIDPOINT);

	    status = SWperiodinfo(SWid, periodID, "Time", &ntype,
				  &rank, dims, &size);

	    datbuf = (float64 *) malloc(size);

	    status = SWextractperiod(SWid, periodID, "Time", 
				     HDFE_INTERNAL, datbuf);
	    
	    free(datbuf);
	}
    }

    SWdetach(SWid);

    SWclose(swfid);

    return;
}
