#include "hdf.h"


main()
{

    intn            i, status;
    
    int32           ptfid, PTid, regionID, periodID, size;
    int32           level;
    
    float64         cornerlon[2], cornerlat[2], starttime, stoptime;
    float64        *datbuf;
    


    /*
     * Open the HDF point file, "PointFile.hdf".
     */

    ptfid = PTopen("PointFile.hdf", DFACC_RDWR);


    if (ptfid != -1)
    {

	PTid = PTattach(ptfid, "FloatBuoy Point");

	if (PTid != -1)
	{
	    cornerlon[0] = -145.;
	    cornerlat[0] = -15.;
	    cornerlon[1] = -135;
	    cornerlat[1] = -8.;

	    regionID = PTdefboxregion(PTid, cornerlon, cornerlat);

	    level = 1;
	    status = PTregioninfo(PTid, regionID, level, 
				  "Longitude,Latitude", &size);
	    datbuf = (float64 *) malloc(size);

	    status = PTextractregion(PTid, regionID, level, 
				  "Longitude,Latitude", (char *) datbuf);

	    for (i=0; i<size/16; i++)
	    {
		printf("%d %lf %lf\n", i, datbuf[2*i], datbuf[2*i+1]);
	    }
	    
	    free(datbuf);


	    starttime = 35208757.6;
	    stoptime = 35984639.2;
	    
	    periodID = PTdeftimeperiod(PTid, starttime, stoptime);

	    level = 1;
	    status = PTperiodinfo(PTid, periodID, level, 
				  "Time", &size);
	    datbuf = (float64 *) malloc(size);

	    status = PTextractperiod(PTid, periodID, level, 
				  "Time", datbuf);

	    for (i=0; i<size/8; i++)
	    {
		printf("%d %lf\n", i, datbuf[i]);
	    }

	    free(datbuf);
	    


	}
    }

    PTdetach(PTid);
    PTclose(ptfid);

    return;
}
