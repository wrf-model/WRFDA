#include "mfhdf.h"

/*   UpdateLevels */

main()
{

    intn            status, i, j;
    int32           ptfid;
    int32           PTid1, PTid2;
    int32           nrec, recs[32];
    int32           outNrec, outRecs[32];
    int32           inLevel, outLevel;
    
    char            datbuf[1024];
    float64         f64;
    

    /*
     * Open the HDF point file, "PointFile.hdf".
     */

    ptfid = PTopen("PointFile.hdf", DFACC_RDWR);


    if (ptfid != -1)
    {

	PTid1 = PTattach(ptfid, "Simple Point");
	PTid2 = PTattach(ptfid, "FixedBuoy Point");

	if (PTid1 != -1 && PTid2 != -1)
	{

	    f64 = 43.2;
	    memcpy(datbuf, &f64, 8);
	    recs[0] = 1;
	    
	    status = PTupdatelevel(PTid2, 0, "Longitude", 1, recs, datbuf);


	    datbuf[0] = 'F';
	    nrec = 1;
	    recs[0] = 0;
	    status = PTupdatelevel(PTid2, 0, "ID", nrec, recs, datbuf);

	    inLevel = 0;
	    outLevel = 1;
	    status = PTgetrecnums(PTid2, inLevel, outLevel, nrec, recs, 
				  &outNrec, outRecs);


	    for (i=0; i<outNrec; i++)
	    {
		datbuf[i] = 'F';
	    }

	    status = PTupdatelevel(PTid2, outLevel, "ID", 
				   outNrec, outRecs, datbuf);

	}
    }

    PTdetach(PTid1);
    PTdetach(PTid2);

    PTclose(ptfid);
    HEprint(stdout,0);
    
    return;
}
