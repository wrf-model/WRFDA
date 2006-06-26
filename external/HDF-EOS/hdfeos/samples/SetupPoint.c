#include "hdf.h"

/* SetupPoint */

main()
{

    intn            status;
    int32           ptfid, PTid1, PTid2, PTid3;


    /*
     * We first open the HDF point file, "PointFile.hdf".  Because this file
     * does not already exist, we use the DFACC_CREATE access code in the
     * open statement.  The PTopen routine returns the point file id, ptfid,
     * which is used to identify the file in subsequent routines in the
     * library.
     */

    ptfid = PTopen("PointFile.hdf", DFACC_CREATE);


    PTid1 = PTcreate(ptfid, "Simple Point");
    PTid2 = PTcreate(ptfid, "FixedBuoy Point");
    PTid3 = PTcreate(ptfid, "FloatBuoy Point");

    /*
     * We now close the point interface with the PTdetach routine.  This step
     * is necessary to properly store the point information within the file.
     */

    PTdetach(PTid1);
    PTdetach(PTid2);
    PTdetach(PTid3);

    HEprint(stdout,0);


    /*
     * Finally, we close the swath file using the PTclose routine.  This will
     * release the point file handles established by PTopen.
     */

    PTclose(ptfid);
    HEprint(stdout,0);

    return;
}


