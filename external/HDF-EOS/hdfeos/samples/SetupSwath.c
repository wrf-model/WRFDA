#include "mfhdf.h"
/*
 * In this example we will (1) open an HDF file, (2) create the swath
 * interface within the file and (3) define the swath field dimensions.
 */


main()
{

    intn            status, i, j;
    int32           swfid, SWid, indx[12]={0,1,3,6,7,8,11,12,14,24,32,39};


    /*
     * We first open the HDF swath file, "SwathFile.hdf".  Because this file
     * does not already exist, we use the DFACC_CREATE access code in the
     * open statement.  The SWopen routine returns the swath file id, swfid,
     * which is used to identify the file in subsequent routines in the
     * library.
     */

    swfid = SWopen("SwathFile.hdf", DFACC_CREATE);


    /*
     * The first of these, SWcreate, creates the swath, "Swath1", within the
     * file designated by the file id, swfid.  It returns the swath id, SWid,
     * which identifies the swath in subsequent routines.  We will show how
     * to define, write and read field swaths in later programs.
     */

    SWid = SWcreate(swfid, "Swath1");


    /*
     * Typically, many fields within a swath share the same dimension. The
     * swath interface therefore provides a way of defining dimensions that
     * will then be used to define swath fields.  A dimension is defined with
     * a name and a size and is connected to the particular swath through the
     * swath id.  In this example, we define the geolocation track and
     * cross track dimensions with size 20 and 10 respectively and two
     * dimensions corresponding to these but with twice the resolution.
     * We also define a dimension corresponding to a number of spectal
     * bands.
     */

    status = SWdefdim(SWid, "GeoTrack", 20);
    status = SWdefdim(SWid, "GeoXtrack", 10);

    status = SWdefdim(SWid, "Res2tr", 40);
    status = SWdefdim(SWid, "Res2xtr", 20);
    status = SWdefdim(SWid, "Bands", 15);
    status = SWdefdim(SWid, "IndxTrack", 12);
    
    /* Define Unlimited Dimension */
    /* -------------------------- */
    status = SWdefdim(SWid, "Unlim", NC_UNLIMITED);


    /*
     * Once the dimensions are defined, the relationship (mapping) between the
     * geolocation dimensions, such as track and cross track, and the data
     * dimensions, must be established.  This is done through the SWdefdimmap
     * routine.  It takes as input the swath id, the names of the dimensions
     * designating the geolocation and data dimensions, respectively, and the
     * offset and increment defining the relation.
     * 
     * In the first example we relate the "GeoTrack" and "Res2tr" dimensions
     * with an offset of 0 and an increment of 2.  Thus the ith element of
     * "Geotrack" corresponds to the 2 * ith element of "Res2tr".
     * 
     * In the second example, the ith element of "GeoXtrack" corresponds to the
     * 2 * ith + 1 element of "Res2xtr".
     *
     * Note that there is no relationship between the geolocation dimensions
     * and the "Bands" dimension.
     */

    status = SWdefdimmap(SWid, "GeoTrack", "Res2tr", 0, 2);
    status = SWdefdimmap(SWid, "GeoXtrack", "Res2xtr", 1, 2);

    /* Define Indexed Mapping */
    status = SWdefidxmap(SWid, "IndxTrack", "Res2tr", indx);

    /*
     * We now close the swath interface with the SWdetach routine.  This step
     * is necessary to properly store the swath information within the file.
     */

    status = SWdetach(SWid);


    /*
     * Finally, we close the swath file using the SWclose routine.  This will
     * release the swath file handles established by SWopen.
     */

    status = SWclose(swfid);

    return;
}
