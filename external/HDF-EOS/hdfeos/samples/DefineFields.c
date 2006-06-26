#include "hdf.h"
#include "HdfEosDef.h"

/*
 * In this example we will (1) open the "SwathFile" HDF file, (2) attach to
 * the "Swath1" swath, and (3) define the swath fields.
 */


main()
{

    intn            status, i, j;
    int32           swfid, SWid;


    /*
     * We first open the HDF swath file, "SwathFile.hdf".  Because this file
     * already exist and we wish to write to it, we use the DFACC_RDWR access
     * code in the open statement.  The SWopen routine returns the swath file
     * id, swfid, which is used to identify the file in subsequent routines.
     */

    swfid = SWopen("SwathFile.hdf", DFACC_RDWR);


    /*
     * If the swath file cannot be found, SWopen will return -1 for the file
     * handle (swfid).  We there check that this is not the case before
     * proceeding with the other routines.
     * 
     * The SWattach routine returns the handle to the existing swath "Swath1",
     * SWid.  If the swath is not found, SWattach returns -1 for the handle.
     */

    if (swfid != -1)
    {

	SWid = SWattach(swfid, "Swath1");

	if (SWid != -1)
	{

	    /*
	     * We define seven fields.  The first three, "Time", "Longitude"
	     * and "Latitude" are geolocation fields and thus we use the
	     * geolocation dimensions "GeoTrack" and "GeoXtrack" in the field
	     * definitions.  We also must specify the data type using the
	     * standard HDF data type codes.  In this example the geolocation
	     * are 4-byte (32 bit) floating point numbers.
	     * 
	     * The next four fields are data fields.  Note that either
	     * geolocation or data dimensions can be used.  If an error
	     * occurs during the definition, such as a dimension that cannot
	     * be found, then the return status will be set to -1.
	     */

	    status = SWdefgeofield(SWid, "Time", "GeoTrack",
				   DFNT_FLOAT64, HDFE_NOMERGE);

	    status = SWdefgeofield(SWid, "Longitude", "GeoTrack,GeoXtrack",
				   DFNT_FLOAT32, HDFE_AUTOMERGE);

	    status = SWdefgeofield(SWid, "Latitude", "GeoTrack,GeoXtrack",
				   DFNT_FLOAT32, HDFE_AUTOMERGE);

	    status = SWdefdatafield(SWid, "Density", "GeoTrack",
				    DFNT_FLOAT32, HDFE_NOMERGE);

	    status = SWdefdatafield(SWid, "Temperature", "GeoTrack,GeoXtrack",
				    DFNT_FLOAT32, HDFE_NOMERGE);

	    status = SWdefdatafield(SWid, "Pressure", "Res2tr,Res2xtr",
				    DFNT_FLOAT64, HDFE_NOMERGE);

	    status = SWdefdatafield(SWid, "Spectra", "Bands,Res2tr,Res2xtr",
				    DFNT_FLOAT64, HDFE_NOMERGE);


	    /* Define Appendable Field */
	    /* ----------------------- */
	    status = SWdefdatafield(SWid, "Count", "Unlim", DFNT_INT16,
				    HDFE_NOMERGE);
	}
    }
    status = SWdetach(SWid);
    status = SWclose(swfid);

    return;
}
