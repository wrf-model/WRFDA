#include "mfhdf.h"

/* DefineLevels */

/*
 * In this example we will open the "PointFile" HDF file and define the point
 * levels.
 */


main()
{

    intn            status;
    int32           ptfid, PTid, fieldtype[8], fieldorder[8];
    char            fldlist[255];


    /*
     * We first open the HDF swath file, "PointFile.hdf".  Because this file
     * already exist and we wish to write to it, we use the DFACC_RDWR access
     * code in the open statement.  The PTopen routine returns the point file
     * id, ptfid, which is used to identify the file in subsequent routines.
     */

    ptfid = PTopen("PointFile.hdf", DFACC_RDWR);



    if (ptfid != -1)
    {


	/* Simple Point */
	/* ------------ */
	PTid = PTattach(ptfid, "Simple Point");
	strcpy(fldlist, "Time,Concentration,Species");

	fieldtype[0] = DFNT_FLOAT64;
	fieldtype[1] = DFNT_FLOAT32;
	fieldtype[2] = DFNT_CHAR8;
	
	fieldorder[0] = 1;
	fieldorder[1] = 4;
	fieldorder[2] = 4;
	
	status = PTdeflevel(PTid, "Sensor", fldlist, fieldtype, fieldorder);
	
	PTdetach(PTid);

	goto skip;

	/* Fixed Buoy Point */
	/* ---------------- */
	PTid = PTattach(ptfid, "FixedBuoy Point");


	/* Define Description/Location Level */
	strcpy(fldlist, "Label,Longitude,Latitude,DeployDate,ID");
	
	fieldtype[0] = DFNT_CHAR8;
	fieldtype[1] = DFNT_FLOAT64;
	fieldtype[2] = DFNT_FLOAT64;
	fieldtype[3] = DFNT_INT32;
	fieldtype[4] = DFNT_CHAR8;

	fieldorder[0] = 8;
	fieldorder[1] = 0;  /* Order 0 same as Order 1 for numeric scalars */
	fieldorder[2] = 0;
	fieldorder[3] = 0;
	fieldorder[4] = 1;

	status = PTdeflevel(PTid, "Desc-Loc", fldlist, 
			    fieldtype, fieldorder);


	/* Define Data Level */
	strcpy(fldlist, "Time,Rainfall,Temperature,ID");
	
	fieldtype[0] = DFNT_FLOAT64;
	fieldtype[1] = DFNT_FLOAT32;
	fieldtype[2] = DFNT_FLOAT32;
	fieldtype[3] = DFNT_CHAR8;

	fieldorder[0] = 0;
	fieldorder[1] = 0;
	fieldorder[2] = 0;
	fieldorder[3] = 1;

	status = PTdeflevel(PTid, "Observations", fldlist, 
			    fieldtype, fieldorder);

	printf("status from PTdeflevel: %d\n");
	
	status = PTdeflinkage(PTid, "Desc-Loc", "Observations", "ID");
	printf("status from PTdeflinkage: %d\n");
	status = PTdetach(PTid);
	printf("status from PTdetach: %d\n");



	

	/* Floating Buoy Point */
	/* ------------------- */
	PTid = PTattach(ptfid, "FloatBuoy Point");


	/* Define Description Level */
	strcpy(fldlist, "Label,DeployDate,Weight,ID");
	
	fieldtype[0] = DFNT_CHAR8;
	fieldtype[1] = DFNT_INT32;
	fieldtype[2] = DFNT_INT16;
	fieldtype[3] = DFNT_CHAR8;

	fieldorder[0] = 8;
	fieldorder[1] = 0;
	fieldorder[2] = 0;
	fieldorder[3] = 1;


	status = PTdeflevel(PTid, "Description", fldlist, 
			    fieldtype, fieldorder);


	/* Define Data Level */
	strcpy(fldlist, "Time,Longitude,Latitude,Rainfall,Temperature,ID");

	fieldtype[0] = DFNT_FLOAT64;
	fieldtype[1] = DFNT_FLOAT64;
	fieldtype[2] = DFNT_FLOAT64;
	fieldtype[3] = DFNT_FLOAT32;
	fieldtype[4] = DFNT_FLOAT32;
	fieldtype[5] = DFNT_CHAR8;

	fieldorder[0] = 0;
	fieldorder[1] = 0;
	fieldorder[2] = 0;
	fieldorder[3] = 0;
	fieldorder[4] = 0;
	fieldorder[5] = 1;	

	status = PTdeflevel(PTid, "Measurements", fldlist, 
			    fieldtype, fieldorder);

	status = PTdeflinkage(PTid, "Description", "Measurements", "ID");

	PTdetach(PTid);

      skip:
	PTclose(ptfid);
    }
    
    return;
}
