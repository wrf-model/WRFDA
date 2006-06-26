#include "mfhdf.h"
#include <math.h>

/* WriteLevels */

main()
{

    intn            status, i, j;
    int16           wgt;
    
    int32           ptfid, dum, n;
    int32           PTid, date, i32 = 9999;
    float32         rain, temp, conc[4], f32 = -7.5;
    float64         lon, lat, time;
    
    char           *pntr, buffer[10000], id[2], desc[16], spc[8];

    FILE           *fp;


    /*
     * Open the HDF swath file, "PointFile.hdf".
     */

    ptfid = PTopen("PointFile.hdf", DFACC_RDWR);


    /* Write to Simple Point */
    /* --------------------- */
    PTid = PTattach(ptfid, "Simple Point");

    fp = fopen("simple.txt", "r");

    n = 0;
    pntr = buffer;
    while(fscanf(fp, "%lf %f %f %f %f %s", 
		 &time, &conc[0], &conc[1], &conc[2], &conc[3], spc) != -1)
    {
	n++;

	spc[strlen(spc)-1] = 0;
	
	memcpy(pntr, &time, 8);
	pntr += 8;
	memcpy(pntr, conc, 4*4);
	pntr += 4*4;
	memcpy(pntr, spc + 1, 4);
	pntr += 4;
    }
    fclose(fp);
    
    status = PTwritelevel(PTid, 0, n, buffer);

    PTdetach(PTid);
    


    /* Write to Fixed Buoy Point */
    /* ------------------------- */
    PTid = PTattach(ptfid, "FixedBuoy Point");


    /* Write First (0th) Level */
    /* ----------------------- */
    fp = fopen("fixedBuoy0.txt", "r");

    n = 0;
    pntr = buffer;
    while(fscanf(fp, "%s %lf %lf %d %s", desc, &lon, &lat, &date, id) != -1)
    {
	n++;
	
	desc[strlen(desc)-1] = 0;
	id[strlen(id)-1] = 0;

	memcpy(pntr, desc + 1, 8);
	pntr += 8;
	memcpy(pntr, &lon, 8);
	pntr += 8;
	memcpy(pntr, &lat, 8);
	pntr += 8;
	memcpy(pntr, &date, 4);
	pntr += 4;
	memcpy(pntr, id + 1, 1);
	pntr += 1;
    }
    fclose(fp);
    
    status = PTwritelevel(PTid, 0, n, buffer);




    /* Write Second (1th) Level */
    /* ------------------------ */
    fp = fopen("fixedBuoy1.txt", "r");

    n = 0;
    pntr = buffer;
    while(fscanf(fp, "%lf %f %f %s", &time, &rain, &temp, id) != -1)
    {
	n++;
	
	id[strlen(id)-1] = 0;

	memcpy(pntr, &time, 8);
	pntr += 8;
	memcpy(pntr, &rain, 4);
	pntr += 4;
	memcpy(pntr, &temp, 4);
	pntr += 4;
	memcpy(pntr, id + 1, 1);
	pntr += 1;
    }
    fclose(fp);
    
    status = PTwritelevel(PTid, 1, n, buffer);

    status = PTwriteattr(PTid, "int32", DFNT_INT32, 1, &i32);

    PTdetach(PTid);
    





    /* Write to Floating Buoy Point */
    /* ---------------------------- */
    PTid = PTattach(ptfid, "FloatBuoy Point");


    /* Write First (0th) Level */
    /* ----------------------- */
    fp = fopen("floatBuoy0.txt", "r");

    n = 0;
    pntr = buffer;
    while(fscanf(fp, "%s %d %d %s", desc, &date, &dum, id) != -1)
    {
	n++;
	
	wgt = (int16) dum;
	
	desc[strlen(desc)-1] = 0;
	id[strlen(id)-1] = 0;

	memcpy(pntr, desc + 1, 8);
	pntr += 8;
	memcpy(pntr, &date, 4);
	pntr += 4;
	memcpy(pntr, &wgt, 2);
	pntr += 2;
	memcpy(pntr, id + 1, 1);
	pntr += 1;
    }
    fclose(fp);
    
    status = PTwritelevel(PTid, 0, n, buffer);




    /* Write Second (1th) Level */
    /* ------------------------ */
    fp = fopen("floatBuoy1.txt", "r");

    n = 0;
    pntr = buffer;
    while(fscanf(fp, "%lf %lf %lf %f %f %s", 
		 &time, &lon, &lat, &rain, &temp, id) != -1)
    {
	n++;
	
	id[strlen(id)-1] = 0;

	memcpy(pntr, &time, 8);
	pntr += 8;
	memcpy(pntr, &lon, 8);
	pntr += 8;
	memcpy(pntr, &lat, 8);
	pntr += 8;
	memcpy(pntr, &rain, 4);
	pntr += 4;
	memcpy(pntr, &temp, 4);
	pntr += 4;
	memcpy(pntr, id + 1, 1);
	pntr += 1;
    }
    fclose(fp);
    
    status = PTwritelevel(PTid, 1, n, buffer);


    status = PTwriteattr(PTid, "float32", DFNT_FLOAT32, 1, &f32);
    
    PTdetach(PTid);
    

    
    PTclose(ptfid);

    return;
}
