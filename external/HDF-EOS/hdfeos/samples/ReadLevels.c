#include "mfhdf.h"
#include <math.h>

/* ReadLevels */

main()
{

    intn            status, i, j;
    
    int32           ptfid, n, nt, sz;
    int32           PTid, date, i32;
    float32         rain, temp, conc[4];
    float64         lon, lat, time;
    int32           fldsz, fldlevels[32], nattr;
    int32           fldtype[32], fldorder[32], recs[128];
    
    char           *pntr, *buffer, id[2], desc[16], spc[8];
    char            fldlist[128], attrnames[16];



    /*
     * Open the HDF swath file, "PointFile.hdf".
     */

    ptfid = PTopen("PointFile.hdf", DFACC_READ);


    /* Read Simple Point */
    /* ----------------- */
    PTid = PTattach(ptfid, "Simple Point");

    status = PTlevelinfo(PTid, 0, fldlist, fldtype, fldorder);
    fldsz = PTsizeof(PTid, fldlist, fldlevels);
    n = PTnrecs(PTid, 0);

    buffer = (char *) calloc(n * fldsz, 1);
    for (i=0; i<n; i++)
    {
	recs[i] = i;
    }
    
    status = PTreadlevel(PTid, 0, fldlist, n, recs, buffer);
    pntr = buffer;

    for (i=0; i<n; i++)
    {
	memcpy(&time, pntr, 8);
	pntr += 8;
	memcpy(conc, pntr, 4*4);
	pntr += 4*4;
	memcpy(spc, pntr, 4);
	pntr += 4;
	printf("%12.1f %6.2f %6.2f %6.2f %6.2f %s\n", 
	       time,conc[0],conc[1],conc[2],conc[3],spc);
    }
    
    free(buffer);
    PTdetach(PTid);
    


    /* Read Fixed Buoy Point */
    /* --------------------- */
    PTid = PTattach(ptfid, "FixedBuoy Point");


    /* Read First (0th) Level */
    /* ----------------------- */
    status = PTlevelinfo(PTid, 0, fldlist, fldtype, fldorder);
    fldsz = PTsizeof(PTid, fldlist, fldlevels);

    n = 2;
    recs[0] = 0;
    recs[1] = 2;
    
    buffer = (char *) calloc(n * fldsz, 1);
    pntr = buffer;
    status = PTreadlevel(PTid, 0, fldlist, n, recs, buffer);

    for (i=0; i<n; i++)
    {
	memcpy(desc, pntr, 8);
	pntr += 8;
	memcpy(&lon, pntr, 8);
	pntr += 8;
	memcpy(&lat, pntr, 8);
	pntr += 8;
	memcpy(&date, pntr, 4);
	pntr += 4;
	memcpy(id, pntr, 1);
	pntr += 1;
	printf("%-10s %12.6f %12.6f %10d %-4s\n", desc,lon,lat,date,id);
    }

    free(buffer);


    /* Read Second (1th) Level */
    /* ------------------------ */
    status = PTlevelinfo(PTid, 1, fldlist, fldtype, fldorder);
    fldsz = PTsizeof(PTid, fldlist, fldlevels);
    
    buffer = (char *) calloc(fldsz, 1);
    n = PTnrecs(PTid, 1);

    for (i=0; i<n; i++)
    {
	pntr = buffer;
	recs[0] = i;
	
	status = PTreadlevel(PTid, 1, fldlist, 1, recs, buffer);
	
	memcpy(&time, pntr, 8);
	pntr += 8;
	memcpy(&rain, pntr, 4);
	pntr += 4;
	memcpy(&temp, pntr, 4);
	pntr += 4;
	memcpy(id, pntr, 1);
	pntr += 1;
	printf("%12.1f %6.2f %6.2f %s\n", time,rain,temp,id);
    }

    free(buffer);

    nattr = PTinqattrs(PTid, attrnames);
    status = PTreadattr(PTid, "int32", &i32);
    printf("%d\n", i32);
    status = PTattrinfo(PTid, "int32", &nt, &sz);
    
	
    PTdetach(PTid);

    
    PTclose(ptfid);

    return;
}

