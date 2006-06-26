#include "hdf.h"

/*
 * In this example we will demonstrate the use of the unlimited dimension
 * in creating a field that can be appended to.
 */


main()
{

    intn            status, i;
    int16           inarray[5] = {1, 2, 3, 4, 5}, outarray[10];
    int32           swfid, SWid, rank, dims[1], ntype;
    int32           start[1], stride[1], count[1];
    

    swfid = SWopen("SwathFile.hdf", DFACC_RDWR);
    SWid = SWattach(swfid, "Swath1");

    status = SWfieldinfo(SWid, "Count", &rank, dims, &ntype, NULL);
    printf("Initial number of elements: %d\n",dims[0]);

    /* Write 5 records to field */
    /* ------------------------ */
    count[0] = 5;
    SWwritefield(SWid, "Count", NULL, NULL, count, inarray);


    status = SWfieldinfo(SWid, "Count", &rank, dims, &ntype, NULL);
    printf("Number of elements after first write: %d\n",dims[0]);


    
    /* Append 1 record to field */
    /* ------------------------ */
    start[0] = dims[0];
    count[0] = 1;
    SWwritefield(SWid, "Count", start, NULL, count, inarray);


    status = SWfieldinfo(SWid, "Count", &rank, dims, &ntype, NULL);
    printf("Number of elements after append: %d\n",dims[0]);

    SWreadfield(SWid, "Count", NULL, NULL, dims, outarray);
    for (i=0; i<dims[0]; i++) printf("Data Element: %d\n",outarray[i]);
    
    status = SWdetach(SWid);
    status = SWclose(swfid);

    return;
}
