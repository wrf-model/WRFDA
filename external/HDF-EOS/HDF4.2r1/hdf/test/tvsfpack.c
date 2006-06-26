#include <stdio.h>
#include <math.h>
#include "vg.h"
#include "tproto.h"
#define NRECORDS 5
#define EPS     0.00001

#define FIELD_1 "Ident"
#define FIELD_2 "Temp"
#define FIELD_3 "Speed"
#define FIELD_4 "Height"
#define FIELD_NAMES "Ident,Temp,Speed,Height"
#define FILENAME    "tvpack.hdf"
struct {
    char         ident;
    float32      temp;
    int16        speed;
    float32      height;
} source[NRECORDS];

int32      file_id, vdata_id, istat, msize = 0;
uint8      *databuf, *pntr;
VOIDP	   databufptr[10];	/* make sure its size can hold all fields */

float32    tempdata[NRECORDS], itemp[NRECORDS];
float32    heightdata[NRECORDS], iheight[NRECORDS];
int16      speeddata[NRECORDS], ispeed[NRECORDS];
char       identdata[NRECORDS], iident[NRECORDS];
int        i, j, rec_size;
int32      vdata_ref, in_recs, iil, irec_size;
char       ifields[256];

static int32 fpack(void);
static int32 funpack(void);

static int32 
fpack(void)
{
    /* Open the HDF file. */
    file_id = Hopen(FILENAME, DFACC_CREATE, 0);

    /* Initialize the Vset interface. */
    Vstart(file_id);

    /* Create a new Vdata. */
    vdata_id = VSattach(file_id, -1, "w");

    /* Define the field to write. */
    istat = VSfdefine(vdata_id, FIELD_1, DFNT_CHAR8, 1);
    istat = VSfdefine(vdata_id, FIELD_2, DFNT_FLOAT32, 1);
    istat = VSfdefine(vdata_id, FIELD_3, DFNT_INT16, 1);
    istat = VSfdefine(vdata_id, FIELD_4, DFNT_FLOAT32, 1);

    /* Set the Vdata name. */
    VSsetname(vdata_id, "myvdata");
   /* Set the Vdata class. */
    VSsetclass(vdata_id, "pack");

    /* Set the field names. */
    istat = VSsetfields(vdata_id, FIELD_NAMES);
    rec_size = 2*sizeof(float32) + sizeof(int16) + sizeof(char);

    databuf = (uint8 *) malloc(((2 * sizeof(float32))
                + sizeof(int16) + sizeof(char)) * NRECORDS);

    pntr = databuf;
    /* pack a record at a time */
    for (i = 0; i < NRECORDS; i++) {
        source[i].temp = (float32)1.11 * (float32)(i+1);
        source[i].height = (float32)2.22 * (float32)(i+1);
        source[i].speed = (int16)i;
        source[i].ident = (char)('A' + i);
         /* test error checks  */ 
        if (i==0) {
	    /* test not enough vdata buffer size */
	    databufptr[0] = &source[i].ident;
	    databufptr[1] = &source[i].temp;
	    databufptr[2] = &source[i].speed;
	    databufptr[3] = &source[i].height;
            istat = VSfpack(vdata_id,_HDF_VSPACK, NULL, pntr, 1,
                   1, NULL, databufptr);
            if (istat != FAIL)  {
                num_errs++;
                printf(">>> VSfpack failed in checking bufsz %d\n", i);
            }
            pntr = databuf;
	    /* test wrong field name */
	    databufptr[0] = &source[i].ident;
	    databufptr[1] = &source[i].temp;
	    databufptr[2] = &source[i].speed;
	    databufptr[3] = &source[i].height;
            istat = VSfpack(vdata_id,_HDF_VSPACK, NULL, pntr, rec_size,
                 1,"Idents,Temp,Speed,Height" , databufptr);
            if (istat != FAIL)  {
                num_errs++;
                printf(">>> VSfpack failed in checking field names.\n");
            }
            pntr = databuf;
        }
          /* normal test */
	databufptr[0] = &source[i].ident;
	databufptr[1] = &source[i].temp;
	databufptr[2] = &source[i].speed;
	databufptr[3] = &source[i].height;
        istat = VSfpack(vdata_id,_HDF_VSPACK, NULL, pntr, rec_size,
                   1, NULL, databufptr);
        if (istat == FAIL)  {
            num_errs++;
            printf(">>> VSfpack failed in packing record %d\n", i);
        }
 
        pntr += rec_size;
    }
    /* Write the data to the Vset object. */
    istat = VSwrite(vdata_id, databuf, NRECORDS, FULL_INTERLACE);
    if (istat != NRECORDS)  {
         num_errs++;
         printf(">>> VSwrite failed in write %d records.\n", NRECORDS);
    }

    /* pack a field at a time */
    pntr = databuf;
    for (i = 0; i < NRECORDS; i++) {
        source[i].temp = (float32)3.33 * (float32)(i+1);
        source[i].height = (float32)4.44 * (float32)(i+1);
        source[i].speed = (int16)(2*i);
        source[i].ident = (char)('a' + i);
	databufptr[0] = &source[i].speed;
        istat = VSfpack(vdata_id,_HDF_VSPACK,NULL, pntr,rec_size,
                   1, FIELD_3, databufptr);
        if (istat == FAIL)  {
            num_errs++;
            printf(">>> VSfpack failed in packing speed.\n");
        }
	databufptr[0] = &source[i].ident;
        istat = VSfpack(vdata_id,_HDF_VSPACK,NULL, pntr,rec_size,
                   1, FIELD_1, databufptr);
        if (istat == FAIL)  {
            num_errs++;
            printf(">>> VSfpack failed in packing ident.\n");
        }
	databufptr[0] = &source[i].temp;
        istat = VSfpack(vdata_id,_HDF_VSPACK,NULL, pntr,rec_size,
                   1, FIELD_2, databufptr);
        if (istat == FAIL)  {
            num_errs++;
            printf(">>> VSfpack failed in packing temp.\n");
        }
	databufptr[0] = &source[i].height;
        istat = VSfpack(vdata_id,_HDF_VSPACK,NULL, pntr,rec_size,
                   1, FIELD_4, databufptr);
        if (istat == FAIL)  {
            num_errs++;
            printf(">>> VSfpack failed in packing height.\n");
        }
        pntr += rec_size;
    }
    /* Write the data to the Vset object. */
    istat = VSwrite(vdata_id, databuf, NRECORDS, FULL_INTERLACE);
    if (istat != NRECORDS)  {
         num_errs++;
         printf(">>> VSwrite failed in write %d records.\n", NRECORDS);
    }

    /*
    * Terminate access to the Vdata, the Vset interface
    * and the HDF file.
    */
    VSdetach(vdata_id);
    Vend(file_id);
    istat = Hclose(file_id);
    return SUCCEED;
}

static int32
funpack(void)
{
    /* read data back */

    /* Open the HDF file. */
    file_id = Hopen(FILENAME, DFACC_RDWR, 0);

    /* Initialize the Vset interface. */
    Vstart(file_id);
    vdata_ref = VSfind(file_id, "myvdata"); 
    if (vdata_ref == 0)  {
         num_errs++;
         printf(">>> VSfind failed in finding myvdata.\n");
    }
    vdata_id = VSattach(file_id, vdata_ref, "w");
    if (vdata_id == FAIL) {
         num_errs++;
         printf(">>> VSattach failed in attaching myvdata.\n");
    }
    istat = VSinquire(vdata_id, &in_recs, &iil, ifields, &irec_size,
                       NULL);
    if (istat == FAIL) {     
         num_errs++;     
         printf(">>> VSinquire failed in for myvdata.\n");
    }     
    if ((in_recs != 2*NRECORDS) || (irec_size != rec_size)) {
         num_errs++;
         printf(">>> VSinquire got wrong info for myvdata.\n");
    }
    istat = VSsetfields(vdata_id, ifields);
    istat = VSread(vdata_id, databuf, NRECORDS, iil);
    pntr = databuf;
    databufptr[0] = iident;
    databufptr[1] = itemp;
    databufptr[2] = ispeed;
    databufptr[3] = iheight;
    istat = VSfpack(vdata_id,_HDF_VSUNPACK,NULL,pntr,rec_size*NRECORDS,
                     NRECORDS, NULL, databufptr);
    if (istat == FAIL)  {
         num_errs++;
         printf(">>> VSfpack failed in unpacking 1st set.\n");
    }

    for (i=0; i<NRECORDS; i++)  
         if ((iident[i] != (char)('A'+i)) || (fabs((double)(itemp[i] - (float32)1.11*(float32)(i+1))) > EPS) ||
            (ispeed[i] != i) || (fabs((double)(iheight[i] - (float32)2.22*(float32)(i+1))) > EPS))  {
            num_errs++;
            printf(">>> Wrong data1 after VSfpack.\n");
        }
    /* check the second set of records */
    istat = VSread(vdata_id, databuf, NRECORDS, iil);
    pntr = databuf;
    databufptr[0] = itemp;
    databufptr[1] = iident;
    istat = VSfpack(vdata_id, _HDF_VSUNPACK, NULL, pntr, 
                     rec_size*NRECORDS, NRECORDS, "Temp,Ident", 
                     databufptr);
    if (istat == FAIL)  {
         num_errs++;
         printf(">>> VSfpack failed in unpacking temp & ident.\n");
    }
    databufptr[0] = iheight;
    databufptr[1] = ispeed;
    istat = VSfpack(vdata_id, _HDF_VSUNPACK, NULL, pntr, 
                     rec_size*NRECORDS, NRECORDS, "Height,Speed", 
                     databufptr);
    if (istat == FAIL)  {
         num_errs++;
         printf(">>> VSfpack failed in unpacking height & speed.\n");
    }
 
    for (i=0; i<NRECORDS; i++)  
         if ((iident[i] != (char)('a'+i)) || (fabs((double)(itemp[i] - (float32)3.33*(float32)(i+1))) > EPS) ||
            (ispeed[i] != 2*i) || (fabs((double)(iheight[i] - (float32)4.44*(float32)(i+1))) > EPS))  {
            num_errs++;
            printf(">>> Wrong data2 after VSfpack.\n");
        }
         /*
    * Terminate access to the Vdata, the Vset interface
    * and the HDF file.
    */
    VSdetach(vdata_id);
    /* buf contains only subset of vdata fields  */
    vdata_id = VSattach(file_id, vdata_ref, "w");
    istat = VSsetfields(vdata_id, "Height,Speed,Ident");
    if (istat == FAIL) {
            num_errs++;
            printf(">>> VSsetfields failed in set 3 flds.\n");
    }
    istat = VSread(vdata_id, databuf, NRECORDS, FULL_INTERLACE);
    if (istat == FAIL) {
            num_errs++;
            printf(">>> VSread failed in reading 3 flds.\n");
    }
    pntr = databuf;
    databufptr[0] = iheight;
    databufptr[1] = ispeed;
    databufptr[2] = iident;
    istat = VSfpack(vdata_id, _HDF_VSUNPACK, "Height,Speed,Ident",
                pntr, rec_size*NRECORDS, NRECORDS, NULL, databufptr);
    if (istat == FAIL)  {
         num_errs++;
         printf(">>> VSfpack failed in unpacking 1st subset.\n");
    }
    for (i=0; i<NRECORDS; i++)
         if ((iident[i] != 'A'+i) || (ispeed[i] != i) || 
              (fabs((double)(iheight[i] - (float32)2.22*(float32)(i+1))) > EPS))  {
            num_errs++;
            printf(">>> Wrong subset data1 after VSfpack.\n");
        }
    /* check the second subset of records */
    istat = VSread(vdata_id, databuf, NRECORDS, FULL_INTERLACE);
    pntr = databuf;
    databufptr[0] = iident;
    databufptr[1] = ispeed;
    databufptr[2] = iheight;
    istat = VSfpack(vdata_id, _HDF_VSUNPACK, "Height,Speed,Ident",
               pntr, rec_size*NRECORDS, NRECORDS, "Ident,Speed,Height",
               databufptr);
    if (istat == FAIL)  {
         num_errs++;
         printf(">>> VSfpack failed in unpacking 2nd subset\n");
    }
    for (i=0; i<NRECORDS; i++)
         if ((iident[i] != (char)('a'+i)) || (ispeed[i] != 2*i) || 
              (fabs((double)(iheight[i] - (float32)4.44*(float32)(i+1))) > EPS)) {
            num_errs++;
            printf(">>> Wrong subset data2 after VSfpack.\n");
        }
 
    VSdetach(vdata_id);
    Vend(file_id);
    Hclose(file_id);
    HDfree(databuf);
    return SUCCEED;
}
/* main test driver */
void
test_vspack(void)
{
     
    if (fpack() == FAIL)
        return;

    if  (funpack() == FAIL)
        return;
}  /* test_vspack */ 
