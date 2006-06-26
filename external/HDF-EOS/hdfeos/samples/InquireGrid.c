#include "hdf.h"
#include "HdfEosDef.h"

/*
 * In this example we will retrieve (1) information about the dimensions,
 * (2) the dimension mappings (geolocation relations), and (3) the grid
 * fields.
 */


main()
{

    intn            status, i;
    int32           gdfid, GDid1, ndim, nmap, nfld, rk, nt, nflds;
    int32           dims[32], rank[32], ntype[32];
    int32           n, strbufsize, sizes[16], GDid2;
    int32           xdimsize, ydimsize, dimsize, projcode, zonecode;
    int32           spherecode;
    
    float64         upleftpt[2], lowrightpt[2], projparm[16];
    
    char            dimname[1024], fieldlist[1024];


    /*
     * Open the Grid File for read only access
     */

    gdfid = GDopen("GridFile.hdf", DFACC_READ);

    
    if (gdfid != -1)
    {

	/* Attach the grid */

	GDid1 = GDattach(gdfid, "UTMGrid");
	GDid2 = GDattach(gdfid, "PolarGrid");
	
	ndim = GDinqdims(GDid1, dimname, dims);
	printf("Dimension list (UTMGrid): %s\n", dimname);
	for (i=0;i<ndim;i++) printf("dim size: %d\n", dims[i]);

	ndim = GDinqdims(GDid2, dimname, dims);
	printf("Dimension list (PolarGrid): %s\n", dimname);
	for (i=0;i<ndim;i++) printf("dim size: %d\n", dims[i]);


	dimsize = GDdiminfo(GDid1, "Time");
	printf("Size of \"Time\" Array: %d\n", dimsize);

	dimsize = GDdiminfo(GDid2, "Bands");
	printf("Size of \"Bands\" Array: %d\n", dimsize);


	
	status = GDgridinfo(GDid1, &xdimsize, &ydimsize,
			    upleftpt, lowrightpt);
	printf("X dim size, Y dim size (UTMGrid): %d %d\n", 
	       xdimsize, ydimsize);
	printf("Up left pt (UTMGrid): %lf %lf\n", 
	       upleftpt[0], upleftpt[1]);
	printf("Low right pt (UTMGrid): %lf %lf\n", 
	       lowrightpt[0], lowrightpt[1]);

	status = GDgridinfo(GDid2, &xdimsize, &ydimsize,
			    upleftpt, lowrightpt);
	printf("X dim size, Y dim size (PolarGrid): %d %d\n", 
	       xdimsize, ydimsize);
	printf("Up left pt (PolarGrid): %lf %lf\n", 
	       upleftpt[0], upleftpt[1]);
	printf("Low right pt (PolarGrid): %lf %lf\n", 
	       lowrightpt[0], lowrightpt[1]);
	
	       
	status = GDprojinfo(GDid1, &projcode, &zonecode,
			    &spherecode, NULL);
	printf("projcode , zonecode (UTMGrid): %d %d\n", projcode, zonecode);
	printf("spherecode (UTMGrid): %d\n", spherecode);	

	
	status = GDprojinfo(GDid2, &projcode, NULL,
			    &spherecode, projparm);
	printf("projcode (PolarGrid): %d\n", projcode);	
	printf("spherecode (PolarGrid): %d\n", spherecode);	
	for (i=0; i<13; i++)
	    printf("Projection Parameter: %d %lf\n",i,projparm[i]);
	
	nflds = GDinqfields(GDid1, fieldlist, rank, ntype);
	if (nflds != 0)
	{
	    printf("Data fields (UTMGrid): %s\n", fieldlist);
	    for (i=0;i<nflds;i++)
		printf("rank type: %d %d\n",rank[i],ntype[i]);
	}

	nflds = GDinqfields(GDid2, fieldlist, rank, ntype);
	if (nflds != 0)
	{
	    printf("Data fields (PolarGrid): %s\n", fieldlist);
	    for (i=0;i<nflds;i++)
		printf("rank type: %d %d\n",rank[i],ntype[i]);
	}
	
	
	status = GDfieldinfo(GDid2, "Spectra", rank, 
			     dims, ntype, dimname);
	printf("Spectra rank dims: %d\n",rank[0]);
	for (i=0; i<rank[0]; i++)
	    printf("Spectra dims: %d %d\n",i,dims[i]);
	printf("Spectra dims: %s\n", dimname);
	
	
	n = GDnentries(GDid1, HDFE_NENTDIM, &strbufsize);
	printf("Number of dimension entries (UTMGrid): %d\n", n);
	printf("Length of Dimension List (UTMGrid): %d\n", strbufsize);
	
	n = GDnentries(GDid1, HDFE_NENTDFLD, &strbufsize);
	printf("Number of data fields (UTMGrid): %d\n", n);
	printf("Length of Field List (UTMGrid): %d\n", strbufsize);
	
    }
    GDdetach(GDid1);
    GDdetach(GDid2);
    GDclose(gdfid);

    return;
}





