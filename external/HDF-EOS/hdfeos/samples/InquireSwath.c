#include "hdf.h"
#include "HdfEosDef.h"

/*
 * In this example we will retrieve (1) information about the dimensions, (2)
 * the dimension mappings (geolocation relations), and (3) the swath fields.
 */


main()
{

    intn            status, i;
    int32           swfid, SWid, ndims, nmaps, rk, nt, dim[8], nflds;
    int32          *dims, *off, *inc, *indx, *rank, *ntype;
    int32           n, strbufsize, dimsize, offset, incr, *sizes;

    char           *dimname, *dimmap, *fieldlist, dimlist[80];


    /*
     * Open the Swath File for read only access
     */

    swfid = SWopen("SwathFile.hdf", DFACC_READ);


    if (swfid != -1)
    {

	/* Attach the swath */

	SWid = SWattach(swfid, "Swath1");

	if (SWid != -1)
	{
	    /* Inquire Dimensions */
	    ndims = SWnentries(SWid, HDFE_NENTDIM, &strbufsize);
	    dims = (int32 *) calloc(ndims, 4);
	    dimname = (char *) calloc(strbufsize + 1, 1);
	    
	    ndims = SWinqdims(SWid, dimname, dims);

	    printf("Dimension list: %s\n", dimname);
	    for (i = 0; i < ndims; i++)
		printf("dim size: %d\n", dims[i]);
	    
	    free(dims);
	    free(dimname);


	    /* Inquire Dimension Mappings */
	    nmaps = SWnentries(SWid, HDFE_NENTMAP, &strbufsize);
	    
	    off = (int32 *) calloc(nmaps, 4);
	    inc = (int32 *) calloc(nmaps, 4);
	    dimmap = (char *) calloc(strbufsize + 1, 1);
	    
	    nmaps = SWinqmaps(SWid, dimmap, off, inc);
	    printf("Dimension map: %s\n", dimmap);
	    for (i = 0; i < nmaps; i++)
		printf("offset increment: %d %d\n",
		       off[i], inc[i]);
	    free(off);
	    free(inc);
	    free(dimmap);


	    /* Inquire Indexed Dimension Mappings */
	    nmaps = SWnentries(SWid, HDFE_NENTIMAP, &strbufsize);
	    sizes = (int32 *) calloc(nmaps, 4);
	    dimmap = (char *) calloc(strbufsize + 1, 1);
	    nmaps = SWinqidxmaps(SWid, dimmap, sizes);

	    printf("Index Dimension map: %s\n", dimmap);
	    for (i = 0; i < nmaps; i++)
		printf("sizes: %d\n", sizes[i]);

	    free(sizes);
	    free(dimmap);


	    /* Inquire Geolocation Fields */
	    nflds = SWnentries(SWid, HDFE_NENTGFLD, &strbufsize);
	    rank = (int32 *) calloc(nflds, 4);
	    ntype = (int32 *) calloc(nflds, 4);
	    fieldlist = (char *) calloc(strbufsize + 1, 1);
	    nflds = SWinqgeofields(SWid, fieldlist, rank, ntype);

	    printf("geo fields: %s\n", fieldlist);
	    for (i = 0; i < nflds; i++)
		printf("rank type: %d %d\n", rank[i], ntype[i]);

	    free(rank);
	    free(ntype);
	    free(fieldlist);



	    /* Inquire Data Fields */
	    nflds = SWnentries(SWid, HDFE_NENTDFLD, &strbufsize);
	    rank = (int32 *) calloc(nflds, 4);
	    ntype = (int32 *) calloc(nflds, 4);
	    fieldlist = (char *) calloc(strbufsize + 1, 1);
	    nflds = SWinqdatafields(SWid, fieldlist, rank, ntype);

	    printf("data fields: %s\n", fieldlist);
	    for (i = 0; i < nflds; i++)
		printf("rank type: %d %d\n", rank[i], ntype[i]);

	    free(rank);
	    free(ntype);
	    free(fieldlist);


	    /* Get info on "GeoTrack" dim */
	    dimsize = SWdiminfo(SWid, "GeoTrack");
	    printf("Size of GeoTrack: %d\n", dimsize);


	    /* Get info on "GeoTrack/Res2tr" mapping */
	    status = SWmapinfo(SWid, "GeoTrack", "Res2tr", &offset, &incr);
	    printf("Mapping Offset: %d\n", offset);
	    printf("Mapping Increment: %d\n", incr);


	    /* Get info on "IndxTrack/Res2tr" indexed mapping */
	    dimsize = SWdiminfo(SWid, "IndxTrack");
	    indx = (int32 *) calloc(dimsize, 4);
	    n = SWidxmapinfo(SWid, "IndxTrack", "Res2tr", indx);
	    for (i = 0; i < n; i++)
		printf("Index Mapping Entry %d: %d\n", i+1, indx[i]);
	    free(indx);


	    /* Get info on "Longitude" Field */
	    status = SWfieldinfo(SWid, "Longitude", &rk, dim, &nt, dimlist);
	    printf("Longitude Rank: %d\n", rk);
	    printf("Longitude NumberType: %d\n", nt);
	    printf("Longitude Dimension List: %s\n", dimlist);	    
	    for (i=0; i<rk; i++)
		printf("Dimension %d: %d\n",i+1,dim[i]);

	}
    }
    status = SWdetach(SWid);
    status = SWclose(swfid);

    return;
}
