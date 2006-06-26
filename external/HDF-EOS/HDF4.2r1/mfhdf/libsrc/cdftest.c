/*
 * 	Copyright 1988 University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
static char mrcsid[] = "Id: cdftest.c,v 1.11 1994/01/10 23:07:27 chouck Exp ";

/*
 *	 Program to create a cdf, exercise all cdf functions.
 *  Creates cdf, stuff it full of numbers, closes it. Then
 *  reopens it, and checks for consistancy.
 *  Leaves the file around afterwards.
 *
 *	Based on a program to test the nasa look-alike program,
 * so not the most appropropriate test. See ../nctest for a
 * complete spec test.
 */

#define REDEF
/* #define SYNCDEBUG */
/* #define NOBUF */
#include <stdio.h>
#include "netcdf.h"
#ifdef HDF
#include "hdf.h"
#endif

#ifdef macintosh
    #include <LowMem.h>
#endif

# define assert(ex) {if (!(ex)){fprintf(stderr,"Assertion failed: file %s, line %d\n", __FILE__, __LINE__);exit(1);}}

#define CDFMAXSHORT	32767
#define CDFMAXLONG		2147483647
#define  CDFMAXBYTE	127

#include <errno.h>
#if defined ERRNO_MISSING
extern int errno;
#endif

#define FILENAME		"test.cdf"
#define	NUM_DIMS 	3
#define DONT_CARE	-1
/* make these numbers big when you want to give this a real workout */
#define NUM_RECS	8
#define SIZE_1		7
#define SIZE_2		8

struct {
	int num_dims ;
	int num_vars ;
	int num_attrs ;
	int xtendim ;
} cdesc[1] ;

struct {
	char mnem[MAX_NC_NAME] ;
	nc_type type ;
	int ndims ;
	int dims[MAX_VAR_DIMS] ;
	int num_attrs ;
} vdesc[1] ;

struct {
	char mnem[MAX_NC_NAME] ;
	nc_type type ;
	int len ;
} adesc[1] ;

union getret
{
    char            by[8] ;
    short           sh[4] ;
    nclong          lng[2] ;
    float           fl[2] ;
    double          dbl;
} ;

static void
chkgot(type, got, check)
nc_type type ;
union getret got ;
double check ;
{
	switch(type){
	case NC_BYTE :
		assert( (char)check == got.by[0] ) ;
		break ;
	case NC_SHORT :
		assert( (short)check == got.sh[0] ) ;
		break ;
	case NC_LONG :
		assert( (nclong)check == got.lng[0] ) ;
		break ;
	case NC_FLOAT :
		assert( (float)check == got.fl[0] ) ;
		break ;
	case NC_DOUBLE :
		assert( check == got.dbl ) ;
		break ;
	}
}

const char *fname = FILENAME ;


int num_dims = NUM_DIMS ;
long sizes[] = { NC_UNLIMITED, SIZE_1 , SIZE_2 } ;
const char *dim_names[] = { "record", "ixx", "iyy"} ;

static void
createtestdims(cdfid, num_dims, sizes, dim_names)
int cdfid ;
int num_dims ;
long *sizes ;
const char *dim_names[] ;
{
	while(num_dims--)
	{
		assert( ncdimdef(cdfid, *dim_names++, *sizes) >= 0) ;
		sizes++ ;
	}

}

static void
testdims(cdfid, num_dims, sizes, dim_names)
int cdfid ;
int num_dims ;
long *sizes ;
const char *dim_names[] ;
{
	int ii ;
	long size ;
	char cp[MAX_NC_NAME] ;
	for(ii=0 ; ii < num_dims ; ii++, sizes++)
	{
		assert( ncdiminq(cdfid, ii, cp, &size) >= 0) ;
		if( size != *sizes)
			fprintf(stderr, "%d: %ld != %ld\n",
				(int)ii, (long)size, (long)*sizes) ;
		assert( size == *sizes) ;
		assert( strcmp(cp, *dim_names++) == 0) ;
	}

}



const char *reqattr[] = {
	"UNITS",
	"VALIDMIN",
	"VALIDMAX",
	"SCALEMIN",
	"SCALEMAX",
	"FIELDNAM",
	_FillValue
} ;
#define NUM_RATTRS	6

struct tcdfvar {
	const char *mnem;
	nc_type type;
	const char *fieldnam;
	double validmin;
	double validmax;
	double scalemin;
	double scalemax;
	const char *units;
	int ndims ;
	int dims[NUM_DIMS];
} testvars[]  = {
#define Byte_id 0
	{ "Byte", NC_BYTE, "Byte sized integer variable",
		-CDFMAXBYTE, CDFMAXBYTE, -CDFMAXBYTE, CDFMAXBYTE , "ones",
			2, {0,1,DONT_CARE} },
#define Short_id 1
	{ "Short", NC_SHORT, "Short variable",
		-CDFMAXSHORT, CDFMAXSHORT, -CDFMAXSHORT, CDFMAXSHORT , "ones",
			2, {0, 2, DONT_CARE }},
#define Long_id 2
	{ "Long", NC_LONG, "Long Integer variable",
		-CDFMAXLONG, CDFMAXLONG, -CDFMAXLONG, CDFMAXLONG, "ones",
			2, {1, 2, DONT_CARE}},
#define Float_id 3
	{ "Float", NC_FLOAT, "Single Precision Floating Point variable",
		-CDFMAXLONG, CDFMAXLONG, -CDFMAXLONG, CDFMAXLONG, "flots",
			3, {0, 1, 2 }},
#define Double_id 4
	{ "Double", NC_DOUBLE, "Double Precision Floating Point variable",
		-CDFMAXLONG, CDFMAXLONG, -CDFMAXLONG, CDFMAXLONG, "dflots",
			3, {0, 1, 2 }},
} ;
#define	NUM_TESTVARS	5

static void
createtestvars(id, testvars, count )
int id ;
struct tcdfvar *testvars ;
int count ;
{
	int ii ;
	struct tcdfvar *vp = testvars ;

	for(ii = 0 ; ii < count ; ii++, vp++ )
	{
		assert(ncvardef(id, vp->mnem, vp->type, vp->ndims, vp->dims) == ii ) ; 

	 	assert(
			ncattput(id,ii,reqattr[0],NC_CHAR,strlen(vp->units), vp->units)
			== 0) ; 
	 	assert(
			ncattput(id,ii,reqattr[1],NC_DOUBLE,1,
				(ncvoid*)&(vp->validmin))
			== 1) ; 
	 	assert(
			ncattput(id,ii,reqattr[2],NC_DOUBLE,1,
				(ncvoid*)&(vp->validmax))
			== 2) ; 
	 	assert(
			ncattput(id,ii,reqattr[3],NC_DOUBLE,1,
				(ncvoid*)&(vp->scalemin))
			== 3) ; 
	 	assert(
			ncattput(id,ii,reqattr[4],NC_DOUBLE,1,
				(ncvoid*)&(vp->scalemax))
			== 4) ; 
	 	assert(
			ncattput(id,ii,reqattr[5],NC_CHAR,strlen(vp->fieldnam), vp->fieldnam)
			== 5) ; 
	}
}

static void
parray(label, count, array)
char *label ;
unsigned count ;
long array[] ;
{
	fprintf(stdout, "%s", label) ;
	fputc('\t',stdout) ;	
	for(; count > 0 ; count--, array++)
		fprintf(stdout," %d", (int)*array) ;
}

static void
fill_seq(id)
int id ;
{
    long vindices[NUM_DIMS];
	long *cc, *mm ;
	float val ;
	int ii = 0 ;

	sizes[0] = NUM_RECS ;
	/* zero the indices */

	cc = vindices;
	while (cc <= &vindices[num_dims-1])
		*cc++ = 0; 

	/* ripple counter */
	cc = vindices;
	mm = sizes;
	while (*vindices < *sizes)
	{
	    while (*cc < *mm)
	    {
		if (mm == &sizes[num_dims - 1])
		{
	val = ii ;
#ifdef VDEBUG
	parray("indices", NUM_DIMS, vindices) ;
	printf("\t val %f\n", val) ;
#endif
	assert( ncvarput1(id, Float_id, vindices, (ncvoid*)&val) != -1) ;
		    (*cc)++; ii++ ;
		    continue;
		}
		cc++;
		mm++;
	    }
		if(cc == vindices)
			break ;
	    *cc = 0;
	    cc--;
	    mm--;
	    (*cc)++;
	}
}

static void
check_fill_seq(id)
int id ;
{
    long vindices[NUM_DIMS];
	long *cc, *mm ;
	union getret got ;
	int ii = 0;
	float val ;

	sizes[0] = NUM_RECS ;
	cc = vindices;
	while (cc <= &vindices[num_dims-1])
		*cc++ = 0; 

	/* ripple counter */
	cc = vindices;
	mm = sizes;
	while (*vindices < *sizes)
	{
	    while (*cc < *mm)
	    {
		if (mm == &sizes[num_dims - 1])
		{
	if(ncvarget1(id, Float_id, vindices, (ncvoid*)&got) == -1) 
		goto bad_ret ;
	val = ii ;
	if(val != got.fl[0])
	{
		parray("indices", (unsigned)NUM_DIMS, vindices) ;
		printf("\t%f != %f\n", val, got.fl[0]) ;
	}
		    (*cc)++; ii++ ;
		    continue;
		}
		cc++;
		mm++;
	    }
		if(cc == vindices)
			break ;
	    *cc = 0;
	    cc--;
	    mm--;
	    (*cc)++;
	}
	return ;
bad_ret :
	printf("couldn't get a var in check_fill_seq() %d\n",
		(int)ii) ;
	return ;
}

long	indices[][3] = {
	{0, 1, 3},
	{0, 3, 0},
	{1, 2, 3},
	{3, 2, 1},
	{2, 1, 3},
	{1, 0, 0},
	{0, 0, 0},
} ;

char chs[] = {'A','B', ((char)0xff) } ;
long s_start[] = {0,1};
long s_edges[] = {NUM_RECS, SIZE_1 - 1};
char sentence[NUM_RECS* SIZE_1 -1] =
	"The red death had long devastated the country." ;
short shs[] = {97, 99} ;
nclong birthday = 82555 ;
#define M_E	2.7182818284590452354
float e = (float)M_E ;
double pinot = 3.25 ;
double zed = 0.0 ;

#if defined TEST_PC || defined TEST_WIN
#include <stdio.h>
FILE *dbg_file;
#endif

#if defined __MWERKS__
#include <console.h>
#endif

#ifdef PROTOTYPE
int main(int argc, char *argv[])
#else
int main(argc, argv)
int argc;
char *argv[];
#endif
{
	int ret ;
	int	 id ;
	char new[256];
#ifdef SYNCDEBUG
	char *str = "one" ;
#endif
	int ii ;
	long iilong ;
	struct tcdfvar *tvp = testvars ;
	union getret got ;

#ifdef macintosh
	Ptr	currStackBase, newApplLimit, currApplLimit, currHeapEnd;
	/*	Expand the stack.  hdf_write_var( ) causes the stack to collide with
		the 68K application heap when only the default stack size is used.  */
	currStackBase = LMGetCurStackBase( );
	newApplLimit = (Ptr) ( (long) currStackBase - 65536L );
	currApplLimit = GetApplLimit( );
	if ( newApplLimit > currApplLimit )	/* If we're about to shrink the stack, ...*/
		 newApplLimit = currApplLimit;	/* ... then don't. */

	currHeapEnd = LMGetHeapEnd( );
	if ( newApplLimit < currHeapEnd )	/* If we're about overlap the stack and heap, */
		 newApplLimit = currHeapEnd;	/* ... then don't. */

	SetApplLimit( newApplLimit );
#endif
#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

#if defined TEST_PC || defined TEST_WIN
    dbg_file=fopen("test.dbg","w+");
#endif

#ifdef MDEBUG
	malloc_debug(2) ;
#endif /* MDEBUG */
	ncopts =  NC_VERBOSE ; /* errors non fatal */


	id = nccreate(fname,NC_NOCLOBBER) ;
	if( id == -1 ) {
		fprintf(stderr, "trying again\n") ;
		id = nccreate(fname,NC_CLOBBER) ;
	}
	if( id == -1 ) 
		exit(errno) ;

#ifdef NOBUF
	assert( ncnobuf(id) != 1 ) ;
#endif /* NOBUF */
	
	assert( ncattput(id, NC_GLOBAL,
		"TITLE", NC_CHAR, 12, "another name") != -1) ;
	assert( ncattget(id, NC_GLOBAL,
		"TITLE", (ncvoid*)new) != -1) ;
/*	printf("title 1 \"%s\"\n", new) ; */
	assert( ncattput(id, NC_GLOBAL,
		"TITLE", NC_CHAR, strlen(fname), fname) != -1) ;
	assert( ncattget(id, NC_GLOBAL,
		"TITLE", (ncvoid*)new) != -1) ;
	new[strlen(fname)] = 0 ;
/*	printf("title 2 \"%s\"\n", new) ; */
	assert( strcmp(fname, new) == 0) ;
	assert( ncattput(id, NC_GLOBAL,
		"RCSID", NC_CHAR, strlen(mrcsid), (ncvoid*)mrcsid) != -1) ;

	createtestdims(id, NUM_DIMS, sizes, dim_names) ;
	testdims(id, NUM_DIMS, sizes, dim_names) ;

	createtestvars(id, testvars, NUM_TESTVARS) ; 

 	{
 	long lfill = -1 ; double dfill = -9999 ;
 	assert( ncattput(id, Long_id,
 		_FillValue, NC_LONG, 1, (ncvoid*)&lfill) != -1) ;
 	assert( ncattput(id, Double_id,
 		_FillValue, NC_DOUBLE, 1, (ncvoid*)&dfill ) != -1) ;
 	}

#ifdef REDEF
	assert( ncendef(id) != -1 ) ;
	assert( ncvarput1(id, Long_id, indices[3], (ncvoid *)&birthday) 
		!= -1 ) ;
	fill_seq(id) ;
	assert( ncredef(id) != -1 ) ;
#endif

	assert( ncdimrename(id,1, "IXX") >= 0) ;
	assert( ncdiminq(id, 1, new, &iilong) >= 0) ;
	printf("dimrename: %s\n", new) ;
	assert( ncdimrename(id,1, dim_names[1]) >= 0) ;

#ifdef ATTRX
	assert( ncattrename(id, 1, "UNITS", "units") != -1) ;
	assert( ncattdel(id, 4, "FIELDNAM") != -1) ;
	assert( ncattdel(id, 2, "SCALEMIN") != -1) ;
	assert( ncattdel(id, 2, "SCALEMAX") != -1) ;
#endif /* ATTRX */

	assert( ncendef(id) != -1 ) ;

#ifndef REDEF
	fill_seq(id) ;
	assert( ncvarput1(id, Long_id, indices[3],(char *)&birthday) != -1 ) ;
#endif

	assert( ncvarput(id, Byte_id, s_start, s_edges, (ncvoid*)sentence)
		!= -1 ) ;

	assert( ncvarput1(id, Byte_id, indices[6], (ncvoid*)(chs+1)) != -1 ) ;
	assert( ncvarput1(id, Byte_id, indices[5], (ncvoid*)chs) != -1 ) ;
	assert( ncvarput1(id, Short_id, indices[4],(ncvoid *)shs) != -1 ) ;
	assert( ncvarput1(id, Float_id, indices[2],(ncvoid *)&e) != -1 ) ;
	assert( ncvarput1(id, Double_id, indices[1],(ncvoid *)&zed) != -1 ) ;
	assert( ncvarput1(id, Double_id, indices[0], (ncvoid *)&pinot) != -1 );


#ifdef SYNCDEBUG
	printf("Hit Return to sync\n");
	gets(str);
	ncsync(id,0) ;
	printf("Sync done. Hit Return to continue\n");
	gets(str);
#endif /* SYNCDEBUG */

	ret = ncclose(id) ;
	printf("ncclose ret = %d\n\n", (int)ret) ;


/*
 *	read it
 */
	id = ncopen(fname,NC_NOWRITE) ;
	if( id == -1 )
	{
		printf("Could not open %s\n", fname );
		exit(1) ;
	}
	printf("reopen id = %d for filename %s\n",
		(int)id, fname) ;

#ifdef NOBUF
	assert( ncnobuf(id) != 1 ) ;
#endif /* NOBUF */

	/*	NC	*/ 
	printf("NC ") ;
	assert( ncinquire(id, &(cdesc->num_dims), &(cdesc->num_vars),
		&(cdesc->num_attrs), &(cdesc->xtendim) ) == id) ;
	if(cdesc->num_dims != num_dims )
	{
		printf(" num_dims  : %d != %d\n", (int)cdesc->num_dims, (int)num_dims ) ; 
		exit(1) ;
	}
	assert(cdesc->num_vars == NUM_TESTVARS) ;
	printf("done\n") ;
	
	/*	GATTR	*/
	printf("GATTR ") ;
	assert(cdesc->num_attrs == 2) ;

	assert( ncattname(id, NC_GLOBAL, 0, adesc->mnem) == 0) ;
	assert(strcmp("TITLE",adesc->mnem) == 0) ;
	assert( ncattinq(id, NC_GLOBAL, adesc->mnem, &(adesc->type), &(adesc->len)) != -1) ;
	assert( adesc->type == NC_CHAR ) ;
	assert( adesc->len == strlen(fname) ) ;
	assert( ncattget(id, NC_GLOBAL, "TITLE", (ncvoid *)new) != -1) ;
	new[adesc->len] = 0 ;
	assert( strcmp(fname, new) == 0) ;
/*	printf("Global %s %s\n", adesc->mnem, new) ; */

	assert( ncattname(id, NC_GLOBAL, 1, adesc->mnem) == 1) ;
	assert(strcmp("RCSID",adesc->mnem) == 0) ;
	assert( ncattinq(id, NC_GLOBAL, adesc->mnem, &(adesc->type), &(adesc->len)) != -1) ;
	assert( adesc->type == NC_CHAR ) ;
	assert( adesc->len == strlen(mrcsid) ) ;
	assert( ncattget(id, NC_GLOBAL, "RCSID", (ncvoid *)new) != -1) ;
	new[adesc->len] = 0 ;
	assert( strcmp(mrcsid, new) == 0) ;
/*	printf("Global %s %s\n", adesc->mnem, new) ; */

	/*	VAR	*/
	printf("VAR ") ;
	assert( cdesc->num_vars == NUM_TESTVARS ) ;

	for(ii = 0 ; ii < cdesc->num_vars ; ii++, tvp++ ) 
	{
		int jj ;
		assert( ncvarinq(id, ii,
			vdesc->mnem,
			&(vdesc->type),
			&(vdesc->ndims),
			vdesc->dims,
			&(vdesc->num_attrs)) == ii) ;
		if(strcmp(tvp->mnem , vdesc->mnem) != 0)
		{
			printf("attr %d mnem mismatch %s, %s\n",
				(int)ii, tvp->mnem, vdesc->mnem) ;
			continue ;
		}
		if(tvp->type != vdesc->type)
		{
			printf("attr %d type mismatch %d, %d\n",
				(int)ii, tvp->type, (int)vdesc->type) ;
			continue ;
		}
		for(jj = 0 ; jj < vdesc->ndims ; jj++ )
		{
			if(tvp->dims[jj] != vdesc->dims[jj] )
			{
		printf(
		"inconsistant dim[%d] for variable %d: %d != %d\n",
		(int)jj, (int)ii, (int)tvp->dims[jj], (int)vdesc->dims[jj] ) ;
			continue ;
			}
		}

		/* VATTR */
		printf("VATTR\n") ;
		for(jj=0 ; jj<vdesc->num_attrs ; jj++ ) 
		{
			assert( ncattname(id, ii, jj, adesc->mnem) == jj) ;
			if( strcmp(adesc->mnem, reqattr[jj]) != 0 )
			{
				printf("var %d attr %d mismatch %s != %s\n",
					(int)ii, (int)jj, adesc->mnem, reqattr[jj] ) ;
				break ;
			}
		}

		if( ncattinq(id, ii, reqattr[0], &(adesc->type), &(adesc->len))
			!= -1) {
		assert( adesc->type == NC_CHAR ) ;
		assert( adesc->len == strlen(tvp->units) ) ;
	 	assert( ncattget(id,ii,reqattr[0],(ncvoid *)new) != -1) ; 
		new[adesc->len] = 0 ;
		assert( strcmp(tvp->units, new) == 0) ;
		}

		if(
			ncattinq(id, ii, reqattr[1], &(adesc->type), &(adesc->len))
			!= -1)
		{
		assert( adesc->type == NC_DOUBLE ) ;
		assert( adesc->len == 1 ) ;
	 	assert( ncattget(id,ii,reqattr[1],(ncvoid *)&got) != -1) ; 
		chkgot(adesc->type, got, tvp->validmin) ;
		}

		if(
			ncattinq(id, ii, reqattr[2], &(adesc->type), &(adesc->len))
			!= -1)
		{
		assert( adesc->type == NC_DOUBLE ) ;
		assert( adesc->len == 1 ) ;
	 	assert( ncattget(id,ii,reqattr[2],(ncvoid *)&got) != -1) ; 
		chkgot(adesc->type, got, tvp->validmax) ;
		}

		if(
			ncattinq(id, ii, reqattr[3], &(adesc->type), &(adesc->len))
			!= -1)
		{
		assert( adesc->type == NC_DOUBLE ) ;
		assert( adesc->len ==1 ) ;
	 	assert( ncattget(id,ii,reqattr[3],(ncvoid *)&got) != -1) ; 
		chkgot(adesc->type, got, tvp->scalemin) ;
		}

		if(
			ncattinq(id, ii, reqattr[4], &(adesc->type), &(adesc->len))
			!= -1)
		{
		assert( adesc->type == NC_DOUBLE ) ;
		assert( adesc->len == 1 ) ;
	 	assert( ncattget(id,ii,reqattr[4],(ncvoid *)&got) != -1) ; 
		chkgot(adesc->type, got, tvp->scalemax) ;
		}

		if( ncattinq(id, ii, reqattr[5], &(adesc->type), &(adesc->len)) != -1)
		{
		assert( adesc->type == NC_CHAR ) ;
		assert( adesc->len == strlen(tvp->fieldnam) ) ;
	 	assert( ncattget(id,ii,reqattr[5],(ncvoid *)new) != -1) ; 
		new[adesc->len] = 0 ;
		assert( strcmp(tvp->fieldnam, new) == 0) ;
		}
	}

	printf("fill_seq ") ;
	check_fill_seq(id) ;
	printf("Done\n") ;

	assert( ncvarget1(id, Double_id, indices[0], (ncvoid *)&got) != -1) ;
	printf("got val = %f\n", got.dbl ) ;

	assert( ncvarget1(id, Double_id, indices[1], (ncvoid *)&got) != -1) ;
	printf("got val = %f\n", got.dbl ) ;

	assert( ncvarget1(id, Float_id, indices[2], (ncvoid *)&got) != -1) ;
	printf("got val = %f\n", got.fl[0] ) ;

	assert( ncvarget1(id, Long_id, indices[3], (ncvoid *)&got) != -1) ;
	printf("got val = %ld\n", (long)got.lng[0] ) ;

	assert( ncvarget1(id, Short_id, indices[4], (ncvoid *)&got) != -1) ;
	printf("got val = %d\n", (int)got.sh[0] ) ;

	assert( ncvarget1(id, Byte_id, indices[5], (ncvoid *)&got) != -1) ;
	printf("got val = %c (0x%02x) \n", got.by[0] , got.by[0]) ;

	assert( ncvarget1(id, Byte_id, indices[6], (ncvoid *)&got) != -1) ;
	printf("got val = %c (0x%02x) \n", got.by[0], got.by[0] ) ;

	/* (void)memset(new,0,256) ; */
	{ char *cp = new; for(; cp < &new[sizeof(new)-1] ; *cp++ = 0) ; }
	assert( ncvarget(id, Byte_id, s_start, s_edges, (ncvoid *)new) != -1 ) ;
	printf("got val = \"%s\"\n", new) ;

	ret = ncclose(id) ;
	printf("re ncclose ret = %d\n", (int)ret) ;

#if defined TEST_PC || defined TEST_WIN
    fclose(dbg_file);
#endif
	return 0;
}

