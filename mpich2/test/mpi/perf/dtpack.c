/*
 * This code may be used to test the performance of some of the 
 * noncontiguous datatype operations, including vector and indexed
 * pack and unpack operations.  To simplify the use of this code for 
 * tuning an MPI implementation, it uses no communication, just the
 * MPI_Pack and MPI_Unpack routines.  In addition, the individual tests are
 * in separate routines, makeing it easier to compare the compiler-generated
 * code for the user (manual) pack/unpack with the code used by 
 * the MPI implementation.  Further, to be fair to the MPI implementation,
 * the routines are passed the source and destination buffers; this ensures
 * that the compiler can't optimize for statically allocated buffers.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "mpi.h"

/* Needed for restrict and const definitions */
#include "mpitestconf.h"

static int verbose = 1;

#define N_REPS 1000
#define THRESHOLD 0.10

/* Here are the tests */

/* Test packing a vector of individual doubles */
int TestVecPackDouble( int n, int stride, 
		       double *avgTimeUser, double *avgTimeMPI,
		       double * restrict dest, const double * restrict src )
{
    double *restrict d_dest;
    const double *restrict d_src;
    register int i;
    int          rep, position;
    double       t1, t2;
    MPI_Datatype vectype;

    t1 = MPI_Wtime();
    /* User code */
    for (rep=0; rep<N_REPS; rep++) {
	i = n;
	d_dest = dest;
	d_src  = src;
	while (i--) {
	    *d_dest++ = *d_src;
	    d_src += stride;
	}
    }
    t2 = MPI_Wtime() - t1;
    *avgTimeUser = t2 / N_REPS;
    
    /* MPI Vector code */
    MPI_Type_vector( n, 1, stride, MPI_DOUBLE, &vectype );
    MPI_Type_commit( &vectype );
    
    t1 = MPI_Wtime();
    for (rep=0; rep<N_REPS; rep++) {
	position = 0;
	MPI_Pack( (void *)src, 1, vectype, dest, n*sizeof(double),
		  &position, MPI_COMM_SELF );
    }
    t2 = MPI_Wtime() - t1;
    *avgTimeMPI = t2 / N_REPS;

    MPI_Type_free( &vectype );

    return 0;
}

/* Test packing a vector of 2-individual doubles */
int TestVecPack2Double( int n, int stride, 
			double *avgTimeUser, double *avgTimeMPI,
			double * restrict dest, const double * restrict src )
{
    double *restrict d_dest;
    const double *restrict d_src;
    register int i;
    int          rep, position;
    double       t1, t2;
    MPI_Datatype vectype;

    t1 = MPI_Wtime();
    /* User code */
    for (rep=0; rep<N_REPS; rep++) {
	i = n;
	d_dest = dest;
	d_src  = src;
	while (i--) {
	    *d_dest++ = d_src[0];
	    *d_dest++ = d_src[1];
	    d_src += stride;
	}
    }
    t2 = MPI_Wtime() - t1;
    *avgTimeUser = t2 / N_REPS;
    
    /* MPI Vector code */
    MPI_Type_vector( n, 2, stride, MPI_DOUBLE, &vectype );
    MPI_Type_commit( &vectype );
    
    t1 = MPI_Wtime();
    for (rep=0; rep<N_REPS; rep++) {
	position = 0;
	MPI_Pack( (void *)src, 1, vectype, dest, 2*n*sizeof(double),
		  &position, MPI_COMM_SELF );
    }
    t2 = MPI_Wtime() - t1;
    *avgTimeMPI = t2 / N_REPS;

    MPI_Type_free( &vectype );

    return 0;
}

/* This creates an indexed type that is like a vector (for simplicity
   of construction).  There is a possibility that the MPI implementation 
   will recognize and simplify this (e.g., in MPI_Type_commit); if so,
   let us know and we'll add a version that is not as regular 
*/
int TestIndexPackDouble( int n, int stride, 
			 double *avgTimeUser, double *avgTimeMPI,
			 double * restrict dest, const double * restrict src )
{
    double *restrict d_dest;
    const double *restrict d_src;
    register int i;
    int          rep, position;
    int          *restrict displs = 0;
    double       t1, t2;
    MPI_Datatype indextype;

    displs = (int *)malloc( n * sizeof(int) );
    for (i=0; i<n; i++) displs[i] = i * stride;

    t1 = MPI_Wtime();
    /* User code */
    for (rep=0; rep<N_REPS; rep++) {
	i = n;
	d_dest = dest;
	d_src  = src;
	for (i=0; i<n; i++) {
	    *d_dest++ = d_src[displs[i]];
	}
    }
    t2 = MPI_Wtime() - t1;
    *avgTimeUser = t2 / N_REPS;
    
    /* MPI Index code */
    MPI_Type_create_indexed_block( n, 1, displs, MPI_DOUBLE, &indextype );
    MPI_Type_commit( &indextype );

    free( displs );
    
    t1 = MPI_Wtime();
    for (rep=0; rep<N_REPS; rep++) {
	position = 0;
	MPI_Pack( (void *)src, 1, indextype, dest, n*sizeof(double),
		  &position, MPI_COMM_SELF );
    }
    t2 = MPI_Wtime() - t1;
    *avgTimeMPI = t2 / N_REPS;

    MPI_Type_free( &indextype );

    return 0;
}

int Report( const char *name, double avgTimeMPI, double avgTimeUser )
{
    double diffTime, maxTime;
    int errs=0;

    /* Move this into a common routine */
    diffTime = avgTimeMPI - avgTimeUser;
    if (diffTime < 0) diffTime = - diffTime;
    if (avgTimeMPI > avgTimeUser) maxTime = avgTimeMPI;
    else                          maxTime = avgTimeUser;

    if (verbose) {
	printf( "%-30s:\t%g\t%g\t(%g%%)\n", name, 
		avgTimeMPI, avgTimeUser,
		100 * (diffTime / maxTime) );
	fflush(stdout);
    }
    if (avgTimeMPI > avgTimeUser && (diffTime > THRESHOLD * maxTime)) {
	errs++;
	printf( "%s:\tMPI Pack code is too slow: MPI %g\t User %g\n",
		name, avgTimeMPI, avgTimeUser );
    }

    return errs;
}

/* Finally, here's the main program */
int main( int argc, char *argv[] )
{
    int n, stride, err, errs = 0;
    void *dest, *src;
    double avgTimeUser, avgTimeMPI;

    MPI_Init( &argc, &argv );

    n      = 10000;
    stride = 4;
    dest = (void *)malloc( n * sizeof(double) );
    src  = (void *)malloc( n * ((1+stride)*sizeof(double)) );
    memset( dest, 0, n * sizeof(double) );
    memset( src, 0, n * (1+stride)*sizeof(double) );
    err = TestVecPackDouble( n, stride, &avgTimeUser, &avgTimeMPI,
			     dest, src );
    errs += Report( "VecPackDouble", avgTimeMPI, avgTimeUser );

    err = TestIndexPackDouble( n, stride, &avgTimeUser, &avgTimeMPI,
			     dest, src );
    errs += Report( "VecIndexDouble", avgTimeMPI, avgTimeUser );

    free(dest);
    free(src);
    
    dest = (void *)malloc( 2*n * sizeof(double) );
    src  = (void *)malloc( (1 + n) * ((1+stride)*sizeof(double)) );
    memset( dest, 0, 2*n * sizeof(double) );
    memset( src, 0, (1+n) * (1+stride)*sizeof(double) );
    err = TestVecPack2Double( n, stride, &avgTimeUser, &avgTimeMPI,
			      dest, src );
    errs += Report( "VecPack2Double", avgTimeMPI, avgTimeUser );
    free(dest);
    free(src);
    


    if (errs == 0) {
	printf( " No Errors\n" );
    }
    else {
	printf( " Found %d performance problems\n", errs );
    }

    fflush(stdout);
    MPI_Finalize();

    return 0;
}
