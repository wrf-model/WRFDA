/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

/* $Id: gentest.c,v 1.10 1997/11/05 19:39:22 koziol Exp $ */

#ifdef RCSID
static char RcsId[] = "@(#)$Revision: 1.10 $";
#endif

/*
   FILE
   gentest.c
   Generate files for HDF testing.

   REMARKS
   This may be a bit of a kludge in some cases, because it's hard
   to determine correct output if you don't know if the routine
   you are generating the test is working yet.

   DESIGN
   Each test should have a seperate function which creates the datafiles
   necessary for testing it.

   BUGS/LIMITATIONS

   EXPORTED ROUTINES
   none

   AUTHOR
   Quincey Koziol

   MODIFICATION HISTORY
   10/27/93 - Started coding.
   1/20/94 - Added N-bit test generation.
 */

#include "hdf.h"

/* Local definitions of filenames and tag/refs of data in them. */
#define BITIO_NAME  "bitio.dat"
#define BITIO_TAG1  1000
#define BITIO_REF1  1000
#define BITIO_SIZE1 4096

#define NBIT_NAME   "nbit.dat"
#define NBIT_TAG1   (uint16)1000
#define NBIT_REF1   (uint16)1000
#define NBIT_SIZE1  4096
#define NBIT_BITS1  6

static int gen_bitio_test(void);
static int gen_nbit_test(void);

/*--------------------------------------------------------------------------

 NAME
        gen_bitio_test -- create datafiles for bitio test
 USAGE
        int gen_bitio_test()
 RETURNS
        returns SUCCEED or FAIL
 DESCRIPTION
        Writes out a sample dataset to the bitio datafile so that
        we can test bitio reading before bitio writing.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static int gen_bitio_test(void)
{
    int32       fid;            /* file ID of bitio HDF file */
    uint8      *bit_data;       /* pointer to the data to store in the datafile */
    intn        i;              /* local counting variable */

    if ((fid = Hopen(BITIO_NAME, DFACC_CREATE, 0)) == FAIL)
        return (FAIL);

    if ((bit_data = (uint8 *) HDmalloc(BITIO_SIZE1 * sizeof(uint8))) == NULL)
      {
          Hclose(fid);
          return (FAIL);
      }     /* end if */

    for (i = 0; i < BITIO_SIZE1; i++)   /* fill with pseudo-random data */
        bit_data[i] = (uint8)((i * 3) % 256);

    if (FAIL == Hputelement(fid, BITIO_TAG1, BITIO_REF1, bit_data, BITIO_SIZE1))
      {
          HDfree(bit_data);
          Hclose(fid);
          return (FAIL);
      }     /* end if */

    HDfree(bit_data);

    if (FAIL == Hclose(fid))
        return (FAIL);

    return (SUCCEED);
}   /* end gen_bitio_test() */

/*--------------------------------------------------------------------------

 NAME
        gen_nbit_test -- create datafiles for n-bit test
 USAGE
        int gen_nbit_test()
 RETURNS
        returns SUCCEED or FAIL
 DESCRIPTION
        Writes out a sample dataset to a sample datafile so that
        we can test n-bit writing before n-bit reading.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static int gen_nbit_test(void)
{
    int32       fid;            /* file ID of n-bit HDF file */
    uint8      *nbit_data;      /* pointer to the initial data */
    uint8      *out_data;       /* pointer to the data to store in the datafile */
    uint32      store;          /* temporary storage for outgoing bits */
    intn        store_bits;     /* number of bits stored */
    uintn       out_num;        /* number of bytes to output */
    intn        i;              /* local counting variable */

    if ((fid = Hopen(NBIT_NAME, DFACC_CREATE, 0)) == FAIL)
        return (FAIL);

    if ((nbit_data = (uint8 *) HDmalloc(NBIT_SIZE1 * sizeof(uint8))) == NULL)
      {
          Hclose(fid);
          return (FAIL);
      }     /* end if */

    if ((out_data = (uint8 *) HDmalloc(NBIT_SIZE1 * sizeof(uint8))) == NULL)
      {
          HDfree(nbit_data);
          Hclose(fid);
          return (FAIL);
      }     /* end if */

    for (i = 0; i < NBIT_SIZE1; i++)    /* fill with pseudo-random data */
        nbit_data[i] = (uint8)((i * 3) % 64);

    store = 0;
    store_bits = 0;
    out_num = 0;
    for (i = 0; i < NBIT_SIZE1; i++)
      {     /* pack the bits together */
          store <<= NBIT_BITS1;
          store |= (uint32)nbit_data[i] & (uint32)maskc[NBIT_BITS1];
          store_bits += NBIT_BITS1;
          if (store_bits >= (intn)BITNUM)
            {   /* have at least a full byte */
                out_data[out_num] = (uint8)((store >> (store_bits - (intn)BITNUM)) & (uint32)maskc[8]);
                out_num++;
                store_bits -= (intn)BITNUM;
                store >>= BITNUM;
            }   /* end if */
      }     /* end for */
    if (store_bits > 0)
      {     /* push over any leftover bits to the left */
          out_data[out_num] = (uint8)(store << ((intn)BITNUM - store_bits));
          out_num++;
      }     /* end if */

    if (FAIL == Hputelement(fid, NBIT_TAG1, NBIT_REF1, out_data, (int32)out_num))
      {
          HDfree(nbit_data);
          HDfree(out_data);
          Hclose(fid);
          return (FAIL);
      }     /* end if */

    HDfree(nbit_data);
    HDfree(out_data);

    if (FAIL == Hclose(fid))
        return (FAIL);

    return (SUCCEED);
}   /* end gen_nbit_test() */

int
main(int argc, char *argv[])
{
    /* shut compiler up */
    argc=argc; argv=argv;

    gen_bitio_test();
    gen_nbit_test();

    return(0);
}   /* end main() */
