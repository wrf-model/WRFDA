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

/*
   FILE - comp.c
       Test HDF compressed data I/O routines

   DESIGN
       These are written to test all combinations of modeling and
       encoding layers with different number types.

   BUGS/LIMITATIONS

   EXPORTED ROUTINES

   AUTHOR
       Quincey Koziol

   MODIFICATION HISTORY
       10/19/93 - Through this header in.
 */

/* $Id: comp.c,v 1.23 1999/01/13 19:19:38 koziol Exp $ */

#ifdef RCSID
static char RcsId[] = "@(#)$Revision: 1.23 $";
#endif

#include <time.h>
#include "tproto.h"
#include "hfile.h"

#define TESTFILE_NAME "tcomp.hdf"

#define BUFSIZE 4096

/* last ditch attempt to define this value... */
#ifndef UINT_MAX
#define UINT_MAX (unsigned)(-1)
#endif /* UINT_MAX */

/* make a guess about RAND_MAX if it isn't defined... */
#ifndef RAND_MAX
#define RAND_MAX (UINT_MAX)
#endif /* RAND_MAX */

/* define aliases for random number generation */
#define RAND rand
#define SEED(a) srand((unsigned)(a))

#define COMP_TAG 1000

/* different modeling layers to test */
comp_model_t test_models[] =
{COMP_MODEL_STDIO};

/* different compression layers to test */
comp_coder_t test_coders[] =
{
    COMP_CODE_NONE
    ,COMP_CODE_RLE
    /*,COMP_CODE_NBIT *//* n-bit testing is done in it's own module, nbit.c */
    ,COMP_CODE_SKPHUFF
    ,COMP_CODE_DEFLATE
};

int32       test_ntypes[] =
{
    DFNT_INT8, DFNT_UINT8, DFNT_INT16, DFNT_UINT16, DFNT_INT32, DFNT_UINT32
    };

#define NUM_OUTBUFS 4   /* the number of different output buffers to test */
/* outbuf #1 is all zeros (very easy to compress) */
/* outbuf #2 is a fibonacci sequence (very hard to compress) */
/* outbuf #3 is random data (also hard to compress) */
/* outbuf #4 is random in the low byte and mostly static in the upper byte(s) */
static int8  *outbuf_int8[NUM_OUTBUFS];
static uint8  *outbuf_uint8[NUM_OUTBUFS];
static int16  *outbuf_int16[NUM_OUTBUFS];
static uint16  *outbuf_uint16[NUM_OUTBUFS];
static int32  *outbuf_int32[NUM_OUTBUFS];
static uint32  *outbuf_uint32[NUM_OUTBUFS];

/* only need one input buffer per type of data */
static int8  *inbuf_int8;
static uint8  *inbuf_uint8;
static int16  *inbuf_int16;
static uint16  *inbuf_uint16;
static int32  *inbuf_int32;
static uint32  *inbuf_uint32;

/* local function prototypes */
static void init_model_info(comp_model_t m_type, model_info * m_info,
                            int32 test_ntype);
static void init_coder_info(comp_coder_t c_type, comp_info * c_info,
                            int32 test_ntype);
static void allocate_buffers(void);
static void init_buffers(void);
static void free_buffers(void);
static uint16 write_data(int32 fid, comp_model_t m_type, model_info * m_info,
                     comp_coder_t c_type, comp_info * c_info, intn test_num,
                         int32 ntype);
static void read_data(int32 fid, uint16 ref_num, intn test_num, int32 ntype);

static void
init_model_info(comp_model_t m_type, model_info * m_info, int32 test_ntype)
{
    /* shut the compiler up */
    m_type = m_type;
    m_info = m_info;
    test_ntype = test_ntype;

    switch (m_type)
      {
          case COMP_MODEL_STDIO:
          default:
              /* don't do anything for this case */
              break;
      }     /* end switch */
}   /* end init_model_info() */

static void
init_coder_info(comp_coder_t c_type, comp_info * c_info, int32 test_ntype)
{
    switch (c_type)
      {
          case COMP_CODE_SKPHUFF:
              c_info->skphuff.skp_size = DFKNTsize(test_ntype);
              break;

          case COMP_CODE_RLE:
          case COMP_CODE_NONE:
          default:
              /* don't do anything for this case */
              break;
      }     /* end switch */
}   /* end init_coder_info() */

static void
allocate_buffers(void)
{
    intn        i;

    for (i = 0; i < NUM_OUTBUFS; i++)
      {
          outbuf_int8[i] = (int8 *) HDmalloc(BUFSIZE * sizeof(int8));
          outbuf_uint8[i] = (uint8 *) HDmalloc(BUFSIZE * sizeof(uint8));
          outbuf_int16[i] = (int16 *) HDmalloc(BUFSIZE * sizeof(int16));
          outbuf_uint16[i] = (uint16 *) HDmalloc(BUFSIZE * sizeof(uint16));
          outbuf_int32[i] = (int32 *) HDmalloc(BUFSIZE * sizeof(int32));
          outbuf_uint32[i] = (uint32 *) HDmalloc(BUFSIZE * sizeof(uint32));
      }     /* end for */
    inbuf_int8 = (int8 *) HDcalloc(BUFSIZE, sizeof(int8));
    inbuf_uint8 = (uint8 *) HDcalloc(BUFSIZE, sizeof(uint8));
    inbuf_int16 = (int16 *) HDcalloc(BUFSIZE, sizeof(int16));
    inbuf_uint16 = (uint16 *) HDcalloc(BUFSIZE, sizeof(uint16));
    inbuf_int32 = (int32 *) HDcalloc(BUFSIZE, sizeof(int32));
    inbuf_uint32 = (uint32 *) HDcalloc(BUFSIZE, sizeof(uint32));
}   /* allocate_buffers() */

static void
init_buffers(void)
{
    intn        i, j;

    for (i = 0; i < NUM_OUTBUFS; i++)
      {
          switch (i)
            {
                case 0: /* all zero filled */
                    HDmemset(outbuf_int8[i], 0, BUFSIZE * sizeof(int8));
                    HDmemset(outbuf_uint8[i], 0, BUFSIZE * sizeof(uint8));
                    HDmemset(outbuf_int16[i], 0, BUFSIZE * sizeof(int16));
                    HDmemset(outbuf_uint16[i], 0, BUFSIZE * sizeof(uint16));
                    HDmemset(outbuf_int32[i], 0, BUFSIZE * sizeof(int32));
                    HDmemset(outbuf_uint32[i], 0, BUFSIZE * sizeof(uint32));
                    break;

                case 1: /* fibonacci sequence */
                    {
                        uint32      last_fib, curr_fib, next_fib;

                        for (j = 0, last_fib = 0, curr_fib = 1; j < BUFSIZE; j++)
                          {
                              outbuf_int8[i][j] = (int8) curr_fib;
                              outbuf_uint8[i][j] = (uint8) curr_fib;
                              outbuf_int16[i][j] = (int16) curr_fib;
                              outbuf_uint16[i][j] = (uint16) curr_fib;
                              outbuf_int32[i][j] = (int32) curr_fib;
                              outbuf_uint32[i][j] = (uint32) curr_fib;
                              next_fib = curr_fib + last_fib;
                              last_fib = curr_fib;
                              curr_fib = next_fib;
                          }     /* end for */
                    }   /* end case */
                    break;

                case 2: /* random #'s */
                    {
                        intn        r;

                        SEED(time(NULL));
                        for (j = 0; j < BUFSIZE; j++)
                          {
                              r = RAND();
                              outbuf_int8[i][j] = (int8) (r - RAND_MAX / 2);
                              outbuf_uint8[i][j] = (uint8) r;
                              outbuf_int16[i][j] = (int16) (r - RAND_MAX / 2);
                              outbuf_uint16[i][j] = (uint16) r;
                              outbuf_int32[i][j] = (int32) (r - RAND_MAX / 2);
                              outbuf_uint32[i][j] = (uint32) r;
                          }     /* end for */
                    }   /* end case */
                    break;

                case 3: /* random in the low byte and static in the upper */
                    {
                        uint32      r;

                        SEED(time(NULL));
                        for (j = 0; j < BUFSIZE; j++)
                          {
                              r = (uint32) RAND();
                              r &= (uint32) 0xff;   /* make the lower byte random */
                              r |= (uint32) (((j / 4) % 4) << 8);   /* make the upper bytes change slowly */
                              outbuf_int8[i][j] = (int8) r;
                              outbuf_uint8[i][j] = (uint8) r;
                              outbuf_int16[i][j] = (int16) r;
                              outbuf_uint16[i][j] = (uint16) r;
                              outbuf_int32[i][j] = (int32) r;
                              outbuf_uint32[i][j] = (uint32) r;
                          }     /* end for */
                    }   /* end case */
                    break;
            }   /* end switch */
      }     /* end for */
}   /* init_buffers() */

static void
free_buffers(void)
{
    intn        i;

    for (i = 0; i < NUM_OUTBUFS; i++)
      {
          HDfree(outbuf_int8[i]);
          HDfree(outbuf_uint8[i]);
          HDfree(outbuf_int16[i]);
          HDfree(outbuf_uint16[i]);
          HDfree(outbuf_int32[i]);
          HDfree(outbuf_uint32[i]);
      }     /* end for */
    HDfree(inbuf_int8);
    HDfree(inbuf_uint8);
    HDfree(inbuf_int16);
    HDfree(inbuf_uint16);
    HDfree(inbuf_int32);
    HDfree(inbuf_uint32);
}   /* free_buffers() */

static uint16
write_data(int32 fid, comp_model_t m_type, model_info * m_info,
        comp_coder_t c_type, comp_info * c_info, intn test_num, int32 ntype)
{
    int32       aid;
    uint16      ret_ref;
    int32       err_ret;
    int32       write_size;
    VOIDP       data_ptr;

    MESSAGE(8,
            {
            char *s = HDgetNTdesc(ntype);
            printf("Writing data for test %d, ntype=%s, model_type=%d, coder_type=%d\n", (int) test_num, (s == NULL ? "Unknown" : s), (int) m_type, (int) c_type);
            HDfree(s);
            }
    )
        ret_ref = Hnewref(fid);
    aid = HCcreate(fid, COMP_TAG, ret_ref, m_type, m_info, c_type, c_info);
    CHECK(aid, FAIL, "HCcreate");
    if (aid == FAIL)
        return (0);

    switch (ntype)
      {
          case DFNT_INT8:
              data_ptr = (VOIDP) outbuf_int8[test_num];
              break;
          case DFNT_UINT8:
              data_ptr = (VOIDP) outbuf_uint8[test_num];
              break;
          case DFNT_INT16:
              data_ptr = (VOIDP) outbuf_int16[test_num];
              break;
          case DFNT_UINT16:
              data_ptr = (VOIDP) outbuf_uint16[test_num];
              break;
          case DFNT_INT32:
              data_ptr = (VOIDP) outbuf_int32[test_num];
              break;
          case DFNT_UINT32:
              data_ptr = (VOIDP) outbuf_uint32[test_num];
              break;
          default:
              return (0);
      }     /* end switch */

    write_size = BUFSIZE * DFKNTsize(ntype);
    err_ret = Hwrite(aid, write_size, data_ptr);
    if (err_ret != write_size)
      {
          fprintf(stderr, "ERROR(%d): Hwrite returned the wrong length: %d\n", __LINE__, (int) err_ret);
          HEprint(stdout, 0);
          num_errs++;
      }

    err_ret = Hendaccess(aid);
    CHECK(err_ret, FAIL, "Hendaccess");

    return (ret_ref);
}   /* end write_data() */

static void
read_data(int32 fid, uint16 ref_num, intn test_num, int32 ntype)
{
    int32       aid;
    int32       err_ret;
    int32       read_size;
    VOIDP       out_ptr;
    VOIDP       in_ptr;
    sp_info_block_t info_block;
    intn        i;

    MESSAGE(8,
            {
            char *s = HDgetNTdesc(ntype);
            printf("Reading data for test %d, ntype=%s\n", (int) test_num, (s == NULL ? "Unknown" : s));
            HDfree(s);
            }
    )

        aid = Hstartread(fid, COMP_TAG, ref_num);
    CHECK(aid, FAIL, "Hstartread");
    if (aid == FAIL)
        return;

    switch (ntype)
      {
          case DFNT_INT8:
              out_ptr = (VOIDP) outbuf_int8[test_num];
              in_ptr = (VOIDP) inbuf_int8;
              break;
          case DFNT_UINT8:
              out_ptr = (VOIDP) outbuf_uint8[test_num];
              in_ptr = (VOIDP) inbuf_uint8;
              break;
          case DFNT_INT16:
              out_ptr = (VOIDP) outbuf_int16[test_num];
              in_ptr = (VOIDP) inbuf_int16;
              break;
          case DFNT_UINT16:
              out_ptr = (VOIDP) outbuf_uint16[test_num];
              in_ptr = (VOIDP) inbuf_uint16;
              break;
          case DFNT_INT32:
              out_ptr = (VOIDP) outbuf_int32[test_num];
              in_ptr = (VOIDP) inbuf_int32;
              break;
          case DFNT_UINT32:
              out_ptr = (VOIDP) outbuf_uint32[test_num];
              in_ptr = (VOIDP) inbuf_uint32;
              break;
          default:
              return;
      }     /* end switch */

    read_size = BUFSIZE * DFKNTsize(ntype);
    err_ret = Hread(aid, read_size, in_ptr);
    if (err_ret != read_size)
      {
          fprintf(stderr, "ERROR(%d): Hwrite returned the wrong length: %d\n", __LINE__, (int) err_ret);
          HEprint(stdout, 0);
          num_errs++;
      }     /* end if */

    if (HDmemcmp(in_ptr, out_ptr, read_size) != 0)
      {
          char       *s = HDgetNTdesc(ntype);

          HDget_special_info(aid, &info_block);
          fprintf(stderr, "ERROR: Data from test: %d, number type: %s, model type: %d, coder type: %d differs\n", test_num, s, (int)info_block.model_type, (int)info_block.comp_type);
          MESSAGE(8,
	  for(i=0; i<read_size*DFKNTsize(ntype); i++) 
	    {
	      if(((char *)in_ptr)[i]!=((char *)out_ptr)[i]) 
		  printf("byte %i differs, written:%d, read in:%d\n",i,((char *)out_ptr)[i],((char *)in_ptr)[i]);
	    } /* end for */
	  )
          HDfree(s);
          num_errs++;
      }     /* end if */

    err_ret = Hendaccess(aid);
    CHECK(err_ret, FAIL, "Hendaccess");
}   /* end read_data() */

void
test_comp(void)
{
    model_info  m_info;
    comp_info   c_info;
    uint16      ref_num;        /* reference number of the data written out */
    int32       fid;            /* file ID of HDF file for testing */
    intn        test_num, ntype_num, model_num, coder_num;
    int32       ret;

    MESSAGE(6, printf("Starting compression test\n");
        )

    /* allocate room for the input and output buffers */
        allocate_buffers();

    /* fill the buffers with interesting data to compress */
    init_buffers();

    /* open the HDF file */
    fid = Hopen(TESTFILE_NAME, DFACC_ALL, 0);

    /* Cycle through the different testing data, the number types, */
    /* the different modeling layers and the different coding layers, */
    /* in that order */
    for (test_num = 0; test_num < NUM_OUTBUFS; test_num++)
      {
          for (ntype_num = 0; (size_t)ntype_num < (sizeof(test_ntypes) / sizeof(test_ntypes[0]));
               ntype_num++)
            {
                for (model_num = 0;
                 (size_t)model_num < (sizeof(test_models) / sizeof(test_models[0]));
                     model_num++)
                  {
                      init_model_info(test_models[model_num], &m_info, test_ntypes[ntype_num]);
                      for (coder_num = 0;
                           (size_t)coder_num < (sizeof(test_coders) / sizeof(test_coders[0]));
                           coder_num++)
                        {
                            init_coder_info(test_coders[coder_num], &c_info, test_ntypes[ntype_num]);

                            ref_num = write_data(fid, test_models[model_num], &m_info, test_coders[coder_num], &c_info, test_num, test_ntypes[ntype_num]);
                            read_data(fid, ref_num, test_num, test_ntypes[ntype_num]);
                            MESSAGE(6,
                                    {
                                    int32 aid;
                                    sp_info_block_t info_block;

                                    aid = Hstartread(fid, COMP_TAG, ref_num);
                                    HDget_special_info(aid, &info_block);
                                    Hendaccess(aid);
                                    printf("size of original HDF element=%ld\n", (long) Hlength(fid, COMP_TAG, ref_num));
                                    printf("size of compressed HDF element=%ld\n", (long) info_block.comp_size);
                                    }
                            )
                        }   /* end for */
                  }     /* end for */
            }   /* end for */
      }     /* end for */

    /* close the HDF file */
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    /* free the input and output buffers */
    free_buffers();

    MESSAGE(6, printf("Finished compression test\n");
        )
}   /* end test_comp() */
