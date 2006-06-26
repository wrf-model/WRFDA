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
 * tsds - writes a 2Dimensional 
 *        dim[0] = numrecs
 *        dim[1] = reclen
 */

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

#include "fmpio.h"

#define DEFAULT_RECLEN  1024
#define DEFAULT_NUMRECS 10

#define USAGE1 \
"Usage:test_mp <output filename> [record_length_in_bytes] [number of records] \n "
#define USAGE2 \
"This program writes out a 2dimensional array where \n"
#define USAGE3 \
" dim[0]=record length,dim[1]=number of records\n"

char t_filename[256] = "test.mp";

int 
main(int argc, char *argv[])    /* main body of code */
{
  char *buffer = NULL;
  MPFILE *fhandle = NULL;
  int sdfid, sdsid;
  int start[2];
  int dims[2];
  int *pindex;
  int nindex;
  int cvalue;
  int status;
  int reclen;
  int numrecs;
  int thisrec;
  int cur_seek_pos;
  int i;

  switch(argc)
    {
    case 1: /* no args */
      printf(USAGE1);
      printf(USAGE2);
      printf(USAGE3);
      reclen = DEFAULT_RECLEN;
      numrecs = DEFAULT_NUMRECS;
      printf("Using defaults: filname=%s,reclen=%d, numrecs=%d\n",
             t_filename,reclen,numrecs);
      break;
    case 2: /* filename */
      strcpy(t_filename,argv[1]);
      reclen = DEFAULT_RECLEN;
      numrecs = DEFAULT_NUMRECS;
      break;
    case 3: /* filename, reclen ?*/
      strcpy(t_filename,argv[1]);
      reclen = (unsigned) abs(atoi(argv[1])); 
      numrecs = DEFAULT_NUMRECS;
      break;
    case 4: /* filename reclen and numrecs  ?*/
      strcpy(t_filename,argv[1]);
      reclen = (unsigned) abs(atoi(argv[2])); 
      numrecs = (unsigned) abs(atoi(argv[3])); 
      break;
    default: /* print usage */
      printf(USAGE1);
      printf(USAGE2);
      printf(USAGE3);
      exit(1);
    }

  start[0] = 0;
  start[1] = 0;
  dims[0] = numrecs;  /* y bytes */
  dims[1] = reclen ;  /* x bytes */

#ifdef TSDS_DEBUG
  printf("dims[0] = %d\n",dims[0]);
  printf("dims[1] = %d\n",dims[1]);

#endif

  if ((buffer = (char *) calloc(1, reclen)) == NULL)
    printf("unable to allocate buffer(reclen) of size %d\n",reclen);

  memset(buffer,'\0',reclen);

  if ((pindex = (int *) calloc(numrecs, sizeof(int))) == NULL)
    printf("unable to allocate pindex[numrecs] of size integer\n");

  for(thisrec = 0; thisrec < numrecs; thisrec++)
    {
      pindex[thisrec] = thisrec;
    }

  /* seed random generator */
  srand(getpid());

  for(thisrec = 0; thisrec < numrecs; thisrec++)
    {
      nindex = rand() % numrecs;
      cvalue = pindex[thisrec];
      pindex[thisrec] = pindex[nindex];
      pindex[nindex] = cvalue;
#ifdef DEBUG
      printf("pindex[%d]=%d, pindex[%d]=%d\n",
             thisrec,pindex[thisrec],nindex,pindex[nindex]);
#endif
    }

#ifdef DEBUG
  for(thisrec = 0; thisrec < numrecs; thisrec++)
    {
      printf("pindex[%d]=%d\n",thisrec,pindex[thisrec]);
    }
#endif

  /* reading time */
  if((fhandle = MPopen(t_filename, DFACC_READ)) == NULL)
    {
      printf("MPopen: Cannot opentemporary file: %s\n", t_filename);
      exit(1);
    }

  /* loop writing "numrecs" for size "recline" out */
  cur_seek_pos = 0;
  for(thisrec = 0; thisrec < numrecs; thisrec++)
    {
      memset(buffer,'\0',reclen);
    cur_seek_pos = pindex[thisrec] * reclen;
    /* Added seeking each time to handle RANDOM_IO case  */
    if (MPseek(fhandle, cur_seek_pos, SEEK_SET) == -1)
      {
        fprintf(stderr,"error with MPseek=%d",cur_seek_pos);
	printf("Error seeking to block %d\n", thisrec);
        MPclose(fhandle);
        exit(1);      
      }
#ifdef DEBUG
    printf("\n");
#endif

    if((status = MPread(fhandle, buffer, reclen)) != reclen)
      {
	printf("Error read %d bytes of block %d\n", status, thisrec);
        fprintf(stderr,"cur_seek_pos=%d",cur_seek_pos);
        printf("status = %d\n", status);
        MPclose(fhandle);
	exit(1);
      }
    /* check data */
    for (i=0; i < reclen; i++)
      {
        if (buffer[i] != (i % 15))
        {
          printf("Error reading block %d, seekpos=%d, status=%d\n", 
                 thisrec,cur_seek_pos,status);
          printf("Error with buffer[%d]=%d, should be=%d\n",i, buffer[i],i%15 );
          MPclose(fhandle);
          exit(1);
        }

      }
    } /* end for "thisrec" */

  /* end access to data set */
  if (MPclose(fhandle) == -1)
    printf("MPclose: error closing file \n");

  exit(0);
}
