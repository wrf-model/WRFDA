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

/*****************************************************************************
 * File: fmpio.h
 *
 * AUTHOR - George V.
 *****************************************************************************/ 

/* $Id: fmpio.h,v 1.9 1996/02/02 17:37:36 georgev Exp $ */

#ifndef _FMPIO_H
#define _FMPIO_H

#include <sys/types.h>
#ifdef __FMPINTERFACE_PRIVATE
#include "fmpconf.h"
#include "fmptypes.h"
#include "fmpool.h"

/* Memory Pool file structure 
* Note that we keep a ptr to the last  page read. 
* This is probably not a good idea in a threaded environment */
typedef struct mpfile_st 
{
  fmp_file_t  fd;     /* file handle */
  int         oflags; /* open flags */
  pageno_t    curpr;  /* current page read */
  pageno_t    curp;   /* current page */
  off_t       poff;   /* offset into current page */
  off_t       foff;   /* current offset into file */
  void        *rpage; /* ptr to last page read */
  MPOOL       *mp;    /* memory pool for this file */
} MPFILE;

/* Global struct to hold current pagesize and maxcache 
* Note we are using signed integers here instead of unsigned */
typedef struct fmp_st {
  int pagesize;   /* a value of 0 implies to use defaults */
  int maxcache;   /* a value of 0 implies to use defaults  */
} fmp;

#define MIN_PAGESIZE  512

#define MP_PAGEALL    0x01  /* page the whole file i.e. no limit on 'maxcache' */

#else /* __FMPINTERFACE_PRIVATE */
/* What we define to the user */

/* Hidden data types */
typedef void *MPFILE;

/* file access codes (also found in fmptypes.h) */
#ifndef DFACC_READ
#define DFACC_READ   1
#define DFACC_WRITE  2
#define DFACC_CREATE 4
#define DFACC_ALL    7
#define DFACC_RDONLY 1
#define DFACC_RDWR   3
#define DFACC_CLOBBER 4
#endif  /* DFACC_READ */

#endif /* __FMPINTERFACE_PRIVATE */

/* File memory pool fucntions */

/******************************************************************************
NAME
     MPset - set pagesize and maximum number of pages to cache on next open/create

DESCRIPTION
     Set the pagesize and maximum number of pages to cache on the next 
     open/create of a file. A pagesize that is a power of 2 is recommended.

     The values set here only affect the next open/creation of a file and
     do not change a particular file's paging behaviour after it has been
     opened or created. This maybe changed in a later release.

     Use flags arguement of 'MP_PAGEALL' if the whole file is to be cached 
     in memory otherwise passs in zero.

RETURNS
     Returns SUCCEED if successful and FAIL otherwise

NOTE
     Currently 'maxcache' has to be greater than 1. Maybe use special 
     case of 0 to specify you want to turn page buffering off or use
     the flags arguement.

     Current memory usage overhead for the Memory Pool is approximately
     ~(2k + maxcache*(28+pagesize) + npages*20) bytes.
******************************************************************************/
extern int     MPset(int pagesize, /* IN: pagesize to use for next open/create */
                     int maxcache, /* IN: max number of pages to cache */
                     int flags     /* IN: flags = 0, MP_PAGEALL */);

/******************************************************************************
NAME
     MPget - get last pagesize and max number of pages cached for open/create

DESCRIPTION
     This gets the last pagesize and maximum number of pages cached for 
     the last open/create of a file.

RETURNS
     Returns SUCCEED.
******************************************************************************/
extern int     MPget(int *pagesize, /*OUT: pagesize to used in last open/create */
                     int *maxcache, /*OUT: max number of pages cached in last open/create */
                     int flags      /* IN: */);

/******************************************************************************
NAME
      MPopen - open/create the file and create a memory pool for file

DESCRIPTION
      Open/Create the file for reading/writing and create a memory pool for
      the file. Currently we let the library decide whether to use the 
      default PAGESIZE for creating pages and MAXCACHE for number of pages 
      to cache in the pool.

RETURNS
      Pointer to MPFILE struct if successful and NULL otherwise
******************************************************************************/
extern MPFILE *MPopen( const char * path, /* IN: filename */
                       int flags          /* IN: DFACC_CREATE, DFACC_READ, DFACC_WRITE,
                                             DFACC_RDWR, DFACC_ALL */);

/******************************************************************************
NAME
    MPclose - close the file, sync the file memory pool to disk and close it.

DESCRIPTION
    First sync the file memory pool to disk. Next close the file memory pool.
    Finally close the file

RETURNS
    Returns SUCCEED on success and FAIL otherwise.
******************************************************************************/
extern int     MPclose(MPFILE *mpfs /* IN: File Memory pool handle */ );

/******************************************************************************
NAME
     MPflush - flush file memory pool to disk 

DESCRIPTION
     Flushes the file memory pool to disk.

RETURNS
     Returns SUCCEED on success and FAIL otherwise
******************************************************************************/
extern int     MPflush(MPFILE *mpfs /* IN: File Memory pool handle */);

/******************************************************************************
NAME
     MPseek - seek to the specified file offset in the memory pool

DESCRIPTION
     Seeks to the correct page in the file depending upon the offset and the
     flag 'whence'. Similiar to the stdio routine. Assumes the flags values
     for SEEK_SET, SEEK_CUR and SEEK_END are universal. May not be true
     for non-Unix OS's.

RETURNS
     Returns offset into the file on success and FAIL otherwise.

NOTE
     Note that it returns an 'int' as opposed to 'off_t'. This is 
     because the HDF library still deals with file offsets in terms of
     signed integers....*sigh*...hopefully this will be changed in a future
     release.
******************************************************************************/
extern int     MPseek(MPFILE *mpfs, /* IN: File Memory pool handle */
                      off_t offset, /* IN: Offset into the file */
                      int whence /* IN: SEEK_CUR, SEEK_SET, SEEK_END */);  

/******************************************************************************
NAME
     MPread  - read 'nbytes' from file memory pool into 'buf'

DESCRIPTION
     This routine handles getting the correct pages to read to satisfy 
     the request. The data is then copied from the memory pool into 
     the user's buffer.

RETURNS
     Returns number of bytes read if successful and FAIL otherwise

NOTE
     The memcpy from the buffer pool to the users buffer is an expensive
     operation.
******************************************************************************/
extern int     MPread(MPFILE *mpfs, /* IN: File Memory pool handle */
                      void *buf,    /* IN: User buffer to read data into */
                      size_t nbytes /* IN: number of bytes to read in  */);

/******************************************************************************
NAME
     MPwrite - write 'nbytes' form 'buf' to the file memory pool

DESCRIPTION
     This routine handles getting the correct pages to write to satisfy 
     the request. The data is then copied from the user's buffer to
     the memory pool.

RETURNS
     Returns number of bytes written if successful and FAIL otherwise

NOTE
     The memcpy from the the users buffer to the memory pool is an expensive
     operation.

WARNING
     this is sample warning.
******************************************************************************/
extern int     MPwrite(MPFILE *mpfs, /* IN: File Memory pool handle */
                       void *buf,    /* IN: User buffer to write data from */
                       size_t nbytes /* IN: number of bytes to write out */);
#endif /* _FMPIO_H */
