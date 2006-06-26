/*-
 * Copyright (c) 1991, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */

/*****************************************************************************
 * File: fmpool.h
 *
 * This is a modfied version of the original Berkley code for
 * manipulating a memory pool. This version however is not 
 * compatible with the original Berkley version.
 *
 * AUTHOR - George V.
 *****************************************************************************/ 

/* $Id: fmpool.h,v 1.13 1996/02/02 17:37:39 georgev Exp $ */

#ifndef _FMPOOL_H
#define _FMPOOL_H

#include "fmpconf.h"
#include "fmptypes.h" /* all the defined types */
#include "queue.h"    /* Circluar queue functions(Macros) */

/*
 * The memory pool scheme is a simple one.  Each in-memory page is referenced
 * by a bucket which is threaded in up to two (three?) ways.  All active pages
 * are threaded on a hash chain (hashed by page number) and an lru chain.
 * (Inactive pages are threaded on a free chain?).  Each reference to a memory
 * pool is handed an opaque MPOOL cookie which stores all of this information.
 */

/* Current Hash table size. Page numbers start with 1 
* (i.e 0 will denote invalid page number) */
#define	HASHSIZE	    128
#define	HASHKEY(pgno)  ((pgno -1) % HASHSIZE)

/* Default pagesize and max # of pages to cache */
#define DEF_PAGESIZE   8192
#define DEF_MAXCACHE   1

/* The BKT structures are the elements of the queues. */
typedef struct _bkt 
{
  CIRCLEQ_ENTRY(_bkt) hq;	/* hash queue */
  CIRCLEQ_ENTRY(_bkt) q;	/* lru queue */
  void    *page;            /* page */
  pageno_t   pgno;          /* page number */

#define	MPOOL_DIRTY	 0x01   /* page needs to be written */
#define	MPOOL_PINNED 0x02   /* page is pinned into memory */
  u_int8_t flags;           /* flags */
} BKT;

/* The element structure for every page referenced(read/written) in file */
typedef struct _lelem
{
  CIRCLEQ_ENTRY(_lelem) hl;	/* hash list */
  pageno_t        pgno;         /* page number */
#ifdef STATISTICS
  u_int32_t	elemhit;            /* # of hits on page */
#endif
#define ELEM_READ       0x01
#define ELEM_WRITTEN    0x02
#define ELEM_SYNC       0x03
  u_int8_t      eflags;         /* 1= read, 2=written, 3=synced */
} L_ELEM;

#define	MPOOL_EXTEND    0x10	/* increase number of pages 
                                   i.e extend file */

/* Memory pool */
typedef struct MPOOL 
{
  CIRCLEQ_HEAD(_lqh, _bkt)    lqh;	      /* lru queue head */
  CIRCLEQ_HEAD(_hqh, _bkt)    hqh[HASHSIZE];  /* hash queue array */
  CIRCLEQ_HEAD(_lhqh, _lelem) lhqh[HASHSIZE]; /* hash of all elements */
  pageno_t	curcache;		      /* current num of cached pages */
  pageno_t	maxcache;		      /* max number of cached pages */
  pageno_t	npages;			      /* number of pages in the file */
  u_int32_t	lastpagesize;	              /* page size of last page */
  u_int32_t	pagesize;		      /* file page size */
  fmp_file_t    fd;			      /* file handle */
  void (*pgin) __P((void *, pageno_t, void *)); /* page in conversion routine */
  void (*pgout) __P((void *, pageno_t, void *));/* page out conversion routine*/
  void	*pgcookie;                         /* cookie for page in/out routines */
#ifdef STATISTICS
  u_int32_t	listhit;                /* # of list hits */
  u_int32_t	listalloc;              /* # of list elems allocated */
  u_int32_t	cachehit;               /* # of cache hits */
  u_int32_t	cachemiss;              /* # of cache misses */
  u_int32_t	pagealloc;              /* # of pages allocated */
  u_int32_t	pageflush;              /* # of pages flushed */
  u_int32_t	pageget;                /* # of pages requested from pool */
  u_int32_t	pagenew;                /* # of new pages */
  u_int32_t	pageput;                /* # of pages put back into pool */
  u_int32_t	pageread;               /* # of pages read from file */
  u_int32_t	pagewrite;              /* # of pages written to file */
#endif /* STATISTICS */
} MPOOL;

__BEGIN_DECLS
/******************************************************************************
NAME
   fmpool_open -- Open a memory pool on the given file

DESCRIPTION
   Initialize a memory pool. 
   We try to find the length of the file using either the stat() calls
   or seeking to the end of the file and getting the offset. We take
   special note of the size of the lastpage when the file size is not even
   multiple of page sizes.

RETURNS
   A memory pool cookie if successful else NULL

NOTE: We don't have much use for the page in/out filters as we rely
      on the interfaces above us to fill the page and we allow the user
      to arbitrarily change the pagesize from one invocation to another.
      This deviates from the original Berkely implemntation.

      The key string byte for sharing buffers is not implemented.
******************************************************************************/
MPOOL *fmpool_open __P((void *key, /* IN:byte string used as handle to share buffers */
                        fmp_file_t fd, /* IN: seekable file handle */
                        pageno_t pagesize, /* IN: size in bytes of the pages to break the file up into */
                        pageno_t maxcache /* IN: maximum number of pages to cache at any time */));

/******************************************************************************
NAME
   fmpool_filter -- Initialize input/output filters.

DESCRIPTION
   Initialize input/output filters for user page processing.

RETURNS
   Nothing

NOTE: the filters must now handle the case where the page
      is the last page which may not be a full 'pagesize' so
      the filters must check for this.

      We don't use these yet.
******************************************************************************/
void	 fmpool_filter __P((MPOOL *mp, /* IN: MPOOL cookie */
                            void (*pgin)(void *, pageno_t, void *) ,/* IN: page in filter */
                            void (*pgout)(void *, pageno_t, void *) , /* IN: page out filter */
                            void * pgcookie /* IN: filter cookie */));

/******************************************************************************
NAME
   fmpool_new -- get a new page of memory

DESCRIPTION
    Get a new page of memory. This is where we get new pages for the file.
    This will only return a full page of memory.
    If the last page is an odd size the user must keep track
    of this as only lastpagesize bytes will be written out.
    As a result if the user fills the last page and
    lastpagesize does not equal pagesize the user will lose data.

    If 'flags' = 0, increase number of pages by 1 and return
                   *pgnoaddr = npages

    If 'flags' = MPOOL_EXTEND, set page to *pgnoaddr and
                               npages = *pgnoaddr.

    All returned pages are pinned.

RETURNS
    Returns the new page if successfull and NULL otherwise
******************************************************************************/
void	*fmpool_new __P((MPOOL *mp, /* IN: MPOOL cookie */
                         pageno_t *pgnoaddr, /* IN/OUT: address of newly create page */
                         pageno_t pagesize, /* IN: page size for last page*/
                         u_int32_t flags /* IN:MPOOL_EXTEND or 0 */));


/******************************************************************************
NAME
   fmpool_get - get a specified page by page number.

DESCRIPTION
    Get a page specified by 'pgno'. If the page is not cached then
    we need to create a new page. All returned pages are pinned.

RETURNS
   The specifed page if successful and NULL otherwise
******************************************************************************/
void	*fmpool_get __P((MPOOL *mp, /* IN: MPOOL cookie */
                         pageno_t pgno, /* IN: page number */
                         u_int32_t flags /* IN: XXX not used? */));

/******************************************************************************
NAME
   fmpool_put -- put a page back into the memory buffer pool

DESCRIPTION
    Return a page to the buffer pool. Unpin it and mark it 
    appropriately i.e. MPOOL_DIRTY

RETURNS
    RET_SUCCESS if succesful and RET_ERROR otherwise
******************************************************************************/
int	 fmpool_put __P((MPOOL *mp, /* IN: MPOOL cookie */
                         void *page, /* IN: page to put */ 
                         u_int32_t flags /* IN: flags = 0, MPOOL_DIRTY */));

/******************************************************************************
NAME
   fmpool_sync -- sync the memory buffer pool

DESCRIPTION
   Sync the pool to disk. Does NOT Free the buffer pool.

RETURNS
   RET_SUCCESS if succesful and RET_ERROR otherwise   
******************************************************************************/
int	 fmpool_sync __P((MPOOL *mp /* IN: MPOOL cookie */));

/******************************************************************************
NAME
   fmpool_close - close the memory buffer pool

DESCRIPTION
   Close the buffer pool.  Frees the buffer pool.
   Does not sync the buffer pool.

RETURNS
   RET_SUCCESS if succesful and RET_ERROR otherwise   
******************************************************************************/
int	 fmpool_close __P((MPOOL *mp /* IN: MPOOL cookie */));

/******************************************************************************
NAME
     fmpool_set_lastpagesize - set the pagesize of the last page in the file.

DESCRIPTION
     Set the pagesize of the last page in the file.

RETURNS
     Returns RET_SUCCESS if successful and RET_ERROR otherwise.
******************************************************************************/
int  fmpool_set_lastpagesize __P((MPOOL *, /* IN: MPOOL cookie */
                               pageno_t /* IN: pagesize to set for last page */));

/******************************************************************************
NAME
     fmpool_get_lastpagsize - returns pagesize of last page in file.

DESCRIPTION
     Finds the size of the last page in the file.

RETURNS
     returns the pagesize of the last page in the file.
******************************************************************************/
pageno_t  fmpool_get_lastpagesize __P((MPOOL *mp /* IN: MPOOL cookie */));

/******************************************************************************
NAME
    fmpool_get_pagsize - returns pagesize for file

DESCRIPTION
    Finds current pagesize used for file.

RETURNS
    returns pagesize for file.
******************************************************************************/
pageno_t  fmpool_get_pagesize __P((MPOOL *mp /* IN: MPOOL cookie */));

/******************************************************************************
NAME
    fmpool_get_maxcache - returns current number of pages cached.

DESCRIPTION
    Finds current number of pages cached for file.

RETURNS
    Returns current number of pages cached.
******************************************************************************/
pageno_t  fmpool_get_maxcache __P((MPOOL *mp /* IN: MPOOL cookie */));

/******************************************************************************
NAME
    fmpool_get_npages - returns current number of pages in file.

DESCRIPTION
    Finds current number of pages in file.

RETURNS
    Returns current number of pages in file.
******************************************************************************/
pageno_t  fmpool_get_npages __P((MPOOL *mp /* IN: MPOOL cookie */));
#ifdef STATISTICS
void	 fmpool_stat __P((MPOOL *));
#endif /* STATISTICS */
#if 0
int	 fmpool_page_sync __P((MPOOL *, pageno_t, u_int32_t));
#endif
__END_DECLS

#endif /* _FMPOOL_H */
