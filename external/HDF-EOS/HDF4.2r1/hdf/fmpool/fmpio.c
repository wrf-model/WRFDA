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

#ifdef RCSID
static char RcsId[] = "@(#)$Revision: 1.13 $";
#endif

/* $Id: fmpio.c,v 1.13 1996/02/02 17:37:35 georgev Exp $ */

/*+
   File Memory Pool level stdio I/O routines 

   Routines
   --------
   MPset    - set pagesize and maximum number of pages to cache on next open/create
   MPget    - get last pagesize and max number of pages cached for open/create
   MPopen   - open/create the file and create a memory pool for file
   MPclose  - close the file, sync the file memory pool to disk and close it.
   MPflush  - flush file memory pool to disk 
   MPseek   - seek to the specified file offset in the memory pool
   MPread   - read data from file memory pool into user's buffer
   MPwrite  - write data from user's buffer to file memory pool 

   AUTHOR - George V.

 +*/

#include <errno.h>
#include <string.h>
#include "fmpio.h"

/* global variable, use defaults */
static fmp cur_fmp = { 0, 0}; 

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
int
MPset(int pagesize, /* IN: pagesize to use for next open/create */
      int maxcache, /* IN: max number of pages to cache */
      int flags     /* IN: flags = 0, MP_PAGEALL */
)
{
  int   ret    = SUCCEED;

  /* set pagesize on next open/create */
  if (pagesize >= MIN_PAGESIZE)
    cur_fmp.pagesize = (pageno_t)pagesize;
  else
    ret = FAIL;

  /* set for number of pages to cache on next open/create */
  if (flags != MP_PAGEALL)
    { /* set user limit for number of pages to cache */
      if (maxcache >= 1)
        cur_fmp.maxcache = (pageno_t)maxcache;
      else
        ret = FAIL;
    }
  else /* we want to cache the whole file */
    cur_fmp.maxcache = (pageno_t)MAX_PAGE_NUMBER - 1;

  return ret;
} /* MPset() */

/******************************************************************************
NAME
     MPget - get last pagesize and max number of pages cached for open/create

DESCRIPTION
     This gets the last pagesize and maximum number of pages cached for 
     the last open/create of a file.

RETURNS
     Returns SUCCEED.
******************************************************************************/
int
MPget(int *pagesize, /* OUT: pagesize to used in last open/create */
      int *maxcache, /* OUT: max number of pages cached in last open/create */
      int flags      /* IN: */
)
{
  int   ret    = SUCCEED;

  if (pagesize != NULL)
    *pagesize = cur_fmp.pagesize;
  if (maxcache != NULL)
    *maxcache = cur_fmp.maxcache;

  return ret;
} /* MPget() */

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
MPFILE * 
MPopen(const char * path, /* IN: filename */
       int flags          /* IN: DFACC_CREATE, DFACC_READ, DFACC_WRITE,
                              DFACC_RDWR, DFACC_ALL */
)
{
  MPFILE *mpfs = NULL; /* File struct */
  int   ret    = SUCCEED;

  if (path == NULL)
    {
      ret = FAIL;
      goto done;
    }

  /* allocate space for file struct */
  if ((mpfs = (MPFILE *)calloc(1,sizeof(MPFILE))) == NULL)
    {
      ret = FAIL;
      goto done;
    }

  /* open file */
  switch(flags)
    {
    case DFACC_CREATE:
      if ((mpfs->fd = FMPI_CREATE(path)) == FMPI_OPEN_FAIL)
        {
          ret = FAIL;
          goto done;
        } 
      break;
    case DFACC_READ:
    case DFACC_WRITE:
    case DFACC_RDWR:
    case DFACC_ALL:
    default:
      if ((mpfs->fd = FMPI_OPEN(path,flags)) == FMPI_OPEN_FAIL)
        {
          ret = FAIL;
          goto done;
        } 
      break;
    }

  mpfs->oflags = flags;
  mpfs->curp = 0; /* set current page to none */
  mpfs->curpr = 0;
  mpfs->poff = 0;
  mpfs->foff = 0;
  mpfs->rpage = NULL;

  /* create private memory pool for file 
  * currently we are sharing the pool*/
  if ((mpfs->mp = fmpool_open(NULL,mpfs->fd,cur_fmp.pagesize,cur_fmp.maxcache)) 
      == NULL)
    {
      ret = FAIL;
      goto done;
    }

#ifdef MP_DEBUG
  fprintf(stderr,"MPopen: mp->npages =%d\n",fmpool_get_npages(mpfs->mp));
  fprintf(stderr,"MPopen: mp->pagesize =%d\n",fmpool_get_pagesize(mpfs->mp));
  fprintf(stderr,"MPopen: mp->lastpagesize=%d\n",fmpool_get_lastpagesize(mpfs->mp));
#endif

  /* Get system defaults for pagesize and maxcache for the first time
     when specifed as such(i.e both are 0) */
  if (cur_fmp.pagesize == 0)
    cur_fmp.pagesize = (int)fmpool_get_pagesize(mpfs->mp);

  if (cur_fmp.maxcache == 0)
    cur_fmp.maxcache = (int)fmpool_get_maxcache(mpfs->mp);

  done:
  if(ret == FAIL)
    { /* error cleanup */
      if (mpfs != NULL)
        free(mpfs);
      
      mpfs = NULL; /* return value */
    }
  /* Normal cleanup */

  return mpfs;
} /* MPopen() */

/******************************************************************************
NAME
    MPclose - close the file, sync the file memory pool to disk and close it.

DESCRIPTION
    First sync the file memory pool to disk. Next close the file memory pool.
    Finally close the file

RETURNS
    Returns SUCCEED on success and FAIL otherwise.
******************************************************************************/
int
MPclose(MPFILE *mpfs /* IN: File Memory pool handle */
)
{
  int ret = SUCCEED;

  if (mpfs == NULL)
    {
      ret = FAIL;
      goto done;
    }

#ifdef MP_DEBUG
  fprintf(stderr,"MPclose: sync the file\n");
  fprintf(stderr,"MPclose: mp->npages =%d\n",fmpool_get_npages(mpfs->mp));
  fprintf(stderr,"MPclose: mp->lastpagesize =%d\n",fmpool_get_lastpagesize(mpfs->mp));
#endif
  /* Don't sync the file for Read only access */
  if (mpfs->oflags != DFACC_READ)
    { /* sync pages and then close mpool*/
      if (fmpool_sync(mpfs->mp) == RET_ERROR)
        {
          ret = FAIL;
          goto done;
        }
    }
#ifdef STATISTICS
  fmpool_stat(mpfs->mp);
#endif 
  if (fmpool_close(mpfs->mp) == RET_ERROR)
    {
      ret = FAIL;
      goto done;
    }

  /* Close the file */
  if (FMPI_CLOSE(mpfs->fd) != FMPI_CLOSE_SUCCEED)
    {
      ret = FAIL;
    }

  done:
  if(ret == FAIL)
    { /* error cleanup */
            
    }
  /* Normal cleanup */

  /* free file struct */
  free(mpfs);

  return ret;
} /* MPclose() */

/******************************************************************************
NAME
     MPflush - flush file memory pool to disk 

DESCRIPTION
     Flushes the file memory pool to disk.

RETURNS
     Returns SUCCEED on success and FAIL otherwise
******************************************************************************/
int
MPflush(MPFILE *mpfs /* IN: File Memory pool handle */
)
{
  int ret = SUCCEED;

  if (mpfs == NULL)
    {
      ret = FAIL;
      goto done;
    }

#ifdef MP_DEBUG
  fprintf(stderr,"MPflush: sync the file\n");
  fprintf(stderr,"MPflush: mp->npages =%d\n",fmpool_get_npages(mpfs->mp));
  fprintf(stderr,"MPflush: mp->lastpagesize =%d\n",fmpool_get_lastpagesize(mpfs->mp));
#endif
  /* sync pages */
  if (fmpool_sync(mpfs->mp) == RET_ERROR)
    {
      ret = FAIL;
      goto done;
    }

  done:
  if(ret == FAIL)
    { /* error cleanup */
            
    }
  /* Normal cleanup */

  return ret;
} /* MPflush() */

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
int 
MPseek(MPFILE *mpfs, /* IN: File Memory pool handle */
       off_t offset, /* IN: Offset into the file */
       int whence    /* IN: SEEK_CUR, SEEK_SET, SEEK_END */
)
{
  pageno_t   new_pgno = 0;
  pageno_t   pagesize = 0;
  pageno_t   oddpagesize = 0;
  pageno_t   lastpagesize = 0;
  pageno_t   npages = 0;
  off_t      cur_off = 0;
  u_int32_t  flags = 0;
  void       *mypage = NULL;
  int        ret = SUCCEED;

  if (mpfs == NULL || offset < 0)
    {
      ret = FAIL;
      goto done;
    }
#ifdef MP_DEBUG
  fprintf(stderr,"ENTER->MPseek: mpfs->curp =%u\n",mpfs->curp);
  fprintf(stderr,"ENTER->MPseek: mpfs->foff =%d\n",mpfs->foff);
  fprintf(stderr,"ENTER->MPseek: mpfs->poff =%d\n",mpfs->poff);
#endif
  pagesize = fmpool_get_pagesize(mpfs->mp);
  npages   = fmpool_get_npages(mpfs->mp);
  lastpagesize = fmpool_get_lastpagesize(mpfs->mp);

  /* Adjust offset deepending on seek flag */
  switch( whence )
    {
    case 0:  /* SEEK_SET */
      cur_off = offset;
      break ;
    case 1: /* SEEK_CUR */
      cur_off = offset + mpfs->foff; /* add current offset in file */
      break ;
    case 2: /* SEEK_END */
      cur_off = pagesize * npages; 
      /* Adjust for odd size last page */
      if (pagesize != lastpagesize)
        cur_off -= (pagesize - lastpagesize);

      cur_off += offset; /* add offset from end of file */
      break ;
    }
#ifdef MP_DEBUG
  fprintf(stderr,"MPseek: seeking to cur_off =%d\n",cur_off);
  fprintf(stderr,"MPseek: mp->npages=%u, mp->pagesize=%u\n",
          npages,pagesize);
#endif
  /* calculate which page number this offset refers to */
  new_pgno = (cur_off / pagesize);
  new_pgno++;
  oddpagesize = (cur_off % pagesize);
  if (!oddpagesize && new_pgno != 1)
    { /* we are even multiple of page sizes */
      oddpagesize = pagesize;
      new_pgno--;
    }

#ifdef MP_DEBUG
  fprintf(stderr,"MPseek: this offset corresponds to new_pgno =%u\n",new_pgno);
#endif

  /* Check to see if this page is the current page */
  if (!(mpfs->curp != 0 && new_pgno == mpfs->curp))
    { /* we need to get it */
      /* Check to see if page exists */
      if ((mypage = fmpool_get(mpfs->mp, new_pgno, 0)) == NULL)
        { /* need to extend file and set lastpagesize*/
#ifdef MP_DEBUG
          fprintf(stderr,"MPseek: page =%u does not exist\n",new_pgno);
          fprintf(stderr,"MPseek: oddpagesize=%u \n",oddpagesize);
#endif
          if ((mypage = fmpool_new(mpfs->mp, &new_pgno, oddpagesize, MPOOL_EXTEND)) 
              == NULL)
            {
              ret = FAIL;
              goto done;
            }
          flags = MPOOL_DIRTY; /* mark page as dirty */
        }
      else
        flags = 0;

#ifdef MP_DEBUG
      fprintf(stderr,"MPseek: put page back \n");
#endif

      /* put page back */
      if (fmpool_put(mpfs->mp, mypage, flags) == RET_ERROR)
        {
          ret = FAIL;
          goto done;
        }
    } /* end if need to get page */

  mpfs->curp = new_pgno;    /* current page */
  mpfs->poff = oddpagesize; /* offset into current page */
  mpfs->foff = cur_off;     /* file offset */

  /* Is is this the last page? */
  if (new_pgno == fmpool_get_npages(mpfs->mp))
    { /* set last page size */
      if (mpfs->poff && (mpfs->poff > fmpool_get_lastpagesize(mpfs->mp)))
        {
          if (fmpool_set_lastpagesize(mpfs->mp,oddpagesize) == RET_ERROR)
            {
              ret = FAIL;
              goto done;
            }
#ifdef MP_DEBUG
          fprintf(stderr,"MPseek last page now,  mpfs->mp->lastpagesize =%u\n",
                  fmpool_get_lastpagesize(mpfs->mp));
#endif
        }
    }

  done:
  if(ret == FAIL)
    { /* error cleanup */
      offset = FAIL;
#ifdef MP_DEBUG
      fprintf(stderr,"EXIT->Error with MPseek: \n");
#endif
    }
  /* Normal cleanup */
#ifdef MP_DEBUG
  fprintf(stderr,"EXIT->MPseek: mpfs->curp =%u\n",mpfs->curp);
  fprintf(stderr,"EXIT->MPseek: mpfs->foff =%d\n",mpfs->foff);
  fprintf(stderr,"EXIT->MPseek: mpfs->poff =%d\n",mpfs->poff);
#endif
  return offset;
} /* MPseek() */

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
int 
MPread(MPFILE *mpfs, /* IN: File Memory pool handle */
       void *buf,    /* IN: User buffer to read data into */
       size_t nbytes /* IN: number of bytes to read in  */
)
{
  size_t nr = 0;
  size_t nbr = 0;
  size_t nbl = 0;
  pageno_t npageno = 0;
  pageno_t pagesize = 0;
  pageno_t npages = 0;
  pageno_t oddpagesize = 0;
  off_t    end_off = 0;
  void *mypage = NULL;
  void *cptr = NULL;
  void *bptr = buf;
  int   ret = SUCCEED;
  int   skip_first_put = 0;

  if (mpfs == NULL || buf == NULL)
    {
      ret = FAIL;
      goto done;
    }

  pagesize = fmpool_get_pagesize(mpfs->mp);
  npages   = fmpool_get_npages(mpfs->mp);

  /* calculate bytes left, number of pages*/
  nbl = nbytes;
  end_off = (off_t)(mpfs->foff + nbytes);
  npageno = (pageno_t)(end_off / pagesize);
  npageno++;
  oddpagesize = (pageno_t)(end_off % pagesize);
  if (!oddpagesize && npageno != 1)
    { /* we are even multiple of page sizes */
      oddpagesize = pagesize;
      npageno--;
    }

#ifdef MP_DEBUG
  fprintf(stderr,"ENTER->MPread: npageno =%u\n",npageno);
  fprintf(stderr,"ENTER->MPread: mpfs->curp =%u\n",mpfs->curp);
  fprintf(stderr,"ENTER->MPread: mpfs->foff =%u\n",mpfs->foff);
  fprintf(stderr,"ETNER->MPread: mpfs->poff =%u\n",mpfs->poff);
#endif 

  /* Check to see if this page is the last page read */
  if (mpfs->curpr != 0 && mpfs->curp == mpfs->curpr )
    {/* we don't need to get it */
      mypage = mpfs->rpage;
      skip_first_put = 1;
    }
  else
    { /* we need to get it copy First page */
      if ((mypage = fmpool_get(mpfs->mp, mpfs->curp, 0)) == NULL)
        {
          ret = FAIL;
          goto done;
        }
    }

  cptr = (char *)mypage + mpfs->poff; /* adjust into current page */

  /* set number of bytes read */
  if (nbl > (pagesize - mpfs->poff))
    nbr = pagesize - mpfs->poff; 
  else
    nbr = nbl;

  memcpy(bptr,cptr,nbr); /* copy from first page */
  mpfs->poff = (off_t)(mpfs->poff + nbr);

#ifdef MP_DEBUG
  fprintf(stderr,"MPread: read %d bytes, mpfs->poff =%u\n",nbr,mpfs->poff);
#endif

  /* return page */
  if (!skip_first_put)
    {
      if (fmpool_put(mpfs->mp, mypage, 0) == RET_ERROR)
        {
          ret = FAIL;
          goto done;
        }
    }
  bptr = (char *)bptr + nbr; /* increment buffer ptr */
  nbl -= nbr;  /* decrement bytes left to read */
  nr += nbr;

#ifdef MP_DEBUG
  fprintf(stderr,"MPread: %u pages to process\n",npageno - mpfs->curp);
#endif
  /* deal with all other pages */
  while (npageno && (npageno - mpfs->curp))
    {
      mpfs->curp++; /* next page */
      if ((mypage = fmpool_get(mpfs->mp, mpfs->curp, 0)) == NULL)
        {
          ret = FAIL;
          goto done;
        }
      if (nbl > pagesize)
        nbr = pagesize;
      else
        nbr = nbl;
      memcpy(bptr,mypage,nbr); /* copy first page */
      mpfs->poff = nbr;
      /* return page */
      if (fmpool_put(mpfs->mp, mypage, 0) == RET_ERROR)
        {
          ret = FAIL;
          goto done;
        }
      bptr = (char *)bptr + nbr; /* increment buffer ptr */
      nbl -= nbr;  /* decrement bytes left to read */
      nr += nbr;
    } /* end while */

  mpfs->foff += nr; /* set file offset */

  /* point last page read */
  if (mpfs->curp != mpfs->curpr )
   {
     mpfs->rpage = mypage;
     mpfs->curpr = mpfs->curp; 
   }

  done:
  if(ret == FAIL)
    { /* error cleanup */
#ifdef MP_DEBUG
      fprintf(stderr,"EXIT->MPread: FAILING\n");
#endif
      nr = FAIL;
    }
  /* Normal cleanup */
#ifdef MP_DEBUG
  fprintf(stderr,"EXIT->MPread: read %d bytes\n",nr);
  fprintf(stderr,"EXIT->MPread: mpfs->curp =%u\n",mpfs->curp);
  fprintf(stderr,"EXIT->MPread: mpfs->foff =%d\n",mpfs->foff);
  fprintf(stderr,"EXIT->MPread: mpfs->poff =%d\n\n",mpfs->poff);
#endif 
  return nr;
} /* MPread() */

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
******************************************************************************/
int 
MPwrite(MPFILE *mpfs,  /* IN: File Memory pool handle */
        void *buf,     /* IN: User buffer to write data from */
        size_t nbytes  /* IN: number of bytes to write out */
)
{
  size_t nw = 0;
  size_t nbw = 0;
  size_t nbl = 0;
  pageno_t npagno = 0;
  pageno_t pageno = 0;
  pageno_t new_pgno = 0;
  pageno_t pagesize = 0;
  pageno_t npages = 0;
  pageno_t oddpagesize = 0;
  off_t    cur_off = 0;
  void *mypage = NULL;
  void *cptr = NULL;
  void *bptr = buf;
  int   ret = SUCCEED;

  if (mpfs == NULL)
    {
      ret = FAIL;
      goto done;
    }

  pagesize = fmpool_get_pagesize(mpfs->mp);
  npages   = fmpool_get_npages(mpfs->mp);

  /* calculate bytes left, number of pages*/
  nbl = nbytes;
  cur_off = (off_t)(mpfs->foff + nbytes);
  pageno = (pageno_t)(mpfs->foff / pagesize);
  pageno++;
  npagno = (pageno_t)(cur_off / pagesize);
  npagno++;
  oddpagesize = (pageno_t)(cur_off % pagesize);
  if (!oddpagesize && npagno != 1)
    { /* we are even multiple of page sizes */
      oddpagesize = pagesize;
      npagno--;
    }
  
#ifdef MP_DEBUG
  fprintf(stderr,"Enter->MPwrite: nbytes =%d, mp->pagesize=%u, mp->npages=%u\n",
          nbytes,pagesize,npages);
  fprintf(stderr,"MPwrite: mpfs->curp =%u\n",mpfs->curp);
  fprintf(stderr,"MPwrite: mpfs->foff =%d\n",mpfs->foff);
  fprintf(stderr,"MPwrite: mpfs->poff =%d\n",mpfs->poff);
  fprintf(stderr,"MPwrite: getting pageno =%u, npagno=%u\n",pageno, npagno);
#endif

  /* Check if this was the last page read */
  if (mpfs->curpr >= pageno &&  mpfs->curpr <= npagno)
     mpfs->curpr = 0; /* reset last page read to invalid */

  /* Check to see if this is the current page */
  if (mpfs->curp != pageno)
    mpfs->poff = 0; /* reset page offset since not current page */

  /* get First page */
  if ((mypage = fmpool_get(mpfs->mp, pageno, 0)) == NULL)
    {
      if ((mypage = fmpool_new(mpfs->mp, &new_pgno, pagesize, 0)) == NULL)
        {
          ret = FAIL;
          goto done;
        }
      mpfs->curp = new_pgno;
      mpfs->poff = 0;
      if (fmpool_set_lastpagesize(mpfs->mp,0) == RET_ERROR)
        {
          ret = FAIL;
          goto done;
        }
#ifdef MP_DEBUG
      fprintf(stderr,"MPwrite: page =%u does not exist\n",pageno);
      fprintf(stderr,"MPwrite: pagesize=%u \n",pagesize);
#endif
    }
  else
    mpfs->curp = pageno;

#ifdef MP_DEBUG
  fprintf(stderr,"MPwrite: now mpfs->curp =%u\n",mpfs->curp);
  fprintf(stderr,"MPwrite: now mpfs->foff =%d\n",mpfs->foff);
  fprintf(stderr,"MPwrite: now mpfs->poff =%d\n",mpfs->poff);
#endif

  cptr = (char *)mypage + mpfs->poff; /* adjust into current page */

  /* set number of bytes to write */
  if (nbl > (pagesize - mpfs->poff))
    nbw = (pagesize - mpfs->poff); 
  else
    nbw = nbl;

  memcpy(cptr,bptr,nbw); /* copy into first page */
  mpfs->poff = (off_t)(mpfs->poff + nbw);

#ifdef MP_DEBUG
  fprintf(stderr,"MPwrite: copied %d bytes\n",nbw);
  fprintf(stderr,"MPwrite: mark dirty page=%u\n",mpfs->curp);
  fprintf(stderr,"MPwrite: after write mpfs->poff =%d\n",mpfs->poff);
#endif
  /* Mark page as dirty */
  if (fmpool_put(mpfs->mp, mypage, MPOOL_DIRTY) == RET_ERROR)
    {
      ret = FAIL;
      goto done;
    }

  bptr = (char *)bptr + nbw; /* increment buffer ptr */
  nbl -= nbw;  /* decrement bytes left to write */
  nw += nbw;

  /* Is is this the last page? */
  if (mpfs->curp == fmpool_get_npages(mpfs->mp))
    { /* set last page size */
      if (mpfs->poff && (mpfs->poff > fmpool_get_lastpagesize(mpfs->mp)))
        {
          if (fmpool_set_lastpagesize(mpfs->mp,mpfs->poff) == RET_ERROR)
            {
              ret = FAIL;
              goto done;
            }
#ifdef MP_DEBUG
          fprintf(stderr,"MPwrite: now lastpagesize=%u \n",
                  fmpool_get_lastpagesize(mpfs->mp));
#endif
        }
    }

  /* deal with all other pages */
#ifdef MP_DEBUG
  fprintf(stderr,"MPwrite: %u pages to process\n",npagno - mpfs->curp);
#endif
  while (npagno && (npagno - mpfs->curp))
    {
      mpfs->curp++; /* next page */
#ifdef MP_DEBUG
      fprintf(stderr,"MPwrite: processing page %u \n",mpfs->curp);
#endif
      if ((mypage = fmpool_get(mpfs->mp, mpfs->curp, 0)) == NULL)
        { /* Is the page we requested still less than total number of pages */
          if (mpfs->curp <= fmpool_get_npages(mpfs->mp))
            {
#ifdef MP_DEBUG
              fprintf(stderr,"EXIT_ERROR->MPwrite: wrote %d bytes\n",nw);
              fprintf(stderr,"MPwrite: mpfs->curp =%u\n",mpfs->curp);
              fprintf(stderr,"MPwrite: mpfs->foff =%u\n",mpfs->foff);
              fprintf(stderr,"MPwrite: mpfs->poff =%u\n\n",mpfs->poff);
#endif
              ret = FAIL;
              goto done;
            }
          else
            {
              if ((mypage = fmpool_new(mpfs->mp, &new_pgno, pagesize, 0)) 
                  == NULL)
                {
                  ret = FAIL;
                  goto done;
                }
#ifdef MP_DEBUG
              fprintf(stderr,"MPwrite: page =%u does not exist\n",mpfs->curp);
              fprintf(stderr,"MPwrite: new page =%u \n",new_pgno);
              fprintf(stderr,"MPwrite: pagesize=%u \n",pagesize);
#endif
              mpfs->curp = new_pgno;
              mpfs->poff = 0;
              if (fmpool_set_lastpagesize(mpfs->mp,0) == RET_ERROR)
                {
                  ret = FAIL;
                  goto done;
                }
            }
        } /* end if mypage */

      if (nbl > pagesize)
        nbw = pagesize;
      else
        nbw = nbl;
      memcpy(mypage,bptr,nbw); /* copy page */
      mpfs->poff = nbw;
#ifdef MP_DEBUG
      fprintf(stderr,"MPwrite: copied %d bytes, mpfs->poff=%u\n",nbw,mpfs->poff);
      fprintf(stderr,"MPwrite: mark dirty page=%u\n",mpfs->curp);
#endif
      /* Mark page as dirty */
      if (fmpool_put(mpfs->mp, mypage, MPOOL_DIRTY) == RET_ERROR)
        {
          ret = FAIL;
          goto done;
        }

      bptr = (char *)bptr + nbw; /* increment buffer ptr */
      nbl -= nbw;  /* decrement bytes left to write */
      nw += nbw;
      /* Is is this the last page? */
      if (mpfs->curp == fmpool_get_npages(mpfs->mp))
        { /* set last page size */
          if (mpfs->poff && (mpfs->poff > fmpool_get_lastpagesize(mpfs->mp)))
            {
              if (fmpool_set_lastpagesize(mpfs->mp,mpfs->poff) == RET_ERROR)
                {
                  ret = FAIL;
                  goto done;
                }
#ifdef MP_DEBUG
              fprintf(stderr,"MPwrite: now lastpagesize=%u \n",
                      fmpool_get_lastpagesize(mpfs->mp));
#endif
            }
        }
    } /* end while */

  mpfs->foff += nw; /* set file offset */

  done:
  if(ret == FAIL)
    { /* error cleanup */
      nw = FAIL;
#ifdef MP_DEBUG
      fprintf(stderr,"EXIT->MPwrite Error\n");
#endif
    }
  /* Normal cleanup */
#ifdef MP_DEBUG
  fprintf(stderr,"EXIT->MPwrite: wrote %d bytes\n",nw);
  fprintf(stderr," MPwrite: mpfs->curp =%u\n",mpfs->curp);
  fprintf(stderr," MPwrite: mpfs->foff =%d\n",mpfs->foff);
  fprintf(stderr," MPwrite: mpfs->poff =%d\n",mpfs->poff);
  fprintf(stderr," MPwrite: now lastpagesize=%u \n\n",
          fmpool_get_lastpagesize(mpfs->mp));
#endif
  return nw;
} /* MPwrite() */
