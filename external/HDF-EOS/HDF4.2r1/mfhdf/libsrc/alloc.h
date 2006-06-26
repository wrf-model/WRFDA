/* $Id: alloc.h,v 1.5 1996/04/23 02:40:06 georgev Exp $ */
#ifndef _ALLOC_H_
#define _ALLOC_H_

#ifndef NO_STDLIB
#include <stdlib.h>
#else
extern char *malloc();
extern char *realloc();
#ifndef NULL
#define NULL  0
#endif /* !NULL */
#endif /* !NO_STDLIB */


#ifdef HDF
#define Alloc(theNum, theType) \
	(theType *)HDmalloc(sizeof(theType) * (theNum))
#else
#define Alloc(theNum, theType) \
	(theType *)malloc(sizeof(theType) * (theNum))
#endif


#ifndef NO_STDLIB
#ifdef HDF
#define Free(ptr)		HDfree((VOIDP)ptr)
#else
#define Free(ptr)		free(ptr)
#define HDfree(ptr)     free(ptr)
#endif
#else
/* old style free */
#ifdef HDF
#define Free(ptr)		(void)HDfree((char *)ptr)
#else
#define Free(ptr)		(void)free((char *)ptr)
#define HDfree(ptr)     (void)free((char *)ptr)
#endif
#endif /* !NO_STDLIB */


/* We need to define these to standard ones when HDF is not defined */
#ifndef HDF
#define HDcalloc(nelem, elsize)   calloc(nelem,elsize)
#define HDmemset(dst,c,n)         memset(dst,c,n)
#define HDrealloc(p,s)            realloc(p,s)
#define HDmalloc(s)               malloc(s)
#endif /* HDF */

#define ARRAYLEN(arr) (sizeof(arr)/sizeof(arr[0]))

#endif /* !_ALLOC_H_ */
