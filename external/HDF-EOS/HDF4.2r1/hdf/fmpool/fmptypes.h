/*-
 * Copyright (c) 1990, 1993, 1994
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
 * File: compat.h
 *
 * This is a modfied version of the original Berkley code for
 * manipulating a memory pool. This version however is not 
 * compatible with the original Berkley version.
 *
 * Author: George V.
 *
 *****************************************************************************/ 

/* $Id: fmptypes.h,v 1.9 1996/02/02 17:37:41 georgev Exp $ */

#ifndef _FMPTYPES_H_
#define	_FMPTYPES_H_

#include <errno.h>
#include <string.h>
#include <sys/types.h>
#ifdef HAVE_CDEFS_H
#include <cdefs.h>
#else
#include "cdefs.h"
#endif

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif /* HAVE_LIMITS_H */

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#ifdef HAVE_FCNTL
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif /* HAVE_FCNTL */

#ifdef HAVE_MIN_MAX
#include <sys/param.h>
#endif /* HAVE_MIN_MAX */

#ifdef HAVE_STAT
#include <sys/stat.h>
#endif /* HAVE_STAT */

#ifdef HAVE_STDLIB
#include <stdlib.h>
#endif /* HAVE_STDLIB */

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif /* HAVE_STDIO_H*/

#ifdef __FMPINTERFACE_PRIVATE
#include "compat.h"
#endif

#define	RET_ERROR	-1		/* Return values. */
#define	RET_SUCCESS	 0
#define	RET_SPECIAL	 1
#ifndef SUCCEED
#define SUCCEED          0
#define FAIL            (-1)
#endif  /* SUCCEED */

#ifndef	__BIT_TYPES_DEFINED__
#define	__BIT_TYPES_DEFINED__
typedef	__signed char		   int8_t;
typedef	unsigned char		 u_int8_t;
typedef	short			  int16_t;
typedef	unsigned short		u_int16_t;
typedef	int			  int32_t;
typedef	unsigned int		u_int32_t;
#ifdef HAVE_INT64
typedef	long long		  int64_t;
typedef	unsigned long long	u_int64_t;
#endif
#endif /* __BIT_TYPES_DEFINED__ */

#define	MAX_PAGE_NUMBER	0xffffffff	/* >= # of pages in a file */
typedef u_int32_t	pageno_t;         /* page type */

/* internal file access codes */
#ifndef DFACC_READ
#define DFACC_READ   1
#define DFACC_WRITE  2
#define DFACC_CREATE 4
#define DFACC_ALL    7
#define DFACC_RDONLY 1
#define DFACC_RDWR   3
#define DFACC_CLOBBER 4
#endif  /* DFACC_READ */

/* -------------------------- File I/O Functions -------------------------- */
/* FILE_IO -- file library to use for file access: 
   1 stdio, 2 fcntl
   default to stdio(1) library i.e. ANSI C buffered I/O */

#ifdef HAVE_STDIO
#define FMPI_OPEN_FAIL    NULL
#define FMPI_CLOSE_SUCCEED   0
/* using C buffered file I/O routines to access files */
typedef FILE *fmp_file_t;

#ifdef VMS
/* For VMS, use "mbc=64" to improve performance     */
#   define FMPI_OPEN(p, a)       (((a) & DFACC_WRITE) ? \
                                fopen((p), "r+", "mbc=64") : \
                                fopen((p), "r", "mbc=64"))
#else  /*  !VMS  */
#ifdef PC386
#   define FMPI_OPEN(p, a)       (((a) & DFACC_WRITE) ? \
                                fopen((p), "rb+") : fopen((p), "rb"))
#else /* !PC386 */
#   define FMPI_OPEN(p, a)       (((a) & DFACC_WRITE) ? \
                                fopen((p), "r+") : fopen((p), "r"))
#endif /* PC386 */
#endif /* !VMS */
#ifdef PC386
#   define FMPI_CREATE(p)      (fopen((p), "wb+"))
#else  /* PC386 */
#   define FMPI_CREATE(p)      (fopen((p), "w+"))
#endif /* PC386 */
#   define FMPI_READ(f, b, n)  (fread((b), 1, (size_t)(n), (f)))
#   define FMPI_WRITE(f, b, n) (fwrite((b), 1, (size_t)(n), (f)))
#   define FMPI_CLOSE(f)       (fclose(f))
#   define FMPI_FLUSH(f)       (fflush(f)==0 ? SUCCEED : FAIL)
#   define FMPI_SEEK(f,o)      (fseek((f), (long)(o), SEEK_SET)==0 ? SUCCEED : FAIL)
#   define FMPI_SEEK_CUR(f,o)  (fseek((f), (long)(o), SEEK_CUR)==0 ? SUCCEED : FAIL)
#   define FMPI_SEEKEND(f)     (fseek((f), (long)0, SEEK_END)==0 ? SUCCEED : FAIL)
#   define FMPI_TELL(f)        (ftell(f))
#endif /* FILE_IO == HAVE_STDIO */

#ifdef HAVE_FCNTL
#define FMPI_OPEN_FAIL    -1
#define FMPI_CLOSE_SUCCEED   0
/* using UNIX unbuffered file I/O routines to access files */
typedef int fmp_file_t;

#   define FMPI_OPEN(p, a)       (((a) & DFACC_WRITE) ? \
                                  open((p), O_RDWR, DEF_FILEMODE) : \
                                  open((p), O_RDONLY, DEF_FILEMODE))
#   define FMPI_CREATE(p)         (open((p), O_RDWR | O_CREAT | O_TRUNC,DEF_FILEMODE))
#   define FMPI_CLOSE(f)          (close(f))
#   define FMPI_FLUSH(f)          (fsync(f)==0 ? SUCCEED : FAIL)
#if 0
#   define FMPI_FLUSH(f)          (SUCCEED)
#endif
#   define FMPI_READ(f, b, n)     (read((f), (char *)(b), (n)))
#   define FMPI_WRITE(f, b, n)    (write((f), (char *)(b), (n)))
#   define FMPI_SEEK(f, o)        (lseek((f), (off_t)(o), SEEK_SET))
#   define FMPI_SEEKEND(f)        (lseek((f), (off_t)0, SEEK_END))
#   define FMPI_TELL(f)           (lseek((f), (off_t)0, SEEK_CUR))
#endif /* FILE_IO == HAVE_FCNTL */

#ifdef HAVE_MACIO
#define FMPI_OPEN_FAIL    -1
#define FMPI_CLOSE_SUCCEED   0
/* using special routines to redirect to Mac Toolkit I/O */
typedef short fmp_file_t;
#   define FMPI_OPEN(x,y)         mopen(x,y)
#   define FMPI_CREATE(name)      mopen(name, DFACC_CREATE)
#   define FMPI_CLOSE(x)          mclose(x)
#   define FMPI_FLUSH(a)          (SUCCEED)
#   define FMPI_READ(a,b,c)       mread(a, (char *) b, (int32) c)
#   define FMPI_WRITE(a,b,c)      mwrite(a, (char *) b, (int32) c)
#   define FMPI_SEEK(x,y)         mlseek(x, (int32 )y, 0)
#   define FMPI_SEEKEND(x)        mlseek(x, 0L, 2)
#   define FMPI_TELL(x)           mlseek(x,0L,1)
#endif /* FILE_IO == HAVE_MACIO */

#ifdef HAVE_PCIO
#define FMPI_OPEN_FAIL    NULL
#define FMPI_CLOSE_SUCCEED   0
/* using special PC functions to enable reading/writing large chunks */
typedef FILE *fmp_file_t;
#   define FMPI_OPEN(p, a)       (((a) & DFACC_WRITE) ? \
                                  fopen((p), "rb+") : fopen((p), "rb"))
#   define FMPI_CREATE(p)        (fopen((p), "wb+"))
/* Alias the FMPI_READ and FMPI_WRITE macros to functions which can handle */
/*  32-bits of data to read/write */
#   define FMPI_READ(f, b, n)    (((int32)(n) == HDfreadbig((b), (n), (f))) ? \
                                  SUCCEED : FAIL)
#   define FMPI_WRITE(f, b, n)   (((int32)(n) == HDfwritebig((b), (n), (f))) ? \
                                  SUCCEED : FAIL)
#   define FMPI_CLOSE(f)          (fclose(f))
#   define FMPI_FLUSH(f)          (fflush(f)==0 ? SUCCEED : FAIL)
#   define FMPI_SEEK(f,o)  (fseek((f), (long)(o), SEEK_SET)==0 ? SUCCEED : FAIL)
#   define FMPI_SEEKEND(f) (fseek((f), (long)0, SEEK_END)==0 ? SUCCEED : FAIL)
#   define FMPI_TELL(f)           (ftell(f))
#endif /* FILE_IO == HAVE_PCIO */

#ifdef HAVE_WINIO
/* using special MS Windows functions to enable reading/writing large chunks */
typedef HFILE fmp_file_t;
#   define FMPI_OPEN(p, a)       (((a) & DFACC_WRITE) ? \
                                  _lopen((p), READ_WRITE) : _lopen((p), READ))
#   define FMPI_CREATE(p)        (_lcreat((p), 0))
/* Alias the FMPI_READ and FMPI_WRITE macros to functions which can handle */
/*  32-bits of data to read/write */
#   define FMPI_READ(f, b, n)    (((int32)(n) == HDfreadbig((b), (n), (f))) ? \
                                  SUCCEED : FAIL)
#   define FMPI_WRITE(f, b, n)   (((int32)(n) == HDfwritebig((b), (n), (f))) ? \
                                  SUCCEED : FAIL)
#   define FMPI_CLOSE(f)          (_lclose(f))
#   define FMPI_FLUSH(f)          (0)
#   define FMPI_SEEK(f, o)        (_llseek((f), (long)(o), SEEK_SET))
#   define FMPI_SEEKEND(f)        (_llseek((f), (long)0, SEEK_END))
#   define FMPI_TELL(f)           (_llseek((f),0l,SEEK_CUR))
#endif /* FILE_IO == HAVE_WINIO */

#ifdef HAVE_WINNTIO
/* using special Windows NT functions to enable reading/writing large chunks */
typedef HFILE fmp_file_t;
#   define FMPI_OPEN(p, a)       (((a) & DFACC_WRITE) ? \
                                  _lopen((p), OF_READWRITE) : _lopen((p), OF_READ))
#   define FMPI_CREATE(p)        (_lcreat((p), 0))
#   define FMPI_READ(f, b, n)    (((int32)(n) == _hread((f), (b), (n))) ? \
                                  SUCCEED : FAIL)
#   define FMPI_WRITE(f, b, n)   (((int32)(n) == _hwrite((f), (b), (n))) ? \
                                  SUCCEED : FAIL)
#   define FMPI_CLOSE(f)        (_lclose(f)==0 ? SUCCEED : FAIL)
#   define FMPI_FLUSH(f)        (0)
#   define FMPI_SEEK(f, o)      (_llseek((f), (long)(o), 0))
#   define FMPI_SEEKEND(f)      (_llseek((f), (long)0, 2))
#   define FMPI_TELL(f)         (_llseek((f),0l,1))
#endif /* FILE_IO == HAVE_WINNTIO */

#endif /* !_FMPTYPES_H_ */
