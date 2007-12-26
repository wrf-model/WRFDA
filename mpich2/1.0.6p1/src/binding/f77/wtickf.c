/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*  
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 *
 * This file is automatically generated by buildiface 
 * DO NOT EDIT
 */
#include "mpi_fortimpl.h"


/* Begin MPI profiling block */
#if defined(USE_WEAK_SYMBOLS) && !defined(USE_ONLY_MPI_NAMES) 
#if defined(HAVE_MULTIPLE_PRAGMA_WEAK) && defined(F77_NAME_LOWER_2USCORE)
extern FORT_DLL_SPEC double FORT_CALL MPI_WTICK(void);
extern FORT_DLL_SPEC double FORT_CALL mpi_wtick__(void);
extern FORT_DLL_SPEC double FORT_CALL mpi_wtick(void);
extern FORT_DLL_SPEC double FORT_CALL mpi_wtick_(void);
extern FORT_DLL_SPEC double FORT_CALL pmpi_wtick_(void);

#pragma weak MPI_WTICK = pmpi_wtick__
#pragma weak mpi_wtick__ = pmpi_wtick__
#pragma weak mpi_wtick_ = pmpi_wtick__
#pragma weak mpi_wtick = pmpi_wtick__
#pragma weak pmpi_wtick_ = pmpi_wtick__


#elif defined(HAVE_PRAGMA_WEAK)

#if defined(F77_NAME_UPPER)
extern FORT_DLL_SPEC double FORT_CALL MPI_WTICK(void);

#pragma weak MPI_WTICK = PMPI_WTICK
#elif defined(F77_NAME_LOWER_2USCORE)
extern FORT_DLL_SPEC double FORT_CALL mpi_wtick__(void);

#pragma weak mpi_wtick__ = pmpi_wtick__
#elif !defined(F77_NAME_LOWER_USCORE)
extern FORT_DLL_SPEC double FORT_CALL mpi_wtick(void);

#pragma weak mpi_wtick = pmpi_wtick
#else
extern FORT_DLL_SPEC double FORT_CALL mpi_wtick_(void);

#pragma weak mpi_wtick_ = pmpi_wtick_
#endif

#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#if defined(F77_NAME_UPPER)
#pragma _HP_SECONDARY_DEF PMPI_WTICK  MPI_WTICK
#elif defined(F77_NAME_LOWER_2USCORE)
#pragma _HP_SECONDARY_DEF pmpi_wtick__  mpi_wtick__
#elif !defined(F77_NAME_LOWER_USCORE)
#pragma _HP_SECONDARY_DEF pmpi_wtick  mpi_wtick
#else
#pragma _HP_SECONDARY_DEF pmpi_wtick_  mpi_wtick_
#endif

#elif defined(HAVE_PRAGMA_CRI_DUP)
#if defined(F77_NAME_UPPER)
#pragma _CRI duplicate MPI_WTICK as PMPI_WTICK
#elif defined(F77_NAME_LOWER_2USCORE)
#pragma _CRI duplicate mpi_wtick__ as pmpi_wtick__
#elif !defined(F77_NAME_LOWER_USCORE)
#pragma _CRI duplicate mpi_wtick as pmpi_wtick
#else
#pragma _CRI duplicate mpi_wtick_ as pmpi_wtick_
#endif
#endif /* HAVE_PRAGMA_WEAK */
#endif /* USE_WEAK_SYMBOLS */
/* End MPI profiling block */


/* These definitions are used only for generating the Fortran wrappers */
#if defined(USE_WEAK_SYBMOLS) && defined(HAVE_MULTIPLE_PRAGMA_WEAK) && \
    defined(USE_ONLY_MPI_NAMES)
extern FORT_DLL_SPEC double FORT_CALL MPI_WTICK(void);
extern FORT_DLL_SPEC double FORT_CALL mpi_wtick__(void);
extern FORT_DLL_SPEC double FORT_CALL mpi_wtick(void);
extern FORT_DLL_SPEC double FORT_CALL mpi_wtick_(void);

#pragma weak MPI_WTICK = mpi_wtick__
#pragma weak mpi_wtick_ = mpi_wtick__
#pragma weak mpi_wtick = mpi_wtick__
#endif

/* Map the name to the correct form */
#ifndef MPICH_MPI_FROM_PMPI
#ifdef F77_NAME_UPPER
#define mpi_wtick_ PMPI_WTICK
#elif defined(F77_NAME_LOWER_2USCORE)
#define mpi_wtick_ pmpi_wtick__
#elif !defined(F77_NAME_LOWER_USCORE)
#define mpi_wtick_ pmpi_wtick
#else
#define mpi_wtick_ pmpi_wtick_
#endif
/* This defines the routine that we call, which must be the PMPI version
   since we're renaming the Fortran entry as the pmpi version.  The MPI name
   must be undefined first to prevent any conflicts with previous renamings,
   such as those put in place by the globus device when it is building on
   top of a vendor MPI. */
#undef MPI_Wtick
#define MPI_Wtick PMPI_Wtick 

#else

#ifdef F77_NAME_UPPER
#define mpi_wtick_ MPI_WTICK
#elif defined(F77_NAME_LOWER_2USCORE)
#define mpi_wtick_ mpi_wtick__
#elif !defined(F77_NAME_LOWER_USCORE)
#define mpi_wtick_ mpi_wtick
/* Else leave name alone */
#endif


#endif /* MPICH_MPI_FROM_PMPI */

/* Prototypes for the Fortran interfaces */
#include "fproto.h"
#include "mpichconf.h"
#include "mpichtimer.h"
FORT_DLL_SPEC double FORT_CALL mpi_wtick_ ( void ) {
    double d; 
    d = MPID_Wtick( );
    return d;
}
