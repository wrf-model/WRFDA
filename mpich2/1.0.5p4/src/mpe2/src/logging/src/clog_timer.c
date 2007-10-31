/*
   (C) 2001 by Argonne National Laboratory.
       See COPYRIGHT in top-level directory.
*/
#include "mpe_logging_conf.h"

#if defined( STDC_HEADERS ) || defined( HAVE_STDIO_H )
#include <stdio.h>
#endif

#if !defined( CLOG_NOMPI )
#include "mpi.h"
#endif

#include "clog_util.h"
#include "clog_timer.h"

static CLOG_Time_t  clog_time_offset;


#if !defined( CLOG_NOMPI )
void CLOG_Timer_start( void )
{
    CLOG_Time_t   local_time;
    int           flag;

    PMPI_Initialized(&flag);
    if (!flag)
        PMPI_Init(0,0);

    if ( CLOG_Util_is_MPIWtime_synchronized() == CLOG_BOOL_TRUE ) {
        /*  Clocks are synchronized  */
        local_time = PMPI_Wtime();
        PMPI_Allreduce( &local_time, &clog_time_offset, 1, MPI_DOUBLE,
                        MPI_MAX, MPI_COMM_WORLD );
    }
    else { /*  Clocks are NOT synchronized  */
        clog_time_offset = PMPI_Wtime();
    }
}

CLOG_Time_t  CLOG_Timer_get( void )
{
    return ( PMPI_Wtime() - clog_time_offset );
}
#else
/*
    Assume gettimeofday() exists, maybe configure needs to check this function.
*/
#if defined( HAVE_SYS_TIME_H )
#include <sys/time.h>
#endif
void CLOG_Timer_start( void )
{
    struct timeval  tval;
    gettimeofday( &tval, NULL );
    clog_time_offset = (CLOG_Time_t) tval.tv_sec
                     + (CLOG_Time_t) tval.tv_usec * 0.000001;
}

CLOG_Time_t  CLOG_Timer_get( void )
{
    struct timeval  tval;
    gettimeofday( &tval, NULL );
    return   (CLOG_Time_t) tval.tv_sec
           + (CLOG_Time_t) tval.tv_usec * 0.000001
           - clog_time_offset; 
}
#endif
