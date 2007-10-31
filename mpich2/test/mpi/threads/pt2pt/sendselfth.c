/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *
 *  (C) 2006 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#include "mpitest.h"

static char MTEST_Descrip[] = "Send to self in a threaded program";


#ifdef HAVE_WINDOWS_H
#include <windows.h>
#define THREAD_RETURN_TYPE DWORD
int start_send_thread(THREAD_RETURN_TYPE (*fn)(void *p))
{
    HANDLE hThread;
    hThread = CreateThread(NULL, 0, (LPTHREAD_START_ROUTINE)fn, NULL, 0, NULL);
    if (hThread == NULL)
    {
	return GetLastError();
    }
    CloseHandle(hThread);
    return 0;
}
#else
#include <pthread.h>
#define THREAD_RETURN_TYPE void *
int start_send_thread(THREAD_RETURN_TYPE (*fn)(void *p));

int start_send_thread(THREAD_RETURN_TYPE (*fn)(void *p))
{
    int err;
    pthread_t thread;
    /*pthread_attr_t attr;*/
    err = pthread_create(&thread, NULL/*&attr*/, fn, NULL);
    return err;
}
#endif

THREAD_RETURN_TYPE send_thread(void *p);

THREAD_RETURN_TYPE send_thread(void *p)
{
    int rank;
    char buffer[100];

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    MPI_Send(buffer, sizeof(buffer), MPI_CHAR, rank, 0, MPI_COMM_WORLD);
    return (THREAD_RETURN_TYPE)0;
}

int main( int argc, char *argv[] )
{
    int rank, size;
    int provided;
    char buffer[100];
    MPI_Status status;

    MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if (provided != MPI_THREAD_MULTIPLE)
    {
	if (rank == 0)
	{
	    printf("MPI_Init_thread must return MPI_THREAD_MULTIPLE in order for this test to run.\n");
	    fflush(stdout);
	}
	MPI_Finalize();
	return -1;
    }

    start_send_thread(send_thread);

    MPI_Probe(MPI_ANY_SOURCE,MPI_ANY_TAG,MPI_COMM_WORLD,&status);

    MPI_Recv(buffer, sizeof(buffer), MPI_CHAR, rank, 0, MPI_COMM_WORLD, &status);

    MTest_Finalize(0);
    MPI_Finalize();
    return 0;
}
