/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
#include "mpidi_ch3_impl.h"

/* FIXME: This doesn't need to be in a separate file.  And is there a 
   better way to specify which business card routines to use ? */

#undef FUNCNAME
#define FUNCNAME MPIDI_CH3I_Get_business_card
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3I_Get_business_card(int myRank, char *value, int length)
{
    int mpi_errno;
    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3I_GET_BUSINESS_CARD);

    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3I_GET_BUSINESS_CARD);

    mpi_errno = MPIDI_CH3U_Get_business_card_sock(myRank, &value, &length);
    if (mpi_errno != MPI_SUCCESS) {
	MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER,"**buscard");
    }

    mpi_errno = MPIDI_CH3U_Get_business_card_sshm(&value, &length);
    if (mpi_errno != MPI_SUCCESS) {
	MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER,"**buscard");
    }

 fn_fail:
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3I_GET_BUSINESS_CARD);
    return mpi_errno;
}
