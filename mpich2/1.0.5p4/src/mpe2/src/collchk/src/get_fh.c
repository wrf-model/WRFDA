/*
   (C) 2004 by Argonne National Laboratory.
       See COPYRIGHT in top-level directory.
*/
#include "collchk.h" 

int CollChk_get_fh(MPI_File fh, MPI_Comm *comm)
{
    int crr_fh=0, found=0;

    /* find the fh */
    while( (crr_fh < CollChk_fh_cnt) && !found ) {
        if(fh == CollChk_fh_list[crr_fh].fh) {
            found = 1;
        } 
        else {
            crr_fh++;
        }
    }

    /* return the comm if found */
    if(found) {
        /* the comm was found */
        *comm = CollChk_fh_list[crr_fh].comm;
        return 1;
    }
    else {
        /* the comm was not found */
        comm = NULL;
        return 0;
    }
}
