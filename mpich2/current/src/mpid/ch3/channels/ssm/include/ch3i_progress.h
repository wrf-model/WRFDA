/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
#ifndef CH3I_PROGRESS_H_INCLUDED
#define CH3I_PROGRESS_H_INCLUDED

#include "mpidi_ch3_impl.h"
#include "pmi.h"
#include "mpidu_sock.h"

#define USE_GCC_X86_CYCLE_ASM 1
#define USE_WIN_X86_CYCLE_ASM 2

/* FIXME: These common definitions (accessing the timer) should be in a single
   file */
/* FIXME: What are these used for (possibly one of the spinwait variations?)
   Document the use */
#if MPICH_CPU_TICK_TYPE == USE_GCC_X86_CYCLE_ASM
/* This cycle counter is the read time stamp (rdtsc) instruction with gcc asm */
#define MPID_CPU_TICK(var_ptr) \
{ \
    __asm__ __volatile__  ( "cpuid ; rdtsc ; mov %%edx,%1 ; mov %%eax,%0" \
                            : "=m" (*((char *) (var_ptr))), \
                              "=m" (*(((char *) (var_ptr))+4)) \
                            :: "eax", "ebx", "ecx", "edx" ); \
}
typedef long long MPID_CPU_Tick_t;

#elif MPICH_CPU_TICK_TYPE == USE_WIN_X86_CYCLE_ASM
/* This cycle counter is the read time stamp (rdtsc) instruction with Microsoft asm */
#define MPID_CPU_TICK(var_ptr) \
{ \
    register int *f1 = (int*)var_ptr; \
    __asm cpuid \
    __asm rdtsc \
    __asm mov ecx, f1 \
    __asm mov [ecx], eax \
    __asm mov [ecx + TYPE int], edx \
}
typedef unsigned __int64 MPID_CPU_Tick_t;

#else
/*#error CPU tick instruction needed to count progress time*/
#undef MPID_CPU_TICK
#endif

extern volatile unsigned int MPIDI_CH3I_progress_completion_count;

extern MPIDU_Sock_set_t MPIDI_CH3I_sock_set;

int MPIDI_CH3I_Progress_handle_sock_event(MPIDU_Sock_event_t * event);
int MPIDI_CH3I_Handle_shm_read(MPIDI_VC_t *vc, int nb);
int MPIDI_CH3I_SHM_write_progress(MPIDI_VC_t * vc);

#endif
