/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2006 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef CH3USOCK_H_INCLUDED
#define CH3USOCK_H_INCLUDED

typedef enum MPIDI_CH3I_Conn_state
{
    CONN_STATE_UNCONNECTED,
    CONN_STATE_LISTENING,
    CONN_STATE_CONNECTING,
    CONN_STATE_CONNECT_ACCEPT, 
    CONN_STATE_OPEN_CSEND,
    CONN_STATE_OPEN_CRECV,
    CONN_STATE_OPEN_LRECV_PKT,
    CONN_STATE_OPEN_LRECV_DATA,
    CONN_STATE_OPEN_LSEND,
    CONN_STATE_CONNECTED,
    CONN_STATE_CLOSING,
    CONN_STATE_CLOSED,
    CONN_STATE_FAILED
} MPIDI_CH3I_Conn_state;

typedef struct MPIDI_CH3I_Connection
{
    MPIDI_VC_t * vc;
    MPIDU_Sock_t sock;
    MPIDI_CH3I_Conn_state state;
    MPID_Request * send_active;
    MPID_Request * recv_active;
    MPIDI_CH3_Pkt_t pkt;
    char * pg_id;
    MPID_IOV iov[2];
} MPIDI_CH3I_Connection_t;


/* These implement the connection state machine for socket connections */
int MPIDI_CH3_Sockconn_handle_accept_event( void );
int MPIDI_CH3_Sockconn_handle_connect_event( MPIDI_CH3I_Connection_t *, int );
int MPIDI_CH3_Sockconn_handle_close_event( MPIDI_CH3I_Connection_t * );
int MPIDI_CH3_Sockconn_handle_conn_event( MPIDI_CH3I_Connection_t * );
int MPIDI_CH3_Sockconn_handle_connopen_event( MPIDI_CH3I_Connection_t * );
int MPIDI_CH3_Sockconn_handle_connwrite( MPIDI_CH3I_Connection_t * );

/* Create/free a new socket connection */
int MPIDI_CH3I_Connection_alloc(MPIDI_CH3I_Connection_t **);
void MPIDI_CH3I_Connection_free(MPIDI_CH3I_Connection_t * conn);

/* Routines to get the socket address */
int MPIDU_CH3U_GetSockInterfaceAddr( int, char *, int, MPIDU_Sock_ifaddr_t * );

/* Return a string for the connection state */
#ifdef USE_DBG_LOGGING
const char * MPIDI_Conn_GetStateString(int);
const char * MPIDI_CH3_VC_GetStateString( int );
#endif

#endif
