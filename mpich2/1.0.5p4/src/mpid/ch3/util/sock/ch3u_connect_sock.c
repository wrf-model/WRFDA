/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpidi_ch3_impl.h"
#include "pmi.h"

#include "mpidu_sock.h"

#include "ch3usock.h"

#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
/* Include this for AF_INET */
#include <sys/socket.h>
#endif
#ifdef HAVE_ARPA_INET_H
/* Include this for inet_pton prototype */
#include <arpa/inet.h>
#endif

/* FIXME: Describe what these routines do */

/* Partial description: 
   This file contains the routines that are used to create socket connections,
   including the routines used to encode/decode the description of a connection
   into/out of the "business card".

   ToDo: change the "host description" to an "interface address" so that
   socket connections are targeted at particularly interfaces, not
   compute nodes, and that the address is in ready-to-use IP address format, 
   and does not require a gethostbyname lookup.  - Partially done
 */

/*
 * Manage the connection information that is exported to other processes
 * 
 */
#define MPIDI_CH3I_HOST_DESCRIPTION_KEY  "description"
#define MPIDI_CH3I_PORT_KEY              "port"
#define MPIDI_CH3I_IFNAME_KEY            "ifname"

/*
 * Routines for establishing a listener socket on the socket set that
 * is used for all communication.  These should be called from the
 * channel init and finalize routines.
 */
static int MPIDI_CH3I_listener_port = 0;
static MPIDI_CH3I_Connection_t * MPIDI_CH3I_listener_conn = NULL;

/* Required for (socket version) upcall to Connect_to_root (see FIXME) */
extern MPIDU_Sock_set_t MPIDI_CH3I_sock_set;

int MPIDU_CH3I_SetupListener( MPIDU_Sock_set_t sock_set )
{
    int mpi_errno = MPI_SUCCESS;
    MPIDU_Sock_t sock;

    mpi_errno = MPIDI_CH3I_Connection_alloc(&MPIDI_CH3I_listener_conn);
    if (mpi_errno != MPI_SUCCESS) {
	return mpi_errno;
    }

    MPIU_DBG_MSG(CH3_CONNECT,TYPICAL,
		 "Setting listener connect state to CONN_STATE_LISTENING");
    MPIDI_CH3I_listener_conn->sock	  = NULL;
    MPIDI_CH3I_listener_conn->vc	  = NULL;
    MPIDI_CH3I_listener_conn->state	  = CONN_STATE_LISTENING;
    MPIDI_CH3I_listener_conn->send_active = NULL;
    MPIDI_CH3I_listener_conn->recv_active = NULL;
    
    mpi_errno = MPIDU_Sock_listen(sock_set, MPIDI_CH3I_listener_conn,
				  &MPIDI_CH3I_listener_port, &sock);
    if (mpi_errno) return mpi_errno;

    MPIDI_CH3I_listener_conn->sock = sock;

    return mpi_errno;
}

int MPIDU_CH3I_ShutdownListener( void )
{
    int mpi_errno;
    MPID_Progress_state progress_state;

    mpi_errno = MPIDU_Sock_post_close(MPIDI_CH3I_listener_conn->sock);
    if (mpi_errno != MPI_SUCCESS) {
	return mpi_errno;
    }
    
    MPID_Progress_start(&progress_state);
    while(MPIDI_CH3I_listener_conn != NULL)
    {
	mpi_errno = MPID_Progress_wait(&progress_state);
	
    }
    MPID_Progress_end(&progress_state);

    return mpi_errno;
}

/* Allocates a connection and the pg_id field for a connection only.
   Does not initialize any connection fields other than pg_id.
   Called by routines that create connections, used in this
   file and in ch3_progress*.c in various channels.
*/
#undef FUNCNAME
#define FUNCNAME MPIDI_CH3I_Connection_alloc
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3I_Connection_alloc(MPIDI_CH3I_Connection_t ** connp)
{
    int mpi_errno = MPI_SUCCESS;
    MPIDI_CH3I_Connection_t * conn = NULL;
    int id_sz;
    MPIU_CHKPMEM_DECL(2);
    MPIDI_STATE_DECL(MPID_STATE_CONNECTION_ALLOC);

    MPIDI_FUNC_ENTER(MPID_STATE_CONNECTION_ALLOC);

    MPIU_CHKPMEM_MALLOC(conn,MPIDI_CH3I_Connection_t*,
			sizeof(MPIDI_CH3I_Connection_t),mpi_errno,"conn");

    /* FIXME: This size is unchanging, so get it only once (at most); 
       we might prefer for connections to simply point at the single process
       group to which the remote process belong */
    mpi_errno = PMI_Get_id_length_max(&id_sz);
    if (mpi_errno != PMI_SUCCESS) {
	MPIU_ERR_SETANDJUMP1(mpi_errno,MPI_ERR_OTHER, 
			     "**pmi_get_id_length_max",
			     "**pmi_get_id_length_max %d", mpi_errno);
    }
    MPIU_CHKPMEM_MALLOC(conn->pg_id,char*,id_sz + 1,mpi_errno,"conn->pg_id");
    conn->pg_id[0] = 0;           /* Be careful about pg_id in case a later 
				     error */
    *connp = conn;

  fn_exit:
    MPIDI_FUNC_EXIT(MPID_STATE_CONNECTION_ALLOC);
    return mpi_errno;
  fn_fail:
    MPIU_CHKPMEM_REAP();
    goto fn_exit;
}


/* FIXME: Why does the name include "to_root"?  */

/* FIXME: Describe the algorithm for the connection logic */
#undef FUNCNAME
#define FUNCNAME  MPIDI_CH3I_Connect_to_root_sock
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3I_Connect_to_root_sock(const char * port_name, 
				    MPIDI_VC_t ** new_vc)
{
    int mpi_errno = MPI_SUCCESS;
    MPIDI_VC_t * vc;
    MPIU_CHKPMEM_DECL(1);
    char host_description[MAX_HOST_DESCRIPTION_LEN];
    int port, port_name_tag;
    MPIDU_Sock_ifaddr_t ifaddr;
    int hasIfaddr = 0;
    MPIDI_CH3I_Connection_t * conn;
    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3I_CONNECT_TO_ROOT_SOCK);

    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3I_CONNECT_TO_ROOT_SOCK);

    MPIU_DBG_MSG_S(CH3_CONNECT,VERBOSE,"Connect to root with portstring %s",
		   port_name );

    mpi_errno = MPIDU_Sock_get_conninfo_from_bc( port_name, host_description,
						 sizeof(host_description),
						 &port, &ifaddr, &hasIfaddr );
    if (mpi_errno) {
	MPIU_ERR_POP(mpi_errno);
    }
    mpi_errno = MPIDI_GetTagFromPort(port_name, &port_name_tag);
    if (mpi_errno != MPIU_STR_SUCCESS) {
	MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**argstr_port_name_tag");
    }

    MPIU_DBG_MSG_D(CH3_CONNECT,VERBOSE,"port tag %d",port_name_tag);

    MPIU_CHKPMEM_MALLOC(vc,MPIDI_VC_t *,sizeof(MPIDI_VC_t),mpi_errno,"vc");
    /* FIXME - where does this vc get freed? */

    *new_vc = vc;

    /* FIXME: Note that these *must* always happen together, so they should
       be in a single routine */
    /* FIXME: There may need to be a THIRD routine here, to ensure that the
       channel is initialized for this pair of process groups (this process
       and the remote process to which the vc will connect). */
    MPIDI_VC_Init(vc, NULL, 0);
    MPIDI_CH3_VC_Init( vc );

    mpi_errno = MPIDI_CH3I_Connection_alloc(&conn);
    if (mpi_errno != MPI_SUCCESS) {
	MPIU_ERR_POP(mpi_errno);
    }

    /* conn->pg_id is not used for this conection */

    /* FIXME: To avoid this global (MPIDI_CH3I_sock_set) which is 
       used only in ch3_progress.c and ch3_progress_connect.c in the channels,
       this should be a call into the channel, asking it to setup the
       socket for a connection and return the connection.  That will
       keep the socket set out of the general ch3 code, even if this
       is the socket utility functions. */
    MPIU_DBG_MSG_FMT(CH3_CONNECT,VERBOSE,(MPIU_DBG_FDEST,
	  "posting connect to host %s, port %d", host_description, port ));
    mpi_errno = MPIDU_Sock_post_connect(MPIDI_CH3I_sock_set, conn, 
					host_description, port, &conn->sock);
    if (mpi_errno == MPI_SUCCESS)
    {
        vc->ch.sock = conn->sock;
        vc->ch.conn = conn;
        vc->ch.state = MPIDI_CH3I_VC_STATE_CONNECTING;
        conn->vc = vc;
	MPIU_DBG_CONNSTATECHANGE(vc,conn,CONN_STATE_CONNECT_ACCEPT);
        conn->state = CONN_STATE_CONNECT_ACCEPT;
        conn->send_active = NULL;
        conn->recv_active = NULL;

        /* place the port name tag in the pkt that will eventually be sent to 
	   the other side */
        conn->pkt.sc_conn_accept.port_name_tag = port_name_tag;
    }
    /* --BEGIN ERROR HANDLING-- */
    else
    {
	if (MPIR_ERR_GET_CLASS(mpi_errno) == MPIDU_SOCK_ERR_BAD_HOST)
        { 
            mpi_errno = MPIR_Err_create_code(
		MPI_SUCCESS, MPIR_ERR_RECOVERABLE, FCNAME, __LINE__, MPI_ERR_OTHER, "**ch3|sock|badhost",
		"**ch3|sock|badhost %s %d %s", conn->pg_id, conn->vc->pg_rank, port_name);
        }
        else if (MPIR_ERR_GET_CLASS(mpi_errno) == MPIDU_SOCK_ERR_CONN_FAILED)
        { 
            mpi_errno = MPIR_Err_create_code(
		MPI_SUCCESS, MPIR_ERR_RECOVERABLE, FCNAME, __LINE__, MPI_ERR_OTHER, "**ch3|sock|connrefused",
		"**ch3|sock|connrefused %s %d %s", conn->pg_id, conn->vc->pg_rank, port_name);
        }
        else
        {
	    MPIU_ERR_POP(mpi_errno);
	}
        vc->ch.state = MPIDI_CH3I_VC_STATE_FAILED;
        MPIU_Free(conn);
        goto fn_fail;
    }
    /* --END ERROR HANDLING-- */

 fn_exit:
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3I_CONNECT_TO_ROOT_SOCK);
    return mpi_errno;
 fn_fail:
    MPIU_CHKPMEM_REAP();
    goto fn_exit;
}

/* ------------------------------------------------------------------------- */
/* Business card management.  These routines insert or extract connection
   information when using sockets from the business card */
/* ------------------------------------------------------------------------- */

/* FIXME: These are small routines; we may want to bring them together 
   into a more specific post-connection-for-sock */

/* The host_description should be of length MAX_HOST_DESCRIPTION_LEN */

#undef FUNCNAME
#define FUNCNAME MPIDU_Sock_get_conninfo_from_bc
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDU_Sock_get_conninfo_from_bc( const char *bc, 
				     char *host_description, int maxlen,
				     int *port, MPIDU_Sock_ifaddr_t *ifaddr, 
				     int *hasIfaddr )
{
    int mpi_errno = MPI_SUCCESS;
    int str_errno;
#if !defined(HAVE_WINDOWS_H) && defined(HAVE_INET_PTON)
    char ifname[256];
#endif
    MPIDI_STATE_DECL(MPID_STATE_MPIDU_SOCK_GET_CONNINFO_FROM_BC);

    MPIDI_FUNC_ENTER(MPID_STATE_MPIDU_SOCK_GET_CONNINFO_FROM_BC);

    str_errno = MPIU_Str_get_string_arg(bc, MPIDI_CH3I_HOST_DESCRIPTION_KEY, 
				 host_description, maxlen);
    if (str_errno != MPIU_STR_SUCCESS) {
	/* --BEGIN ERROR HANDLING */
	if (str_errno == MPIU_STR_FAIL) {
	    MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER,"**argstr_missinghost");
	}
	else {
	    /* MPIU_STR_TRUNCATED or MPIU_STR_NONEM */
	    MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**argstr_hostd");
	}
	/* --END ERROR HANDLING-- */
    }
    str_errno = MPIU_Str_get_int_arg(bc, MPIDI_CH3I_PORT_KEY, port);
    if (str_errno != MPIU_STR_SUCCESS) {
	/* --BEGIN ERROR HANDLING */
	if (str_errno == MPIU_STR_FAIL) {
	    MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**argstr_missingport");
	}
	else {
	    /* MPIU_STR_TRUNCATED or MPIU_STR_NONEM */
	    MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**argstr_port");
	}
	/* --END ERROR HANDLING-- */
    }
    /* ifname is optional */
    /* FIXME: This is a hack to allow Windows to continue to use
       the host description string instead of the interface address
       bytes when posting a socket connection.  This should be fixed 
       by changing the Sock_post_connect to only accept interface
       address.  Note also that Windows does not have the inet_pton 
       routine; the Windows version of this routine will need to 
       be identified or written.  See also channels/sock/ch3_progress.c and
       channels/ssm/ch3_progress_connect.c */
    *hasIfaddr = 0;
#if !defined(HAVE_WINDOWS_H) && defined(HAVE_INET_PTON)
    str_errno = MPIU_Str_get_string_arg(bc, MPIDI_CH3I_IFNAME_KEY, 
					ifname, sizeof(ifname) );
    if (str_errno == MPIU_STR_SUCCESS) {
	/* Convert ifname into 4-byte ip address */
	/* Use AF_INET6 for IPv6 (inet_pton may still be used).
	   An address with more than 3 :'s is an IPv6 address */
	
	int rc = inet_pton( AF_INET, (const char *)ifname, ifaddr->ifaddr );
	if (rc == 0) {
	    MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER,"**ifnameinvalid");
	}
	else if (rc < 0) {
	    /* af_inet not supported */
	    MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER,"**afinetinvalid");
	}
	else {
	    /* Success */
	    *hasIfaddr = 1;
	    ifaddr->len = 4;  /* IPv4 address */
	    ifaddr->type = AF_INET;
	}
    }
#endif
    
 fn_exit:
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDU_SOCK_GET_CONNINFO_FROM_BC);
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}


/*  MPIDI_CH3U_Get_business_card_sock - does socket specific portion of 
 *  setting up a business card
 *  
 *  Parameters:
 *     bc_val_p     - business card value buffer pointer, updated to the next 
 *                    available location or freed if published.
 *     val_max_sz_p - ptr to maximum value buffer size reduced by the number 
 *                    of characters written
 *                               
 */

#undef FUNCNAME
#define FUNCNAME MPIDI_CH3U_Get_business_card_sock
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3U_Get_business_card_sock(int myRank, 
				      char **bc_val_p, int *val_max_sz_p)
{
    int mpi_errno = MPI_SUCCESS;
    MPIDU_Sock_ifaddr_t ifaddr;
    char ifname[MAX_HOST_DESCRIPTION_LEN];
    char *bc_orig = *bc_val_p;
    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3U_GET_BUSINESS_CARD_SOCK);

    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3U_GET_BUSINESS_CARD_SOCK);

#if 0
    mpi_errno = MPIDU_Sock_get_host_description( myRank, 
				  host_description, MAX_HOST_DESCRIPTION_LEN);
    if (mpi_errno != MPI_SUCCESS) {
	MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**init_description");
    }

    printf( "Host description is %s\n", host_description );
#endif
    MPIDU_CH3U_GetSockInterfaceAddr( myRank, ifname, sizeof(ifname), &ifaddr );

#if 0
    { int i;
	printf( "Host name from GetSockInterface is %s\n", ifname );
	printf( "ifaddr is " );
	for (i=0; i<ifaddr.len; i++) {
	    printf ("%02x", ifaddr.ifaddr[i] );
	}
	printf( "\n" );
    }
#endif

    mpi_errno = MPIU_Str_add_int_arg(bc_val_p, val_max_sz_p, 
			     MPIDI_CH3I_PORT_KEY, MPIDI_CH3I_listener_port);
    /* --BEGIN ERROR HANDLING-- */
    if (mpi_errno != MPIU_STR_SUCCESS)
    {
	if (mpi_errno == MPIU_STR_NOMEM) {
	    MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**buscard_len");
	}
	else {
	    MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**buscard");
	}
    }
    /* --END ERROR HANDLING-- */
    
    mpi_errno = MPIU_Str_add_string_arg(bc_val_p, val_max_sz_p, 
			   MPIDI_CH3I_HOST_DESCRIPTION_KEY, ifname );
    /* --BEGIN ERROR HANDLING-- */
    if (mpi_errno != MPIU_STR_SUCCESS)
    {
	if (mpi_errno == MPIU_STR_NOMEM) {
	    MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**buscard_len");
	}
	else {
	    MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**buscard");
	}
	return mpi_errno;
    }
    /* --END ERROR HANDLING-- */

    /* Look up the interface address cooresponding to this host description */
    /* FIXME: We should start switching to getaddrinfo instead of 
       gethostbyname */
    /* FIXME: We don't make use of the ifname in Windows in order to 
       provide backward compatibility with the (undocumented) host
       description string used by the socket connection routine 
       MPIDU_Sock_post_connect.  We need to change to an interface-address
       (already resolved) based description for better scalability and
       to eliminate reliance on fragile DNS services. Note that this is
       also more scalable, since the DNS server may serialize address 
       requests.  On most systems, asking for the host info of yourself
       is resolved locally (i.e., perfectly parallel).  Regrettably, not
       all systems do this (e.g., some versions of FreeBSD).
    */
#if 0
#ifndef HAVE_WINDOWS_H
    {
	struct hostent *info;
	char ifname[256];
	unsigned char *p;
	info = gethostbyname( ifname );
	if (info && info->h_addr_list) {
	    p = (unsigned char *)(info->h_addr_list[0]);
	    MPIU_Snprintf( ifname, sizeof(ifname), "%u.%u.%u.%u", 
			   p[0], p[1], p[2], p[3] );
	    MPIU_DBG_MSG_S(CH3_CONNECT,VERBOSE,"ifname = %s",ifname );
	    mpi_errno = MPIU_Str_add_string_arg( bc_val_p, 
						 val_max_sz_p, 
						 MPIDI_CH3I_IFNAME_KEY,
						 ifname );
	    if (mpi_errno != MPIU_STR_SUCCESS) {
		if (mpi_errno == MPIU_STR_NOMEM) {
		    MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**buscard_len");
		}
		else {
		    MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**buscard");
		}
	    }
	}
    }
#endif
#endif 

    {
	char ifname[256];
	unsigned char *p;
	if (ifaddr.len > 0 && ifaddr.type == AF_INET) {
	    p = (unsigned char *)(ifaddr.ifaddr);
	    MPIU_Snprintf( ifname, sizeof(ifname), "%u.%u.%u.%u", 
			   p[0], p[1], p[2], p[3] );
	    MPIU_DBG_MSG_S(CH3_CONNECT,VERBOSE,"ifname = %s",ifname );
	    mpi_errno = MPIU_Str_add_string_arg( bc_val_p, 
						 val_max_sz_p, 
						 MPIDI_CH3I_IFNAME_KEY,
						 ifname );
	    if (mpi_errno != MPIU_STR_SUCCESS) {
		if (mpi_errno == MPIU_STR_NOMEM) {
		    MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**buscard_len");
		}
		else {
		    MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**buscard");
		}
	    }
	}
    }
    
    if (0) {
	fprintf( stdout, "business card is %s\n", bc_orig );
	fflush(stdout);
    }

 fn_exit:
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3U_GET_BUSINESS_CARD_SOCK);
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

/* ------------------------------------------------------------------------- */
/* Below will be/is the code that is used to create a connection and
 * to handle changes to the state of a connection.  
 */
/* ------------------------------------------------------------------------- */
static int connection_post_recv_pkt(MPIDI_CH3I_Connection_t * conn);
static int connection_post_send_pkt(MPIDI_CH3I_Connection_t * conn);
static int connection_post_send_pkt_and_pgid(MPIDI_CH3I_Connection_t * conn);
static int connection_post_sendq_req(MPIDI_CH3I_Connection_t * conn);
static void connection_destroy(MPIDI_CH3I_Connection_t * conn);

/* This routine is called in response to an MPIDU_SOCK_OP_ACCEPT event 
   in ch3_progress */
#undef FUNCNAME
#define FUNCNAME MPIDI_CH3_Sockconn_handle_accept_event
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3_Sockconn_handle_accept_event( void )
{
    int mpi_errno = MPI_SUCCESS;
    MPIDI_CH3I_Connection_t * conn;
    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_ACCEPT_EVENT);

    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_ACCEPT_EVENT);
    
    mpi_errno = MPIDI_CH3I_Connection_alloc(&conn);
    if (mpi_errno != MPI_SUCCESS) {
	MPIU_ERR_POP(mpi_errno);
    }
    mpi_errno = MPIDU_Sock_accept(MPIDI_CH3I_listener_conn->sock, 
				  MPIDI_CH3I_sock_set, conn, &conn->sock);
    if (mpi_errno != MPI_SUCCESS) {
	MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**ch3|sock|accept");
    }
    
    conn->vc = NULL;
    MPIU_DBG_CONNSTATECHANGE(conn->vc,conn,CONN_STATE_OPEN_LRECV_PKT);
    conn->state = CONN_STATE_OPEN_LRECV_PKT;
    conn->send_active = NULL;
    conn->recv_active = NULL;
    
    mpi_errno = connection_post_recv_pkt(conn);
    if (mpi_errno != MPI_SUCCESS) {
	MPIU_ERR_POP(mpi_errno);
    }

 fn_exit:
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_ACCEPT_EVENT);

    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

#undef FUNCNAME
#define FUNCNAME MPIDI_CH3_Sockconn_handle_connect_event
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3_Sockconn_handle_connect_event( MPIDI_CH3I_Connection_t *conn, 
					     int event_error )
{
    int mpi_errno = MPI_SUCCESS;
    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_CONNECT_EVENT);

    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_CONNECT_EVENT);
    
    /* --BEGIN ERROR HANDLING-- */
    if (event_error != MPI_SUCCESS) {
	/* If the connection fails, conn->vc etc is probably invalid,
	   so we can only report that the connection failed */
	mpi_errno = event_error;
	MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER,"**ch3|sock|connfailed" );
    }
    /* --END ERROR HANDLING-- */

    if (conn->state == CONN_STATE_CONNECTING) {
	MPIU_DBG_CONNSTATECHANGE(conn->vc,conn,CONN_STATE_OPEN_CSEND);
	conn->state = CONN_STATE_OPEN_CSEND;
	MPIDI_Pkt_init(&conn->pkt, MPIDI_CH3I_PKT_SC_OPEN_REQ);
	conn->pkt.sc_open_req.pg_id_len = (int) strlen(MPIDI_Process.my_pg->id) + 1;
	conn->pkt.sc_open_req.pg_rank = MPIR_Process.comm_world->rank;
	
	mpi_errno = connection_post_send_pkt_and_pgid(conn);
	if (mpi_errno) { MPIU_ERR_POP(mpi_errno); }
    }
    else {
	/* CONN_STATE_CONNECT_ACCEPT */
	int port_name_tag;

	MPIU_Assert(conn->state == CONN_STATE_CONNECT_ACCEPT);
	MPIU_DBG_CONNSTATECHANGE(conn->vc,conn,CONN_STATE_OPEN_CSEND);
	conn->state = CONN_STATE_OPEN_CSEND;
	
	/* pkt contains port name tag. In memory debugging mode, 
	   MPIDI_Pkt_init resets the packet contents. Therefore,
	   save the port name tag and then add it back. */
	port_name_tag = conn->pkt.sc_conn_accept.port_name_tag;
	
	MPIDI_Pkt_init(&conn->pkt, MPIDI_CH3I_PKT_SC_CONN_ACCEPT);
	
	conn->pkt.sc_conn_accept.port_name_tag = port_name_tag;
	
	mpi_errno = connection_post_send_pkt(conn);
	if (mpi_errno != MPI_SUCCESS) {
	    MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_INTERN,
				"**ch3|sock|scconnaccept");
	}
    }

 fn_exit:
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_CONNECT_EVENT);
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

#undef FUNCNAME
#define FUNCNAME MPIDI_CH3_Sockconn_handle_close_event
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3_Sockconn_handle_close_event( MPIDI_CH3I_Connection_t * conn )
{
    int mpi_errno = MPI_SUCCESS;
    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_CLOSE_EVENT);

    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_CLOSE_EVENT);
		
    /* If the conn pointer is NULL then the close was intentional */
    /* FIXME: What does the above comment mean? */
    if (conn != NULL) {
	if (conn->state == CONN_STATE_CLOSING) {
	    MPIU_Assert(conn->send_active == NULL);
	    MPIU_Assert(conn->recv_active == NULL);
	    if (conn->vc != NULL) {
		MPIU_DBG_VCCHSTATECHANGE(conn->vc,VC_STATE_UNCONNECTED);
		conn->vc->ch.state = MPIDI_CH3I_VC_STATE_UNCONNECTED;
		conn->vc->ch.sock  = MPIDU_SOCK_INVALID_SOCK;
		/* FIXME: Make sure that this is the correct state for the vc */
		/* conn->vc->state    = MPIDI_VC_STATE_INACTIVE; */
		/* Handle_connection takes care of updating the state on the VC */
		mpi_errno = MPIDI_CH3U_Handle_connection(conn->vc, MPIDI_VC_EVENT_TERMINATED);
		if (mpi_errno) { MPIU_ERR_POP(mpi_errno); }
	    }
	}
	else {
	    MPIU_Assert(conn->state == CONN_STATE_LISTENING);
	    MPIDI_CH3I_listener_conn = NULL;
	    MPIDI_CH3I_listener_port = 0;
	    
	    MPIDI_CH3_Progress_signal_completion();
	    /* FIXME: Why is this commented out? */
	    /* MPIDI_CH3I_progress_completion_count++; */
	}
		
	conn->sock = MPIDU_SOCK_INVALID_SOCK;
	MPIU_DBG_CONNSTATECHANGE(conn->vc,conn,CONN_STATE_CLOSED);
	conn->state = CONN_STATE_CLOSED;
	if (conn->vc) {
	    /* This step is important; without this, test
	       disconnect_reconnect fails because the vc->ch.conn 
	       connection will continue to be used, even though
	       the memory has been freed */
	    if (conn->vc->ch.conn == conn) conn->vc->ch.conn = 0;
	    /* FIXME: If this isn't the associated connection, 
	       there may be a problem */
	}
	connection_destroy(conn); 
    }
 fn_exit:
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_CLOSE_EVENT);
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

/* Cycle through the connection setup states */
/* FIXME: separate out the accept and connect sides to make it easier
   to follow the logic */
#undef FUNCNAME
#define FUNCNAME MPIDI_CH3_Sockconn_handle_conn_event
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3_Sockconn_handle_conn_event( MPIDI_CH3I_Connection_t * conn )
{
    int mpi_errno = MPI_SUCCESS;
    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_CONN_EVENT);

    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_CONN_EVENT);

    /* FIXME: Is there an assumption about conn->state? */

    if (conn->pkt.type == MPIDI_CH3I_PKT_SC_OPEN_REQ) {
	/* Answer to fixme: it appears from the control flow that this is
	   the required state) */
	MPIU_Assert( conn->state == CONN_STATE_OPEN_LRECV_PKT);
	MPIU_DBG_CONNSTATECHANGE(conn->vc,conn,CONN_STATE_OPEN_LRECV_DATA);
	conn->state = CONN_STATE_OPEN_LRECV_DATA;
	mpi_errno = MPIDU_Sock_post_read(conn->sock, conn->pg_id, conn->pkt.sc_open_req.pg_id_len, 
					 conn->pkt.sc_open_req.pg_id_len, NULL);   
	if (mpi_errno != MPI_SUCCESS) {
	    MPIU_ERR_POP(mpi_errno);
	}
    }
    else if (conn->pkt.type == MPIDI_CH3I_PKT_SC_CONN_ACCEPT) {
	MPIDI_VC_t *vc; 
	int port_name_tag;

	vc = (MPIDI_VC_t *) MPIU_Malloc(sizeof(MPIDI_VC_t));
	/* --BEGIN ERROR HANDLING-- */
	if (vc == NULL) {
	    mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER,
					     "**nomem", NULL);
	    goto fn_fail;
	}
	/* --END ERROR HANDLING-- */
	/* FIXME - where does this vc get freed? */

	/* FIXME: There should be a single VC init function; this should
	   invoke a channel-specific function to initialize channel-specific
	   items */
	MPIDI_VC_Init(vc, NULL, 0);
	vc->ch.sendq_head = NULL;
	vc->ch.sendq_tail = NULL;
	MPIU_DBG_VCCHSTATECHANGE(vc,VC_STATE_CONNECTING);
	vc->ch.state = MPIDI_CH3I_VC_STATE_CONNECTING;
	vc->ch.sock = conn->sock;
	vc->ch.conn = conn;
	conn->vc = vc;
	port_name_tag = conn->pkt.sc_conn_accept.port_name_tag;
	
	MPIDI_Pkt_init(&conn->pkt, MPIDI_CH3I_PKT_SC_OPEN_RESP);
	conn->pkt.sc_open_resp.ack = TRUE;
	
	/* FIXME: Possible ambiguous state (two ways to get to OPEN_LSEND) */
	MPIU_DBG_CONNSTATECHANGE(conn->vc,conn,CONN_STATE_OPEN_LSEND);
	conn->state = CONN_STATE_OPEN_LSEND;
	mpi_errno = connection_post_send_pkt(conn);
	if (mpi_errno != MPI_SUCCESS) {
	    MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_INTERN,
				"**ch3|sock|scconnaccept");
	}
	
	/* ENQUEUE vc */
	MPIDI_CH3I_Acceptq_enqueue(vc, port_name_tag);

    }
    else if (conn->pkt.type == MPIDI_CH3I_PKT_SC_OPEN_RESP) {
	/* FIXME: is this the correct assert? */
	MPIU_Assert( conn->state == CONN_STATE_OPEN_CRECV );
	if (conn->pkt.sc_open_resp.ack) {
	    MPIU_DBG_CONNSTATECHANGE(conn->vc,conn,CONN_STATE_CONNECTED);
	    conn->state = CONN_STATE_CONNECTED;
	    conn->vc->ch.state = MPIDI_CH3I_VC_STATE_CONNECTED;
	    MPIU_Assert(conn->vc->ch.conn == conn);
	    MPIU_Assert(conn->vc->ch.sock == conn->sock);
	    
	    mpi_errno = connection_post_recv_pkt(conn);
	    if (mpi_errno != MPI_SUCCESS) {
		MPIU_ERR_POP(mpi_errno);
	    }
	    mpi_errno = connection_post_sendq_req(conn);
	    if (mpi_errno != MPI_SUCCESS) {
		MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_INTERN,
				    "**ch3|sock|scopenresp");
	    }
	}
	else {
	    /* FIXME: Should conn->vc be freed? Who allocated? Why not? */
	    /* FIXME: Should probably reduce ref count on conn->vc */
	    /* FIXME: What happens to the state of the associated VC? 
	       Why isn't it changed?  Is there an assert here, 
	       such as conn->vc->conn != conn (there is another connection 
	       chosen for the vc)? */
	    /* MPIU_Assert( conn->vc->ch.conn != conn ); */
	    /* Set the candidate vc for this connection to NULL (we
	       are discarding this connection because (I think) we
	       are performing a head-to-head connection, and this
	       connection is being rejected in favor of the connection
	       from the other side. */
	    if (conn->vc->ch.conn == conn) conn->vc->ch.conn = NULL;
	    conn->vc = NULL;
	    MPIU_DBG_CONNSTATECHANGE_MSG(conn->vc,conn,CONN_STATE_CLOSING,
					"because ack on OPEN_CRECV was false");
	    conn->state = CONN_STATE_CLOSING;
	    /* FIXME: What does post close do here? */
	    MPIDU_Sock_post_close(conn->sock);
	}
    }
    /* --BEGIN ERROR HANDLING-- */
    else {
	MPIU_DBG_STMT(CH3_CONNECT,VERBOSE,MPIDI_DBG_Print_packet(&conn->pkt));
	mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_INTERN,
					 "**ch3|sock|badpacket", "**ch3|sock|badpacket %d", conn->pkt.type);
	goto fn_fail;
    }
    /* --END ERROR HANDLING-- */


 fn_exit:
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_CONN_EVENT);
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

/* FIXME: This should really be combined with handle_conn_event */
#undef FUNCNAME
#define FUNCNAME MPIDI_CH3_Sockconn_handle_connopen_event
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3_Sockconn_handle_connopen_event( MPIDI_CH3I_Connection_t * conn )
{
    int mpi_errno = MPI_SUCCESS;
    MPIDI_PG_t * pg;
    int pg_rank;
    MPIDI_VC_t * vc;
    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_CONNOPEN_EVENT);

    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_CONNOPEN_EVENT);

    /* Look up pg based on conn->pg_id */
    mpi_errno = MPIDI_PG_Find(conn->pg_id, &pg);
    if (pg == NULL) {
	MPIU_ERR_SETANDJUMP1(mpi_errno,MPI_ERR_OTHER,
			     "**pglookup", 
			     "**pglookup %s", conn->pg_id);
    }
    
    pg_rank = conn->pkt.sc_open_req.pg_rank;
    MPIDI_PG_Get_vc(pg, pg_rank, &vc);
    MPIU_Assert(vc->pg_rank == pg_rank);
    
    if (vc->ch.conn == NULL) {
	/* no head-to-head connects, accept the connection */
	MPIU_DBG_VCCHSTATECHANGE(vc,VC_STATE_CONNECTING);
	vc->ch.state = MPIDI_CH3I_VC_STATE_CONNECTING;
	vc->ch.sock = conn->sock;
	vc->ch.conn = conn;
	conn->vc = vc;
	
	MPIDI_Pkt_init(&conn->pkt, MPIDI_CH3I_PKT_SC_OPEN_RESP);
	conn->pkt.sc_open_resp.ack = TRUE;
    }
    else {
	/* head to head situation */
	if (pg == MPIDI_Process.my_pg) {
	    /* the other process is in the same comm_world; just compare the 
	       ranks */
	    if (MPIR_Process.comm_world->rank < pg_rank) {
		/* accept connection */
		MPIU_DBG_VCCHSTATECHANGE(vc,VC_STATE_CONNECTING);
		vc->ch.state = MPIDI_CH3I_VC_STATE_CONNECTING;
		vc->ch.sock = conn->sock;
		vc->ch.conn = conn;
		conn->vc = vc;
		
		MPIDI_Pkt_init(&conn->pkt, MPIDI_CH3I_PKT_SC_OPEN_RESP);
		conn->pkt.sc_open_resp.ack = TRUE;
	    }
	    else {
		/* refuse connection */
		MPIU_DBG_MSG_FMT(CH3_CONNECT,TYPICAL,(MPIU_DBG_FDEST,
                "vc=%p,conn=%p:Refuse head-to-head connection (my process group)",vc,conn));
		MPIDI_Pkt_init(&conn->pkt, MPIDI_CH3I_PKT_SC_OPEN_RESP);
		conn->pkt.sc_open_resp.ack = FALSE;
	    }
	}
	else {
	    /* the two processes are in different comm_worlds; compare their 
	       unique pg_ids. */
	    if (strcmp(MPIDI_Process.my_pg->id, pg->id) < 0) {
		/* accept connection */
		MPIU_DBG_VCCHSTATECHANGE(vc,VC_STATE_CONNECTING);
		vc->ch.state = MPIDI_CH3I_VC_STATE_CONNECTING;
		vc->ch.sock = conn->sock;
		vc->ch.conn = conn;
		conn->vc = vc;
		
		MPIDI_Pkt_init(&conn->pkt, MPIDI_CH3I_PKT_SC_OPEN_RESP);
		conn->pkt.sc_open_resp.ack = TRUE;
	    }
	    else {
		/* refuse connection */
		MPIU_DBG_MSG_FMT(CH3_CONNECT,TYPICAL,(MPIU_DBG_FDEST,
			"vc=%p,conn=%p:Refuse head-to-head connection (two process groups)",vc,conn));
		MPIDI_Pkt_init(&conn->pkt, MPIDI_CH3I_PKT_SC_OPEN_RESP);
		conn->pkt.sc_open_resp.ack = FALSE;
	    }
	}
    }
    
    MPIU_DBG_CONNSTATECHANGE(conn->vc,conn,CONN_STATE_OPEN_LSEND);
    conn->state = CONN_STATE_OPEN_LSEND;
    mpi_errno = connection_post_send_pkt(conn);
    if (mpi_errno != MPI_SUCCESS) {
	MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_INTERN,
			    "**ch3|sock|open_lrecv_data");
    }

 fn_exit:
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_CONNOPEN_EVENT);
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

/* FIXME: This routine is called when?  What is valid in conn? */
#undef FUNCNAME
#define FUNCNAME MPIDI_CH3_Sockconn_handle_connwrite
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3_Sockconn_handle_connwrite( MPIDI_CH3I_Connection_t * conn )
{
    int mpi_errno = MPI_SUCCESS;
    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_CONNWRITE);

    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_CONNWRITE);

    if (conn->state == CONN_STATE_OPEN_CSEND) {
	/* finished sending open request packet */
	/* post receive for open response packet */
	MPIU_DBG_CONNSTATECHANGE(conn->vc,conn,CONN_STATE_OPEN_CRECV);
	conn->state = CONN_STATE_OPEN_CRECV;
	mpi_errno = connection_post_recv_pkt(conn);
	if (mpi_errno != MPI_SUCCESS) {
	    MPIU_ERR_POP(mpi_errno);
	}
    }
    else if (conn->state == CONN_STATE_OPEN_LSEND) {
	/* finished sending open response packet */
	if (conn->pkt.sc_open_resp.ack == TRUE) {
	    /* post receive for packet header */
	    MPIU_DBG_CONNSTATECHANGE(conn->vc,conn,CONN_STATE_CONNECTED);
	    conn->state = CONN_STATE_CONNECTED;
	    MPIU_DBG_VCCHSTATECHANGE(conn->vc,VC_STATE_CONNECTED);
	    conn->vc->ch.state = MPIDI_CH3I_VC_STATE_CONNECTED;
	    mpi_errno = connection_post_recv_pkt(conn);
	    if (mpi_errno != MPI_SUCCESS) {
		MPIU_ERR_POP(mpi_errno);
	    }
	    
	    mpi_errno = connection_post_sendq_req(conn);
	    if (mpi_errno != MPI_SUCCESS) {
		MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_INTERN,
				    "**ch3|sock|openlsend");
	    }
	}
	else {
	    /* head-to-head connections - close this connection */
	    MPIU_DBG_CONNSTATECHANGE(conn->vc,conn,CONN_STATE_CLOSING);
	    /* FIXME: the connect side of this sets conn->vc to NULL. Why is
	       this different? The code that checks CONN_STATE_CLOSING uses
	       conn == NULL to identify intentional close, which this 
	       appears to be. */
	    conn->state = CONN_STATE_CLOSING;
	    mpi_errno = MPIDU_Sock_post_close(conn->sock);
	    if (mpi_errno != MPI_SUCCESS) {
		MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER,
				    "**sock_post_close");
	    }
	}
    }

 fn_exit:
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3_SOCKCONN_HANDLE_CONNWRITE);
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

/* ----------------------------------------------------------------------- */
/* FIXME: What does this do? */
#undef FUNCNAME
#define FUNCNAME MPIDI_CH3I_VC_post_sockconnect
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3I_VC_post_sockconnect(MPIDI_VC_t * vc)
{
    int mpi_errno = MPI_SUCCESS;
    char val[MPIDI_MAX_KVS_VALUE_LEN];
    char host_description[MAX_HOST_DESCRIPTION_LEN];
    int port;
    MPIDU_Sock_ifaddr_t ifaddr;
    int hasIfaddr = 0;
    MPIDI_CH3I_Connection_t * conn = 0;
    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3I_VC_POST_SOCKCONNECT);

    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3I_VC_POST_SOCKCONNECT);
    
    MPIU_Assert(vc->ch.state == MPIDI_CH3I_VC_STATE_UNCONNECTED);
    
    MPIU_DBG_VCCHSTATECHANGE(vc,VC_STATE_CONNECTING);
    vc->ch.state = MPIDI_CH3I_VC_STATE_CONNECTING;

    mpi_errno = MPIDI_PG_GetConnString( vc->pg, vc->pg_rank, val, sizeof(val));
    if (mpi_errno != MPI_SUCCESS) {
	MPIU_ERR_POP(mpi_errno);
    }
    mpi_errno = MPIDU_Sock_get_conninfo_from_bc( val, host_description,
						 sizeof(host_description),
						 &port, &ifaddr, &hasIfaddr );
    if (mpi_errno) {
	MPIU_ERR_POP(mpi_errno);
    }

    mpi_errno = MPIDI_CH3I_Connection_alloc(&conn);
    if (mpi_errno == MPI_SUCCESS)
    {
	/* FIXME: This is a hack to allow Windows to continue to use
	   the host description string instead of the interface address
	   bytes when posting a socket connection.  This should be fixed 
	   by changing the Sock_post_connect to only accept interface
	   address.  See also channels/ssm/ch3_progress_connect.c */
#ifndef HAVE_WINDOWS_H
	if (hasIfaddr) {
	    mpi_errno = MPIDU_Sock_post_connect_ifaddr(MPIDI_CH3I_sock_set, 
						       conn, &ifaddr, port, 
						       &conn->sock);
	}
	else 
#endif
	{
	    mpi_errno = MPIDU_Sock_post_connect(MPIDI_CH3I_sock_set, conn, 
						host_description, port, 
						&conn->sock);
	}
	if (mpi_errno == MPI_SUCCESS)
	{
	    MPIU_DBG_CONNSTATECHANGE(vc,conn,CONN_STATE_CONNECTING);
	    vc->ch.sock = conn->sock;
	    vc->ch.conn = conn;
	    conn->vc = vc;
	    conn->state = CONN_STATE_CONNECTING;
	    conn->send_active = NULL;
	    conn->recv_active = NULL;
	}
	/* --BEGIN ERROR HANDLING-- */
	else
	{
	    MPIU_DBG_VCCHSTATECHANGE(vc,VC_STATE_FAILED);
	    vc->ch.state = MPIDI_CH3I_VC_STATE_FAILED;
	    mpi_errno = MPIR_Err_create_code(mpi_errno, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**ch3|sock|postconnect",
		"**ch3|sock|postconnect %d %d %s", MPIR_Process.comm_world->rank, vc->pg_rank, val);
	    goto fn_fail;
	}
	/* --END ERROR HANDLING-- */
    }
    else {
	MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**ch3|sock|connalloc");
    }

  fn_exit:
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3I_VC_POST_SOCKCONNECT);
    return mpi_errno;
 fn_fail:
    /* --BEGIN ERROR HANDLING-- */
    if (conn) {
	connection_destroy(conn);
    }
    goto fn_exit;
    /* --END ERROR HANDLING-- */
}
/* end MPIDI_CH3I_VC_post_sockconnect() */


/* FIXME: What does this do? */
/* Guess: Setup a wait-to-read on the socket that was set after the accept 
   was handled */
/* Wrong guess.  */
#undef FUNCNAME
#define FUNCNAME connection_post_recv_pkt
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
static int connection_post_recv_pkt(MPIDI_CH3I_Connection_t * conn)
{
    int mpi_errno = MPI_SUCCESS;
    MPIDI_STATE_DECL(MPID_STATE_CONNECTION_POST_RECV_PKT);

    MPIDI_FUNC_ENTER(MPID_STATE_CONNECTION_POST_RECV_PKT);

    mpi_errno = MPIDU_Sock_post_read(conn->sock, &conn->pkt, sizeof(conn->pkt),
				     sizeof(conn->pkt), NULL);
    if (mpi_errno != MPI_SUCCESS) {
	MPIU_ERR_POP(mpi_errno);
    }

 fn_fail:    
    MPIDI_FUNC_EXIT(MPID_STATE_CONNECTION_POST_RECV_PKT);
    return mpi_errno;
}


#undef FUNCNAME
#define FUNCNAME connection_post_send_pkt
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
static int connection_post_send_pkt(MPIDI_CH3I_Connection_t * conn)
{
    int mpi_errno = MPI_SUCCESS;
    MPIDI_STATE_DECL(MPID_STATE_CONNECTION_POST_SEND_PKT);

    MPIDI_FUNC_ENTER(MPID_STATE_CONNECTION_POST_SEND_PKT);
 
    MPIU_DBG_PKT(conn,&conn->pkt,"connect");
    mpi_errno = MPIDU_Sock_post_write(conn->sock, &conn->pkt, sizeof(conn->pkt), sizeof(conn->pkt), NULL);
    if (mpi_errno != MPI_SUCCESS) {
	MPIU_ERR_POP(mpi_errno);
    }
    
 fn_fail:
    MPIDI_FUNC_EXIT(MPID_STATE_CONNECTION_POST_SEND_PKT);
    return mpi_errno;
}

#undef FUNCNAME
#define FUNCNAME connection_post_send_pkt_and_pgid
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
static int connection_post_send_pkt_and_pgid(MPIDI_CH3I_Connection_t * conn)
{
    int mpi_errno;
    MPIDI_STATE_DECL(MPID_STATE_CONNECTION_POST_SEND_PKT_AND_PGID);

    MPIDI_FUNC_ENTER(MPID_STATE_CONNECTION_POST_SEND_PKT_AND_PGID);
    
    conn->iov[0].MPID_IOV_BUF = (MPID_IOV_BUF_CAST) &conn->pkt;
    conn->iov[0].MPID_IOV_LEN = (int) sizeof(conn->pkt);

    conn->iov[1].MPID_IOV_BUF = (MPID_IOV_BUF_CAST) MPIDI_Process.my_pg->id;
    conn->iov[1].MPID_IOV_LEN = (int) strlen(MPIDI_Process.my_pg->id) + 1;

    MPIU_DBG_PKT(conn,&conn->pkt,"connect-pgid");
    mpi_errno = MPIDU_Sock_post_writev(conn->sock, conn->iov, 2, NULL);
    if (mpi_errno != MPI_SUCCESS) {
	MPIU_ERR_POP(mpi_errno);
    }
    
 fn_fail:
    MPIDI_FUNC_EXIT(MPID_STATE_CONNECTION_POST_SEND_PKT_AND_PGID);
    return mpi_errno;
}

/* FIXME: This function also used in channels/sock/src/ch3_progress.c */
#undef FUNCNAME
#define FUNCNAME connection_post_sendq_req
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
static int connection_post_sendq_req(MPIDI_CH3I_Connection_t * conn)
{
    int mpi_errno = MPI_SUCCESS;
    MPIDI_STATE_DECL(MPID_STATE_CONNECTION_POST_SENDQ_REQ);

    MPIDI_FUNC_ENTER(MPID_STATE_CONNECTION_POST_SENDQ_REQ);

    /* post send of next request on the send queue */
    conn->send_active = MPIDI_CH3I_SendQ_head(conn->vc); /* MT */
    if (conn->send_active != NULL)
    {
	MPIU_DBG_MSG_P(CH3,TYPICAL,"conn=%p: Posting message from connection send queue", conn );
	mpi_errno = MPIDU_Sock_post_writev(conn->sock, conn->send_active->dev.iov, conn->send_active->dev.iov_count, NULL);
	if (mpi_errno != MPI_SUCCESS) {
	    MPIU_ERR_POP(mpi_errno);
	}
    }
    
 fn_fail:
    MPIDI_FUNC_EXIT(MPID_STATE_CONNECTION_POST_SENDQ_REQ);
    return mpi_errno;
}


/* This routine frees all of the memory associated with a connection.
   It is named destroy instead of free because routines with name "free" 
   should have MPI semantics - free means to 
   decrement reference count and free if reference count is zero */
#undef FUNCNAME
#define FUNCNAME connection_destroy
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
static void connection_destroy(MPIDI_CH3I_Connection_t * conn)
{
    MPIDI_STATE_DECL(MPID_STATE_CONNECTION_DESTROY);

    MPIDI_FUNC_ENTER(MPID_STATE_CONNECTION_DESTROY);

    MPIU_Free(conn->pg_id);
    MPIU_Free(conn);
    
    MPIDI_FUNC_EXIT(MPID_STATE_CONNECTION_DESTROY);
}


#ifdef USE_DBG_LOGGING
const char * MPIDI_CH3_VC_GetStateString( int state )
{
    const char *name = "unknown";
    static char asdigits[20];
    
    switch (state) {
    case MPIDI_CH3I_VC_STATE_UNCONNECTED: name = "CH3I_VC_STATE_UNCONNECTED"; break;
    case MPIDI_CH3I_VC_STATE_CONNECTING:  name = "CH3I_VC_STATE_CONNECTING"; break;
    case MPIDI_CH3I_VC_STATE_CONNECTED:   name = "CH3I_VC_STATE_CONNECTED"; break;
    case MPIDI_CH3I_VC_STATE_FAILED:      name = "CH3I_VC_STATE_FAILED"; break;
    default:
	MPIU_Snprintf( asdigits, sizeof(asdigits), "%d", state );
	asdigits[20-1] = 0;
	name = (const char *)asdigits;
    }
    return name;
}
#endif
