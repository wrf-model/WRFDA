       MODULE MPI_BASE
       IMPLICIT NONE
!      This module was created by the script buildiface
       INTERFACE
       SUBROUTINE MPI_TYPE_CREATE_DARRAY(v0,v1,v2,v3,v4,v5,v6,v7,v8,v9,ierror)
       INTEGER v0, v1, v2, v3(*), v4(*), v5(*), v6(*), v7, v8, v9
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_CREATE_DARRAY

       SUBROUTINE MPI_COMM_FREE_KEYVAL(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_COMM_FREE_KEYVAL

       SUBROUTINE MPI_TYPE_EXTENT(v0,v1,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_ADDRESS_KIND
       INTEGER v0
       INTEGER(KIND=MPI_ADDRESS_KIND) v1
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_EXTENT

       SUBROUTINE MPI_TYPE_GET_NAME(v0,v1,v2,ierror)
       INTEGER v0
       CHARACTER (LEN=*) v1
       INTEGER v2
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_GET_NAME

       SUBROUTINE MPI_GROUP_INTERSECTION(v0,v1,v2,ierror)
       INTEGER v0, v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_GROUP_INTERSECTION

       SUBROUTINE MPI_WIN_LOCK(v0,v1,v2,v3,ierror)
       INTEGER v0, v1, v2, v3
       INTEGER ierror
       END SUBROUTINE MPI_WIN_LOCK

       SUBROUTINE MPI_CARTDIM_GET(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_CARTDIM_GET

       SUBROUTINE MPI_WIN_GET_ERRHANDLER(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_WIN_GET_ERRHANDLER

       SUBROUTINE MPI_COMM_SPLIT(v0,v1,v2,v3,ierror)
       INTEGER v0, v1, v2, v3
       INTEGER ierror
       END SUBROUTINE MPI_COMM_SPLIT

       SUBROUTINE MPI_CANCEL(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_CANCEL

       SUBROUTINE MPI_WIN_POST(v0,v1,v2,ierror)
       INTEGER v0, v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_WIN_POST

       SUBROUTINE MPI_WIN_COMPLETE(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_WIN_COMPLETE

       SUBROUTINE MPI_TEST_CANCELLED(v0,v1,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_STATUS_SIZE
       INTEGER v0(MPI_STATUS_SIZE)
       LOGICAL v1
       INTEGER ierror
       END SUBROUTINE MPI_TEST_CANCELLED

       SUBROUTINE MPI_GROUP_SIZE(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_GROUP_SIZE

       SUBROUTINE MPI_ADD_ERROR_STRING(v0,v1,ierror)
       INTEGER v0
       CHARACTER (LEN=*) v1
       INTEGER ierror
       END SUBROUTINE MPI_ADD_ERROR_STRING

       SUBROUTINE MPI_PACK_SIZE(v0,v1,v2,v3,ierror)
       INTEGER v0, v1, v2, v3
       INTEGER ierror
       END SUBROUTINE MPI_PACK_SIZE

       SUBROUTINE MPI_GET_ELEMENTS(v0,v1,v2,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_STATUS_SIZE
       INTEGER v0(MPI_STATUS_SIZE), v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_GET_ELEMENTS

       SUBROUTINE MPI_ERRHANDLER_GET(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_ERRHANDLER_GET

       SUBROUTINE MPI_FILE_GET_ERRHANDLER(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_FILE_GET_ERRHANDLER

       SUBROUTINE MPI_TYPE_LB(v0,v1,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_ADDRESS_KIND
       INTEGER v0
       INTEGER(KIND=MPI_ADDRESS_KIND) v1
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_LB

       SUBROUTINE MPI_REQUEST_FREE(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_REQUEST_FREE

       SUBROUTINE MPI_GROUP_RANGE_INCL(v0,v1,v2,v3,ierror)
       INTEGER v0, v1, v2(3,*), v3
       INTEGER ierror
       END SUBROUTINE MPI_GROUP_RANGE_INCL

       SUBROUTINE MPI_TYPE_GET_TRUE_EXTENT(v0,v1,v2,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_ADDRESS_KIND
       INTEGER v0
       INTEGER(KIND=MPI_ADDRESS_KIND) v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_GET_TRUE_EXTENT

       SUBROUTINE MPI_BARRIER(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_BARRIER

       SUBROUTINE MPI_IS_THREAD_MAIN(v0,ierror)
       LOGICAL v0
       INTEGER ierror
       END SUBROUTINE MPI_IS_THREAD_MAIN

       SUBROUTINE MPI_WIN_FREE_KEYVAL(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_WIN_FREE_KEYVAL

       SUBROUTINE MPI_TYPE_COMMIT(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_COMMIT

       SUBROUTINE MPI_GROUP_RANGE_EXCL(v0,v1,v2,v3,ierror)
       INTEGER v0, v1, v2(3,*), v3
       INTEGER ierror
       END SUBROUTINE MPI_GROUP_RANGE_EXCL

       SUBROUTINE MPI_REQUEST_GET_STATUS(v0,v1,v2,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_STATUS_SIZE
       INTEGER v0
       LOGICAL v1
       INTEGER v2(MPI_STATUS_SIZE)
       INTEGER ierror
       END SUBROUTINE MPI_REQUEST_GET_STATUS

       SUBROUTINE MPI_QUERY_THREAD(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_QUERY_THREAD

       SUBROUTINE MPI_ERRHANDLER_CREATE(v0,v1,ierror)
       INTERFACE 
       SUBROUTINE v0(vv0,vv1)
       INTEGER vv0,vv1
       END SUBROUTINE
       END INTERFACE
       INTEGER v1
       INTEGER ierror
       END SUBROUTINE MPI_ERRHANDLER_CREATE

       SUBROUTINE MPI_COMM_SPAWN_MULTIPLE(v0,v1,v2,v3,v4,v5,v6,v7,v8,ierror)
       INTEGER v0
       CHARACTER (LEN=*) v1(*), v2(v0,*)
       INTEGER v3(*), v4(*), v5, v6, v7, v8(*)
       INTEGER ierror
       END SUBROUTINE MPI_COMM_SPAWN_MULTIPLE

       SUBROUTINE MPI_COMM_REMOTE_GROUP(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_COMM_REMOTE_GROUP

       SUBROUTINE MPI_TYPE_GET_EXTENT(v0,v1,v2,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_ADDRESS_KIND
       INTEGER v0
       INTEGER(KIND=MPI_ADDRESS_KIND) v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_GET_EXTENT

       SUBROUTINE MPI_COMM_COMPARE(v0,v1,v2,ierror)
       INTEGER v0, v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_COMM_COMPARE

       SUBROUTINE MPI_INFO_GET_VALUELEN(v0,v1,v2,v3,ierror)
       INTEGER v0
       CHARACTER (LEN=*) v1
       INTEGER v2
       LOGICAL v3
       INTEGER ierror
       END SUBROUTINE MPI_INFO_GET_VALUELEN

       SUBROUTINE MPI_INFO_GET(v0,v1,v2,v3,v4,ierror)
       INTEGER v0
       CHARACTER (LEN=*) v1
       INTEGER v2
       CHARACTER (LEN=*) v3
       LOGICAL v4
       INTEGER ierror
       END SUBROUTINE MPI_INFO_GET

       SUBROUTINE MPI_OP_CREATE(v0,v1,v2,ierror)
       EXTERNAL v0
       LOGICAL v1
       INTEGER v2
       INTEGER ierror
       END SUBROUTINE MPI_OP_CREATE

       SUBROUTINE MPI_TYPE_CREATE_STRUCT(v0,v1,v2,v3,v4,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_ADDRESS_KIND
       INTEGER v0, v1(*)
       INTEGER(KIND=MPI_ADDRESS_KIND) v2(*)
       INTEGER v3(*), v4
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_CREATE_STRUCT

       SUBROUTINE MPI_TYPE_VECTOR(v0,v1,v2,v3,v4,ierror)
       INTEGER v0, v1, v2, v3, v4
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_VECTOR

       SUBROUTINE MPI_WIN_GET_GROUP(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_WIN_GET_GROUP

       SUBROUTINE MPI_GROUP_COMPARE(v0,v1,v2,ierror)
       INTEGER v0, v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_GROUP_COMPARE

       SUBROUTINE MPI_CART_SHIFT(v0,v1,v2,v3,v4,ierror)
       INTEGER v0, v1, v2, v3, v4
       INTEGER ierror
       END SUBROUTINE MPI_CART_SHIFT

       SUBROUTINE MPI_WIN_SET_ERRHANDLER(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_WIN_SET_ERRHANDLER

       SUBROUTINE MPI_COMM_SPAWN(v0,v1,v2,v3,v4,v5,v6,v7,ierror)
       CHARACTER (LEN=*) v0, v1(*)
       INTEGER v2, v3, v4, v5, v6, v7(*)
       INTEGER ierror
       END SUBROUTINE MPI_COMM_SPAWN

       SUBROUTINE MPI_COMM_GROUP(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_COMM_GROUP

       SUBROUTINE MPI_WIN_CALL_ERRHANDLER(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_WIN_CALL_ERRHANDLER

       SUBROUTINE MPI_LOOKUP_NAME(v0,v1,v2,ierror)
       CHARACTER (LEN=*) v0
       INTEGER v1
       CHARACTER (LEN=*) v2
       INTEGER ierror
       END SUBROUTINE MPI_LOOKUP_NAME

       SUBROUTINE MPI_INFO_FREE(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_INFO_FREE

       SUBROUTINE MPI_COMM_SET_ERRHANDLER(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_COMM_SET_ERRHANDLER

       SUBROUTINE MPI_GRAPH_GET(v0,v1,v2,v3,v4,ierror)
       INTEGER v0, v1, v2, v3(*), v4(*)
       INTEGER ierror
       END SUBROUTINE MPI_GRAPH_GET

       SUBROUTINE MPI_GROUP_FREE(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_GROUP_FREE

       SUBROUTINE MPI_STATUS_SET_ELEMENTS(v0,v1,v2,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_STATUS_SIZE
       INTEGER v0(MPI_STATUS_SIZE), v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_STATUS_SET_ELEMENTS

       SUBROUTINE MPI_WIN_TEST(v0,v1,ierror)
       INTEGER v0
       LOGICAL v1
       INTEGER ierror
       END SUBROUTINE MPI_WIN_TEST

       SUBROUTINE MPI_WIN_FREE(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_WIN_FREE

       SUBROUTINE MPI_GRAPH_MAP(v0,v1,v2,v3,v4,ierror)
       INTEGER v0, v1, v2(*), v3(*), v4
       INTEGER ierror
       END SUBROUTINE MPI_GRAPH_MAP

       SUBROUTINE MPI_PACK_EXTERNAL_SIZE(v0,v1,v2,v3,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_ADDRESS_KIND
       CHARACTER (LEN=*) v0
       INTEGER v1, v2
       INTEGER(KIND=MPI_ADDRESS_KIND) v3
       INTEGER ierror
       END SUBROUTINE MPI_PACK_EXTERNAL_SIZE

       SUBROUTINE MPI_PUBLISH_NAME(v0,v1,v2,ierror)
       CHARACTER (LEN=*) v0
       INTEGER v1
       CHARACTER (LEN=*) v2
       INTEGER ierror
       END SUBROUTINE MPI_PUBLISH_NAME

       SUBROUTINE MPI_TYPE_CREATE_F90_REAL(v0,v1,v2,ierror)
       INTEGER v0, v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_CREATE_F90_REAL

       SUBROUTINE MPI_OPEN_PORT(v0,v1,ierror)
       INTEGER v0
       CHARACTER (LEN=*) v1
       INTEGER ierror
       END SUBROUTINE MPI_OPEN_PORT

       SUBROUTINE MPI_GROUP_UNION(v0,v1,v2,ierror)
       INTEGER v0, v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_GROUP_UNION

       SUBROUTINE MPI_COMM_ACCEPT(v0,v1,v2,v3,v4,ierror)
       CHARACTER (LEN=*) v0
       INTEGER v1, v2, v3, v4
       INTEGER ierror
       END SUBROUTINE MPI_COMM_ACCEPT

       SUBROUTINE MPI_FILE_CREATE_ERRHANDLER(v0,v1,ierror)
       INTERFACE 
       SUBROUTINE v0(vv0,vv1)
       INTEGER vv0,vv1
       END SUBROUTINE
       END INTERFACE
       INTEGER v1
       INTEGER ierror
       END SUBROUTINE MPI_FILE_CREATE_ERRHANDLER

       SUBROUTINE MPI_WIN_GET_NAME(v0,v1,v2,ierror)
       INTEGER v0
       CHARACTER (LEN=*) v1
       INTEGER v2
       INTEGER ierror
       END SUBROUTINE MPI_WIN_GET_NAME

       SUBROUTINE MPI_INFO_CREATE(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_INFO_CREATE

       SUBROUTINE MPI_TYPE_CREATE_F90_INTEGER(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_CREATE_F90_INTEGER

       SUBROUTINE MPI_TYPE_SET_NAME(v0,v1,ierror)
       INTEGER v0
       CHARACTER (LEN=*) v1
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_SET_NAME

       SUBROUTINE MPI_ATTR_DELETE(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_ATTR_DELETE

       SUBROUTINE MPI_GROUP_INCL(v0,v1,v2,v3,ierror)
       INTEGER v0, v1, v2(*), v3
       INTEGER ierror
       END SUBROUTINE MPI_GROUP_INCL

       SUBROUTINE MPI_COMM_CREATE_ERRHANDLER(v0,v1,ierror)
       INTERFACE 
       SUBROUTINE v0(vv0,vv1)
       INTEGER vv0,vv1
       END SUBROUTINE
       END INTERFACE
       INTEGER v1
       INTEGER ierror
       END SUBROUTINE MPI_COMM_CREATE_ERRHANDLER

       SUBROUTINE MPI_COMM_CONNECT(v0,v1,v2,v3,v4,ierror)
       CHARACTER (LEN=*) v0
       INTEGER v1, v2, v3, v4
       INTEGER ierror
       END SUBROUTINE MPI_COMM_CONNECT

       SUBROUTINE MPI_ERROR_STRING(v0,v1,v2,ierror)
       INTEGER v0
       CHARACTER (LEN=*) v1
       INTEGER v2
       INTEGER ierror
       END SUBROUTINE MPI_ERROR_STRING

       SUBROUTINE MPI_TYPE_GET_CONTENTS(v0,v1,v2,v3,v4,v5,v6,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_ADDRESS_KIND
       INTEGER v0, v1, v2, v3, v4(*)
       INTEGER(KIND=MPI_ADDRESS_KIND) v5(*)
       INTEGER v6(*)
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_GET_CONTENTS

       SUBROUTINE MPI_TYPE_STRUCT(v0,v1,v2,v3,v4,ierror)
       INTEGER v0, v1(*), v2(*), v3(*), v4
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_STRUCT

       SUBROUTINE MPI_TYPE_CREATE_INDEXED_BLOCK(v0,v1,v2,v3,v4,ierror)
       INTEGER v0, v1, v2(*), v3, v4
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_CREATE_INDEXED_BLOCK

       SUBROUTINE MPI_TYPE_CREATE_HVECTOR(v0,v1,v2,v3,v4,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_ADDRESS_KIND
       INTEGER v0, v1
       INTEGER(KIND=MPI_ADDRESS_KIND) v2
       INTEGER v3, v4
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_CREATE_HVECTOR

       SUBROUTINE MPI_TYPE_FREE_KEYVAL(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_FREE_KEYVAL

       SUBROUTINE MPI_START(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_START

       SUBROUTINE MPI_ABORT(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_ABORT

       SUBROUTINE MPI_INTERCOMM_CREATE(v0,v1,v2,v3,v4,v5,ierror)
       INTEGER v0, v1, v2, v3, v4, v5
       INTEGER ierror
       END SUBROUTINE MPI_INTERCOMM_CREATE

       SUBROUTINE MPI_COMM_RANK(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_COMM_RANK

       SUBROUTINE MPI_COMM_GET_PARENT(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_COMM_GET_PARENT

       SUBROUTINE MPI_FINALIZED(v0,ierror)
       LOGICAL v0
       INTEGER ierror
       END SUBROUTINE MPI_FINALIZED

       SUBROUTINE MPI_INTERCOMM_MERGE(v0,v1,v2,ierror)
       INTEGER v0
       LOGICAL v1
       INTEGER v2
       INTEGER ierror
       END SUBROUTINE MPI_INTERCOMM_MERGE

       SUBROUTINE MPI_INFO_GET_NTHKEY(v0,v1,v2,ierror)
       INTEGER v0, v1
       CHARACTER (LEN=*) v2
       INTEGER ierror
       END SUBROUTINE MPI_INFO_GET_NTHKEY

       SUBROUTINE MPI_TYPE_MATCH_SIZE(v0,v1,v2,ierror)
       INTEGER v0, v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_MATCH_SIZE

       SUBROUTINE MPI_STATUS_SET_CANCELLED(v0,v1,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_STATUS_SIZE
       INTEGER v0(MPI_STATUS_SIZE), v1
       INTEGER ierror
       END SUBROUTINE MPI_STATUS_SET_CANCELLED

       SUBROUTINE MPI_FILE_SET_ERRHANDLER(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_FILE_SET_ERRHANDLER

       SUBROUTINE MPI_INFO_DELETE(v0,v1,ierror)
       INTEGER v0
       CHARACTER (LEN=*) v1
       INTEGER ierror
       END SUBROUTINE MPI_INFO_DELETE

       SUBROUTINE MPI_UNPUBLISH_NAME(v0,v1,v2,ierror)
       CHARACTER (LEN=*) v0
       INTEGER v1
       CHARACTER (LEN=*) v2
       INTEGER ierror
       END SUBROUTINE MPI_UNPUBLISH_NAME

       SUBROUTINE MPI_TYPE_CONTIGUOUS(v0,v1,v2,ierror)
       INTEGER v0, v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_CONTIGUOUS

       SUBROUTINE MPI_INITIALIZED(v0,ierror)
       LOGICAL v0
       INTEGER ierror
       END SUBROUTINE MPI_INITIALIZED

       SUBROUTINE MPI_TYPE_CREATE_RESIZED(v0,v1,v2,v3,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_ADDRESS_KIND
       INTEGER v0
       INTEGER(KIND=MPI_ADDRESS_KIND) v1, v2
       INTEGER v3
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_CREATE_RESIZED

       SUBROUTINE MPI_TYPE_UB(v0,v1,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_ADDRESS_KIND
       INTEGER v0
       INTEGER(KIND=MPI_ADDRESS_KIND) v1
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_UB

       SUBROUTINE MPI_INFO_DUP(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_INFO_DUP

       SUBROUTINE MPI_TYPE_DUP(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_DUP

       SUBROUTINE MPI_ERRHANDLER_SET(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_ERRHANDLER_SET

       SUBROUTINE MPI_WIN_DELETE_ATTR(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_WIN_DELETE_ATTR

       SUBROUTINE MPI_INFO_GET_NKEYS(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_INFO_GET_NKEYS

       SUBROUTINE MPI_GROUP_EXCL(v0,v1,v2,v3,ierror)
       INTEGER v0, v1, v2(*), v3
       INTEGER ierror
       END SUBROUTINE MPI_GROUP_EXCL

       SUBROUTINE MPI_INFO_SET(v0,v1,v2,ierror)
       INTEGER v0
       CHARACTER (LEN=*) v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_INFO_SET

       SUBROUTINE MPI_WAIT(v0,v1,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_STATUS_SIZE
       INTEGER v0, v1(MPI_STATUS_SIZE)
       INTEGER ierror
       END SUBROUTINE MPI_WAIT

       SUBROUTINE MPI_COMM_DELETE_ATTR(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_COMM_DELETE_ATTR

       SUBROUTINE MPI_COMM_GET_NAME(v0,v1,v2,ierror)
       INTEGER v0
       CHARACTER (LEN=*) v1
       INTEGER v2
       INTEGER ierror
       END SUBROUTINE MPI_COMM_GET_NAME

       SUBROUTINE MPI_TEST(v0,v1,v2,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_STATUS_SIZE
       INTEGER v0
       LOGICAL v1
       INTEGER v2(MPI_STATUS_SIZE)
       INTEGER ierror
       END SUBROUTINE MPI_TEST

       SUBROUTINE MPI_GET_COUNT(v0,v1,v2,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_STATUS_SIZE
       INTEGER v0(MPI_STATUS_SIZE), v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_GET_COUNT

       SUBROUTINE MPI_ADD_ERROR_CLASS(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_ADD_ERROR_CLASS

       SUBROUTINE MPI_COMM_FREE(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_COMM_FREE

       SUBROUTINE MPI_COMM_SET_NAME(v0,v1,ierror)
       INTEGER v0
       CHARACTER (LEN=*) v1
       INTEGER ierror
       END SUBROUTINE MPI_COMM_SET_NAME

       SUBROUTINE MPI_COMM_DISCONNECT(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_COMM_DISCONNECT

       SUBROUTINE MPI_IPROBE(v0,v1,v2,v3,v4,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_STATUS_SIZE
       INTEGER v0, v1, v2
       LOGICAL v3
       INTEGER v4(MPI_STATUS_SIZE)
       INTEGER ierror
       END SUBROUTINE MPI_IPROBE

       SUBROUTINE MPI_ADD_ERROR_CODE(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_ADD_ERROR_CODE

       SUBROUTINE MPI_COMM_GET_ERRHANDLER(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_COMM_GET_ERRHANDLER

       SUBROUTINE MPI_COMM_CREATE(v0,v1,v2,ierror)
       INTEGER v0, v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_COMM_CREATE

       SUBROUTINE MPI_OP_FREE(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_OP_FREE

       SUBROUTINE MPI_TOPO_TEST(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_TOPO_TEST

       SUBROUTINE MPI_GET_PROCESSOR_NAME(v0,v1,ierror)
       CHARACTER (LEN=*) v0
       INTEGER v1
       INTEGER ierror
       END SUBROUTINE MPI_GET_PROCESSOR_NAME

       SUBROUTINE MPI_COMM_SIZE(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_COMM_SIZE

       SUBROUTINE MPI_WIN_UNLOCK(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_WIN_UNLOCK

       SUBROUTINE MPI_ERRHANDLER_FREE(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_ERRHANDLER_FREE

       SUBROUTINE MPI_COMM_REMOTE_SIZE(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_COMM_REMOTE_SIZE

       SUBROUTINE MPI_PROBE(v0,v1,v2,v3,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_STATUS_SIZE
       INTEGER v0, v1, v2, v3(MPI_STATUS_SIZE)
       INTEGER ierror
       END SUBROUTINE MPI_PROBE

       SUBROUTINE MPI_TYPE_HINDEXED(v0,v1,v2,v3,v4,ierror)
       INTEGER v0, v1(*), v2(*), v3, v4
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_HINDEXED

       SUBROUTINE MPI_WIN_WAIT(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_WIN_WAIT

       SUBROUTINE MPI_WIN_SET_NAME(v0,v1,ierror)
       INTEGER v0
       CHARACTER (LEN=*) v1
       INTEGER ierror
       END SUBROUTINE MPI_WIN_SET_NAME

       SUBROUTINE MPI_TYPE_SIZE(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_SIZE

       SUBROUTINE MPI_TYPE_CREATE_SUBARRAY(v0,v1,v2,v3,v4,v5,v6,ierror)
       INTEGER v0, v1(*), v2(*), v3(*), v4, v5, v6
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_CREATE_SUBARRAY

       SUBROUTINE MPI_WIN_CREATE_ERRHANDLER(v0,v1,ierror)
       INTERFACE 
       SUBROUTINE v0(vv0,vv1)
       INTEGER vv0,vv1
       END SUBROUTINE
       END INTERFACE
       INTEGER v1
       INTEGER ierror
       END SUBROUTINE MPI_WIN_CREATE_ERRHANDLER

       SUBROUTINE MPI_WIN_START(v0,v1,v2,ierror)
       INTEGER v0, v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_WIN_START

       SUBROUTINE MPI_TYPE_FREE(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_FREE

       SUBROUTINE MPI_WIN_FENCE(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_WIN_FENCE

       SUBROUTINE MPI_GRAPHDIMS_GET(v0,v1,v2,ierror)
       INTEGER v0, v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_GRAPHDIMS_GET

       SUBROUTINE MPI_FILE_CALL_ERRHANDLER(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_FILE_CALL_ERRHANDLER

       SUBROUTINE MPI_TYPE_GET_ENVELOPE(v0,v1,v2,v3,v4,ierror)
       INTEGER v0, v1, v2, v3, v4
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_GET_ENVELOPE

       SUBROUTINE MPI_TYPE_DELETE_ATTR(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_DELETE_ATTR

       SUBROUTINE MPI_TYPE_CREATE_HINDEXED(v0,v1,v2,v3,v4,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_ADDRESS_KIND
       INTEGER v0, v1(*)
       INTEGER(KIND=MPI_ADDRESS_KIND) v2(*)
       INTEGER v3, v4
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_CREATE_HINDEXED

       SUBROUTINE MPI_TYPE_INDEXED(v0,v1,v2,v3,v4,ierror)
       INTEGER v0, v1(*), v2(*), v3, v4
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_INDEXED

       SUBROUTINE MPI_GREQUEST_COMPLETE(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_GREQUEST_COMPLETE

       SUBROUTINE MPI_GRAPH_NEIGHBORS_COUNT(v0,v1,v2,ierror)
       INTEGER v0, v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_GRAPH_NEIGHBORS_COUNT

       SUBROUTINE MPI_GET_VERSION(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_GET_VERSION

       SUBROUTINE MPI_TYPE_HVECTOR(v0,v1,v2,v3,v4,ierror)
       USE MPI_CONSTANTS,ONLY:MPI_ADDRESS_KIND
       INTEGER v0, v1
       INTEGER(KIND=MPI_ADDRESS_KIND) v2
       INTEGER v3, v4
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_HVECTOR

       SUBROUTINE MPI_KEYVAL_FREE(v0,ierror)
       INTEGER v0
       INTEGER ierror
       END SUBROUTINE MPI_KEYVAL_FREE

       SUBROUTINE MPI_COMM_CALL_ERRHANDLER(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_COMM_CALL_ERRHANDLER

       SUBROUTINE MPI_COMM_JOIN(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_COMM_JOIN

       SUBROUTINE MPI_COMM_TEST_INTER(v0,v1,ierror)
       INTEGER v0
       LOGICAL v1
       INTEGER ierror
       END SUBROUTINE MPI_COMM_TEST_INTER

       SUBROUTINE MPI_CLOSE_PORT(v0,ierror)
       CHARACTER (LEN=*) v0
       INTEGER ierror
       END SUBROUTINE MPI_CLOSE_PORT

       SUBROUTINE MPI_TYPE_CREATE_F90_COMPLEX(v0,v1,v2,ierror)
       INTEGER v0, v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_TYPE_CREATE_F90_COMPLEX

       SUBROUTINE MPI_GROUP_DIFFERENCE(v0,v1,v2,ierror)
       INTEGER v0, v1, v2
       INTEGER ierror
       END SUBROUTINE MPI_GROUP_DIFFERENCE

       SUBROUTINE MPI_COMM_DUP(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_COMM_DUP

       SUBROUTINE MPI_ERROR_CLASS(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_ERROR_CLASS

       SUBROUTINE MPI_GROUP_RANK(v0,v1,ierror)
       INTEGER v0, v1
       INTEGER ierror
       END SUBROUTINE MPI_GROUP_RANK


        SUBROUTINE MPI_INIT(ierror)
        INTEGER ierror
        END SUBROUTINE MPI_INIT

        SUBROUTINE MPI_INIT_THREAD(v0,v1,ierror)
        INTEGER v0, v1, ierror
        END SUBROUTINE MPI_INIT_THREAD

        FUNCTION MPI_WTIME()
            DOUBLE PRECISION MPI_WTIME
        END FUNCTION MPI_WTIME
!
        FUNCTION MPI_WTICK()
            DOUBLE PRECISION MPI_WTICK
        END FUNCTION MPI_WTICK

        FUNCTION PMPI_WTIME()
            DOUBLE PRECISION PMPI_WTIME
        END FUNCTION PMPI_WTIME
!
        FUNCTION PMPI_WTICK()
            DOUBLE PRECISION PMPI_WTICK
        END FUNCTION PMPI_WTICK

        SUBROUTINE MPI_NULL_DELETE_FN(a,b,c,d,e)
          INTEGER a,b,c,d,e
        END SUBROUTINE MPI_NULL_DELETE_FN

        SUBROUTINE MPI_DUP_FN(a,b,c,d,e,f,g)
          INTEGER a,b,c,d,e,g
          LOGICAL f
        END SUBROUTINE MPI_DUP_FN

        SUBROUTINE MPI_NULL_COPY_FN(a,b,c,d,e,f,g)
          INTEGER a,b,c,d,e,g
          LOGICAL f
        END SUBROUTINE MPI_NULL_COPY_FN

        SUBROUTINE MPI_COMM_NULL_DELETE_FN(a,b,c,d,e)
          USE MPI_CONSTANTS,ONLY: MPI_ADDRESS_KIND
          INTEGER a,b,e
          INTEGER (KIND=MPI_ADDRESS_KIND) c, d
        END SUBROUTINE MPI_COMM_NULL_DELETE_FN

        SUBROUTINE MPI_COMM_DUP_FN(a,b,c,d,e,f,g)
          USE MPI_CONSTANTS,ONLY: MPI_ADDRESS_KIND
          INTEGER a,b,g
          INTEGER (KIND=MPI_ADDRESS_KIND) c,d,e
          LOGICAL f
        END SUBROUTINE MPI_COMM_DUP_FN

        SUBROUTINE MPI_COMM_NULL_COPY_FN(a,b,c,d,e,f,g)
          USE MPI_CONSTANTS,ONLY: MPI_ADDRESS_KIND
          INTEGER a,b,g
          INTEGER (KIND=MPI_ADDRESS_KIND) c,d,e
          LOGICAL f
        END SUBROUTINE MPI_COMM_NULL_COPY_FN

        SUBROUTINE MPI_TYPE_NULL_DELETE_FN(a,b,c,d,e)
          USE MPI_CONSTANTS,ONLY: MPI_ADDRESS_KIND
          INTEGER a,b,e
          INTEGER (KIND=MPI_ADDRESS_KIND) c, d
        END SUBROUTINE MPI_TYPE_NULL_DELETE_FN

        SUBROUTINE MPI_TYPE_DUP_FN(a,b,c,d,e,f,g)
          USE MPI_CONSTANTS,ONLY: MPI_ADDRESS_KIND
          INTEGER a,b,g
          INTEGER (KIND=MPI_ADDRESS_KIND) c,d,e
          LOGICAL f
        END SUBROUTINE MPI_TYPE_DUP_FN

        SUBROUTINE MPI_TYPE_NULL_COPY_FN(a,b,c,d,e,f,g)
          USE MPI_CONSTANTS,ONLY: MPI_ADDRESS_KIND
          INTEGER a,b,g
          INTEGER (KIND=MPI_ADDRESS_KIND) c,d,e
          LOGICAL f
        END SUBROUTINE MPI_TYPE_NULL_COPY_FN

        SUBROUTINE MPI_WIN_NULL_DELETE_FN(a,b,c,d,e)
          USE MPI_CONSTANTS,ONLY: MPI_ADDRESS_KIND
          INTEGER a,b,e
          INTEGER (KIND=MPI_ADDRESS_KIND) c, d
        END SUBROUTINE MPI_WIN_NULL_DELETE_FN

        SUBROUTINE MPI_WIN_DUP_FN(a,b,c,d,e,f,g)
          USE MPI_CONSTANTS,ONLY: MPI_ADDRESS_KIND
          INTEGER a,b,g
          INTEGER (KIND=MPI_ADDRESS_KIND) c,d,e
          LOGICAL f
        END SUBROUTINE MPI_WIN_DUP_FN

        SUBROUTINE MPI_WIN_NULL_COPY_FN(a,b,c,d,e,f,g)
          USE MPI_CONSTANTS,ONLY: MPI_ADDRESS_KIND
          INTEGER a,b,g
          INTEGER (KIND=MPI_ADDRESS_KIND) c,d,e
          LOGICAL f
        END SUBROUTINE MPI_WIN_NULL_COPY_FN

       END INTERFACE
       END MODULE MPI_BASE
