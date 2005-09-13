SUBROUTINE wrf_restartin ( fid , grid , config_flags , switch , ierr )
    USE module_domain
    USE module_state_description
    USE module_configure
    USE module_io
    USE module_io_wrf
    USE module_date_time
    USE module_bc_time_utilities
    USE WRF_ESMF_MOD
    IMPLICIT NONE

      integer, parameter  :: WRF_FILE_NOT_OPENED                  = 100
      integer, parameter  :: WRF_FILE_OPENED_NOT_COMMITTED        = 101
      integer, parameter  :: WRF_FILE_OPENED_FOR_WRITE            = 102
      integer, parameter  :: WRF_FILE_OPENED_FOR_READ             = 103
      integer, parameter  :: WRF_REAL                             = 104
      integer, parameter  :: WRF_DOUBLE               = 105
      integer, parameter  :: WRF_INTEGER                          = 106
      integer, parameter  :: WRF_LOGICAL                          = 107
      integer, parameter  :: WRF_COMPLEX                          = 108
      integer, parameter  :: WRF_DOUBLE_COMPLEX                   = 109
      integer, parameter  :: WRF_FILE_OPENED_FOR_UPDATE           = 110

  
!WRF Error and Warning messages (1-999)
!All i/o package-specific status codes you may want to add must be handled by your package (see below)
! WRF handles these and netCDF messages only
  integer, parameter  :: WRF_NO_ERR                  =  0       !no error
  integer, parameter  :: WRF_WARN_FILE_NF            = -1       !file not found, or incomplete
  integer, parameter  :: WRF_WARN_MD_NF              = -2       !metadata not found
  integer, parameter  :: WRF_WARN_TIME_NF            = -3       !timestamp not found
  integer, parameter  :: WRF_WARN_TIME_EOF           = -4       !no more timestamps
  integer, parameter  :: WRF_WARN_VAR_NF             = -5       !variable not found
  integer, parameter  :: WRF_WARN_VAR_EOF            = -6       !no more variables for the current time
  integer, parameter  :: WRF_WARN_TOO_MANY_FILES     = -7       !too many open files
  integer, parameter  :: WRF_WARN_TYPE_MISMATCH      = -8       !data type mismatch
  integer, parameter  :: WRF_WARN_WRITE_RONLY_FILE   = -9       !attempt to write readonly file
  integer, parameter  :: WRF_WARN_READ_WONLY_FILE    = -10      !attempt to read writeonly file
  integer, parameter  :: WRF_WARN_FILE_NOT_OPENED    = -11      !attempt to access unopened file
  integer, parameter  :: WRF_WARN_2DRYRUNS_1VARIABLE = -12      !attempt to do 2 trainings for 1 variable
  integer, parameter  :: WRF_WARN_READ_PAST_EOF      = -13      !attempt to read past EOF
  integer, parameter  :: WRF_WARN_BAD_DATA_HANDLE    = -14      !bad data handle
  integer, parameter  :: WRF_WARN_WRTLEN_NE_DRRUNLEN = -15      !write length not equal to training length
  integer, parameter  :: WRF_WARN_TOO_MANY_DIMS      = -16      !more dimensions requested than training
  integer, parameter  :: WRF_WARN_COUNT_TOO_LONG     = -17      !attempt to read more data than exists
  integer, parameter  :: WRF_WARN_DIMENSION_ERROR    = -18      !input dimension inconsistent
  integer, parameter  :: WRF_WARN_BAD_MEMORYORDER    = -19      !input MemoryOrder not recognized
  integer, parameter  :: WRF_WARN_DIMNAME_REDEFINED  = -20      !a dimension name with 2 different lengths
  integer, parameter  :: WRF_WARN_CHARSTR_GT_LENDATA = -21      !string longer than provided storage
  integer, parameter  :: WRF_WARN_NOTSUPPORTED       = -22      !function not supportable
  integer, parameter  :: WRF_WARN_NOOP               = -23      !package implements this routine as NOOP

!Fatal errors 
  integer, parameter  :: WRF_ERR_FATAL_ALLOCATION_ERROR  = -100 !allocation error
  integer, parameter  :: WRF_ERR_FATAL_DEALLOCATION_ERR  = -101 !dealloc error
  integer, parameter  :: WRF_ERR_FATAL_BAD_FILE_STATUS   = -102 !bad file status


!Package specific errors (1000+)        
!Netcdf status codes
!WRF will accept status codes of 1000+, but it is up to the package to handle
! and return the status to the user.

  integer, parameter  :: WRF_ERR_FATAL_BAD_VARIABLE_DIM  = -1004
  integer, parameter  :: WRF_ERR_FATAL_MDVAR_DIM_NOT_1D  = -1005
  integer, parameter  :: WRF_ERR_FATAL_TOO_MANY_TIMES    = -1006
  integer, parameter  :: WRF_WARN_BAD_DATA_TYPE      = -1007    !this code not in either spec?
  integer, parameter  :: WRF_WARN_FILE_NOT_COMMITTED = -1008    !this code not in either spec?
  integer, parameter  :: WRF_WARN_FILE_OPEN_FOR_READ = -1009
  integer, parameter  :: WRF_IO_NOT_INITIALIZED      = -1010
  integer, parameter  :: WRF_WARN_MD_AFTER_OPEN      = -1011
  integer, parameter  :: WRF_WARN_TOO_MANY_VARIABLES = -1012
  integer, parameter  :: WRF_WARN_DRYRUN_CLOSE       = -1013
  integer, parameter  :: WRF_WARN_DATESTR_BAD_LENGTH = -1014
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_READ   = -1015
  integer, parameter  :: WRF_WARN_DATA_TYPE_NOT_FOUND = -1016
  integer, parameter  :: WRF_WARN_DATESTR_ERROR      = -1017
  integer, parameter  :: WRF_WARN_DRYRUN_READ        = -1018
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_GET    = -1019
  integer, parameter  :: WRF_WARN_ZERO_LENGTH_PUT    = -1020
  integer, parameter  :: WRF_WARN_NETCDF             = -1021    
  integer, parameter  :: WRF_WARN_LENGTH_LESS_THAN_1 = -1022    
  integer, parameter  :: WRF_WARN_MORE_DATA_IN_FILE  = -1023    
  integer, parameter  :: WRF_WARN_DATE_LT_LAST_DATE  = -1024

! For HDF5 only
  integer, parameter  :: WRF_HDF5_ERR_FILE                 = -200
  integer, parameter  :: WRF_HDF5_ERR_MD                   = -201
  integer, parameter  :: WRF_HDF5_ERR_TIME                 = -202
  integer, parameter  :: WRF_HDF5_ERR_TIME_EOF             = -203
  integer, parameter  :: WRF_HDF5_ERR_MORE_DATA_IN_FILE    = -204
  integer, parameter  :: WRF_HDF5_ERR_DATE_LT_LAST_DATE    = -205
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_FILES       = -206
  integer, parameter  :: WRF_HDF5_ERR_TYPE_MISMATCH        = -207
  integer, parameter  :: WRF_HDF5_ERR_LENGTH_LESS_THAN_1   = -208
  integer, parameter  :: WRF_HDF5_ERR_WRITE_RONLY_FILE     = -209
  integer, parameter  :: WRF_HDF5_ERR_READ_WONLY_FILE      = -210
  integer, parameter  :: WRF_HDF5_ERR_FILE_NOT_OPENED      = -211
  integer, parameter  :: WRF_HDF5_ERR_DATESTR_ERROR        = -212
  integer, parameter  :: WRF_HDF5_ERR_DRYRUN_READ          = -213
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_GET      = -214
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_PUT      = -215
  integer, parameter  :: WRF_HDF5_ERR_2DRYRUNS_1VARIABLE   = -216
  integer, parameter  :: WRF_HDF5_ERR_DATA_TYPE_NOTFOUND   = -217
  integer, parameter  :: WRF_HDF5_ERR_READ_PAST_EOF        = -218
  integer, parameter  :: WRF_HDF5_ERR_BAD_DATA_HANDLE      = -219
  integer, parameter  :: WRF_HDF5_ERR_WRTLEN_NE_DRRUNLEN   = -220
  integer, parameter  :: WRF_HDF5_ERR_DRYRUN_CLOSE         = -221
  integer, parameter  :: WRF_HDF5_ERR_DATESTR_BAD_LENGTH   = -222
  integer, parameter  :: WRF_HDF5_ERR_ZERO_LENGTH_READ     = -223
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_DIMS        = -224
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_VARIABLES   = -225
  integer, parameter  :: WRF_HDF5_ERR_COUNT_TOO_LONG       = -226
  integer, parameter  :: WRF_HDF5_ERR_DIMENSION_ERROR      = -227
  integer, parameter  :: WRF_HDF5_ERR_BAD_MEMORYORDER      = -228
  integer, parameter  :: WRF_HDF5_ERR_DIMNAME_REDEFINED    = -229
  integer, parameter  :: WRF_HDF5_ERR_MD_AFTER_OPEN        = -230
  integer, parameter  :: WRF_HDF5_ERR_CHARSTR_GT_LENDATA   = -231
  integer, parameter  :: WRF_HDF5_ERR_BAD_DATA_TYPE        = -232
  integer, parameter  :: WRF_HDF5_ERR_FILE_NOT_COMMITTED   = -233

  integer, parameter  :: WRF_HDF5_ERR_ALLOCATION        = -2001
  integer, parameter  :: WRF_HDF5_ERR_DEALLOCATION      = -2002
  integer, parameter  :: WRF_HDF5_ERR_BAD_FILE_STATUS   = -2003
  integer, parameter  :: WRF_HDF5_ERR_BAD_VARIABLE_DIM  = -2004
  integer, parameter  :: WRF_HDF5_ERR_MDVAR_DIM_NOT_1D  = -2005
  integer, parameter  :: WRF_HDF5_ERR_TOO_MANY_TIMES    = -2006
  integer, parameter ::  WRF_HDF5_ERR_DATA_ID_NOTFOUND  = -2007

  integer, parameter ::  WRF_HDF5_ERR_DATASPACE         = -300
  integer, parameter ::  WRF_HDF5_ERR_DATATYPE          = -301
  integer, parameter :: WRF_HDF5_ERR_PROPERTY_LIST      = -302

  integer, parameter :: WRF_HDF5_ERR_DATASET_CREATE     = -303
  integer, parameter :: WRF_HDF5_ERR_DATASET_READ       = -304
  integer, parameter :: WRF_HDF5_ERR_DATASET_WRITE      = -305
  integer, parameter :: WRF_HDF5_ERR_DATASET_OPEN       = -306
  integer, parameter :: WRF_HDF5_ERR_DATASET_GENERAL    = -307
  integer, parameter :: WRF_HDF5_ERR_GROUP              = -308

  integer, parameter :: WRF_HDF5_ERR_FILE_OPEN          = -309
  integer, parameter :: WRF_HDF5_ERR_FILE_CREATE        = -310
  integer, parameter :: WRF_HDF5_ERR_DATASET_CLOSE      = -311
  integer, parameter :: WRF_HDF5_ERR_FILE_CLOSE         = -312
  integer, parameter :: WRF_HDF5_ERR_CLOSE_GENERAL      = -313

  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_CREATE   = -314
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_READ     = -315
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_WRITE    = -316
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_OPEN     = -317
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_GENERAL  = -318
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_CLOSE    = -319

  integer, parameter :: WRF_HDF5_ERR_OTHERS             = -320
  integer, parameter :: WRF_HDF5_ERR_ATTRIBUTE_OTHERS   = -321

    TYPE(domain) :: grid
    TYPE(grid_config_rec_type),  INTENT(INOUT)    :: config_flags
    INTEGER, INTENT(IN) :: fid
    INTEGER, INTENT(IN) :: switch
    INTEGER, INTENT(INOUT) :: ierr

    ! Local data
    INTEGER ids , ide , jds , jde , kds , kde , &
            ims , ime , jms , jme , kms , kme , &
            ips , ipe , jps , jpe , kps , kpe

    INTEGER       iname(9)
    INTEGER       iordering(3)
    INTEGER       icurrent_date(24)
    INTEGER       i,j,k
    INTEGER       icnt
    INTEGER       ndim
    INTEGER       ilen
    INTEGER , DIMENSION(3) :: domain_start , domain_end
    INTEGER , DIMENSION(3) :: memory_start , memory_end
    INTEGER , DIMENSION(3) :: patch_start , patch_end
    CHARACTER*256 errmess
    CHARACTER*40            :: this_datestr, next_datestr
    CHARACTER*9   NAMESTR
    INTEGER       IBDY, NAMELEN
    LOGICAL wrf_dm_on_monitor
    EXTERNAL wrf_dm_on_monitor
    Type(ESMF_Time)    time, oldtime, newtime
    Type(ESMF_TimeInterval)    timetonext
    CHARACTER*19  new_date
    CHARACTER*24  base_date
    INTEGER idt
    INTEGER itmp, dyn_opt
    INTEGER :: ide_compare , jde_compare , kde_compare
    ierr = 0

    CALL get_ijk_from_grid (  grid ,                        &
                              ids, ide, jds, jde, kds, kde,    &
                              ims, ime, jms, jme, kms, kme,    &
                              ips, ipe, jps, jpe, kps, kpe    )

    CALL nl_get_dyn_opt ( 1 , dyn_opt )


!STARTOFREGISTRYGENERATEDINCLUDE inc/wrf_restartin.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LU_INDEX'               , &  ! Data Name 
                       grid%lu_index               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LU_INDEX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_1'               , &  ! Data Name 
                       grid%em_u_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field U_1 memorder XZY' , & ! Debug message
ids , ide , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( ide, ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_2'               , &  ! Data Name 
                       grid%em_u_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field U_2 memorder XZY' , & ! Debug message
ids , ide , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( ide, ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_U_1'             , &  ! Data Name 
                       grid%em_g_u_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field U_1 memorder XZY' , & ! Debug message
ids , ide , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( ide, ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_U_2'             , &  ! Data Name 
                       grid%em_g_u_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field U_2 memorder XZY' , & ! Debug message
ids , ide , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( ide, ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_U_1'             , &  ! Data Name 
                       grid%em_a_u_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field U_1 memorder XZY' , & ! Debug message
ids , ide , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( ide, ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_U_2'             , &  ! Data Name 
                       grid%em_a_u_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field U_2 memorder XZY' , & ! Debug message
ids , ide , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( ide, ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_1'               , &  ! Data Name 
                       grid%em_v_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field V_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , jde ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( jde, jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_2'               , &  ! Data Name 
                       grid%em_v_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field V_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , jde ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( jde, jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_V_1'             , &  ! Data Name 
                       grid%em_g_v_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field V_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , jde ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( jde, jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_V_2'             , &  ! Data Name 
                       grid%em_g_v_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field V_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , jde ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( jde, jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_V_1'             , &  ! Data Name 
                       grid%em_a_v_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field V_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , jde ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( jde, jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_V_2'             , &  ! Data Name 
                       grid%em_a_v_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field V_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , jde ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( jde, jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_1'               , &  ! Data Name 
                       grid%em_w_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field W_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W_2'               , &  ! Data Name 
                       grid%em_w_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field W_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_W_1'             , &  ! Data Name 
                       grid%em_g_w_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field W_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_W_2'             , &  ! Data Name 
                       grid%em_g_w_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field W_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_W_1'             , &  ! Data Name 
                       grid%em_a_w_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field W_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_W_2'             , &  ! Data Name 
                       grid%em_a_w_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field W_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'WW'               , &  ! Data Name 
                       grid%em_ww               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field WW memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_WW'             , &  ! Data Name 
                       grid%em_g_ww               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field WW memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_WW'             , &  ! Data Name 
                       grid%em_a_ww               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field WW memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_1'               , &  ! Data Name 
                       grid%em_ph_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field PH_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_2'               , &  ! Data Name 
                       grid%em_ph_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field PH_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_PH_1'             , &  ! Data Name 
                       grid%em_g_ph_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field PH_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_PH_2'             , &  ! Data Name 
                       grid%em_g_ph_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field PH_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_PH_1'             , &  ! Data Name 
                       grid%em_a_ph_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field PH_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_PH_2'             , &  ! Data Name 
                       grid%em_a_ph_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field PH_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PHB'               , &  ! Data Name 
                       grid%em_phb               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field PHB memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_PHB'             , &  ! Data Name 
                       grid%em_g_phb               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field PHB memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_PHB'             , &  ! Data Name 
                       grid%em_a_phb               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field PHB memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH0'               , &  ! Data Name 
                       grid%em_ph0               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field PH0 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PHP'               , &  ! Data Name 
                       grid%em_php               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field PHP memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_PHP'             , &  ! Data Name 
                       grid%em_g_php               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field PHP memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_PHP'             , &  ! Data Name 
                       grid%em_a_php               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field PHP memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_1'               , &  ! Data Name 
                       grid%em_t_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field T_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_2'               , &  ! Data Name 
                       grid%em_t_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field T_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_T_1'             , &  ! Data Name 
                       grid%em_g_t_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field T_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_T_2'             , &  ! Data Name 
                       grid%em_g_t_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field T_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_T_1'             , &  ! Data Name 
                       grid%em_a_t_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field T_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_T_2'             , &  ! Data Name 
                       grid%em_a_t_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field T_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_INIT'               , &  ! Data Name 
                       grid%em_t_init               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field T_INIT memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_1'               , &  ! Data Name 
                       grid%em_mu_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MU_1 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_2'               , &  ! Data Name 
                       grid%em_mu_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MU_2 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_MU_1'             , &  ! Data Name 
                       grid%em_g_mu_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MU_1 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_MU_2'             , &  ! Data Name 
                       grid%em_g_mu_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MU_2 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_MU_1'             , &  ! Data Name 
                       grid%em_a_mu_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MU_1 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_MU_2'             , &  ! Data Name 
                       grid%em_a_mu_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MU_2 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MUB'               , &  ! Data Name 
                       grid%em_mub               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MUB memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_MUB'             , &  ! Data Name 
                       grid%em_g_mub               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MUB memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_MUB'             , &  ! Data Name 
                       grid%em_a_mub               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MUB memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU0'               , &  ! Data Name 
                       grid%em_mu0               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MU0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TKE_1'               , &  ! Data Name 
                       grid%em_tke_1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field TKE_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TKE_2'               , &  ! Data Name 
                       grid%em_tke_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field TKE_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'P'               , &  ! Data Name 
                       grid%em_p               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field P memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_P'             , &  ! Data Name 
                       grid%em_g_p               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field P memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_P'             , &  ! Data Name 
                       grid%em_a_p               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field P memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AL'               , &  ! Data Name 
                       grid%em_al               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field AL memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_AL'             , &  ! Data Name 
                       grid%em_g_al               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field AL memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_AL'             , &  ! Data Name 
                       grid%em_a_al               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field AL memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALT'               , &  ! Data Name 
                       grid%em_alt               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field ALT memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_ALT'             , &  ! Data Name 
                       grid%em_g_alt               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field ALT memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_ALT'             , &  ! Data Name 
                       grid%em_a_alt               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field ALT memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALB'               , &  ! Data Name 
                       grid%em_alb               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field ALB memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PB'               , &  ! Data Name 
                       grid%em_pb               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field PB memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'FNM'               , &  ! Data Name 
                       grid%em_fnm               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field FNM memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'FNP'               , &  ! Data Name 
                       grid%em_fnp               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field FNP memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RDNW'               , &  ! Data Name 
                       grid%em_rdnw               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RDNW memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RDN'               , &  ! Data Name 
                       grid%em_rdn               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RDN memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DNW'               , &  ! Data Name 
                       grid%em_dnw               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field DNW memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DN '               , &  ! Data Name 
                       grid%em_dn               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field DN  memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ZNU'               , &  ! Data Name 
                       grid%em_znu               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field ZNU memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ZNW'               , &  ! Data Name 
                       grid%em_znw               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field ZNW memorder Z' , & ! Debug message
kds , kde , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( kde, kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BASE'               , &  ! Data Name 
                       grid%em_t_base               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field T_BASE memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CFN'               , &  ! Data Name 
                       grid%cfn               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field CFN memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CFN1'               , &  ! Data Name 
                       grid%cfn1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field CFN1 memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'EPSTS'               , &  ! Data Name 
                       grid%epsts               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field EPSTS memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'STEP_NUMBER'               , &  ! Data Name 
                       grid%step_number               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field STEP_NUMBER memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Q2'               , &  ! Data Name 
                       grid%q2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field Q2 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T2'               , &  ! Data Name 
                       grid%t2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field T2 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TH2'               , &  ! Data Name 
                       grid%th2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field TH2 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PSFC'               , &  ! Data Name 
                       grid%psfc               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field PSFC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U10'               , &  ! Data Name 
                       grid%u10               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field U10 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V10'               , &  ! Data Name 
                       grid%v10               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field V10 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RDX'               , &  ! Data Name 
                       grid%rdx               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RDX memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RDY'               , &  ! Data Name 
                       grid%rdy               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RDY memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DTS'               , &  ! Data Name 
                       grid%dts               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field DTS memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DTSEPS'               , &  ! Data Name 
                       grid%dtseps               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field DTSEPS memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RESM'               , &  ! Data Name 
                       grid%resm               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RESM memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ZETATOP'               , &  ! Data Name 
                       grid%zetatop               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field ZETATOP memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CF1'               , &  ! Data Name 
                       grid%cf1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field CF1 memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CF2'               , &  ! Data Name 
                       grid%cf2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field CF2 memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CF3'               , &  ! Data Name 
                       grid%cf3               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field CF3 memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ITIMESTEP'               , &  ! Data Name 
                       grid%itimestep               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field ITIMESTEP memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
IF ( P_qv .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QVAPOR_1'               , &  ! Data Name 
                       grid%moist_1(ims,kms,jms,P_qv)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QVAPOR_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( P_qv .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QVAPOR_2'               , &  ! Data Name 
                       grid%moist_2(ims,kms,jms,P_qv)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QVAPOR_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qv .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QVAPOR_1'             , &  ! Data Name 
                       grid%g_moist_1(ims,kms,jms,P_qv)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QVAPOR_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qv .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QVAPOR_2'             , &  ! Data Name 
                       grid%g_moist_2(ims,kms,jms,P_qv)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QVAPOR_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qv .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QVAPOR_1'             , &  ! Data Name 
                       grid%a_moist_1(ims,kms,jms,P_qv)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QVAPOR_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qv .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QVAPOR_2'             , &  ! Data Name 
                       grid%a_moist_2(ims,kms,jms,P_qv)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QVAPOR_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( P_qc .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QCLOUD_1'               , &  ! Data Name 
                       grid%moist_1(ims,kms,jms,P_qc)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QCLOUD_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( P_qc .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QCLOUD_2'               , &  ! Data Name 
                       grid%moist_2(ims,kms,jms,P_qc)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QCLOUD_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qc .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QCLOUD_1'             , &  ! Data Name 
                       grid%g_moist_1(ims,kms,jms,P_qc)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QCLOUD_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qc .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QCLOUD_2'             , &  ! Data Name 
                       grid%g_moist_2(ims,kms,jms,P_qc)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QCLOUD_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qc .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QCLOUD_1'             , &  ! Data Name 
                       grid%a_moist_1(ims,kms,jms,P_qc)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QCLOUD_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qc .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QCLOUD_2'             , &  ! Data Name 
                       grid%a_moist_2(ims,kms,jms,P_qc)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QCLOUD_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( P_qr .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QRAIN_1'               , &  ! Data Name 
                       grid%moist_1(ims,kms,jms,P_qr)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QRAIN_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( P_qr .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QRAIN_2'               , &  ! Data Name 
                       grid%moist_2(ims,kms,jms,P_qr)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QRAIN_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qr .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QRAIN_1'             , &  ! Data Name 
                       grid%g_moist_1(ims,kms,jms,P_qr)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QRAIN_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qr .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QRAIN_2'             , &  ! Data Name 
                       grid%g_moist_2(ims,kms,jms,P_qr)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QRAIN_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qr .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QRAIN_1'             , &  ! Data Name 
                       grid%a_moist_1(ims,kms,jms,P_qr)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QRAIN_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qr .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QRAIN_2'             , &  ! Data Name 
                       grid%a_moist_2(ims,kms,jms,P_qr)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QRAIN_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( P_qi .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QICE_1'               , &  ! Data Name 
                       grid%moist_1(ims,kms,jms,P_qi)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QICE_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( P_qi .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QICE_2'               , &  ! Data Name 
                       grid%moist_2(ims,kms,jms,P_qi)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QICE_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qi .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QICE_1'             , &  ! Data Name 
                       grid%g_moist_1(ims,kms,jms,P_qi)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QICE_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qi .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QICE_2'             , &  ! Data Name 
                       grid%g_moist_2(ims,kms,jms,P_qi)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QICE_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qi .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QICE_1'             , &  ! Data Name 
                       grid%a_moist_1(ims,kms,jms,P_qi)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QICE_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qi .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QICE_2'             , &  ! Data Name 
                       grid%a_moist_2(ims,kms,jms,P_qi)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QICE_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( P_qs .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QSNOW_1'               , &  ! Data Name 
                       grid%moist_1(ims,kms,jms,P_qs)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QSNOW_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( P_qs .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QSNOW_2'               , &  ! Data Name 
                       grid%moist_2(ims,kms,jms,P_qs)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QSNOW_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qs .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QSNOW_1'             , &  ! Data Name 
                       grid%g_moist_1(ims,kms,jms,P_qs)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QSNOW_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qs .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QSNOW_2'             , &  ! Data Name 
                       grid%g_moist_2(ims,kms,jms,P_qs)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QSNOW_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qs .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QSNOW_1'             , &  ! Data Name 
                       grid%a_moist_1(ims,kms,jms,P_qs)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QSNOW_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qs .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QSNOW_2'             , &  ! Data Name 
                       grid%a_moist_2(ims,kms,jms,P_qs)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QSNOW_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( P_qg .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QGRAUP_1'               , &  ! Data Name 
                       grid%moist_1(ims,kms,jms,P_qg)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QGRAUP_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( P_qg .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QGRAUP_2'               , &  ! Data Name 
                       grid%moist_2(ims,kms,jms,P_qg)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QGRAUP_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qg .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QGRAUP_1'             , &  ! Data Name 
                       grid%g_moist_1(ims,kms,jms,P_qg)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QGRAUP_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qg .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QGRAUP_2'             , &  ! Data Name 
                       grid%g_moist_2(ims,kms,jms,P_qg)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QGRAUP_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qg .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QGRAUP_1'             , &  ! Data Name 
                       grid%a_moist_1(ims,kms,jms,P_qg)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QGRAUP_1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qg .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QGRAUP_2'             , &  ! Data Name 
                       grid%a_moist_2(ims,kms,jms,P_qg)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QGRAUP_2 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'FCX'               , &  ! Data Name 
                       grid%fcx               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'C'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field FCX memorder C' , & ! Debug message
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_FCX'             , &  ! Data Name 
                       grid%g_fcx               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'C'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field FCX memorder C' , & ! Debug message
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_FCX'             , &  ! Data Name 
                       grid%a_fcx               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'C'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field FCX memorder C' , & ! Debug message
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GCX'               , &  ! Data Name 
                       grid%gcx               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'C'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field GCX memorder C' , & ! Debug message
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_GCX'             , &  ! Data Name 
                       grid%g_gcx               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'C'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field GCX memorder C' , & ! Debug message
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_GCX'             , &  ! Data Name 
                       grid%a_gcx               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'C'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field GCX memorder C' , & ! Debug message
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%spec_bdy_width , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DTBC'               , &  ! Data Name 
                       grid%dtbc               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field DTBC memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_DTBC'             , &  ! Data Name 
                       grid%g_dtbc               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field DTBC memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_DTBC'             , &  ! Data Name 
                       grid%a_dtbc               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field DTBC memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LANDMASK'               , &  ! Data Name 
                       grid%landmask               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LANDMASK memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SHDMAX'               , &  ! Data Name 
                       grid%shdmax               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SHDMAX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SHDMIN'               , &  ! Data Name 
                       grid%shdmin               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SHDMIN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNOALB'               , &  ! Data Name 
                       grid%snoalb               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SNOALB memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TSLB'               , &  ! Data Name 
                       grid%tslb               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field TSLB memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ZS'               , &  ! Data Name 
                       grid%zs               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field ZS memorder Z' , & ! Debug message
1 , config_flags%num_soil_layers , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%num_soil_layers , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%num_soil_layers , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'DZS'               , &  ! Data Name 
                       grid%dzs               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field DZS memorder Z' , & ! Debug message
1 , config_flags%num_soil_layers , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%num_soil_layers , 1 , 1 , 1 , 1 ,  & 
1 , config_flags%num_soil_layers , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SMOIS'               , &  ! Data Name 
                       grid%smois               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SMOIS memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SH2O'               , &  ! Data Name 
                       grid%sh2o               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SH2O memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XICE'               , &  ! Data Name 
                       grid%xice               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field XICE memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SMSTAV'               , &  ! Data Name 
                       grid%smstav               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SMSTAV memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SMSTOT'               , &  ! Data Name 
                       grid%smstot               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SMSTOT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SFROFF'               , &  ! Data Name 
                       grid%sfcrunoff               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SFROFF memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'UDROFF'               , &  ! Data Name 
                       grid%udrunoff               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field UDROFF memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'IVGTYP'               , &  ! Data Name 
                       grid%ivgtyp               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field IVGTYP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ISLTYP'               , &  ! Data Name 
                       grid%isltyp               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field ISLTYP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'VEGFRA'               , &  ! Data Name 
                       grid%vegfra               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field VEGFRA memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SFCEVP'               , &  ! Data Name 
                       grid%sfcevp               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SFCEVP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GRDFLX'               , &  ! Data Name 
                       grid%grdflx               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field GRDFLX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SFCEXC '               , &  ! Data Name 
                       grid%sfcexc               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SFCEXC  memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ACSNOW'               , &  ! Data Name 
                       grid%acsnow               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field ACSNOW memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ACSNOM'               , &  ! Data Name 
                       grid%acsnom               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field ACSNOM memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNOW'               , &  ! Data Name 
                       grid%snow               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SNOW memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNOWH'               , &  ! Data Name 
                       grid%snowh               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SNOWH memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CANWAT'               , &  ! Data Name 
                       grid%canwat               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field CANWAT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SST'               , &  ! Data Name 
                       grid%sst               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SST memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SMFR3D'               , &  ! Data Name 
                       grid%smfr3d               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SMFR3D memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'KEEPFR3DFLAG          '               , &  ! Data Name 
                       grid%keepfr3dflag               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field KEEPFR3DFLAG           memorder XZY' , & ! Debug message
ids , (ide-1) , 1 , config_flags%num_soil_layers , jds , (jde-1) ,  & 
ims , ime , 1 , config_flags%num_soil_layers , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , 1 , config_flags%num_soil_layers , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TKE_MYJ'               , &  ! Data Name 
                       grid%tke_myj               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field TKE_MYJ memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'EXCH_H'               , &  ! Data Name 
                       grid%exch_h               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field EXCH_H memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CT'               , &  ! Data Name 
                       grid%ct               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field CT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'THZ0'               , &  ! Data Name 
                       grid%thz0               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field THZ0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Z0'               , &  ! Data Name 
                       grid%z0               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field Z0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QZ0'               , &  ! Data Name 
                       grid%qz0               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QZ0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'UZ0'               , &  ! Data Name 
                       grid%uz0               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field UZ0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'VZ0'               , &  ! Data Name 
                       grid%vz0               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field VZ0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QSFC'               , &  ! Data Name 
                       grid%qsfc               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QSFC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AKHS'               , &  ! Data Name 
                       grid%akhs               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field AKHS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'AKMS'               , &  ! Data Name 
                       grid%akms               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field AKMS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'KPBL'               , &  ! Data Name 
                       grid%kpbl               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field KPBL memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HTOP'               , &  ! Data Name 
                       grid%htop               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field HTOP memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HBOT'               , &  ! Data Name 
                       grid%hbot               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field HBOT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CUPPT'               , &  ! Data Name 
                       grid%cuppt               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field CUPPT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MASS_AER_DRY'               , &  ! Data Name 
                       grid%aer_dry               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MASS_AER_DRY memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MASS_AER_WATER'               , &  ! Data Name 
                       grid%aer_water               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MASS_AER_WATER memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F_ICE_PHY'               , &  ! Data Name 
                       grid%f_ice_phy               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field F_ICE_PHY memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F_RAIN_PHY'               , &  ! Data Name 
                       grid%f_rain_phy               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field F_RAIN_PHY memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F_RIMEF_PHY'               , &  ! Data Name 
                       grid%f_rimef_phy               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field F_RIMEF_PHY memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'H_DIABATIC'               , &  ! Data Name 
                       grid%h_diabatic               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field H_DIABATIC memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MAPFAC_M'               , &  ! Data Name 
                       grid%msft               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MAPFAC_M memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MAPFAC_U'               , &  ! Data Name 
                       grid%msfu               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MAPFAC_U memorder XY' , & ! Debug message
ids , ide , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( ide, ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MAPFAC_V'               , &  ! Data Name 
                       grid%msfv               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MAPFAC_V memorder XY' , & ! Debug message
ids , (ide-1) , jds , jde , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( jde, jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'F'               , &  ! Data Name 
                       grid%f               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field F memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'E'               , &  ! Data Name 
                       grid%e               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field E memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SINALPHA'               , &  ! Data Name 
                       grid%sina               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SINALPHA memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'COSALPHA'               , &  ! Data Name 
                       grid%cosa               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field COSALPHA memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HGT'               , &  ! Data Name 
                       grid%ht               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field HGT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TSK'               , &  ! Data Name 
                       grid%tsk               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field TSK memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_BASE'               , &  ! Data Name 
                       grid%u_base               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field U_BASE memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_BASE'               , &  ! Data Name 
                       grid%v_base               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field V_BASE memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QV_BASE'               , &  ! Data Name 
                       grid%qv_base               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QV_BASE memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'Z_BASE'               , &  ! Data Name 
                       grid%z_base               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'Z'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field Z_BASE memorder Z' , & ! Debug message
kds , (kde-1) , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( (kde-1), kpe ) , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U_FRAME'               , &  ! Data Name 
                       grid%u_frame               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field U_FRAME memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V_FRAME'               , &  ! Data Name 
                       grid%v_frame               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field V_FRAME memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'P_TOP'               , &  ! Data Name 
                       grid%p_top               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field P_TOP memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'IMICROGRAM'               , &  ! Data Name 
                       grid%imicrogram               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field IMICROGRAM memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_LL_T'               , &  ! Data Name 
                       grid%em_lat_ll_t               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LAT_LL_T memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_UL_T'               , &  ! Data Name 
                       grid%em_lat_ul_t               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LAT_UL_T memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_UR_T'               , &  ! Data Name 
                       grid%em_lat_ur_t               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LAT_UR_T memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_LR_T'               , &  ! Data Name 
                       grid%em_lat_lr_t               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LAT_LR_T memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_LL_U'               , &  ! Data Name 
                       grid%em_lat_ll_u               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LAT_LL_U memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_UL_U'               , &  ! Data Name 
                       grid%em_lat_ul_u               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LAT_UL_U memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_UR_U'               , &  ! Data Name 
                       grid%em_lat_ur_u               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LAT_UR_U memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_LR_U'               , &  ! Data Name 
                       grid%em_lat_lr_u               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LAT_LR_U memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_LL_V'               , &  ! Data Name 
                       grid%em_lat_ll_v               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LAT_LL_V memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_UL_V'               , &  ! Data Name 
                       grid%em_lat_ul_v               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LAT_UL_V memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_UR_V'               , &  ! Data Name 
                       grid%em_lat_ur_v               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LAT_UR_V memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_LR_V'               , &  ! Data Name 
                       grid%em_lat_lr_v               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LAT_LR_V memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_LL_D'               , &  ! Data Name 
                       grid%em_lat_ll_d               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LAT_LL_D memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_UL_D'               , &  ! Data Name 
                       grid%em_lat_ul_d               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LAT_UL_D memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_UR_D'               , &  ! Data Name 
                       grid%em_lat_ur_d               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LAT_UR_D memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LAT_LR_D'               , &  ! Data Name 
                       grid%em_lat_lr_d               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LAT_LR_D memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_LL_T'               , &  ! Data Name 
                       grid%em_lon_ll_t               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LON_LL_T memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_UL_T'               , &  ! Data Name 
                       grid%em_lon_ul_t               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LON_UL_T memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_UR_T'               , &  ! Data Name 
                       grid%em_lon_ur_t               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LON_UR_T memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_LR_T'               , &  ! Data Name 
                       grid%em_lon_lr_t               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LON_LR_T memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_LL_U'               , &  ! Data Name 
                       grid%em_lon_ll_u               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LON_LL_U memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_UL_U'               , &  ! Data Name 
                       grid%em_lon_ul_u               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LON_UL_U memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_UR_U'               , &  ! Data Name 
                       grid%em_lon_ur_u               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LON_UR_U memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_LR_U'               , &  ! Data Name 
                       grid%em_lon_lr_u               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LON_LR_U memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_LL_V'               , &  ! Data Name 
                       grid%em_lon_ll_v               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LON_LL_V memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_UL_V'               , &  ! Data Name 
                       grid%em_lon_ul_v               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LON_UL_V memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_UR_V'               , &  ! Data Name 
                       grid%em_lon_ur_v               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LON_UR_V memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_LR_V'               , &  ! Data Name 
                       grid%em_lon_lr_v               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LON_LR_V memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_LL_D'               , &  ! Data Name 
                       grid%em_lon_ll_d               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LON_LL_D memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_UL_D'               , &  ! Data Name 
                       grid%em_lon_ul_d               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LON_UL_D memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_UR_D'               , &  ! Data Name 
                       grid%em_lon_ur_d               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LON_UR_D memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LON_LR_D'               , &  ! Data Name 
                       grid%em_lon_lr_d               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LON_LR_D memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
END IF
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RTHCUTEN'               , &  ! Data Name 
                       grid%rthcuten               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RTHCUTEN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQVCUTEN'               , &  ! Data Name 
                       grid%rqvcuten               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RQVCUTEN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQRCUTEN'               , &  ! Data Name 
                       grid%rqrcuten               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RQRCUTEN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQCCUTEN'               , &  ! Data Name 
                       grid%rqccuten               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RQCCUTEN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQSCUTEN'               , &  ! Data Name 
                       grid%rqscuten               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RQSCUTEN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQICUTEN'               , &  ! Data Name 
                       grid%rqicuten               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RQICUTEN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W0AVG'               , &  ! Data Name 
                       grid%w0avg               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field W0AVG memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RAINC'               , &  ! Data Name 
                       grid%rainc               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RAINC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RAINNC'               , &  ! Data Name 
                       grid%rainnc               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RAINNC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RAINCV'               , &  ! Data Name 
                       grid%raincv               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RAINCV memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RAINNCV'               , &  ! Data Name 
                       grid%rainncv               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RAINNCV memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RAINBL'               , &  ! Data Name 
                       grid%rainbl               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RAINBL memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'NCA'               , &  ! Data Name 
                       grid%nca               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field NCA memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MASS_FLUX'               , &  ! Data Name 
                       grid%mass_flux               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MASS_FLUX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'APR_GR'               , &  ! Data Name 
                       grid%apr_gr               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field APR_GR memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'APR_W'               , &  ! Data Name 
                       grid%apr_w               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field APR_W memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'APR_MC'               , &  ! Data Name 
                       grid%apr_mc               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field APR_MC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'APR_ST'               , &  ! Data Name 
                       grid%apr_st               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field APR_ST memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'APR_AS'               , &  ! Data Name 
                       grid%apr_as               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field APR_AS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'APR_CAPMA'               , &  ! Data Name 
                       grid%apr_capma               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field APR_CAPMA memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'APR_CAPME'               , &  ! Data Name 
                       grid%apr_capme               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field APR_CAPME memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'APR_CAPMI'               , &  ! Data Name 
                       grid%apr_capmi               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field APR_CAPMI memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XF_ENS'               , &  ! Data Name 
                       grid%xf_ens               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XYZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field XF_ENS memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%ensdim ,  & 
ims , ime , jms , jme , 1 , config_flags%ensdim ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%ensdim ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PR_ENS'               , &  ! Data Name 
                       grid%pr_ens               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XYZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field PR_ENS memorder XYZ' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , config_flags%ensdim ,  & 
ims , ime , jms , jme , 1 , config_flags%ensdim ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , config_flags%ensdim ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RTHFTEN'               , &  ! Data Name 
                       grid%rthften               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RTHFTEN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQVFTEN'               , &  ! Data Name 
                       grid%rqvften               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RQVFTEN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'STEPCU'               , &  ! Data Name 
                       grid%stepcu               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field STEPCU memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RTHRATEN'               , &  ! Data Name 
                       grid%rthraten               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RTHRATEN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RTHRATLW'               , &  ! Data Name 
                       grid%rthratenlw               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RTHRATLW memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RTHRATSW'               , &  ! Data Name 
                       grid%rthratensw               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RTHRATSW memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CLDFRA'               , &  ! Data Name 
                       grid%cldfra               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field CLDFRA memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SWDOWN'               , &  ! Data Name 
                       grid%swdown               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SWDOWN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GSW'               , &  ! Data Name 
                       grid%gsw               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field GSW memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GLW'               , &  ! Data Name 
                       grid%glw               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field GLW memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XLAT'               , &  ! Data Name 
                       grid%xlat               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field XLAT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XLONG'               , &  ! Data Name 
                       grid%xlong               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field XLONG memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALBEDO'               , &  ! Data Name 
                       grid%albedo               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field ALBEDO memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALBBCK'               , &  ! Data Name 
                       grid%albbck               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field ALBBCK memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'EMISS'               , &  ! Data Name 
                       grid%emiss               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field EMISS memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CLDEFI'               , &  ! Data Name 
                       grid%cldefi               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field CLDEFI memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'STEPRA'               , &  ! Data Name 
                       grid%stepra               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field STEPRA memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RUBLTEN'               , &  ! Data Name 
                       grid%rublten               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RUBLTEN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RVBLTEN'               , &  ! Data Name 
                       grid%rvblten               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RVBLTEN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RTHBLTEN'               , &  ! Data Name 
                       grid%rthblten               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RTHBLTEN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQVBLTEN'               , &  ! Data Name 
                       grid%rqvblten               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RQVBLTEN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQCBLTEN'               , &  ! Data Name 
                       grid%rqcblten               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RQCBLTEN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQIBLTEN'               , &  ! Data Name 
                       grid%rqiblten               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RQIBLTEN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TMN'               , &  ! Data Name 
                       grid%tmn               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field TMN memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'XLAND'               , &  ! Data Name 
                       grid%xland               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field XLAND memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ZNT'               , &  ! Data Name 
                       grid%znt               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field ZNT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'UST'               , &  ! Data Name 
                       grid%ust               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field UST memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RMOL'               , &  ! Data Name 
                       grid%rmol               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field RMOL memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MOL'               , &  ! Data Name 
                       grid%mol               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MOL memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PBLH'               , &  ! Data Name 
                       grid%pblh               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field PBLH memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CAPG'               , &  ! Data Name 
                       grid%capg               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field CAPG memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'THC'               , &  ! Data Name 
                       grid%thc               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field THC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'HFX'               , &  ! Data Name 
                       grid%hfx               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field HFX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QFX'               , &  ! Data Name 
                       grid%qfx               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QFX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'LH'               , &  ! Data Name 
                       grid%lh               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field LH memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'FLHC'               , &  ! Data Name 
                       grid%flhc               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field FLHC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'FLQC'               , &  ! Data Name 
                       grid%flqc               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field FLQC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QSG'               , &  ! Data Name 
                       grid%qsg               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QSG memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QVG'               , &  ! Data Name 
                       grid%qvg               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QVG memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QCG'               , &  ! Data Name 
                       grid%qcg               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field QCG memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SOILT1'               , &  ! Data Name 
                       grid%soilt1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SOILT1 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TSNAV'               , &  ! Data Name 
                       grid%tsnav               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field TSNAV memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'SNOWC'               , &  ! Data Name 
                       grid%snowc               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field SNOWC memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MAVAIL'               , &  ! Data Name 
                       grid%mavail               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field MAVAIL memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TKESFCF'               , &  ! Data Name 
                       grid%tkesfcf               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field TKESFCF memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'STEPBL'               , &  ! Data Name 
                       grid%stepbl               , &  ! Field 
                       WRF_integer             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       '0'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field STEPBL memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TAUCLDI'               , &  ! Data Name 
                       grid%taucldi               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field TAUCLDI memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'TAUCLDC'               , &  ! Data Name 
                       grid%taucldc               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_restartin.inc ext_read_field TAUCLDC memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
!ENDOFREGISTRYGENERATEDINCLUDE
    IF ( grid%id .GT. 1 ) THEN

!STARTOFREGISTRYGENERATEDINCLUDE inc/wrf_bdyin.inc
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit.  Your changes to this file will be lost.
!
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RU_BXS'               , &  ! Data Name 
                       grid%em_u_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RU_BXS'             , &  ! Data Name 
                       grid%em_u_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RU_BXS'             , &  ! Data Name 
                       grid%em_u_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RU_BXE'               , &  ! Data Name 
                       grid%em_u_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RU_BXE'             , &  ! Data Name 
                       grid%em_u_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RU_BXE'             , &  ! Data Name 
                       grid%em_u_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RU_BYS'               , &  ! Data Name 
                       grid%em_u_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BYS memorder YSZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RU_BYS'             , &  ! Data Name 
                       grid%em_u_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BYS memorder YSZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RU_BYS'             , &  ! Data Name 
                       grid%em_u_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BYS memorder YSZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RU_BYE'               , &  ! Data Name 
                       grid%em_u_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BYE memorder YEZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RU_BYE'             , &  ! Data Name 
                       grid%em_u_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BYE memorder YEZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RU_BYE'             , &  ! Data Name 
                       grid%em_u_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BYE memorder YEZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RU_BTXS'               , &  ! Data Name 
                       grid%em_u_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RU_BTXS'             , &  ! Data Name 
                       grid%em_u_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RU_BTXS'             , &  ! Data Name 
                       grid%em_u_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RU_BTXE'               , &  ! Data Name 
                       grid%em_u_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RU_BTXE'             , &  ! Data Name 
                       grid%em_u_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RU_BTXE'             , &  ! Data Name 
                       grid%em_u_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RU_BTYS'               , &  ! Data Name 
                       grid%em_u_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BTYS memorder YSZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RU_BTYS'             , &  ! Data Name 
                       grid%em_u_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BTYS memorder YSZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RU_BTYS'             , &  ! Data Name 
                       grid%em_u_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BTYS memorder YSZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RU_BTYE'               , &  ! Data Name 
                       grid%em_u_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BTYE memorder YEZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RU_BTYE'             , &  ! Data Name 
                       grid%em_u_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BTYE memorder YEZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RU_BTYE'             , &  ! Data Name 
                       grid%em_u_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RU_BTYE memorder YEZ' , & ! Debug message
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, ide, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RV_BXS'               , &  ! Data Name 
                       grid%em_v_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BXS memorder XSZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RV_BXS'             , &  ! Data Name 
                       grid%em_v_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BXS memorder XSZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RV_BXS'             , &  ! Data Name 
                       grid%em_v_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BXS memorder XSZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RV_BXE'               , &  ! Data Name 
                       grid%em_v_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BXE memorder XEZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RV_BXE'             , &  ! Data Name 
                       grid%em_v_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BXE memorder XEZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RV_BXE'             , &  ! Data Name 
                       grid%em_v_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BXE memorder XEZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RV_BYS'               , &  ! Data Name 
                       grid%em_v_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RV_BYS'             , &  ! Data Name 
                       grid%em_v_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RV_BYS'             , &  ! Data Name 
                       grid%em_v_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RV_BYE'               , &  ! Data Name 
                       grid%em_v_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RV_BYE'             , &  ! Data Name 
                       grid%em_v_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RV_BYE'             , &  ! Data Name 
                       grid%em_v_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RV_BTXS'               , &  ! Data Name 
                       grid%em_v_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BTXS memorder XSZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RV_BTXS'             , &  ! Data Name 
                       grid%em_v_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BTXS memorder XSZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RV_BTXS'             , &  ! Data Name 
                       grid%em_v_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BTXS memorder XSZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RV_BTXE'               , &  ! Data Name 
                       grid%em_v_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BTXE memorder XEZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RV_BTXE'             , &  ! Data Name 
                       grid%em_v_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BTXE memorder XEZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RV_BTXE'             , &  ! Data Name 
                       grid%em_v_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BTXE memorder XEZ' , & ! Debug message
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, jde, kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RV_BTYS'               , &  ! Data Name 
                       grid%em_v_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RV_BTYS'             , &  ! Data Name 
                       grid%em_v_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RV_BTYS'             , &  ! Data Name 
                       grid%em_v_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RV_BTYE'               , &  ! Data Name 
                       grid%em_v_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RV_BTYE'             , &  ! Data Name 
                       grid%em_v_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RV_BTYE'             , &  ! Data Name 
                       grid%em_v_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RV_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RW_BXS'               , &  ! Data Name 
                       grid%em_w_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RW_BXS'             , &  ! Data Name 
                       grid%em_w_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RW_BXS'             , &  ! Data Name 
                       grid%em_w_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RW_BXE'               , &  ! Data Name 
                       grid%em_w_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RW_BXE'             , &  ! Data Name 
                       grid%em_w_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RW_BXE'             , &  ! Data Name 
                       grid%em_w_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RW_BYS'               , &  ! Data Name 
                       grid%em_w_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RW_BYS'             , &  ! Data Name 
                       grid%em_w_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RW_BYS'             , &  ! Data Name 
                       grid%em_w_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RW_BYE'               , &  ! Data Name 
                       grid%em_w_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RW_BYE'             , &  ! Data Name 
                       grid%em_w_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RW_BYE'             , &  ! Data Name 
                       grid%em_w_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RW_BTXS'               , &  ! Data Name 
                       grid%em_w_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RW_BTXS'             , &  ! Data Name 
                       grid%em_w_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RW_BTXS'             , &  ! Data Name 
                       grid%em_w_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RW_BTXE'               , &  ! Data Name 
                       grid%em_w_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RW_BTXE'             , &  ! Data Name 
                       grid%em_w_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RW_BTXE'             , &  ! Data Name 
                       grid%em_w_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RW_BTYS'               , &  ! Data Name 
                       grid%em_w_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RW_BTYS'             , &  ! Data Name 
                       grid%em_w_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RW_BTYS'             , &  ! Data Name 
                       grid%em_w_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RW_BTYE'               , &  ! Data Name 
                       grid%em_w_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RW_BTYE'             , &  ! Data Name 
                       grid%em_w_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RW_BTYE'             , &  ! Data Name 
                       grid%em_w_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RW_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BXS'               , &  ! Data Name 
                       grid%em_ph_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_PH_BXS'             , &  ! Data Name 
                       grid%em_ph_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_PH_BXS'             , &  ! Data Name 
                       grid%em_ph_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BXE'               , &  ! Data Name 
                       grid%em_ph_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_PH_BXE'             , &  ! Data Name 
                       grid%em_ph_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_PH_BXE'             , &  ! Data Name 
                       grid%em_ph_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BYS'               , &  ! Data Name 
                       grid%em_ph_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_PH_BYS'             , &  ! Data Name 
                       grid%em_ph_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_PH_BYS'             , &  ! Data Name 
                       grid%em_ph_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BYE'               , &  ! Data Name 
                       grid%em_ph_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_PH_BYE'             , &  ! Data Name 
                       grid%em_ph_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_PH_BYE'             , &  ! Data Name 
                       grid%em_ph_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BTXS'               , &  ! Data Name 
                       grid%em_ph_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_PH_BTXS'             , &  ! Data Name 
                       grid%em_ph_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_PH_BTXS'             , &  ! Data Name 
                       grid%em_ph_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BTXE'               , &  ! Data Name 
                       grid%em_ph_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_PH_BTXE'             , &  ! Data Name 
                       grid%em_ph_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_PH_BTXE'             , &  ! Data Name 
                       grid%em_ph_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BTYS'               , &  ! Data Name 
                       grid%em_ph_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_PH_BTYS'             , &  ! Data Name 
                       grid%em_ph_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_PH_BTYS'             , &  ! Data Name 
                       grid%em_ph_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH_BTYE'               , &  ! Data Name 
                       grid%em_ph_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_PH_BTYE'             , &  ! Data Name 
                       grid%em_ph_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_PH_BTYE'             , &  ! Data Name 
                       grid%em_ph_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field PH_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, kde, 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BXS'               , &  ! Data Name 
                       grid%em_t_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_T_BXS'             , &  ! Data Name 
                       grid%em_t_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_T_BXS'             , &  ! Data Name 
                       grid%em_t_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BXE'               , &  ! Data Name 
                       grid%em_t_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_T_BXE'             , &  ! Data Name 
                       grid%em_t_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_T_BXE'             , &  ! Data Name 
                       grid%em_t_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BYS'               , &  ! Data Name 
                       grid%em_t_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_T_BYS'             , &  ! Data Name 
                       grid%em_t_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_T_BYS'             , &  ! Data Name 
                       grid%em_t_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BYE'               , &  ! Data Name 
                       grid%em_t_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_T_BYE'             , &  ! Data Name 
                       grid%em_t_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_T_BYE'             , &  ! Data Name 
                       grid%em_t_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTXS'               , &  ! Data Name 
                       grid%em_t_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_T_BTXS'             , &  ! Data Name 
                       grid%em_t_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_T_BTXS'             , &  ! Data Name 
                       grid%em_t_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTXE'               , &  ! Data Name 
                       grid%em_t_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_T_BTXE'             , &  ! Data Name 
                       grid%em_t_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_T_BTXE'             , &  ! Data Name 
                       grid%em_t_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTYS'               , &  ! Data Name 
                       grid%em_t_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_T_BTYS'             , &  ! Data Name 
                       grid%em_t_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_T_BTYS'             , &  ! Data Name 
                       grid%em_t_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T_BTYE'               , &  ! Data Name 
                       grid%em_t_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_T_BTYE'             , &  ! Data Name 
                       grid%em_t_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_T_BTYE'             , &  ! Data Name 
                       grid%em_t_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field T_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BXS'               , &  ! Data Name 
                       grid%em_mu_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_MU_BXS'             , &  ! Data Name 
                       grid%em_mu_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_MU_BXS'             , &  ! Data Name 
                       grid%em_mu_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BXE'               , &  ! Data Name 
                       grid%em_mu_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_MU_BXE'             , &  ! Data Name 
                       grid%em_mu_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_MU_BXE'             , &  ! Data Name 
                       grid%em_mu_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BYS'               , &  ! Data Name 
                       grid%em_mu_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_MU_BYS'             , &  ! Data Name 
                       grid%em_mu_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_MU_BYS'             , &  ! Data Name 
                       grid%em_mu_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BYE'               , &  ! Data Name 
                       grid%em_mu_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_MU_BYE'             , &  ! Data Name 
                       grid%em_mu_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_MU_BYE'             , &  ! Data Name 
                       grid%em_mu_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BTXS'               , &  ! Data Name 
                       grid%em_mu_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BTXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_MU_BTXS'             , &  ! Data Name 
                       grid%em_mu_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BTXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_MU_BTXS'             , &  ! Data Name 
                       grid%em_mu_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BTXS memorder XS' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BTXE'               , &  ! Data Name 
                       grid%em_mu_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BTXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_MU_BTXE'             , &  ! Data Name 
                       grid%em_mu_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BTXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_MU_BTXE'             , &  ! Data Name 
                       grid%em_mu_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BTXE memorder XE' , & ! Debug message
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (jde-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BTYS'               , &  ! Data Name 
                       grid%em_mu_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BTYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_MU_BTYS'             , &  ! Data Name 
                       grid%em_mu_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BTYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_MU_BTYS'             , &  ! Data Name 
                       grid%em_mu_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YS'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BTYS memorder YS' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU_BTYE'               , &  ! Data Name 
                       grid%em_mu_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BTYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_MU_BTYE'             , &  ! Data Name 
                       grid%em_mu_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BTYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_MU_BTYE'             , &  ! Data Name 
                       grid%em_mu_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YE'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field MU_BTYE memorder YE' , & ! Debug message
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
1, MAX( ide , jde ), 1, config_flags%spec_bdy_width, 1, 1, &
1, (ide-1), 1, config_flags%spec_bdy_width, 1, 1, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQV_BXS'               , &  ! Data Name 
                       grid%em_rqv_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQV_BXS'             , &  ! Data Name 
                       grid%em_rqv_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQV_BXS'             , &  ! Data Name 
                       grid%em_rqv_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQV_BXE'               , &  ! Data Name 
                       grid%em_rqv_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQV_BXE'             , &  ! Data Name 
                       grid%em_rqv_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQV_BXE'             , &  ! Data Name 
                       grid%em_rqv_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQV_BYS'               , &  ! Data Name 
                       grid%em_rqv_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQV_BYS'             , &  ! Data Name 
                       grid%em_rqv_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQV_BYS'             , &  ! Data Name 
                       grid%em_rqv_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQV_BYE'               , &  ! Data Name 
                       grid%em_rqv_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQV_BYE'             , &  ! Data Name 
                       grid%em_rqv_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQV_BYE'             , &  ! Data Name 
                       grid%em_rqv_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQV_BTXS'               , &  ! Data Name 
                       grid%em_rqv_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQV_BTXS'             , &  ! Data Name 
                       grid%em_rqv_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQV_BTXS'             , &  ! Data Name 
                       grid%em_rqv_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQV_BTXE'               , &  ! Data Name 
                       grid%em_rqv_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQV_BTXE'             , &  ! Data Name 
                       grid%em_rqv_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQV_BTXE'             , &  ! Data Name 
                       grid%em_rqv_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQV_BTYS'               , &  ! Data Name 
                       grid%em_rqv_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQV_BTYS'             , &  ! Data Name 
                       grid%em_rqv_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQV_BTYS'             , &  ! Data Name 
                       grid%em_rqv_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQV_BTYE'               , &  ! Data Name 
                       grid%em_rqv_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQV_BTYE'             , &  ! Data Name 
                       grid%em_rqv_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQV_BTYE'             , &  ! Data Name 
                       grid%em_rqv_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQV_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
END IF
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQC_BXS'               , &  ! Data Name 
                       grid%rqc_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQC_BXS'             , &  ! Data Name 
                       grid%rqc_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQC_BXS'             , &  ! Data Name 
                       grid%rqc_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQC_BXE'               , &  ! Data Name 
                       grid%rqc_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQC_BXE'             , &  ! Data Name 
                       grid%rqc_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQC_BXE'             , &  ! Data Name 
                       grid%rqc_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQC_BYS'               , &  ! Data Name 
                       grid%rqc_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQC_BYS'             , &  ! Data Name 
                       grid%rqc_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQC_BYS'             , &  ! Data Name 
                       grid%rqc_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQC_BYE'               , &  ! Data Name 
                       grid%rqc_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQC_BYE'             , &  ! Data Name 
                       grid%rqc_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQC_BYE'             , &  ! Data Name 
                       grid%rqc_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQC_BTXS'               , &  ! Data Name 
                       grid%rqc_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQC_BTXS'             , &  ! Data Name 
                       grid%rqc_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQC_BTXS'             , &  ! Data Name 
                       grid%rqc_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQC_BTXE'               , &  ! Data Name 
                       grid%rqc_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQC_BTXE'             , &  ! Data Name 
                       grid%rqc_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQC_BTXE'             , &  ! Data Name 
                       grid%rqc_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQC_BTYS'               , &  ! Data Name 
                       grid%rqc_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQC_BTYS'             , &  ! Data Name 
                       grid%rqc_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQC_BTYS'             , &  ! Data Name 
                       grid%rqc_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQC_BTYE'               , &  ! Data Name 
                       grid%rqc_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQC_BTYE'             , &  ! Data Name 
                       grid%rqc_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQC_BTYE'             , &  ! Data Name 
                       grid%rqc_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQC_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQR_BXS'               , &  ! Data Name 
                       grid%rqr_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQR_BXS'             , &  ! Data Name 
                       grid%rqr_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQR_BXS'             , &  ! Data Name 
                       grid%rqr_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQR_BXE'               , &  ! Data Name 
                       grid%rqr_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQR_BXE'             , &  ! Data Name 
                       grid%rqr_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQR_BXE'             , &  ! Data Name 
                       grid%rqr_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQR_BYS'               , &  ! Data Name 
                       grid%rqr_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQR_BYS'             , &  ! Data Name 
                       grid%rqr_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQR_BYS'             , &  ! Data Name 
                       grid%rqr_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQR_BYE'               , &  ! Data Name 
                       grid%rqr_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQR_BYE'             , &  ! Data Name 
                       grid%rqr_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQR_BYE'             , &  ! Data Name 
                       grid%rqr_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQR_BTXS'               , &  ! Data Name 
                       grid%rqr_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQR_BTXS'             , &  ! Data Name 
                       grid%rqr_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQR_BTXS'             , &  ! Data Name 
                       grid%rqr_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQR_BTXE'               , &  ! Data Name 
                       grid%rqr_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQR_BTXE'             , &  ! Data Name 
                       grid%rqr_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQR_BTXE'             , &  ! Data Name 
                       grid%rqr_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQR_BTYS'               , &  ! Data Name 
                       grid%rqr_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQR_BTYS'             , &  ! Data Name 
                       grid%rqr_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQR_BTYS'             , &  ! Data Name 
                       grid%rqr_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQR_BTYE'               , &  ! Data Name 
                       grid%rqr_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQR_BTYE'             , &  ! Data Name 
                       grid%rqr_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQR_BTYE'             , &  ! Data Name 
                       grid%rqr_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQR_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQI_BXS'               , &  ! Data Name 
                       grid%rqi_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQI_BXS'             , &  ! Data Name 
                       grid%rqi_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQI_BXS'             , &  ! Data Name 
                       grid%rqi_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQI_BXE'               , &  ! Data Name 
                       grid%rqi_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQI_BXE'             , &  ! Data Name 
                       grid%rqi_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQI_BXE'             , &  ! Data Name 
                       grid%rqi_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQI_BYS'               , &  ! Data Name 
                       grid%rqi_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQI_BYS'             , &  ! Data Name 
                       grid%rqi_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQI_BYS'             , &  ! Data Name 
                       grid%rqi_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQI_BYE'               , &  ! Data Name 
                       grid%rqi_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQI_BYE'             , &  ! Data Name 
                       grid%rqi_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQI_BYE'             , &  ! Data Name 
                       grid%rqi_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQI_BTXS'               , &  ! Data Name 
                       grid%rqi_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQI_BTXS'             , &  ! Data Name 
                       grid%rqi_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQI_BTXS'             , &  ! Data Name 
                       grid%rqi_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQI_BTXE'               , &  ! Data Name 
                       grid%rqi_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQI_BTXE'             , &  ! Data Name 
                       grid%rqi_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQI_BTXE'             , &  ! Data Name 
                       grid%rqi_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQI_BTYS'               , &  ! Data Name 
                       grid%rqi_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQI_BTYS'             , &  ! Data Name 
                       grid%rqi_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQI_BTYS'             , &  ! Data Name 
                       grid%rqi_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQI_BTYE'               , &  ! Data Name 
                       grid%rqi_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQI_BTYE'             , &  ! Data Name 
                       grid%rqi_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQI_BTYE'             , &  ! Data Name 
                       grid%rqi_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQI_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQS_BXS'               , &  ! Data Name 
                       grid%rqs_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQS_BXS'             , &  ! Data Name 
                       grid%rqs_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQS_BXS'             , &  ! Data Name 
                       grid%rqs_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQS_BXE'               , &  ! Data Name 
                       grid%rqs_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQS_BXE'             , &  ! Data Name 
                       grid%rqs_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQS_BXE'             , &  ! Data Name 
                       grid%rqs_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQS_BYS'               , &  ! Data Name 
                       grid%rqs_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQS_BYS'             , &  ! Data Name 
                       grid%rqs_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQS_BYS'             , &  ! Data Name 
                       grid%rqs_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQS_BYE'               , &  ! Data Name 
                       grid%rqs_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQS_BYE'             , &  ! Data Name 
                       grid%rqs_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQS_BYE'             , &  ! Data Name 
                       grid%rqs_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQS_BTXS'               , &  ! Data Name 
                       grid%rqs_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQS_BTXS'             , &  ! Data Name 
                       grid%rqs_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQS_BTXS'             , &  ! Data Name 
                       grid%rqs_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQS_BTXE'               , &  ! Data Name 
                       grid%rqs_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQS_BTXE'             , &  ! Data Name 
                       grid%rqs_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQS_BTXE'             , &  ! Data Name 
                       grid%rqs_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQS_BTYS'               , &  ! Data Name 
                       grid%rqs_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQS_BTYS'             , &  ! Data Name 
                       grid%rqs_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQS_BTYS'             , &  ! Data Name 
                       grid%rqs_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQS_BTYE'               , &  ! Data Name 
                       grid%rqs_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQS_BTYE'             , &  ! Data Name 
                       grid%rqs_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQS_BTYE'             , &  ! Data Name 
                       grid%rqs_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQS_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQG_BXS'               , &  ! Data Name 
                       grid%rqg_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQG_BXS'             , &  ! Data Name 
                       grid%rqg_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQG_BXS'             , &  ! Data Name 
                       grid%rqg_b(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQG_BXE'               , &  ! Data Name 
                       grid%rqg_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQG_BXE'             , &  ! Data Name 
                       grid%rqg_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQG_BXE'             , &  ! Data Name 
                       grid%rqg_b(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQG_BYS'               , &  ! Data Name 
                       grid%rqg_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQG_BYS'             , &  ! Data Name 
                       grid%rqg_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQG_BYS'             , &  ! Data Name 
                       grid%rqg_b(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQG_BYE'               , &  ! Data Name 
                       grid%rqg_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQG_BYE'             , &  ! Data Name 
                       grid%rqg_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQG_BYE'             , &  ! Data Name 
                       grid%rqg_b(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQG_BTXS'               , &  ! Data Name 
                       grid%rqg_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQG_BTXS'             , &  ! Data Name 
                       grid%rqg_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQG_BTXS'             , &  ! Data Name 
                       grid%rqg_bt(1,kds,1,1)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BTXS memorder XSZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQG_BTXE'               , &  ! Data Name 
                       grid%rqg_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQG_BTXE'             , &  ! Data Name 
                       grid%rqg_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQG_BTXE'             , &  ! Data Name 
                       grid%rqg_bt(1,kds,1,2)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BTXE memorder XEZ' , & ! Debug message
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (jde-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQG_BTYS'               , &  ! Data Name 
                       grid%rqg_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQG_BTYS'             , &  ! Data Name 
                       grid%rqg_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQG_BTYS'             , &  ! Data Name 
                       grid%rqg_bt(1,kds,1,3)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YSZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BTYS memorder YSZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RQG_BTYE'               , &  ! Data Name 
                       grid%rqg_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RQG_BTYE'             , &  ! Data Name 
                       grid%rqg_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RQG_BTYE'             , &  ! Data Name 
                       grid%rqg_bt(1,kds,1,4)     , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator , &  ! Comm
                       grid%iocommunicator , &  ! Comm
                       grid%domdesc      , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'YEZ'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_bdyin.inc ext_read_field RQG_BTYE memorder YEZ' , & ! Debug message
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
1, MAX( ide , jde ), kds, kde, 1, config_flags%spec_bdy_width, &
1, (ide-1), kds, (kde-1), 1, config_flags%spec_bdy_width, &
                       ierr )
!ENDOFREGISTRYGENERATEDINCLUDE
    ENDIF

    RETURN
    END
