SUBROUTINE wrf_auxhist2in ( fid , grid , config_flags , switch , ierr )
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


!STARTOFREGISTRYGENERATEDINCLUDE inc/wrf_auxhist2in.inc
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
'inc/wrf_auxhist2in.inc ext_read_field LU_INDEX memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'U'               , &  ! Data Name 
                       grid%em_u_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field U memorder XZY' , & ! Debug message
ids , ide , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( ide, ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_U'             , &  ! Data Name 
                       grid%em_g_u_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field U memorder XZY' , & ! Debug message
ids , ide , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( ide, ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_U'             , &  ! Data Name 
                       grid%em_a_u_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'X'               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field U memorder XZY' , & ! Debug message
ids , ide , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( ide, ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'V'               , &  ! Data Name 
                       grid%em_v_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field V memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , jde ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( jde, jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_V'             , &  ! Data Name 
                       grid%em_g_v_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field V memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , jde ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( jde, jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_V'             , &  ! Data Name 
                       grid%em_a_v_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Y'               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field V memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , jde ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( jde, jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'W'               , &  ! Data Name 
                       grid%em_w_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field W memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_W'             , &  ! Data Name 
                       grid%em_g_w_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field W memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_W'             , &  ! Data Name 
                       grid%em_a_w_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field W memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PH'               , &  ! Data Name 
                       grid%em_ph_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field PH memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_PH'             , &  ! Data Name 
                       grid%em_g_ph_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field PH memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_PH'             , &  ! Data Name 
                       grid%em_a_ph_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       'Z'               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field PH memorder XZY' , & ! Debug message
ids , (ide-1) , kds , kde , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( kde, kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'T'               , &  ! Data Name 
                       grid%em_t_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field T memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_T'             , &  ! Data Name 
                       grid%em_g_t_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field T memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_T'             , &  ! Data Name 
                       grid%em_a_t_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field T memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'MU'               , &  ! Data Name 
                       grid%em_mu_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field MU memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_MU'             , &  ! Data Name 
                       grid%em_g_mu_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field MU memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_MU'             , &  ! Data Name 
                       grid%em_a_mu_2               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field MU memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field MU0 memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
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
'inc/wrf_auxhist2in.inc ext_read_field P memorder XZY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field P memorder XZY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field P memorder XZY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field AL memorder XZY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field AL memorder XZY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field AL memorder XZY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field ALT memorder XZY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field ALT memorder XZY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field ALT memorder XZY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field ALB memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'ALPHA'               , &  ! Data Name 
                       grid%em_alpha               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field ALPHA memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_ALPHA'             , &  ! Data Name 
                       grid%em_g_alpha               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field ALPHA memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_ALPHA'             , &  ! Data Name 
                       grid%em_a_alpha               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field ALPHA memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A'               , &  ! Data Name 
                       grid%em_a               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field A memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_A'             , &  ! Data Name 
                       grid%em_g_a               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field A memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_A'             , &  ! Data Name 
                       grid%em_a_a               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field A memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'GAMMA'               , &  ! Data Name 
                       grid%em_gamma               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field GAMMA memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_GAMMA'             , &  ! Data Name 
                       grid%em_g_gamma               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field GAMMA memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_GAMMA'             , &  ! Data Name 
                       grid%em_a_gamma               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field GAMMA memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'C2A'               , &  ! Data Name 
                       grid%em_c2a               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field C2A memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_C2A'             , &  ! Data Name 
                       grid%em_g_c2a               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field C2A memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_C2A'             , &  ! Data Name 
                       grid%em_a_c2a               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field C2A memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'RHO'               , &  ! Data Name 
                       grid%em_rho               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field RHO memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_RHO'             , &  ! Data Name 
                       grid%em_g_rho               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field RHO memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_RHO'             , &  ! Data Name 
                       grid%em_a_rho               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field RHO memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PHM'               , &  ! Data Name 
                       grid%em_phm               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field PHM memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_PHM'             , &  ! Data Name 
                       grid%em_g_phm               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field PHM memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_PHM'             , &  ! Data Name 
                       grid%em_a_phm               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field PHM memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CQU'               , &  ! Data Name 
                       grid%em_cqu               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field CQU memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_CQU'             , &  ! Data Name 
                       grid%em_g_cqu               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field CQU memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_CQU'             , &  ! Data Name 
                       grid%em_a_cqu               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field CQU memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CQV'               , &  ! Data Name 
                       grid%em_cqv               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field CQV memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_CQV'             , &  ! Data Name 
                       grid%em_g_cqv               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field CQV memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_CQV'             , &  ! Data Name 
                       grid%em_a_cqv               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field CQV memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'CQW'               , &  ! Data Name 
                       grid%em_cqw               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field CQW memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_CQW'             , &  ! Data Name 
                       grid%em_g_cqw               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field CQW memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_CQW'             , &  ! Data Name 
                       grid%em_a_cqw               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field CQW memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( mod(grid%dyn_opt,100) .EQ. dyn_em ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'PM1'               , &  ! Data Name 
                       grid%em_pm1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field PM1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. dyn_em_TL) .or. (grid%dyn_opt .EQ. dyn_em_AD)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_PM1'             , &  ! Data Name 
                       grid%em_g_pm1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field PM1 memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( grid%dyn_opt .EQ. dyn_em_AD ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_PM1'             , &  ! Data Name 
                       grid%em_a_pm1               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field PM1 memorder XZY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field FNM memorder Z' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field FNP memorder Z' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field RDNW memorder Z' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field RDN memorder Z' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field DNW memorder Z' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field DN  memorder Z' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field ZNU memorder Z' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field ZNW memorder Z' , & ! Debug message
kds , kde , 1 , 1 , 1 , 1 ,  & 
kms , kme , 1 , 1 , 1 , 1 ,  & 
kps , MIN( kde, kpe ) , 1 , 1 , 1 , 1 ,  & 
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
'inc/wrf_auxhist2in.inc ext_read_field CFN memorder 0' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field CFN1 memorder 0' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field EPSTS memorder 0' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field Q2 memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field T2 memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field TH2 memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field U10 memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field V10 memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field RDX memorder 0' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field RDY memorder 0' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field DTS memorder 0' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field DTSEPS memorder 0' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field RESM memorder 0' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field ZETATOP memorder 0' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field CF1 memorder 0' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field CF2 memorder 0' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field CF3 memorder 0' , & ! Debug message
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
1 , 1 , 1 , 1 , 1 , 1 ,  & 
                       ierr )
IF ( P_qv .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QVAPOR'               , &  ! Data Name 
                       grid%moist_2(ims,kms,jms,P_qv)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QVAPOR memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qv .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QVAPOR'             , &  ! Data Name 
                       grid%g_moist_2(ims,kms,jms,P_qv)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QVAPOR memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qv .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QVAPOR'             , &  ! Data Name 
                       grid%a_moist_2(ims,kms,jms,P_qv)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QVAPOR memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( P_qc .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QCLOUD'               , &  ! Data Name 
                       grid%moist_2(ims,kms,jms,P_qc)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QCLOUD memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qc .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QCLOUD'             , &  ! Data Name 
                       grid%g_moist_2(ims,kms,jms,P_qc)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QCLOUD memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qc .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QCLOUD'             , &  ! Data Name 
                       grid%a_moist_2(ims,kms,jms,P_qc)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QCLOUD memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( P_qr .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QRAIN'               , &  ! Data Name 
                       grid%moist_2(ims,kms,jms,P_qr)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QRAIN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qr .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QRAIN'             , &  ! Data Name 
                       grid%g_moist_2(ims,kms,jms,P_qr)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QRAIN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qr .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QRAIN'             , &  ! Data Name 
                       grid%a_moist_2(ims,kms,jms,P_qr)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QRAIN memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( P_qi .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QICE'               , &  ! Data Name 
                       grid%moist_2(ims,kms,jms,P_qi)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QICE memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qi .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QICE'             , &  ! Data Name 
                       grid%g_moist_2(ims,kms,jms,P_qi)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QICE memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qi .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QICE'             , &  ! Data Name 
                       grid%a_moist_2(ims,kms,jms,P_qi)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QICE memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( P_qs .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QSNOW'               , &  ! Data Name 
                       grid%moist_2(ims,kms,jms,P_qs)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QSNOW memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qs .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QSNOW'             , &  ! Data Name 
                       grid%g_moist_2(ims,kms,jms,P_qs)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QSNOW memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qs .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QSNOW'             , &  ! Data Name 
                       grid%a_moist_2(ims,kms,jms,P_qs)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QSNOW memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ( P_qg .GE. PARAM_FIRST_SCALAR ) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'QGRAUP'               , &  ! Data Name 
                       grid%moist_2(ims,kms,jms,P_qg)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QGRAUP memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF(((grid%dyn_opt .EQ. DYN_EM_TL) .or. (grid%dyn_opt .EQ. DYN_EM_AD)) .and. (P_qg .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'G_QGRAUP'             , &  ! Data Name 
                       grid%g_moist_2(ims,kms,jms,P_qg)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QGRAUP memorder XZY' , & ! Debug message
ids , (ide-1) , kds , (kde-1) , jds , (jde-1) ,  & 
ims , ime , kms , kme , jms , jme ,  & 
ips , MIN( (ide-1), ipe ) , kps , MIN( (kde-1), kpe ) , jps , MIN( (jde-1), jpe ) ,  & 
                       ierr )
END IF
IF ((grid%dyn_opt .EQ. DYN_EM_AD) .and. (P_qg .GE. PARAM_FIRST_SCALAR)) THEN
CALL wrf_ext_read_field (  &
                       fid                , &  ! DataHandle 
                       current_date(1:19) , &  ! DateStr 
                       'A_QGRAUP'             , &  ! Data Name 
                       grid%a_moist_2(ims,kms,jms,P_qg)               , &  ! Field 
                       WRF_real             , &  ! FieldType 
                       grid%communicator  , &  ! Comm
                       grid%iocommunicator  , &  ! Comm
                       grid%domdesc       , &  ! Comm
                       grid%bdy_mask     , &  ! bdy_mask
                       'XZY'               , &  ! MemoryOrder
                       ''               , &  ! Stagger
'inc/wrf_auxhist2in.inc ext_read_field QGRAUP memorder XZY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field FCX memorder C' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field FCX memorder C' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field FCX memorder C' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field GCX memorder C' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field GCX memorder C' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field GCX memorder C' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field DTBC memorder 0' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field DTBC memorder 0' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field DTBC memorder 0' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field LANDMASK memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field TSLB memorder XZY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field ZS memorder Z' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field DZS memorder Z' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field SMOIS memorder XZY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field SH2O memorder XZY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field XICE memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field IVGTYP memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field ISLTYP memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field VEGFRA memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field SNOW memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field SNOWH memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field CANWAT memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field SST memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
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
'inc/wrf_auxhist2in.inc ext_read_field MAPFAC_M memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field MAPFAC_U memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field MAPFAC_V memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field F memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field E memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field SINALPHA memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field COSALPHA memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field HGT memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field TSK memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field XLAT memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field XLONG memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field ALBBCK memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
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
'inc/wrf_auxhist2in.inc ext_read_field TMN memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field XLAND memorder XY' , & ! Debug message
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
'inc/wrf_auxhist2in.inc ext_read_field ZNT memorder XY' , & ! Debug message
ids , (ide-1) , jds , (jde-1) , 1 , 1 ,  & 
ims , ime , jms , jme , 1 , 1 ,  & 
ips , MIN( (ide-1), ipe ) , jps , MIN( (jde-1), jpe ) , 1 , 1 ,  & 
                       ierr )
!ENDOFREGISTRYGENERATEDINCLUDE

    RETURN
    END
