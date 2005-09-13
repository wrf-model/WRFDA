SUBROUTINE wrf_bdyin ( fid , grid , config_flags , switch , ierr )
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

    RETURN
    END
