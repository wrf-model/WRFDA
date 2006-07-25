!WRF:MEDIATION:IO
  SUBROUTINE wrf_ext_read_field( DataHandle,DateStr,Var,Field,FieldType,Comm,IOComm, &
                                 DomainDesc, bdy_mask, MemoryOrder,Stagger,             &
                                 debug_message ,                              &
                                 ds1, de1, ds2, de2, ds3, de3,                &
                                 ms1, me1, ms2, me2, ms3, me3,                &
                                 ps1, pe1, ps2, pe2, ps3, pe3, Status          )
    USE module_io
    USE module_wrf_error
#ifdef WRFVAR
    USE module_state_description
#endif
    IMPLICIT NONE

    integer                                      :: DataHandle
    character*(*)                                :: DateStr
    character*(*)                                :: Var
    integer                                      :: Field(*)
    integer                                      :: FieldType
    integer                                      :: Comm
    integer                                      :: IOComm
    integer                                      :: DomainDesc
    logical, dimension(4)                        :: bdy_mask
    character*(*)                                :: MemoryOrder
    character*(*)                                :: Stagger
    character*(*)                                :: debug_message

    INTEGER ,       INTENT(IN   ) :: ds1, de1, ds2, de2, ds3, de3, &
                                     ms1, me1, ms2, me2, ms3, me3, &
                                     ps1, pe1, ps2, pe2, ps3, pe3

    INTEGER , DIMENSION(3) :: domain_start , domain_end
    INTEGER , DIMENSION(3) :: memory_start , memory_end
    INTEGER , DIMENSION(3) :: patch_start , patch_end
    CHARACTER*80 , DIMENSION(3) :: dimnames
    INTEGER l
#ifdef WRFVAR
    INTEGER Hndl, Hopened, dyn_opt
    LOGICAL for_out
    CHARACTER*20 DataSet
#endif

    integer                       ,intent(inout)   :: Status

    domain_start(1) = ds1 ; domain_end(1) = de1 ;
    patch_start(1)  = ps1 ; patch_end(1)  = pe1 ;
    memory_start(1) = ms1 ; memory_end(1) = me1 ;
    domain_start(2) = ds2 ; domain_end(2) = de2 ;
    patch_start(2)  = ps2 ; patch_end(2)  = pe2 ;
    memory_start(2) = ms2 ; memory_end(2) = me2 ;
    domain_start(3) = ds3 ; domain_end(3) = de3 ;
    patch_start(3)  = ps3 ; patch_end(3)  = pe3 ;
    memory_start(3) = ms3 ; memory_end(3) = me3 ;

    CALL debug_io_wrf ( debug_message,DateStr,                          &
                        domain_start,domain_end,patch_start,patch_end,  &
                        memory_start,memory_end                          )

#ifdef WRFVAR
    CALL get_handle ( Hndl, Hopened, for_out, DataSet, DataHandle )

    CALL nl_get_dyn_opt( 1, dyn_opt )

    l = min(len(trim(var)),2)

    IF      ( dyn_opt .EQ. dyn_em     ) THEN
      IF (                                        VAR(1:l) .EQ. 'A_' .OR. VAR(1:l) .EQ. 'G_'  ) RETURN
    ELSE IF ( dyn_opt .EQ. dyn_em_tl ) THEN
      IF ( TRIM(DataSet) .EQ. 'INPUT'     .AND.   VAR(1:l) .EQ. 'A_'                          ) RETURN
      IF ( TRIM(DataSet) .EQ. 'AUXINPUT2' .AND. ( VAR(1:l) .EQ. 'A_' .OR. VAR(1:l) .EQ. 'G_' )) RETURN
      IF ( TRIM(DataSet) .EQ. 'AUXINPUT3' .AND. ( VAR(1:l) .EQ. 'A_' .OR. VAR(1:l) .EQ. 'G_' )) RETURN
      IF ( TRIM(DataSet) .EQ. 'HISTORY'   .AND.   VAR(1:l) .EQ. 'A_'                          ) RETURN
      IF ( TRIM(DataSet) .EQ. 'AUXHIST3'  .AND.   VAR(1:l) .EQ. 'A_'                          ) RETURN
    ELSE IF ( dyn_opt .EQ. dyn_em_ad ) THEN
      IF ( TRIM(DataSet) .EQ. 'AUXINPUT2' .AND. ( VAR(1:l) .EQ. 'A_' .OR. VAR(1:l) .EQ. 'G_' )) RETURN
      IF ( TRIM(DataSet) .EQ. 'AUXINPUT3' .AND. ( .NOT.  VAR(1:l) .EQ. 'G_'                  )) RETURN
    ENDIF
#endif

    Status = 1
    if ( de1 - ds1 < 0 ) return
    if ( de2 - ds2 < 0 ) return
    if ( de3 - ds3 < 0 ) return
    if ( pe1 - ps1 < 0 ) return
    if ( pe2 - ps2 < 0 ) return
    if ( pe3 - ps3 < 0 ) return
    if ( me1 - ms1 < 0 ) return
    if ( me2 - ms2 < 0 ) return
    if ( me3 - ms3 < 0 ) return
    Status = 0

    CALL wrf_read_field (   &
                       DataHandle                 &  ! DataHandle
                      ,DateStr                    &  ! DateStr
                      ,Var                        &  ! Data Name
                      ,Field                      &  ! Field
                      ,FieldType                  &  ! FieldType
                      ,Comm                       &  ! Comm
                      ,IOComm                     &  ! IOComm
                      ,DomainDesc                 &  ! DomainDesc
                      ,bdy_mask                   &  ! bdy_mask
                      ,MemoryOrder                &  ! MemoryOrder
                      ,Stagger                    &  ! Stagger
                      ,dimnames                   &  ! JMMOD 1109
                      ,domain_start               &  ! DomainStart
                      ,domain_end                 &  ! DomainEnd
                      ,memory_start               &  ! MemoryStart
                      ,memory_end                 &  ! MemoryEnd
                      ,patch_start                &  ! PatchStart
                      ,patch_end                  &  ! PatchEnd
                      ,Status )
    IF ( wrf_at_debug_level(300) ) THEN
      WRITE(wrf_err_message,*) debug_message,' Status = ',Status
      CALL wrf_message ( TRIM(wrf_err_message) )
    ENDIF

  END SUBROUTINE wrf_ext_read_field

