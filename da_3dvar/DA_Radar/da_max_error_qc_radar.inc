subroutine da_max_error_qc_Radar (it, info, field, max_error, ix, ixf, iunit, var, &
                                  failed, print)

  IMPLICIT NONE

  INTEGER          ,   INTENT (IN)   :: it
  TYPE (info_type) ,   INTENT(in)    :: info
  TYPE (field_type),   INTENT(inout) :: field
  REAL             ,   INTENT(in)    :: max_error
  INTEGER          ,   INTENT(inout) :: ix, ixf
  INTEGER          ,   INTENT(in)    :: iunit
  CHARACTER (LEN=*),   INTENT(in)    :: var
  LOGICAL          ,   INTENT(out)   :: failed
  LOGICAL          ,   INTENT(in)    :: print

  REAL                               :: err, err_max
  INTEGER                            :: qc_flag
 
  failed = .false.
 
     qc_flag = field % qc
     err_max = field % error * max_error
     err     = field % inv
     err     = ABS (err)

     ix     = ix + 1
     if( it > 1 .and. qc_flag == fails_error_max ) field%qc = 0
     IF (err > err_max) THEN
       if (field % qc > fails_error_max) field % qc = fails_error_max 
!rizvi  field % inv = 0.0

        ixf = ixf + 1
        failed = .TRUE.

        IF (print .and. failed) THEN
           IF (err_max .LE. 0.) THEN
              WRITE (iunit , FMT = '(A,3(F12.1,1X),A,A,A,A,A,3f10.2)')   &
                     "Err_max < 0 ==> ",err,err_max,max_error, " for ", var, &
                     " OBS ID: ", info % platform,     &
                     " LA/LON/ELV:", info % lat, info % lon, info % elv
!              STOP
           ENDIF

           WRITE (iunit , FMT = '(A,A,A,I5,A,I5,A,F4.1,A,A,A,2F12.1)') &
                  "Err_max failed:ID=", info % platform,&
                  "  Ix=", ix, "  Ixf=", ixf, " Err_max ratio =",err/err_max, &
                  " for ", var, " inv, error:",field % inv, field % error
        ENDIF
!rizvi
        field % inv = 0.0
     ENDIF

end subroutine da_max_error_qc_Radar

