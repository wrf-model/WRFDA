subroutine da_check_max_iv_gpspw(it, gpspw, ittpw,ittpwf)

  !-------------------------------------------------------------------------
  !
  !  History:
  !
  !     Additions:        02/26/2003                    S. R. H. Rizvi
  !
  !            outer iteration and extra counters are added in the  argument
  !            list for generating detailed information about rejected obs in
  !            various outer iterations
  !
  !-------------------------------------------------------------------------

  implicit none

  integer,           intent (in)         :: it      ! Outer iteration
  type (gpspw_type), intent(inout)       :: gpspw
  integer, intent(inout) :: ittpw,ittpwf
  logical                                :: failed 

  if ( check_max_iv_print )then
    write (check_max_iv_unit,'(A)')  &
      '----------------------------------------------------------------'
    write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'
    write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
     'Error max test ratio for gps_tpw = ',max_error_pw
  end if

  !-----------------------------------------------------------------------------!
  ! [2.0] Perform maximum innovation vector check:
  !-----------------------------------------------------------------------------!

  failed = .false.

  ! TPW:
  call da_max_error_qc(it, gpspw % info, gpspw % tpw, max_error_pw, ittpw, &
    ittpwf, check_max_iv_unit, 'TPW  ', failed, check_max_iv_print)

end subroutine da_check_max_iv_gpspw

