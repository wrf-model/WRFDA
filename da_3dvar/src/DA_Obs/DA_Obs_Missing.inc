subroutine DA_Obs_missing (obs, miss)

  IMPLICIT NONE

  TYPE(multi_level_type),    intent(inout) :: obs
  LOGICAL,                   intent(out)   :: miss

  INTEGER            :: i

  MISS = .TRUE.

! PW:
  if (abs(obs % loc % pw  % inv - missing_r) > 1.0) then
      MISS = .false.
  else
      if (obs % loc % pw  % qc /= missing_data) &
                     obs % loc % pw  % qc = missing_data
  endif

! SLP:
  if (abs(obs % loc % slp % inv - missing_r) > 1.0) then
      MISS = .false.
  else
      if (obs % loc % slp % qc /= missing_data) &
                     obs % loc % slp % qc = missing_data
  endif

loop_levels: do i = 1, obs % info % levels

! U:  
  if (abs(obs % each(i) % u    % inv - missing_r) > 1.0) then
      MISS = .false.
  else
      if (obs % each(i) % u    % qc /= missing_data) &
                     obs % each(i) % u    % qc = missing_data
  endif

! V:
  if (abs(obs % each(i) % v    % inv - missing_r) > 1.0) then
      MISS = .false.
  else
      if (obs % each(i) % v    % qc /= missing_data) &
                     obs % each(i) % v    % qc = missing_data
  endif

! P:
  if (abs(obs % each(i) % p    % inv - missing_r) > 1.0) then
      MISS = .false.
  else
      if (obs % each(i) % p    % qc /= missing_data) &
                     obs % each(i) % p    % qc = missing_data
  endif

! T:
  if (abs(obs % each(i) % t    % inv - missing_r) > 1.0) then
      MISS = .false.
  else
      if (obs % each(i) % t    % qc /= missing_data) &
                     obs % each(i) % t    % qc = missing_data
  endif

! Q:
  if (abs(obs % each(i) % q    % inv - missing_r) > 1.0) then
      MISS = .false.
  else
      if (obs % each(i) % q    % qc /= missing_data) &
                     obs % each(i) % q    % qc = missing_data
  endif
  
! RH:
  if (abs(obs % each(i) % rh  % inv - missing_r) > 1.0) then
      MISS = .false.
  else
      if (obs % each(i) % rh  % qc /= missing_data) &
                     obs % each(i) % rh  % qc = missing_data
  endif

! TD:
  if (abs(obs % each(i) % td   % inv - missing_r) > 1.0) then
      MISS = .false.
  else
      if (obs % each(i) % td   % qc /= missing_data) &
                     obs % each(i) % td   % qc = missing_data
  endif

! SPEED:
  if (abs(obs % each(i) % speed % inv - missing_r) > 1.0) then
      MISS = .false.
  else
      if (obs % each(i) % speed % qc /= missing_data) &
                     obs % each(i) % speed % qc = missing_data
  endif
 
enddo loop_levels
 
end subroutine DA_Obs_missing
