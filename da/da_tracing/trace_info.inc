subroutine trace_info( name,     &           ! in
                       message)              ! in, optional

   implicit none

!--------------------------------------------------------------------
!--Purpose: General trace within a subroutine
!--------------------------------------------------------------------

   character (len=*), intent(in)           :: name         ! Subroutine name
   character (len=*), optional, intent(in) :: message      ! Text to trace

   integer           :: IOStatus     ! I-O return code
   integer           :: Loop         ! General loop counter
   integer           :: TotalSpace
   character(len=25) :: Change

!-----------------------------------------------------------------------
!--Check depth of trace
!-----------------------------------------------------------------------

   if (TraceDepth > TraceMaxDepth) then
      write(unit=0, fmt='(2a)') 'Reached TraceMaxDepth for routine: ', trim(name)
      return
   end if

end subroutine trace_info

