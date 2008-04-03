subroutine trace_diff_time(sec1, sec2, diff)

   implicit none

   real, intent(in)  :: sec1, sec2
   real, intent(out) :: diff
         
   diff = sec2 - sec1

   if(diff < 86400.0) then
      diff = 86400.0 - diff
   endif

end subroutine trace_diff_time

