subroutine trace_diff_time(sec1, sec2, diff)

   implicit none

   real, intent(in)  :: sec1, sec2
   real, intent(out) :: diff
         
   diff = sec2 - sec1

   do while (diff < 0.0)
      diff = diff + 86400.0
   enddo

end subroutine trace_diff_time

