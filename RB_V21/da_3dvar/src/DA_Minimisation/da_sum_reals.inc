subroutine da_sum_reals( n, val1, val2, val3, val4, val5, val6, val7 )

   integer, intent(in)             :: n
   real, intent(inout)             :: val1, val2, val3, val4, val5, val6, val7

   real                            :: val(1:7)

   val(:) = 0.0

   val(1) = val1
   if ( n > 1 ) val(2) = val2
   if ( n > 2 ) val(3) = val3
   if ( n > 3 ) val(4) = val4
   if ( n > 4 ) val(5) = val5
   if ( n > 5 ) val(6) = val6
   if ( n > 6 ) val(7) = val7

   call proc_sum_real( 7, val )

   val1 = val(1)
   val2 = val(2)
   val3 = val(3)
   val4 = val(4)
   val5 = val(5)
   val6 = val(6)
   val7 = val(7)
         
end subroutine da_sum_reals

