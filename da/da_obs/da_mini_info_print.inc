SUBROUTINE DA_Mini_Info_print(print_unit, oi_each, re_each, gr_each, k)

   implicit none

   TYPE (each_level_type), intent(in) :: oi_each
   TYPE (residual_type),   intent(in) :: re_each, gr_each
   INTEGER,                intent(in) :: print_unit, k

   write(unit=print_unit, fmt='(a,i3,8e12.4)') &
        '  oi  ', k, &
        oi_each%u%inv, oi_each%v%inv, &
        oi_each%t%inv, oi_each%p%inv, oi_each%q%inv

   write(unit=print_unit, fmt='(a,i3,8e12.4)') &
        '  re  ', k, &
        re_each%u, re_each%v, re_each%t, re_each%p, re_each%q

   write(unit=print_unit, fmt='(a,i3,8e12.4)') &
        ' grad ', k, &
        gr_each%u, gr_each%v, gr_each%t, gr_each%p, gr_each%q

END SUBROUTINE DA_Mini_Info_print

