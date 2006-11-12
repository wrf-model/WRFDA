subroutine da_read_ssmi_real_array(aa,nn,i0,i_step)

  integer,                 intent(in) :: nn, i0, i_step
  real   ,  dimension(:), intent(out) :: aa

  integer     :: i, ii

  do i = 1, nn
    ii = i_step*(i-1) + i0
    read (unit=char_ned (ii:ii+8),fmt='((f7.2,2X))') aa(i)
  end do
end subroutine da_read_ssmi_real_array
