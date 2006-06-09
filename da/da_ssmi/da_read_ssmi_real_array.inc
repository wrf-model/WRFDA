subroutine da_read_ssmi_real_array(aa,nn,i0,i_step)

  integer,                 intent(in) :: nn, i0, i_step
  real   ,  dimension(:), intent(out) :: aa

  integer     :: i, ii

  do i = 1, nn
    ii = i_step*(i-1) + i0
    READ (UNIT=char_ned (ii:ii+8),FMT='((f7.2,2X))') aa(i)
  enddo
end subroutine da_read_ssmi_real_array
