subroutine da_read_ssmi_integer_array(aa,nn,i0,i_step)

  integer,                 intent(in) :: nn, i0, i_step
  integer,  dimension(:), intent(out) :: aa

  integer     :: i, ii

  do i = 1, nn
    ii = i_step*(i-1) + i0
    READ (UNIT=char_ned (ii:ii+8),FMT='((I7,2X))') aa(i)
  enddo
end subroutine da_read_ssmi_integer_array
