subroutine DA_print_sub_header(sub_header)

   implicit none

   type (sub_record_header), intent(in) :: sub_header

!  write(UNIT=stdout, fmt='(a, i5)')  'ndim:        ', sub_header%ndim
!  write(UNIT=stdout, fmt='(a,4i5)')  'start_index: ', sub_header%start_index
!  write(UNIT=stdout, fmt='(a,4i5)')  'end_index:   ', sub_header%end_index
!  write(UNIT=stdout, fmt='(a,f8.1)') 'time:        ', sub_header%current_time
!  write(UNIT=stdout, fmt='(3a)')     'staggering: <', sub_header%staggering, '>'
!  write(UNIT=stdout, fmt='(3a)')     'ordering:   <', sub_header%ordering, '>'
!  write(UNIT=stdout, fmt='(3a)')     'date/time:  <', sub_header%current_date, '>'
!  write(UNIT=stdout, fmt='(3a)')     'name:       <', sub_header%name, '>'
!  write(UNIT=stdout, fmt='(3a)')     'units:      <', sub_header%units, '>'
!  write(UNIT=stdout, fmt='(3a)')     'description:<', sub_header%description, '>'

   write(UNIT=stdout, fmt='(i2,4i4,f8.1,6(2x,a))') &
         sub_header%ndim, &
         sub_header%end_index(1)-sub_header%start_index(1)+1, &
         sub_header%end_index(2)-sub_header%start_index(2)+1, &
         sub_header%end_index(3)-sub_header%start_index(3)+1, &
         sub_header%end_index(4)-sub_header%start_index(4)+1, &
         sub_header%current_time, &
         sub_header%staggering, &
         sub_header%ordering, &
         sub_header%current_date, &
         sub_header%name, &
         sub_header%units

!         sub_header%description

end subroutine DA_print_sub_header
