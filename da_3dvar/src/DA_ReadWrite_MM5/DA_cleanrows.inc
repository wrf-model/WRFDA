subroutine DA_cleanrows(data, sub_header)

   implicit none

   real(kind=4), dimension(:, :, :, :), intent(inout) :: data
   type (sub_record_header),            intent(in)    :: sub_header

   integer :: i,j,k,l,ib,jb,ie,je

   if(sub_header%ndim > 1) then
      if(sub_header%staggering(1:1) == 'c' .or. &
         sub_header%staggering(1:1) == 'C') then

!        write(unit=*, fmt='(5a,i6,a,8i4)') &
!             'call DA_cleanrows for <', sub_header%name, &
!             '>    sub_header%staggering(1:1)=<', sub_header%staggering(1:1), &
!             '>    sub_header%ndim=', sub_header%ndim, &
!             '     dimensions:', sub_header%end_index(1), sub_header%end_index(2)

         ib = sub_header%start_index(1)
         jb = sub_header%start_index(2)

         ie = sub_header%end_index(1)
         je = sub_header%end_index(2)

         do l=sub_header%start_index(4),sub_header%end_index(4)
         do k=sub_header%start_index(3),sub_header%end_index(3)
!!!            do j=ib,je
            do j=jb,je
               data(ie,j,k,l)=data(ie-1,j,k,l)
            end do
    
            do i=ib,ie
               data(i,je,k,l)=data(i,je-1,k,l)
            end do
         end do
         end do
      end if
   end if

end subroutine DA_cleanrows

