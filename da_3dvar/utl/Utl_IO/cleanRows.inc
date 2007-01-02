subroutine cleanRows(data, sub_header)

   implicit none

   real(kind=4), dimension(:, :, :, :), intent(inout) :: data
   type (sub_record_header),            intent(in)    :: sub_header

   integer :: i,j,k,l

   integer :: ib, ie, jb, je, kb, ke, lb, le

   ie = size(data, dim=1)
   je = size(data, dim=2)
   ke = size(data, dim=3)
   le = size(data, dim=4)

   if(ie /= mix .or. je /= mjx) return

   ib = 1
   jb = 1
   kb = 1
   lb = 1

!  ie = sub_header%end_index(1)
!  je = sub_header%end_index(2)
!  ke = sub_header%end_index(3)
!  le = sub_header%end_index(4)

!  ib = sub_header%start_index(1)
!  jb = sub_header%start_index(2)
!  kb = sub_header%start_index(3)
!  lb = sub_header%start_index(4)

!  write(unit=*, fmt='(3a,5(2x,a,2i5))') &
!       'Clean Field <', sub_header%name, '>', &
!       'ib,ie=', ib, ie, &
!       'jb,je=', jb, je, &
!       'kb,ke=', kb, ke, &
!       'lb,le=', lb, le, &
!       'iy,jx=', mix, mjx

   if(sub_header%ndim > 1) then
      if(sub_header%staggering(1:1) == 'c' .or. &
         sub_header%staggering(1:1) == 'C') then

         do l=lb,le
         do k=kb,ke
            do j=jb,je-1
               data(ie,j,k,l)=data(ie-1,j,k,l)
            end do
 
            do i=ib,ie
               data(i,je,k,l)=data(i,je-1,k,l)
            end do
         end do
         end do
      end if
   end if

end subroutine cleanRows
