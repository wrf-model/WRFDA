subroutine make_large(sub_header_small, sub_header_large, &
                      data_small, iy, jx, iy_large, jx_large, &
                      lliy, lljx, output_unit)

   implicit none

   type (sub_record_header),         intent(inout) :: sub_header_large, &
                                                      sub_header_small
   real(kind=4), dimension(:,:,:,:), intent(in)    :: data_small
   integer,                          intent(in)    :: iy, iy_large, lliy
   integer,                          intent(in)    :: jx, jx_large, lljx
   integer,                          intent(in)    :: output_unit


   real(kind=4), dimension(:,:,:,:), allocatable   :: data_large

   integer                                         :: i, j, k
   logical                                         :: print_info




   print_info = .true.


   if(sub_header_small%ndim > 1  .and. &
      sub_header_small%end_index(1) == iy) then

      if(sub_header_small%end_index(2) == jx) then
         do k=1,2
            sub_header_small%start_index(k)=sub_header_large%start_index(k)
            sub_header_small%end_index(k)  =sub_header_large%end_index(k)
         enddo

         if(sub_header_small%ndim == 2) then
            do k=3,4
               sub_header_small%start_index(k)=1
               sub_header_small%end_index(k)  =1
            end do
         end if
      else if(sub_header_small%end_index(2) == mkx) then
         sub_header_small%start_index(1)=sub_header_large%start_index(1)
         sub_header_small%end_index(1)  =sub_header_large%end_index(1)

         sub_header_small%start_index(2)=1
         sub_header_small%end_index(2)  =mkx

!        do k=3,4
!           sub_header_small%start_index(k)=1
!           sub_header_small%end_index(k)  =1
!        end do

!        if(sub_header_small%ndim == 3) then
!           sub_header_small%start_index(3)=1
!           sub_header_small%end_index(3)  =mkx
!        end if
      end if
   endif

   if(print_info) then
      call print_sub_header(sub_header_small)

      write(unit=*, fmt='(a,e24.14)') &
           'Sample value:', data_small(1,1,1,1)
   end if

   allocate(data_large(sub_header_small%start_index(1):sub_header_small%end_index(1), &
                       sub_header_small%start_index(2):sub_header_small%end_index(2), &
                       sub_header_small%start_index(3):sub_header_small%end_index(3), &
                       sub_header_small%start_index(4):sub_header_small%end_index(4)))

   data_large = 0.0

   if(index(sub_header_small%name, '_EVALG') > 0 .or. &
      index(sub_header_small%name, '_EVECG') > 0) then
      data_large = data_small
   else if(sub_header_small%end_index(1) == iy_large) then
      if(sub_header_small%end_index(2) == jx_large ) then
         do k=sub_header_small%start_index(3), sub_header_large%end_index(3)
            do j=1,jx
               do i=1,iy
                  data_large(i+lliy,j+lljx,k,1) = data_small(i,j,k,1)
               end do
  
            
               do i=1,lliy
                  data_large(i,j+lljx,k,1) = data_small(1,j,k,1)
               end do
 
               do i=iy+lliy+1,iy_large
                  data_large(i,j+lljx,k,1) = data_small(iy,j,k,1)
               end do
            end do

            do j=1,lljx
               do i=1,iy_large
                  data_large(i,j,k,1) = data_large(i,1+lljx,k,1)
               end do
            end do

            do j=jx+lljx+1,jx_large
               do i=1,iy_large
                  data_large(i,j,k,1) = data_large(i,jx+lljx,k,1)
               end do
            end do
         end do
      else if(sub_header_small%end_index(2) == mkx) then
            write(unit=*, fmt='(2a, 8i4)') &
                  sub_header_small%name, &
                  '   index=', size(data_large, dim=1), &
                               size(data_large, dim=2), &
                               size(data_large, dim=3), &
                               size(data_large, dim=4), &
                               size(data_small, dim=1), &
                               size(data_small, dim=2), &
                               size(data_small, dim=3), &
                               size(data_small, dim=4)

         write(unit=*, fmt='(a,3i4)') &
              'iy,lliy,iy_large=', iy,lliy,iy_large

         write(unit=*, fmt='(a,2i4)') &
              'start_index(2)=', sub_header_small%start_index(2), sub_header_small%end_index(2), &
              'start_index(3)=', sub_header_small%start_index(3), sub_header_small%end_index(3)


         do k=sub_header_small%start_index(3), sub_header_small%end_index(3)

            do j=sub_header_small%start_index(2), sub_header_small%end_index(2)

               do i=1,iy
                  data_large(i+lliy,j,k,1) = data_small(i,j,k,1)
               end do
           
               do i=1,lliy+1
                  data_large(i,j,k,1) = data_small(2,j,k,1)
               end do

               do i=iy+lliy-2,iy_large
                  data_large(i,j,k,1) = data_large(i-1,j,k,1)
               end do
            end do
         end do

         do i=iy_large,1,-1
            if(i > lliy .and. i < iy+lliy ) then
               write(unit=*, fmt='(a, i4, 2e20.8)') 'i,data=', i, data_large(i,1,1,1), data_small(i-lliy,1,1,1)
            else
               write(unit=*, fmt='(a, i4, e20.8)') 'i,data=', i, data_large(i,1,1,1)
            end if
         end do

      else
         write(unit=*, fmt='(a)') 'Something is wrong.'
         stop 'check dimensions.'
      end if
   else
      data_large = data_small
   end if

   if(index(sub_header_small%name, '_EVALG') > 0) then
      sub_header_small%ndim = 1
      sub_header_small%staggering = 'M'
      sub_header_small%ordering   = 'M  '
   else if(index(sub_header_small%name, '_EVECG') > 0) then
      sub_header_small%ndim = 2
      sub_header_small%staggering = 'K'
      sub_header_small%ordering   = 'KM '
   else if(index(sub_header_small%name, '_EVALL') > 0) then
      sub_header_small%staggering = 'Y'
      sub_header_small%ordering   = 'YM'
   else if(index(sub_header_small%name, '_EVECL') > 0) then
      sub_header_small%staggering = 'Y'
      sub_header_small%ordering   = 'YKM'
   else if(index(sub_header_small%name, 'PB_REG') > 0) then
      sub_header_small%staggering = 'Y'
      sub_header_small%ordering   = 'YKM'
   end if

   write(output_unit) &
         sub_header_small%ndim, &
         sub_header_small%start_index, &
         sub_header_small%end_index, &
         sub_header_small%current_time, &
         sub_header_small%staggering, &
         sub_header_small%ordering, &
         sub_header_small%current_date, &
         sub_header_small%name, &
         sub_header_small%units, &
         sub_header_small%description

   write(output_unit) data_large

   deallocate(data_large)

end subroutine make_large

