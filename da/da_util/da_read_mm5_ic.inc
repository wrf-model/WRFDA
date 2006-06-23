subroutine read_mm5_ic(input_unit, error)

   implicit none

   integer,               intent(in)  :: input_unit
   integer,               intent(out) :: error

   integer(kind=4) :: flag

   integer :: l, ier, n, i, j, k, nb

   logical :: print_info

   real(kind=4), dimension(:,:,:,:), allocatable :: data

   print_info  = .true.

   nb=5

   do

      read(input_unit, iostat=ier) flag

      if (flag == 0) then
         read(input_unit,iostat=ier) big_header%bhi,  big_header%bhr, &
                                     big_header%bhic, big_header%bhrc

         if(ier/=0) then
            write(*,'("Error reading big header")')
            call abort()
         endif

         if(print_info) &
         call print_big_header(big_header%bhi, big_header%bhr, &
                               big_header%bhic,big_header%bhrc)

         mix = big_header%bhi(16, 1)
         mjx = big_header%bhi(17, 1)
         mkx = big_header%bhi(12, 5)

         allocate(uuu(mix,mjx,mkx))
         allocate(vvv(mix,mjx,mkx))
         allocate(www(mix,mjx,mkx+1))
         allocate(ttt(mix,mjx,mkx))
         allocate(qqq(mix,mjx,mkx))
         allocate(ppp(mix,mjx,mkx))

         allocate(ps_crs(mix,mjx))
         allocate(ps_dot(mix,mjx))

      elseif (flag == 1) then
     
         read(input_unit,iostat=ier) &
              sub_header%ndim, &
              sub_header%start_index, &
              sub_header%end_index, &
              sub_header%current_time, &
              sub_header%staggering, &
              sub_header%ordering, &
              sub_header%current_date, &
              sub_header%name, &
              sub_header%units, &
              sub_header%description

         if(ier/=0) then
            write(*,'("Error reading subheader")')
            call abort()
         endif

         if(print_info) then
            call print_sub_header(sub_header)
         endif

         allocate(data(sub_header%start_index(1):sub_header%end_index(1), &
                       sub_header%start_index(2):sub_header%end_index(2), &
                       sub_header%start_index(3):sub_header%end_index(3), &
                       sub_header%start_index(4):sub_header%end_index(4)))

         read(input_unit) data

         if (sub_header%name ==  'U        ') then
            uuu(:,:,:) = data(:,:,:,1)
         elseif (sub_header%name ==  'V        ') then
            vvv(:,:,:) = data(:,:,:,1)
         elseif (sub_header%name ==  'T        ') then
            call cleanRows(data, sub_header)
            ttt(:,:,:) = data(:,:,:,1)
         elseif (sub_header%name ==  'Q        ') then
            call cleanRows(data, sub_header)
            qqq(:,:,:) = data(:,:,:,1)
         elseif (sub_header%name ==  'W        ') then
            call cleanRows(data, sub_header)
            www(:,:,:) = data(:,:,:,1)
         elseif (sub_header%name ==  'PP       ') then
            call cleanRows(data, sub_header)
            ppp(:,:,:) = data(:,:,:,1)
         elseif (sub_header%name ==  'PSTARCRS ') then
            call cleanRows(data, sub_header)
            ps_crs(:,:) = data(:,:,1,1)

            call crs_2_dot(ps_crs, ps_dot)
         endif

         deallocate(data)

      elseif (flag == 2) then
         error = 0

!--------couple variables here.

         do j=1,mjx
         do k=1,mkx
         do i=1,mix
            uuu(i,j,k)=uuu(i,j,k)*ps_dot(i,j)*0.001
            vvv(i,j,k)=vvv(i,j,k)*ps_dot(i,j)*0.001
            ttt(i,j,k)=ttt(i,j,k)*ps_crs(i,j)*0.001
            qqq(i,j,k)=qqq(i,j,k)*ps_crs(i,j)*0.001
            ppp(i,j,k)=ppp(i,j,k)*ps_crs(i,j)*0.001
         end do
         end do
         end do

         do j=1,mjx
         do k=1,mkx+1
         do i=1,mix
            www(i,j,k)=www(i,j,k)*ps_crs(i,j)*0.001
         end do
         end do
         end do

         return
      else
         stop
      endif

   enddo

   error = 1

   write(*,'(/,"Hit the end of file of unit ", I3)') &
         input_unit

END SUBROUTINE read_mm5_ic
