program generate_difference

   use record_header
   use module_io

   implicit none

   type (big_record_header) :: big_header_one, big_header_two
   type (sub_record_header) :: sub_header_one, sub_header_two

   integer, parameter :: input_unit_1 = 11, &
                         input_unit_2 = 12, &
                         output_unit  = 20

   integer :: ier, flag, flag_2, &
              miy_one, mjx_one, miy_two, mjx_two, &
              m, write_index

   real(kind=4), dimension(:,:,:,:), allocatable :: data_one, data_two

   logical :: it_is_3dvar_analysis

   do
      read(input_unit_1, iostat=ier) flag

      if(ier/=0) then
         print *, 'ier 1=', ier

         exit
      end if

      if (flag == 0) then
         read(input_unit_1,iostat=ier) big_header_one%bhi,  big_header_one%bhr, &
                                       big_header_one%bhic, big_header_one%bhrc

         if(ier/=0) then
            write(*,'("Error reading big header one domain")')
            call abort()
         endif

!        call print_big_header(big_header_one%bhi, big_header_one%bhr, &
!                              big_header_one%bhic,big_header_one%bhrc)

         miy_one=big_header_one%bhi(16,1)
         mjx_one=big_header_one%bhi(17,1)

         read(input_unit_2, iostat=ier) flag_2

         if(ier/=0) then
            print *, 'ier 2=', ier

            exit
         end if
 
         write(output_unit) flag_2

         read(input_unit_2,iostat=ier) big_header_two%bhi,  big_header_two%bhr, &
                                       big_header_two%bhic, big_header_two%bhrc

         it_is_3dvar_analysis = .false.

         if(index(big_header_two%bhic(1,1), '3DVAR') > 0) it_is_3dvar_analysis = .true.

         if(ier/=0) then
            write(*,'("Error reading big header one domain")')
            call abort()
         endif

         call print_big_header(big_header_two%bhi, big_header_two%bhr, &
                               big_header_two%bhic,big_header_two%bhrc)

         miy_two=big_header_two%bhi(16,1)
         mjx_two=big_header_two%bhi(17,1)

         if(miy_one /= miy_two .or. mjx_one /= mjx_two) then
            write(unit=*, fmt='(2(a,i6)/)') &
                 'miy_one=', miy_one, 'mjx_one=', mjx_one, &
                 'miy_two=', miy_two, 'mjx_two=', mjx_two

            stop 'Files did not match. Stop'
         end if

         write(output_unit) big_header_two%bhi,  big_header_two%bhr, &
                            big_header_two%bhic, big_header_two%bhrc

      elseif (flag == 1) then
         read(input_unit_1,iostat=ier) &
              sub_header_one%ndim, &
              sub_header_one%start_index, &
              sub_header_one%end_index, &
              sub_header_one%current_time, &
              sub_header_one%staggering, &
              sub_header_one%ordering, &
              sub_header_one%current_date, &
              sub_header_one%name, &
              sub_header_one%units, &
              sub_header_one%description

         allocate(data_one(sub_header_one%start_index(1):sub_header_one%end_index(1), &
                           sub_header_one%start_index(2):sub_header_one%end_index(2), &
                           sub_header_one%start_index(3):sub_header_one%end_index(3), &
                           sub_header_one%start_index(4):sub_header_one%end_index(4)))

         read(input_unit_1) data_one

         call cleanRows(data_one, sub_header_one)

         if(it_is_3dvar_analysis) then
            if(sub_header_one%name == 'TSFC     ' .or. &
               sub_header_one%name == 'HSFC     ' .or. &
               sub_header_one%name == 'USFC     ' .or. &
               sub_header_one%name == 'VSFC     ' .or. &
               sub_header_one%name == 'RHSFC    ' .or. &
               sub_header_one%name == 'TSEASFC  ' .or. &
               sub_header_one%name == 'LATITDOT ' .or. &
               sub_header_one%name == 'LONGIDOT ' .or. &
               sub_header_one%name == 'PSEALVLC ' .or. &
               sub_header_one%name == 'PSEALVLD ') then

               deallocate(data_one)

               cycle
            end if
         end if

         read(input_unit_2, iostat=ier) flag_2

         if(ier/=0) then
            print *, 'ier 2=', ier

            exit
         end if
 
         write(output_unit) flag_2

         read(input_unit_2,iostat=ier) &
              sub_header_two%ndim, &
              sub_header_two%start_index, &
              sub_header_two%end_index, &
              sub_header_two%current_time, &
              sub_header_two%staggering, &
              sub_header_two%ordering, &
              sub_header_two%current_date, &
              sub_header_two%name, &
              sub_header_two%units, &
              sub_header_two%description

         allocate(data_two(sub_header_two%start_index(1):sub_header_two%end_index(1), &
                           sub_header_two%start_index(2):sub_header_two%end_index(2), &
                           sub_header_two%start_index(3):sub_header_two%end_index(3), &
                           sub_header_two%start_index(4):sub_header_two%end_index(4)))

         read(input_unit_2) data_two

         call cleanRows(data_two, sub_header_two)

         write_index = 0

         do m=1,4
            if(sub_header_one%start_index(m) /= sub_header_two%start_index(m)) then
               write(unit=*, fmt='(2(a,i1,a,i6,5x)/)') &
                    'sub_header_one%start_index(',m,')=', sub_header_one%start_index(m), &
                    'sub_header_two%start_index(',m,')=', sub_header_two%start_index(m)

               write_index = write_index + 1

!              stop 'Wrong index'
            end if

            if(sub_header_one%end_index(m) /= sub_header_two%end_index(m)) then
               write(unit=*, fmt='(2(a,i1,a,i6,5x)/)') &
                    'sub_header_one%end_index(',m,')=', sub_header_one%end_index(m), &
                    'sub_header_two%end_index(',m,')=', sub_header_two%end_index(m)

               write_index = write_index + 1

!              stop 'Wrong index'
            end if
         end do

         write(output_unit) &
               sub_header_two%ndim, &
               sub_header_two%start_index, &
               sub_header_two%end_index, &
               sub_header_two%current_time, &
               sub_header_two%staggering, &
               sub_header_two%ordering, &
               sub_header_two%current_date, &
               sub_header_two%name, &
               sub_header_two%units, &
               sub_header_two%description

         if(sub_header_two%name /= 'PSTARCRS ' .and. &
            sub_header_two%name /= 'GROUND T ' .and. &
!           sub_header_two%name /= 'TERRAIN  ' .and. &
            sub_header_two%name /= 'MAPFACCR ' .and. &
            sub_header_two%name /= 'MAPFACDT ' .and. &
            sub_header_two%name /= 'CORIOLIS ' .and. &
            sub_header_two%name /= 'LATITCRS ' .and. &
            sub_header_two%name /= 'LONGICRS ' .and. &
            sub_header_two%name /= 'LATITDOT ' .and. &
            sub_header_two%name /= 'LONGIDOT ' .and. &
            sub_header_two%name /= 'PRESSURE ' .and. &
            sub_header_two%name /= 'SNOWCOVR ' .and. &
            sub_header_two%name /= 'SIGMAH   ' .and. &
            write_index == 0) then
            data_two(:,:,:,:)=data_two(:,:,:,:)-data_one(:,:,:,:)
         end if

         call print_sub_header(sub_header_one)
         call print_sub_header(sub_header_two)

         write(output_unit) data_two

         deallocate(data_one)
         deallocate(data_two)

      elseif (flag == 2) then

         do
            read(input_unit_2, iostat=ier) flag_2

            if(ier /= 0 .or. flag_2 == 2) then
               print *, 'ier  2=', ier
               print *, 'flag_2=', flag_2

               exit
            end if

            read(input_unit_2,iostat=ier) &
                 sub_header_two%ndim, &
                 sub_header_two%start_index, &
                 sub_header_two%end_index, &
                 sub_header_two%current_time, &
                 sub_header_two%staggering, &
                 sub_header_two%ordering, &
                 sub_header_two%current_date, &
                 sub_header_two%name, &
                 sub_header_two%units, &
                 sub_header_two%description

            allocate(data_two(sub_header_two%start_index(1):sub_header_two%end_index(1), &
                              sub_header_two%start_index(2):sub_header_two%end_index(2), &
                              sub_header_two%start_index(3):sub_header_two%end_index(3), &
                              sub_header_two%start_index(4):sub_header_two%end_index(4)))

            read(input_unit_2) data_two

            call cleanRows(data_two, sub_header_two)

            deallocate(data_two)
         end do
 
         write(output_unit) flag

         cycle
      else
         stop
      endif

   enddo

end program generate_difference
