program to_be_file

   use record_header
   use module_io
   use module_trans

   implicit none

   type (big_record_header) :: big_header_large, big_header_small
   type (sub_record_header) :: sub_header_large, sub_header_small

   integer, parameter :: input_unit_1 = 11, &
                         input_unit_2 = 12, &
                         output_unit  = 20, &
                          print_unit  = 40

   integer :: ier, flag, lliy, lljx, miy_large, mjx_large, miy_small, mjx_small, &
              miy_large_coarse, mjx_large_coarse, miy_small_coarse, mjx_small_coarse, &
              lliy_large, lljx_large, lliy_small, lljx_small, i, j, k, one

   integer :: idx, vert_corr

   real(kind=4), dimension(:,:,:,:), allocatable :: data_large, data_small

   logical :: print_info

   open(unit=print_unit, file='Eigen_Value_Vector.prt', status='unknown')

   one = 1
   print_info = .false.

   header_search: do
      read(input_unit_1, iostat=ier) flag

      if (flag == 0) then
         read(input_unit_1,iostat=ier) big_header_large%bhi,  big_header_large%bhr, &
                                       big_header_large%bhic, big_header_large%bhrc

         if(ier/=0) then
            write(*,'("Error reading big header large domain")')
            call abort()
         endif

         if(print_info) then
            call print_big_header(big_header_large%bhi, big_header_large%bhr, &
                                  big_header_large%bhic,big_header_large%bhrc)
         end if

         miy_large=big_header_large%bhi(16,1)
         mjx_large=big_header_large%bhi(17,1)

         lliy_large=big_header_large%bhi(18,1)
         lljx_large=big_header_large%bhi(19,1)

         miy_large_coarse=big_header_large%bhi(5,1)
         mjx_large_coarse=big_header_large%bhi(6,1)
      elseif (flag == 1) then
         read(input_unit_1,iostat=ier) &
              sub_header_large%ndim, &
              sub_header_large%start_index, &
              sub_header_large%end_index, &
              sub_header_large%current_time, &
              sub_header_large%staggering, &
              sub_header_large%ordering, &
              sub_header_large%current_date, &
              sub_header_large%name, &
              sub_header_large%units, &
              sub_header_large%description

         if(print_info) then
            call print_sub_header(sub_header_large)
         end if

         if(sub_header_large%ndim == 3) exit
      endif
   end do header_search

   do
      read(input_unit_2, iostat=ier) flag

      if (flag == 0) then
         read(input_unit_2,iostat=ier) big_header_small%bhi,  big_header_small%bhr, &
                                       big_header_small%bhic, big_header_small%bhrc

         if(ier/=0) then
            write(*,'("Error reading big header small domain")')
            call abort()
         endif

         idx       = big_header_small%bhi(1,1)
         vert_corr = big_header_small%bhi(26,idx)


         miy_small=big_header_small%bhi(16,1)
         mjx_small=big_header_small%bhi(17,1)

         lliy_small=big_header_small%bhi(18,1)
         lljx_small=big_header_small%bhi(19,1)

         miy_small_coarse=big_header_small%bhi(5,1)
         mjx_small_coarse=big_header_small%bhi(6,1)

         call get_lliy_lljx(big_header_small, big_header_large, lliy, lljx)

         mkx=big_header_small%bhi(12,5)

         write(unit=*, fmt='(/)')

         write(unit=*, fmt='(20x,i5,40x,i5)') &
               one, mjx_large
         write(unit=*, fmt='(10x,i5,5x,a,5x,i5)') &
              miy_large, '+--------------------------------------------------+', miy_large
         write(unit=*, fmt='(20x,a,5x,i5,25x,i5,10x,a)') &
              '|', lljx+1, mjx_small+lljx, '|'
         write(unit=*, fmt='(20x,a,50x,a)') &
              '|', '|'
         write(unit=*, fmt='(20x,a,10x,i5,15x,i5,15x,a)') &
              '|', one, mjx_small, '|'
         write(unit=*, fmt='(20x,a,2i4,1x,a,2i4,1x,a)') &
              '|', miy_small+lliy, miy_small, '+------------------------------+', &
                   miy_small+lliy, miy_small,  '|'
         write(unit=*, fmt='(20x,a,9x,a,30x,a,9x,a)') &
              '|', '|', '|', '|'
         write(unit=*, fmt='(20x,a,9x,a,30x,a,9x,a)') &
              '|', '|', '|', '|'
         write(unit=*, fmt='(20x,a,9x,a,30x,a,9x,a)') &
              '|', '|', '|', '|'
         write(unit=*, fmt='(20x,a,9x,a,30x,a,9x,a)') &
              '|', '|', '|', '|'
         write(unit=*, fmt='(20x,a,9x,a,30x,a,9x,a)') &
              '|', '|', '|', '|'
         write(unit=*, fmt='(20x,a,2i4,1x,a,2i4,1x,a)') &
              '|', lliy+1, one, '+------------------------------+', &
                   lliy+1, one,  '|'
         write(unit=*, fmt='(20x,a,10x,i5,15x,i5,15x,a)') &
              '|', one, mjx_small, '|'
         write(unit=*, fmt='(20x,a,50x,a)') &
              '|', '|'
         write(unit=*, fmt='(20x,a,5x,i5,25x,i5,10x,a)') &
              '|', lljx+1, mjx_small+lljx, '|'
         write(unit=*, fmt='(10x,i5,5x,a,5x,i5)') &
              one, '+--------------------------------------------------+', one
         write(unit=*, fmt='(20x,i5,40x,i5)') &
               one, mjx_large

         write(unit=*, fmt='(/)')

         do i=2,50
            if(big_header_large%bhi (i,1) >= 0) then
               big_header_small%bhi (i,1) = big_header_large%bhi (i,1)
               big_header_small%bhic(i,1) = big_header_large%bhic(i,1)
            endif
         enddo

         do i=1,20
            if(big_header_large%bhr (i,1) >= 0.0) then
               big_header_small%bhr (i,1) = big_header_large%bhr (i,1)
               big_header_small%bhrc(i,1) = big_header_large%bhrc(i,1)
            endif
         enddo

         if(print_info) then
            call print_big_header(big_header_small%bhi, big_header_small%bhr, &
                                  big_header_small%bhic,big_header_small%bhrc)
         end if

         write(output_unit) flag

         write(output_unit) big_header_small%bhi,  big_header_small%bhr, &
                            big_header_small%bhic, big_header_small%bhrc
      elseif (flag == 1) then
         read(input_unit_2,iostat=ier) &
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

         allocate(data_small(sub_header_small%start_index(1):sub_header_small%end_index(1), &
                             sub_header_small%start_index(2):sub_header_small%end_index(2), &
                             sub_header_small%start_index(3):sub_header_small%end_index(3), &
                             sub_header_small%start_index(4):sub_header_small%end_index(4)))

         read(input_unit_2) data_small

         if(vert_corr == 1 .or. &
            index(sub_header_small%name, 'PB_VERREG') > 0 .or. &
            index(sub_header_small%name, '_EVEC') > 0 .or. &
            index(sub_header_small%name, '_EVAL') > 0) then

            if(index(sub_header_small%name, 'PSI') > 0 .or. &
               index(sub_header_small%name, 'CHI') > 0 .or. &
               index(sub_header_small%name, 'P_U') > 0 .or. &
               index(sub_header_small%name, 'T_')  > 0 .or. &
               index(sub_header_small%name, 'RH_') > 0 .or. &
               index(sub_header_small%name, 'Q_')  > 0 .or. &
               index(sub_header_small%name, 'PB_VERREG') > 0) then

               write(output_unit) flag

               call make_large(sub_header_small, sub_header_large, data_small, &
                               miy_small, mjx_small, miy_large, mjx_large, &
                               lliy, lljx, output_unit)

               write(unit=print_unit, fmt='(/a/)') sub_header_small%name

               if(index(sub_header_small%name, '_EVALG') > 0 .or. &
                  index(sub_header_small%name, '_EVECG') > 0) then
                  do k=1,mkx
                     write(unit=print_unit, fmt='(i4,e24.14)') data_small(k,1,1,1)
                  end do
               else
                  do k=1,mkx
                     write(unit=print_unit, fmt='(i4,e24.14)') data_small(1,k,1,1)
                  end do
               end if
            end if
         end if


         deallocate(data_small)

         if(print_info) then
            call print_sub_header(sub_header_small)
         end if

      elseif (flag == 2) then
         write(output_unit) flag

         exit
      else
         stop 11111
      endif

   enddo

   close(unit=print_unit)

   stop 99999

end program to_be_file

