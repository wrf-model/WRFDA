program just_be

   use record_header
   use module_io
   use module_trans

   implicit none

   integer, parameter :: input_unit = 10, &
                        output_unit = 20, &
                         print_unit = 40

   integer :: ier, flag, k

   integer :: idx, vert_corr

   real(kind=4), dimension(:,:,:,:), allocatable :: data

   logical :: print_info

   open(unit=print_unit, file='Eigen_Value_Vector.prt', status='unknown')

   print_info = .false.

   do
      read(input_unit, iostat=ier) flag

      if (flag == 0) then
         read(input_unit,iostat=ier) big_header%bhi,  big_header%bhr, &
                                     big_header%bhic, big_header%bhrc

         if(ier/=0) then
            write(*,'("Error reading big header small domain")')
            call abort()
         endif

         idx       = big_header%bhi(1,1)
         vert_corr = big_header%bhi(26,idx)

         if(print_info) then
            call print_big_header(big_header%bhi, big_header%bhr, &
                                  big_header%bhic,big_header%bhrc)
         end if

         write(output_unit) flag

         write(output_unit) big_header%bhi,  big_header%bhr, &
                            big_header%bhic, big_header%bhrc
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

         allocate(data(sub_header%start_index(1):sub_header%end_index(1), &
                       sub_header%start_index(2):sub_header%end_index(2), &
                       sub_header%start_index(3):sub_header%end_index(3), &
                       sub_header%start_index(4):sub_header%end_index(4)))

         read(input_unit) data

         if(vert_corr == 1 .or. &
            index(sub_header%name, 'PB_VERREG') > 0 .or. &
            index(sub_header%name, '_EVEC') > 0 .or. &
            index(sub_header%name, '_EVAL') > 0) then

            if(index(sub_header%name, 'PSI') > 0 .or. &
               index(sub_header%name, 'CHI') > 0 .or. &
               index(sub_header%name, 'P_U') > 0 .or. &
               index(sub_header%name, 'T_')  > 0 .or. &
               index(sub_header%name, 'RH_') > 0 .or. &
               index(sub_header%name, 'Q_')  > 0 .or. &
               index(sub_header%name, 'PB_VERREG') > 0) then

               write(output_unit) flag

               write(output_unit) data

               write(unit=print_unit, fmt='(/a/)') sub_header%name

               if(index(sub_header%name, '_EVALG') > 0 .or. &
                  index(sub_header%name, '_EVECG') > 0) then
                  do k=1,mkx
                     write(unit=print_unit, fmt='(i4,e24.14)') data(k,1,1,1)
                  end do
               else
                  do k=1,mkx
                     write(unit=print_unit, fmt='(i4,e24.14)') data(1,k,1,1)
                  end do
               end if
            end if
         end if

         deallocate(data)

         if(print_info) then
            call print_sub_header(sub_header)
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

end program just_be

