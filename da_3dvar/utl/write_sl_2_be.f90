program write_sl_2_be

   use record_header
   use module_io

   implicit none

   integer, parameter :: input_unit = 10, &
                            sl_unit = 12, &
                        output_unit = 20

   integer :: ier, flag, iy, jx, kz, k, m

   real, dimension(:,:,:,:), allocatable :: data
   real, dimension(:),       allocatable :: ml, scale_length

   logical :: print_info

   print_info = .true.

   do
      read(input_unit, iostat=ier) flag

      if (flag == 0) then
         read(input_unit,iostat=ier) big_header%bhi,  big_header%bhr, &
                                     big_header%bhic, big_header%bhrc

         if(ier/=0) then
            write(*,'("Error reading big header large domain")')
            call abort()
         endif

         if(print_info) &
         call print_big_header(big_header%bhi,  big_header%bhr, &
                               big_header%bhic, big_header%bhrc)

         iy=big_header%bhi(5,1)
         jx=big_header%bhi(6,1)
         kz=big_header%bhi(12,5)

         big_header%bhi (30, 10) = 1
         big_header%bhic(30, 10) = 'SCALE LENGTH AVIALABLE? 1, YES; 0, NO.'

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

!        if(print_info) &
!        call print_sub_header(sub_header)

         write(output_unit) flag

         write(output_unit) &
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

         write(output_unit) data

         deallocate(data)

      elseif (flag == 2) then
         exit
      else
         stop 11111
      endif

   enddo

   flag = 1

   do k=1,4
      sub_header%start_index(k) = 1
      sub_header%end_index(k)   = 1
   end do

   sub_header%end_index(1) = kz

   allocate(ml(kz))
   allocate(scale_length(kz))

!-----For PSI's (stream function) scale-lenght

   call make_new_subheader('sl_print.psi', &
                           'PSI_S_LEN', &
                           'Number of Grid Points    ', &
                           'Scale-Length for PSI (stream function)', &
                            sub_header)

!-----For CHI's (velocity potential) scale-lenght

   call make_new_subheader('sl_print.chi', &
                           'CHI_S_LEN', &
                           'Number of Grid Points    ', &
                           'Scale-Length for CHI (velocity potential)', &
                            sub_header)

!-----For P_U's (unbalanced pressure) scale-lenght

      call make_new_subheader('sl_print.p_u', &
                              'P_U_S_LEN', &
                              'Number of Grid Points    ', &
                              'Scale-Length for P_U (unbalanced pressure)', &
                               sub_header)

!-----For Q's scale-lenght

      call make_new_subheader('sl_print.q_m', &
                              'Q_S_LEN  ', &
                              'Number of Grid Points    ', &
                              'Scale-Length for Q (specific humidity)', &
                               sub_header)

!-----For RH's scale-lenght

      call make_new_subheader('sl_print.rhm', &
                              'RH_S_LEN  ', &
                              'Number of Grid Points    ', &
                              'Scale-Length for RH (relative humidity)', &
                               sub_header)

!-----Flag end of file

      flag = 2

      write(output_unit) flag

      deallocate(ml)
      deallocate(scale_length)

CONTAINS

   subroutine make_new_subheader(filename, name, &
                                 units, description, sub_header)

      implicit none

      character(len= 9),        intent(in)    :: name
      character(   *  ),        intent(in)    :: filename
      character(   *  ),        intent(in)    :: units
      character(   *  ),        intent(in)    :: description
      type (sub_record_header), intent(inout) :: sub_header

      character(len=20) :: plot_label
      character(len= 5) :: signal

      write(unit=*, fmt='(3a)') &
           'Append file<', filename, '>'

      open(sl_unit, file=filename, form='formatted', status='old')

      read(sl_unit, fmt='(a)') sub_header%description(1:3)
      read(sl_unit, fmt='(a)') signal

      write(unit=*, fmt='(2a)') &
            sub_header%description(1:3), signal

      do k=1,kz
         read(sl_unit, fmt='(i4,2e20.8)') &
              m, ml(m), scale_length(m)

         write(unit=*, fmt='(i4,2e20.8)') &
              m, ml(m), scale_length(m)
      end do

      close(sl_unit)

      plot_label(1:3) = sub_header%description(1:3)

      if(plot_label(1:3) /= name(1:3)) then
         if(plot_label(1:1) /= name(1:1) .and. &
            plot_label(3:3) /= name(3:3)) then
            write(unit=*, fmt='(2a)') 'plot_label=', plot_label
            write(unit=*, fmt='(2a)') '      name=', name

            stop 'Wrong variable.'
         end if
      end if

      do k=1,46
         sub_header%description(k:k) = ' '
      end do

      k= len(description)

      sub_header%ndim             = 1
      sub_header%ordering         = 'MODE'
      sub_header%name             = name
      sub_header%units            = units
      sub_header%description(1:k) = description(1:k)

      if(print_info) &
      call print_sub_header(sub_header)

      write(output_unit) flag

      write(output_unit) &
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

      write(output_unit) scale_length

   end subroutine make_new_subheader

end program write_sl_2_be
