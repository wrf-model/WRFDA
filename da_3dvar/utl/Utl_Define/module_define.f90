
module record_header

   implicit none

   type big_record_header
      integer(kind=4), dimension(50,20) :: bhi
      real(kind=4),    dimension(20,20) :: bhr
      character(80),   dimension(50,20) :: bhic
      character(80),   dimension(20,20) :: bhrc
   end type big_record_header

   type sub_record_header
      integer(kind=4) :: ndim

      integer(kind=4), dimension(4) :: start_index, end_index

      real(kind=4)                  :: current_time

      character(len= 4) :: staggering, ordering
      character(len= 8) :: char_date
      character(len= 9) :: name
      character(len=24) :: current_date
      character(len=25) :: units
      character(len=46) :: description
   end type sub_record_header

   type (big_record_header) :: big_header
   type (sub_record_header) :: sub_header

end module record_header


module color_table

   implicit none

   integer, parameter :: black = 0, &
                         white = 1, &
                           red = 2, &
                         green = 3, &
                          blue = 4, &
                        violet = 5, &
                         cyran = 6, &
                       magenta = 7, &
                      freshred = 8, &
                           tan = 9, &
                        yellow = 10, &
                          gray = 11

end module color_table


module plot_parameters

   implicit none

   integer, parameter :: solid_line = 65535, &    ! PATTERN = 1111111111111111
                         thick_dash = 21845, &    ! PATTERN = 0101010101010101
                          thin_dash =  3855, &    ! PATTERN = 0000111100001111
                         solid_like = 31710       ! PATTERN = 0111101111011110

   real, parameter    :: xfb = 0.10, &
                         xfe = 0.90, &
                         yfb = 0.10, &
                         yfe = 0.90

end module plot_parameters

module mm5_model_state
 
   implicit none

   integer :: mix,mjx,mkx,kl,cb,ce,nl,nb

   real    :: time_interval

   real, dimension(:,:,:), allocatable :: uuu, vvv, www, &
                                          ttt, qqq, ppp
   real, dimension(:,:,:), allocatable :: wbn, ebn, sbn, nbn, &
                                          wbo, ebo, sbo, nbo, &
                                          wbt, ebt, sbt, nbt

   real, dimension(:,:), allocatable :: ps_crs, ps_dot

   logical :: new_variable

   character(len=9) :: var_name

   integer, parameter :: dot=0, &
                         crs=1

end module mm5_model_state

