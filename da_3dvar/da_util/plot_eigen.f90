program plot_eigen

   use color_table
   use record_header
   use plot_parameters
   use module_io
   use module_graph

   implicit none

   type eigen_type
      real, pointer :: g_val(:)
      real, pointer :: g_vec(:,:)
      real, pointer :: l_val(:,:,:)
   end type eigen_type

   type (eigen_type) :: psi, chi, p_u, u, v, t, p, q, w

   integer, parameter :: input_unit = 11, &
                        output_unit = 20

   integer :: ier, flag

   integer :: iy, jx, kz

   real, dimension(:,:,:,:), allocatable :: data

   logical :: print_info


   print_info = .true.


   call opngks

   call setup_color_table

   do
      read(input_unit, iostat=ier) flag

      if (flag == 0) then
         read(input_unit,iostat=ier) big_header%bhi,  big_header%bhr, &
                                     big_header%bhic, big_header%bhrc

         if(ier/=0) then
            write(*,'("Error reading big header small domain")')
            call abort()
         endif

         iy=big_header%bhi(16,1)
         jx=big_header%bhi(17,1)
         kz=big_header%bhi(12,5)

         if(print_info) then
            call print_big_header(big_header%bhi, big_header%bhr, &
                                  big_header%bhic,big_header%bhrc)
         end if

         call allocate_eigen(psi, iy, kz)
         call allocate_eigen(chi, iy, kz)
         call allocate_eigen(p_u, iy, kz)
         call allocate_eigen(  u, iy, kz)
         call allocate_eigen(  v, iy, kz)
         call allocate_eigen(  t, iy, kz)
         call allocate_eigen(  q, iy, kz)
         call allocate_eigen(  p, iy, kz)
         call allocate_eigen(  w, iy, kz+1)

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

         if(print_info) then
            call print_sub_header(sub_header)
            write(unit=*, fmt='(a,e24.14)') &
                 'Sample value:', data(1,1,1,1)
         end if

         if(sub_header%name == 'PSI_EVALG ') then
            psi%g_val(:) = data(:,1,1,1)
         else if(sub_header%name == 'PSI_EVECG ') then
            psi%g_vec(:,:) = data(:,:,1,1)
         else if(sub_header%name == 'PSI_EVECL ') then
            psi%l_val(:,:,:) = data(:,:,:,1)
         else if(sub_header%name == 'CHI_EVALG ') then
            chi%g_val(:) = data(:,1,1,1)
         else if(sub_header%name == 'CHI_EVECG ') then
            chi%g_vec(:,:) = data(:,:,1,1)
         else if(sub_header%name == 'CHI_EVECL ') then
            chi%l_val(:,:,:) = data(:,:,:,1)
         else if(sub_header%name == 'P_U_EVALG ') then
            p_u%g_val(:) = data(:,1,1,1)
         else if(sub_header%name == 'P_U_EVECG ') then
            p_u%g_vec(:,:) = data(:,:,1,1)
         else if(sub_header%name == 'P_U_EVECL ') then
            p_u%l_val(:,:,:) = data(:,:,:,1)
         else if(sub_header%name == 'PP_EVALG  ') then
            p%g_val(:) = data(:,1,1,1)
         else if(sub_header%name == 'PP_EVECG  ') then
            p%g_vec(:,:) = data(:,:,1,1)
         else if(sub_header%name == 'PP_EVECL  ') then
            p%l_val(:,:,:) = data(:,:,:,1)
         else if(sub_header%name == 'U_EVALG   ') then
            u%g_val(:) = data(:,1,1,1)
         else if(sub_header%name == 'U_EVECG   ') then
            u%g_vec(:,:) = data(:,:,1,1)
         else if(sub_header%name == 'U_EVECL   ') then
            u%l_val(:,:,:) = data(:,:,:,1)
         else if(sub_header%name == 'V_EVALG   ') then
            v%g_val(:) = data(:,1,1,1)
         else if(sub_header%name == 'V_EVECG   ') then
            v%g_vec(:,:) = data(:,:,1,1)
         else if(sub_header%name == 'V_EVECL   ') then
            v%l_val(:,:,:) = data(:,:,:,1)
         else if(sub_header%name == 'T_EVALG   ') then
            t%g_val(:) = data(:,1,1,1)
         else if(sub_header%name == 'T_EVECG   ') then
            t%g_vec(:,:) = data(:,:,1,1)
         else if(sub_header%name == 'T_EVECL   ') then
            t%l_val(:,:,:) = data(:,:,:,1)
         else if(sub_header%name == 'Q_EVALG   ') then
            q%g_val(:) = data(:,1,1,1)
         else if(sub_header%name == 'Q_EVECG   ') then
            q%g_vec(:,:) = data(:,:,1,1)
         else if(sub_header%name == 'Q_EVECL   ') then
            q%l_val(:,:,:) = data(:,:,:,1)
         else if(sub_header%name == 'W_EVALG   ') then
            w%g_val(:) = data(:,1,1,1)
         else if(sub_header%name == 'W_EVECG   ') then
            w%g_vec(:,:) = data(:,:,1,1)
         else if(sub_header%name == 'W_EVECL   ') then
            w%l_val(:,:,:) = data(:,:,:,1)
         end if

         deallocate(data)

      elseif (flag == 2) then
         exit
      else
         print *, 'something is going on here'
         stop 11111
      endif

   enddo

   call eigen_plot('PSI', psi)
   call eigen_plot('CHI', chi)
   call eigen_plot('P_U', p_u)

   call close_gks

CONTAINS

   subroutine allocate_eigen(eigen_var, iy, kz)

      implicit none

      type (eigen_type), intent(inout) :: eigen_var
      integer,           intent(in)    :: iy, kz

      allocate(eigen_var%g_val(kz))
      allocate(eigen_var%g_vec(kz, kz))
      allocate(eigen_var%l_val(iy, kz, kz))

      eigen_var%g_val = 0.0
      eigen_var%g_vec = 0.0
      eigen_var%l_val = 0.0

   end subroutine allocate_eigen

   subroutine norm_min_max(xmin, xmax, magnitude)

      implicit none

      real,    intent(inout) :: xmin, xmax
      integer, intent(out)   :: magnitude


      real                   :: ymin, ymax

      magnitude = 0

      if(abs(xmin) < 1.0e-30 .and. abs(xmax) < 1.0e-30) return


      do
         if(abs(xmin) > 100.0 .or. abs(xmax) > 100.0) then
            magnitude = magnitude - 1
            xmin = 0.1*xmin
            xmax = 0.1*xmax
         else
            exit
         end if
      end do

      do
         if(abs(xmin) < 10.0 .and. abs(xmax) < 10.0) then
            magnitude = magnitude + 1
            xmin = 10.0*xmin
            xmax = 10.0*xmax
         else
            exit
         end if
      end do

      xmin = -100.0
      xmax =  100.0

   end subroutine norm_min_max

   subroutine eigen_plot(vn, ev)

      implicit none

      character(len= 3), intent(in)   :: vn
      type (eigen_type), intent(in)   :: ev
   
      character(len=80)               :: x_title, y_title
      real, dimension(501)            :: x, y
   
      integer :: mn, plot_type, mmx, mnx, mmy, mny, magnitude, &
                 k, iy, jx, kz, line_color

      real    :: xb, xe, yb, ye, xmin, xmax, fct


      plot_type = 1

      iy = size(ev%l_val, dim=1)
      jx = size(ev%l_val, dim=2)
      kz = size(ev%l_val, dim=3)

      mmy       = 6

      mny       = 2

      yb        = 1.0
      ye        = real(kz)
   
      y_title   = 'VERTICAL LEVELS'

      do k=1, 501
         y(k) = real(k)
      end do
   
      mmx       = 1
      mnx       = 1
   
      do k=1,kz
         x(k)   = ev%g_val(k)
      end do
   
      x_title = vn // ' E VAL'
   
      xmin = minval(x(1:kz))
      xmax = maxval(x(1:kz))
   
      call norm_min_max(xmin, xmax, magnitude)
   
      fct = 10.0**magnitude
    
      x(1:kz)  = fct * x(1:kz)
   
      xb = 1.0e-5
      xe = 100.0
   
      mn = 0
   
      call line_plot(x_title, y_title, &
                     x,y,kz,mn,xb,xe,yb,ye,3, &
                     mmx, mnx, mmy, mny, &
                     red, 2000, thick_dash, magnitude)
      call frame
   
   
      x_title = vn // ' E VEC'
   
      mmx       = 10
      mnx       = 2
   
      do k=1,kz
         x(k) = ev%g_vec(kz+1-k,1)
      end do
   
      xmin = minval(x(1:kz))
      xmax = maxval(x(1:kz))
   
      call norm_min_max(xmin, xmax, magnitude)
   
      fct = 10.0**magnitude
   
      xb =-100.0
      xe = 100.0
   
      line_color = red
   
      do mn=1,5
         fct = 10.0**magnitude *ev%g_val(mn) / ev%g_val(1)
   
         do k=1,kz
            x(k) = ev%g_vec(kz+1-k,mn)*fct
         end do
   
         call line_plot(x_title, y_title, &
                        x,y,kz,mn,xb,xe,yb,ye,plot_type, &
                        mmx, mnx, mmy, mny, &
                        line_color, 6000-1000*mn, thick_dash, magnitude)
         line_color = line_color + 1
   
         if(line_color == cyran ) line_color = line_color + 1
         if(line_color > 7) line_color = red
      end do
   
      call frame
   
   end subroutine eigen_plot
   
   subroutine line_plot(x_title, y_title, &
                        z,y,n,mn,xb,xe,yb,ye,plot_type, &
                        mmx, mix, mmy, iy, &
                        color, width, line_pattern, magnitude)
   
      implicit none
   
      character(len=80),  intent(in) :: x_title, y_title
      integer,            intent(in) :: n, &      !  vertical points
                                        mn, &     !  mode number
                                        plot_type, &
                                        mmx, mix, mmy, iy, &
                                        color, width, line_pattern, magnitude
      real, dimension(501), intent(in) :: z, y
      real,               intent(in) :: xb,xe,yb,ye
   
      integer                        :: lx, ly
      real                           :: xc, yc, xw, yw
      character(len=120)             :: title
      character(len=  2)             :: chr_2
      character(len=  3)             :: chr_3
   
      real, dimension(501)           :: x
   
      call gsplci(cyran)
      call gspmci(cyran)
      call gstxci(cyran)
   
      call set(xfb,xfe,yfb,yfe,xb,xe,yb,ye,plot_type)
   
      call setusv('LW',1000)
   
      if(plot_type > 2) then
         call labmod('(E7.1)','(I4)',7,4,15,15,0,0,0)
      else
         call labmod('(I4)','(I4)',4,4,15,15,0,0,0)
      end if
   
      call halfax(mmx, mix, mmy, iy, xb, yb, 1, 1)
   
      if(plot_type < 3) call line(0.0, y(1), 0.0, y(n))
   
      call gsplci(color)
      call gspmci(color)
      call gstxci(color)
   
      call dashdb (line_pattern)
   
      call setusv('LW',width)
   
      x = z
   
   !  do lx=1,n
   !     write(unit=*, fmt='(a, i5, 2e12.4)') &
   !          'n,x,y=', lx, x(lx), y(lx)
   !     if(x(n) < 1.0e-10) x(lx) = real(lx)
   !  end do
   
      call curve(x,y,n)
   
      if(mn > 0) then
         print *, 'mn=', mn

         write(unit=*, fmt='(a, i9)') 'mn=', mn

         write(chr_2(1:2), fmt='(i2.2)') mn
   
         call pwrity(x(1), y(1)-0.005*(y(n)-y(1)), chr_2, 2, 16, 0, 0)
      end if
   
      call set(xfb,xfe,yfb,yfe,xfb,xfe,yfb,yfe,1)
   
      write(chr_3(1:3), fmt='(i3.3)') magnitude
   
      title = trim(x_title) // '  MAGNIFIED BY 10**(' // chr_3 // ')'
   
      lx = len_trim(  title)
      ly = len_trim(y_title)
   
      call pwrity(xfb-0.075,0.5,trim(y_title),ly,24,90,0)
      call pwrity(0.5,yfb-0.075,       title, lx,24, 0,0)
   
   end subroutine line_plot
   
end program plot_eigen
