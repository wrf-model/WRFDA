program plot_eigen_scale

   implicit none

   type eigen_type
      real(kind=8), pointer :: g_val(:)
      real(kind=8), pointer :: g_vec(:,:)
      real(kind=8), pointer :: l_val(:,:)
      real(kind=8), pointer :: l_vec(:,:,:)
      real(kind=8), pointer :: sl(:)
      real(kind=8), pointer :: power(:,:)
      logical       :: ok_ev, ok_sl
   end type eigen_type

   type (eigen_type) :: cv

   integer, parameter :: input_unit = 11

   integer :: ix, jy, kz, nk, kdum
   integer :: i, j, k
   integer :: ier

   integer :: bin_type, num_bins, num_bins2d, num_bins_hgt
   real (kind=8) :: binwidth_lat, binwidth_hgt

   character(len=80)  ::name,filename
   character(len=10)  ::cvv, variable
   character(len=80)  ::infile

   namelist /plot_title/main_title1,main_title2,KM_resolution, &
                        cv_options, fg_format, im,jm, km,&
                        cvv, be_method,scale_length, power_spectra, infile 

   character(len=120)          :: main_title1
   character(len=120)          :: main_title2
 
   real     :: KM_resolution
   integer  :: cv_options, fg_format
   integer  :: im,jm,km
   logical  :: scale_length, power_spectra

   logical  :: first_time
   integer             :: max_wavenumber
   real(kind=8), allocatable   :: total_power(:,:)           ! Total Power spectrum.

   character(len=2)    :: ck
   character*1         :: k_label
   character*3         :: be_method
   logical             :: use_global_eofs, data_on_levels
!
! For contour plot:

      integer  :: IOFFP, ndot, jj, interval_j, len_title
      real     :: flo, hi, spval0

      character (len=120) :: Title
      common /conre1/ IOFFP, SPVAL0
      IOFFP = 1
      SPVAL0 = -99999.9
!
   call opngks

   call setup_color_table

!
   open(unit=5, file='namelist.title', status='old')
   read(unit=5, nml=plot_title)
   print*,' main title to be written : ',main_title1
   print*,' main title to be written : ',main_title2
   print*,' jm, km                   : ',jm,km
   print*,infile
   print*,scale_length, power_spectra
   close (5)
!-----------------------------------------------------
        cv%ok_sl = .false.
!-----------------------------------------------------
        ix=im; jy=jm ; kz=km
 
        call allocate_eigen(cv, ix,jy, kz)

!read : cv
        open(11,file=infile, form='unformatted',status='old')

!old.0912        read(11) bin_type, num_bins_hgt, binwidth_hgt, &
!                 binwidth_lat, num_bins2d,nk
        read(11) variable
        read(11) nk, num_bins2d
        read(11) cv%g_vec(1:kz,1:kz)
        read(11) cv%g_val(1:kz)
        read(11) cv%l_vec(1:kz,1:kz,1:jy)
        read(11) cv%l_val(1:kz,1:jy)
        cv%ok_ev = .true.

        close(11)

! ---------------------------------------------------------------
   call eigen_plot(cvv, cv, main_title1,km_resolution)

   call eigen_plot2(cvv, cv, main_title1,km_resolution,99) !local_eigenvalue

   if (scale_length) then
   filename=trim(cvv)//'/'//'sl_print.'//trim(cvv)
   open(11,file=trim(filename),status='old')

        cv%ok_sl = .true.
        read(11,'(a80)') name
        read(11,'(a80)') name
        do k=1,kz
        read(11,'(24x,e20.8)')    cv%sl(k)
        enddo
        close(11)
 
   call scale_plot(cvv, cv, main_title1, km_resolution) 

   endif

   if (power_spectra) then
   first_time = .true.
!    Data on vertical modes:
     k_label = 'm'

   do k=1,5
      write(ck,'(i2)')k
      if ( k < 10 ) ck = '0'//ck(2:2)

      filename = trim(cvv)//'/'//trim(cvv)
      FILename = trim(filename)//'.'//trim(be_method)//'.'//ck//'.spectrum'

      open (11, file = filename, form='unformatted')
      read(11)variable
      read(11)max_wavenumber,kdum 
      read(11) data_on_levels, use_global_eofs
      if ( first_time ) allocate( total_power(0:max_wavenumber,1:5) )
      read(11)total_power(0:max_wavenumber,k)
      cv%power(0:max_wavenumber,k)=total_power(0:max_wavenumber,k)

      close(11)
      first_time = .false. 
   enddo
   close(11)

   
   call eigen_plot2(cvv, cv, main_title1,km_resolution,98)

   endif
    
   call clsgks

CONTAINS

   subroutine allocate_eigen(eigen_var, ix, jy, kz)

      implicit none

      type (eigen_type), intent(inout) :: eigen_var
      integer,           intent(in)    :: ix,jy, kz

      integer                          :: max_waveno

      max_waveno = ix/2 - 1
      allocate(eigen_var%g_val(kz))
      allocate(eigen_var%g_vec(kz, kz))
      allocate(eigen_var%l_vec(kz, kz,jy))
      allocate(eigen_var%l_val(kz,jy))
      allocate(eigen_var%sl(kz))
      allocate(eigen_var%power(max_waveno,kz))

      eigen_var%g_val = 0.0
      eigen_var%g_vec = 0.0
      eigen_var%l_val = 0.0
      eigen_var%l_vec = 0.0
      eigen_var%sl    = 0.0

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

   subroutine eigen_plot(vn, ev, main_title, dist)

      implicit none

      integer, parameter :: solid_line = 65535, & ! PATTERN = 1111111111111111
                         thick_dash = 21845, &    ! PATTERN = 0101010101010101
                          thin_dash =  3855, &    ! PATTERN = 0000111100001111
                         solid_like = 31710       ! PATTERN = 0111101111011110

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

      character(len= 5), intent(in)   :: vn
      type (eigen_type), intent(in)   :: ev
      character(len= 120), intent(in) :: main_title
      real,                intent(in) :: dist 
      character(len=80)               :: x_title, y_title
      real, dimension(501)            :: x, y
   
      integer :: mn, plot_type, mmx, mnx, mmy, mny, magnitude, &
                 k, ix, jy, kz, line_color

      real    :: xb, xe, yb, ye, xmin, xmax, fct

      print '("PLOT:", a,2x,a)', vn, main_title(1:40)

      plot_type = 1

      ix = size(ev%l_vec, dim=3)
      jy = size(ev%l_vec, dim=1)
      kz = size(ev%l_vec, dim=2)

      mmy       = 6

      mny       = 2

      yb        = 1.0
      ye        = real(kz)
   
      y_title   = 'VERTICAL LEVEL'

      do k=1, 501
         y(k) = real(k)
      end do

      if (ev%ok_ev) then

!------------- plot eigen value ---------------------   
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
   
!mslee      xb = 1.0e-15
      xb = 1.0e-5
      xe = 100.0
   
      mn = 0
   
      call line_plot(x_title, y_title, main_title,  &
                     x,y,kz,mn,xb,xe,yb,ye,3, &
                     mmx, mnx, mmy, mny, &
                     red, 3500, thick_dash, magnitude,0)
   
      call frame
!-----------------------------------------------------   
   
      x_title = vn // ' E VEC'
    
      mmx       = 8
      mnx       = 2
   
      do k=1,kz
         x(k) = ev%g_vec(k,1)
      end do
   
      xmin = minval(x(1:kz))
      xmax = maxval(x(1:kz))
   
      call norm_min_max(xmin, xmax, magnitude)
   
      fct = 10.0**magnitude
   
      xb =-80.0
      xe = 80.0
   
      line_color = red
   
      do mn=1,5
         fct = 10.0**magnitude *ev%g_val(mn) / ev%g_val(1)
   
         do k=1,kz
!test            x(k) = ev%g_vec(k,mn)*fct
        x(k) = ev%g_vec(k,mn)*10.0**magnitude

         end do
         call line_plot(x_title, y_title, main_title,  &
                        x,y,kz,mn,xb,xe,yb,ye,plot_type, &
                        mmx, mnx, mmy, mny, &
                        line_color, 6000-1000*mn, thick_dash, magnitude,0)

         line_color = line_color + 1
   
         if(line_color == cyran ) line_color = line_color + 1
         if(line_color > 7) line_color = red
      end do
   
      call frame

      endif

!------------- Now plot scale length  ---------------------   

      if (ev%ok_sl) then

      mmx       = 5
      mnx       = 5
      write(unit=15, fmt='(a,a5)') 'Scale Length for ',vn         
      do k=1,kz
         x(k)   = ev%sl(k)
      write(unit=15, fmt='(i4,f10.1,3x,f20.1)') k,x(k), dist*x(k)
      end do
      x_title = vn // ' SCALE LENGTH(KM)'
   
      x(1:kz)  = x(1:kz) * dist
      xmin = minval(x(1:kz))
      xmax = maxval(x(1:kz))
  
      call norm_min_max(xmin, xmax, magnitude)
   print*,vn,' --> scale length min, mx and magnitude =',xmin,xmax,magnitude

! Scaled to be in unit of 100 KM     
!      magnitude = -2

      fct = 10.0**magnitude
!      fct = 1.0
!rizvi      x(1:kz)  = fct * x(1:kz) * dist
      x(1:kz)  = fct * x(1:kz) 
   
!      xb = 0.0
!      xe = 25.0
      xb = 0.0
      xe = maxval (x(1:kz))
   
      mn = 0
  
      y_title   = 'VERTICAL MODE'
 
      call line_plot(x_title, y_title, main_title,  &
                     x,y,kz,mn,xb,xe,yb,ye, 1, &
                     mmx, mnx, mmy, mny, &
                     red, 3500, thick_dash, magnitude,0)
      call frame

      endif
!-----------------------------------------------------   
   
   end subroutine eigen_plot

   subroutine eigen_plot2 (vn, ev, main_title, dist, iopt)
      implicit none

      integer, parameter :: solid_line = 65535, & ! PATTERN = 1111111111111111
                         thick_dash = 21845, &    ! PATTERN = 0101010101010101
                          thin_dash =  3855, &    ! PATTERN = 0000111100001111
                         solid_like = 31710       ! PATTERN = 0111101111011110

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

      character(len= 5), intent(in)   :: vn
      type (eigen_type), intent(in)   :: ev
      character(len= 120), intent(in) :: main_title
      real,                intent(in) :: dist
      character(len=80)               :: x_title, y_title
      real, dimension(501)            :: x, y

      integer                         :: iopt

      integer :: mn, plot_type, mmx, mnx, mmy, mny, magnitude, &
                 k, ix, jy, kz, line_color

      real    :: xb, xe, yb, ye, ymin, ymax, fct

      print '("PLOT:", a,2x,a)', vn, main_title(1:40)

      plot_type = 1

      if (iopt.eq.99) then
      ix = size(ev%l_vec, dim=3)
      jy = size(ev%l_vec, dim=1)
      kz = size(ev%l_vec, dim=2)

      elseif (iopt.eq.98) then
      ix = size(ev%power, dim=1)
      jy = size(ev%power, dim=2)
      endif

      mmy       = 6
      mny       = 2

      xb        = 1.0
      xe        = real(ix)

      if (iopt .eq. 99) y_title   = 'EIGEN VALUE'
      if (iopt .eq. 98) y_title   = 'POWER SPECTRUM'

      do k=1, ix
         x(k) = real(k)
      end do

!-----------------------------------------------------

      if (iopt .eq. 99) x_title = ' LATITUDE (GRID)'
      if (iopt .eq. 98) x_title = ' WAVE NUMBER'

      mmx       = 8
      mnx       = 2

      do j=1,ix
      if (iopt.eq.99)   y(j) = ev%l_val(1,j)
      if (iopt.eq.98)   y(j) = ev%power(j,1)
      enddo

      ymin = minval(y(1:ix))
      ymax = maxval(y(1:ix))

      call norm_min_max(ymin, ymax, magnitude)

      fct = 10.0**magnitude

      if (iopt .eq. 99) then

      yb =0.0
      ye =100.0

      if (vn.eq.'chi_u'.or.vn.eq.'chi'.or.vn.eq.'chi_b') then 
      ye=50.0
      elseif (vn.eq.'rh') then
      ye =10.0
      endif

    else 

      yb = 0.0
      ye = 150.0
      if (vn.eq.'chi_u'.or.vn.eq.'chi'.or.vn.eq.'chi_b'.or.  &
          vn.eq.'rh')  ye=20.0
      if (vn.eq.'t_u'.or.vn.eq.'t'.or.vn.eq.'t_b') &
      ye=50.0

    endif

!      if (iopt.eq.98.and.vn.eq.'rh') ye=150.
      if (iopt.eq.98.and.vn.eq.'chi_b') ye=100.
      line_color = red
   
      do mn=1,5

         do j=1,ix
!test            x(k) = ev%g_vec(k,mn)*fct
        if (iopt.eq.99) y(j) = ev%l_val(mn,j)*10.0**magnitude
        if (iopt.eq.98) y(j) = ev%power(j,mn)*10.0**magnitude
       
         end do
         call line_plot(x_title, y_title, main_title,  &
                        x,y,ix,mn,xb,xe,yb,ye,plot_type, &
                        mmx, mnx, mmy, mny, &
                        line_color, 6000-1000*mn, thick_dash, magnitude,2)

         line_color = line_color + 1

         if(line_color == cyran ) line_color = line_color + 1
         if(line_color > 7) line_color = red
      end do

      call frame

   end subroutine eigen_plot2
   
   subroutine line_plot(x_title, y_title, main_title,  &
                        z,y,n,mn,xb,xe,yb,ye,plot_type, &
                        mmx, mix, mmy, iy, &
                        color, width, line_pattern, magnitude,iopt)
   
      implicit none

      real, parameter    :: xfb = 0.10, &
                         xfe = 0.90, &
                         yfb = 0.10, &
                         yfe = 0.90

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
  
      integer                        :: iopt 
      character(len=80),  intent(in) :: x_title, y_title
      character(len=120), intent(in) :: main_title
      integer,            intent(in) :: n, &      !  vertical points
                                        mn, &     !  mode number
                                        plot_type, &
                                        mmx, mix, mmy, iy, &
                                        color, width, line_pattern, magnitude
      real, dimension(501), intent(in) :: z, y
      real,               intent(in) :: xb,xe,yb,ye
   
      integer                        :: lx, ly
      real                           :: xc, yc, xw, yw
      character(len=120)             :: title,title2
      character(len=  2)             :: chr_2
      character(len=  3)             :: chr_3
   
      real, dimension(501)           :: x

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      call set(0.,1.,0.,1.,0.,1.,0.,1.,1)

      write(chr_3(1:3), fmt='(i3)') magnitude
  
      if (iopt.eq.0)then
      title = trim(x_title) // ' MAGNIFIED BY 10**(' // chr_3 // ')'
      title2 = trim(y_title)
      elseif (iopt.eq.2) then
      title = trim(x_title) 
      title2 = trim(y_title)//' MAGNIFIED BY 10**(' // chr_3 // ')' 
      endif
      print '(I2," magnitude=",i6,2x,a)', mn, magnitude, chr_3
      print '("title=",a)', title

      lx = len_trim(  title)
      ly = len_trim(title2)

      call setusv('LW',1500)

      call gsplci(5)
      call gspmci(5)
      call gstxci(5)

      call pwrity(0.1-0.075,0.5,trim(title2),ly,16,90,0)
      call pwrity(0.5,0.1-0.075,       title, lx,16, 0,0)
      lx = len_trim( main_title)
      call pwrity(0.5,1.0-0.05,   main_title, lx,16, 0,0)
   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!      call gsplci(cyran)
!      call gspmci(cyran)
!      call gstxci(cyran)
      call gsplci(blue)
      call gspmci(blue)
      call gstxci(blue)

      call set(xfb,xfe,yfb,yfe,xb,xe,yb,ye,plot_type)
   
      call setusv('LW',2000)
   
      if(plot_type > 2) then
         call labmod('(E7.1)','(I4)',7,4,15,15,0,0,0)
      else
         call labmod('(I3)','(I3)',3,3,15,15,0,0,0)
      end if
   
      call halfax(mmx, mix, mmy, iy, xb, yb, 1, 1)
   
      if(plot_type < 3) call line(0.0, y(1), 0.0, y(n))
   
      call gsplci(color)
      call gspmci(color)
      call gstxci(color)
   
      call dashdb (line_pattern)
   
      call setusv('LW',width)
   
      x = z
     do lx=1,n
        write(unit=*, fmt='(a, i5, 2e12.4)') &
             'n,x,y=', lx, x(lx), y(lx)
        if(x(lx) < xb) x(lx) = xb
     end do
   
      call curve(x,y,n)
   
      if(mn > 0) then

         write(chr_2(1:2), fmt='(i2.2)') mn
   
         call pwrity(x(1), y(1)-0.005*(y(n)-y(1)), chr_2, 2, 16, 0, 0)
      end if

      if (x_title(7:9) == 'VEC') then
         call line(xe-25.0, ye-mn*1.5, xe-3.0,ye-mn*1.5)
         write(title,'("VECT ",I2)') mn
         call setusv('LW',2000)
         call pwrity(xe-15.0, ye-mn*1.5+0.5, title(1:7),7,10,0,0)
      endif
   
   end subroutine line_plot
  
subroutine setup_color_table

   implicit none

   call gscr(1,  0, 1.00, 1.00, 1.00) ! White
   call gscr(1,  1, 0.00, 0.00, 0.00) ! BLACK
   call gscr(1,  2, 1.00, 0.00, 0.00) ! Red
   call gscr(1,  3, 0.00, 1.00, 0.00) ! Green
   call gscr(1,  4, 0.00, 0.00, 1.00) ! Blue
   call gscr(1,  5, 0.60, 0.00, 0.80) ! Dark violet
   call gscr(1,  6, 0.00, 1.00, 1.00) ! Cyran
   call gscr(1,  7, 1.00, 0.00, 1.00) ! Magenta
   call gscr(1,  8, 0.90, 0.25, 0.00) ! FreshRed
   call gscr(1,  9, 0.40, 0.30, 0.20) ! Tan
   call gscr(1, 10, 1.00, 1.00, 0.00) ! Yellow
   call gscr(1, 11, 0.60, 0.60, 0.60) ! Gray

end subroutine setup_color_table

   subroutine scale_plot(vn, ev, main_title, dist)

   implicit none
   integer, parameter :: solid_line = 65535, & ! PATTERN = 1111111111111111
                         thick_dash = 21845, &    ! PATTERN = 0101010101010101
                          thin_dash =  3855, &    ! PATTERN = 0000111100001111
                         solid_like = 31710       ! PATTERN = 0111101111011110

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

      character(len= 5), intent(in)   :: vn
      type (eigen_type), intent(in)   :: ev
      character(len= 120), intent(in) :: main_title
      real,                intent(in) :: dist
      character(len=80)               :: x_title, y_title
      real, dimension(501)            :: x, y

      integer :: mn, plot_type, mmx, mnx, mmy, mny, magnitude, &
                 k, ix, jy, kz, line_color

      real    :: xb, xe, yb, ye, xmin, xmax, fct

      print '("PLOT:", a,2x,a)', vn, main_title(1:40)

      plot_type = 1

      ix = size(ev%l_vec, dim=3)
      jy = size(ev%l_vec, dim=1)
      kz = size(ev%l_vec, dim=2)

      mmy       = 6

      mny       = 2

      yb        = 1.0
      ye        = real(kz)

      y_title   = 'VERTICAL LEVEL'

      do k=1, 501
         y(k) = real(k)
      end do

!------------- Now plot scale length  ---------------------
      if (ev%ok_sl) then

      mmx       = 5
      mnx       = 5
      write(unit=15, fmt='(a,a5)') 'Scale Length for ',vn
      do k=1,kz
         x(k)   = ev%sl(k)
      write(unit=15, fmt='(i4,f10.1,3x,f20.1)') k,x(k), dist*x(k)
      end do
      x_title = vn // ' SCALE LENGTH(KM)'

      x(1:kz)  = x(1:kz) * dist
      xmin = minval(x(1:kz))
      xmax = maxval(x(1:kz))

      call norm_min_max(xmin, xmax, magnitude)
   print*,vn,' --> scale length min, mx and magnitude =',xmin,xmax,magnitude
! Scaled to be in unit of 100 KM
!      magnitude = -2

      fct = 10.0**magnitude
      x(1:kz)  = fct * x(1:kz)
      print*,'x ',x
      xb = 0.0
      xe = maxval (x(1:kz))

      mn = 0

      y_title   = 'VERTICAL MODE'

      call line_plot(x_title, y_title, main_title,  &
                     x,y,kz,mn,xb,xe,yb,ye, 1, &
                     mmx, mnx, mmy, mny, &
                     red, 3500, thick_dash, magnitude,0)
      call frame


      endif
!-----------------------------------------------------

end subroutine scale_plot


end program plot_eigen_scale
