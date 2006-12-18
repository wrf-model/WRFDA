program scale_length

   implicit none

   integer, parameter :: plot_style =  1, &
                         input_unit = 10, &
                         input_scnd = 12, &
                         nmlst_uint =  9, &
                                 nt = 62

   integer, parameter :: plot_switch = 0

   real, parameter :: xwb=0.00, xwe=1.00, ywb=0.00, ywe=1.00, &
                      xlb=0.50, ylb=0.97

   integer :: iy, jx, kz, ier, num, n

   real, dimension(:,:,:,:), allocatable :: var
   real, dimension(:,:,:), allocatable   :: tmp

   character(len=7)          :: varname
   real                      :: cut_dist_km, resolution_km
   namelist /control_param/ varname, resolution_km, cut_dist_km
   integer                   :: dgrid

!----------------------------------------------------------------------

   call opngks

   call setup_color_table

!  set background white

   call gscr(1, 0, 1.00, 1.00, 1.00)

!----------------------------------------------------------------------
   open(unit=nmlst_uint, file='namelist.input', status='old')
   read(unit=nmlst_uint, nml=control_param, iostat=ier)
   print*,' Doing job for : ',varname
   print*,' Cut off Grids : ',dgrid
   if(ier /= 0) stop 'Wrong namelist.input.'
   close(unit=nmlst_uint)
   dgrid = cut_dist_km/resolution_km

   read(input_unit, iostat=ier) iy, jx, kz
   read(input_scnd, iostat=ier) iy, jx, kz
   if(ier /= 0) stop 'Wrong input file.'

   allocate(var(1:iy, 1:jx, 1:kz, 1:nt))
   allocate(tmp(1:iy, 1:jx, 1:kz))
   ier = 0
   num = 0


   do
      num=num+1

      write(unit=*, fmt='(a, 4i6)') &
            'iy, jx, kz, num=', iy, jx, kz, num

      if(varname(1:3) == 'PSI' .or. varname(1:3) == 'psi') then
         read(input_unit) var(:,:,:,num)
         read(input_unit) tmp(:,:,:)
         read(input_unit) tmp(:,:,:)
         read(input_unit) tmp(:,:,:)
      else if(varname(1:3) == 'CHI' .or. varname(1:3) == 'chi') then
         read(input_unit) tmp(:,:,:)
         read(input_unit) var(:,:,:,num)
         read(input_unit) tmp(:,:,:)
         read(input_unit) tmp(:,:,:)
      else if(varname(1:3) == 'P_U' .or. varname(1:3) == 'p_u') then
         read(input_unit) tmp(:,:,:)
         read(input_unit) tmp(:,:,:)
         read(input_unit) var(:,:,:,num)
         read(input_unit) tmp(:,:,:)
      else if(varname(1:3) == 'Q_M' .or. varname(1:3) == 'q_m') then
         read(input_unit) tmp(:,:,:)
         read(input_unit) tmp(:,:,:)
         read(input_unit) tmp(:,:,:)
         read(input_unit) var(:,:,:,num)
      else
         do n = 1,4
         read(input_unit) tmp(:,:,:)
         enddo
      end if
!
      if(varname(1:3) == 'RHM' .or. varname(1:3) == 'rhm') then
         read(input_scnd) tmp(:,:,:)
         read(input_scnd) tmp(:,:,:)
         read(input_scnd) tmp(:,:,:)
         read(input_scnd) var(:,:,:,num)
       else
         do n =1,4
         read(input_scnd) tmp(:,:,:)
         end do
       end if   
         do n = 1,6
         read(input_unit) tmp(:,:,:)
         read(input_scnd) tmp(:,:,:)
         enddo
      read(input_unit, iostat=ier) iy, jx, kz
      read(input_scnd, iostat=ier) iy, jx, kz

      if(ier /= 0) exit
   end do

   close(input_unit)

   deallocate(tmp)

   call process_single_variable(var,varname,dgrid,num,iy,jx,kz,nt)

   call clsgks

CONTAINS

   subroutine process_single_variable(var,varname,dgrid,num,iy,jx,kz,nt)

      implicit none

      integer,                      intent(in) :: dgrid, num,iy,jx,kz,nt
      real, dimension(iy,jx,kz,nt), intent(in) :: var
      character(len=7),             intent(in) :: varname

      real, dimension(iy,jx,nt) :: plt

      integer :: i, j, k, n, output_unit

      character(len=12) :: flnm

      output_unit = 20

      write(flnm(1:12), fmt='(a,a)') 'sl_print.', varname(5:7)

      open(unit=output_unit, &
           file=flnm, &
           form='formatted', &
         action='write', &
         access='sequential', &
!      position='rewind', &
         status='replace')

      write(unit=output_unit, fmt='(2a/a)') &
            varname(1:3), ' scale length', &
           ' Lvl            m            scale-length'

      do k=1,kz
         do n=1,num
         do j=1,jx
         do i=1,iy
            plt(i,j,n)=var(i,j,k,n)
         end do
         end do
         end do

         call make_scale_length(plt,num,k,varname, dgrid, &
                                iy,jx,nt,output_unit)

       call plot_it(plt,num,k,varname,iy,jx,nt)
      end do

      close(unit=output_unit, status='keep')

   end subroutine process_single_variable

   subroutine make_scale_length(plt,num,k,varname,dgrid, nx,ny,nt,output_unit)

      implicit none

      integer,                   intent(in)    :: dgrid, num,k,nx,ny,nt, &
                                                  output_unit
      real, dimension(nx,ny,nt), intent(inout) :: plt
      character(len=7),          intent(in)    :: varname

      real :: radius, value, tinv, rmax, rmin, valmin,valmax

      integer :: i, j, ib, jb, ie, je, m, n, nn, nl, nnn

!-----Steps of fitting Gaussian Distribution:

!     B(r) = B(0) exp(-r**2/(8*s**2)      (1)

!     Log at both side of (1):

!        ln[B(0)/B(r)] = r**2/(8*s**2)   (2)

!        {8*ln[B(0)/B(r)]}**0.5 = r/s = m * r

!     Let:

!        y(r) = {8*ln[B(0)/B(r)]}**0.5

!        m = sum[r * y(r)]/sum[r*r]



      real(kind=8), dimension(:), allocatable ::  yr
      real(kind=8), dimension(:), allocatable :: nr, r, bb

      real(kind=8) :: criteria, ml, sl, cl, a, b, c, d, e

      call zero_mean(plt, nx, ny, nt, num)
      
      nn=nx+ny
      allocate( yr(0:nn))
!
      allocate(nr(0:nn))
      allocate(r(0:nn))
      allocate(bb(0:nn))
!
      yr = 0.0
!
      nr = 0.0
      r = 0.0
      bb = 0.0

      ib = 4 ; jb = 6 ; ie = nx - 4; je= ny - 6
! start of the time loop

      j_loop: do j=jb,je
      i_loop: do i=ib,ie
         n_loop: do n=jb, min((j+dgrid),je)
         m_loop: do m=ib, min((i+dgrid),ie)
            radius=sqrt(real((m-i)*(m-i)+(n-j)*(n-j)))
            if(radius == 0) then
            nl = 0
            else
            nl = int(radius + 0.5)
            end if
             t_loop: do nn=1,num
               value = plt(m,n,nn)*plt(i,j,nn)
               bb(nl)=bb(nl) + value
               nr(nl)=nr(nl) + 1.0
                r(nl)= r(nl) + radius
              end do t_loop
         end do m_loop
         end do n_loop
      end do i_loop
      end do j_loop
      nn=nx+ny
      
      write(unit=*, fmt='(2a/)') varname(1:3), &
           ' dist       points           radius               y(r)'
      r(0)=0.0
      nl = 0
      bb(0)=bb(0)/nr(0)
      do n=1,nn
         if(nr(n) < 1.0) exit
         if(bb(n) <= 0.0 ) then
!          print*,n,' ---> -ve corr bb= ',bb(n),' nr= ',nr(n) 
!         if( n ==  1) cycle
         exit
         endif
         radius=r(n)/nr(n)
!
         nl=nl+1
         bb(nl)=bb(n)/nr(n)
         r(nl)=radius
         if(bb(nl) < bb(0)) then
            yr(nl)=sqrt(8.0*(log(bb(0)/bb(nl))))
         else
            yr(nl)=0.0
         end if

!         r(n)=radius
!         nl=nl+1

         write(unit=*, fmt='(i4,f12.0,f20.4,e20.8)') &
               nl, nr(n), radius, yr(nl)

!        if(nl > 1 .and. yr(n) > 5.0) exit
         if(nl > 1 .and. yr(nl) > 3.0) exit
      end do

!      a=nr(0)
!      b=0.0
!      c=0.0
      d=0.0
      e=0.0
      if( nl == 0 ) stop ' Did not get any point with +ve corr'
      do n=1,nl
!         a=a+nr(n)
!         b=b+nr(n)*yr(n)
!         c=c+nr(n)* r(n)
         d=d+nr(n)* r(n)*yr(n)
         e=e+nr(n)* r(n)* r(n)
      end do

!     ml=(a*d-c*b)/(a*e-c*c)
!     cl=(b*e-d*c)/(a*e-c*c)
      print*,' ml num= ',d,'  ml denom= ',e
      ml=d/e
      print*,'inverse of scale length = ',ml    
!      cl=0.0

      sl=1.0/ml

      write(unit=*,  fmt='(/2a,i4,3e30.8/)') varname(1:3), &
           ' scale-length at mode:', k, ml, sl

      write(unit=output_unit, fmt='(i4,2e20.8)') k, ml, sl

      if(nl > 1) &
         call plot_sl(yr,r,nl,nn,ml,cl,k,varname)

   end subroutine make_scale_length

   subroutine zero_mean(a, nx, ny, nt, nm)

      implicit none
  
      integer,                   intent(in)    :: nx, ny, nt, nm
      real, dimension(nx,ny,nt), intent(inout) :: a

      real :: sum

      integer :: i, j, n

      do n=1,nm
         sum = 0.0

         do j=1,ny
         do i=1,nx
            sum=sum+a(i,j,n)
         end do
         end do

         sum = sum/real(nx*ny)

         do j=1,ny
         do i=1,nx
            a(i,j,n)=a(i,j,n)-sum
         end do
         end do
      end do

   end subroutine zero_mean

end program scale_length

   subroutine plot_it(plt,num,k,varname,nx,ny,nt,plot_switch)
      
      implicit none

      integer,                   intent(in)    :: num,k,nx,ny,nt, plot_switch
      real, dimension(nx,ny,nt), intent(inout) :: plt
      character(len=7),          intent(in)    :: varname

      real :: radius, value, xpb, xpe, ypb, ype

      integer :: i, j, ib, jb, ie, je, m, n, mm, nn

      real, dimension(nx,ny,nt) :: pltsqr

      character(len=20) :: pltlab

      ib=4
      jb=6

      ie=nx-4
      je=ny-6

      write(pltlab(1:20),fmt='(2a,i5)') &
            varname(1:3), ' FOR MODE : ', k

      write(unit=*, fmt='(a)') pltlab

      call set(xwb,xwe,ywb,ywe,xwb,xwe,ywb,ywe,plot_style)

      call gsplci(red)
      call gspmci(red)
      call gstxci(red)

      call pwritx(xlb,ylb,pltlab,20,1,0,0)

      xpb=0.0
      xpe=sqrt(real((nx-1)*(nx-1)+(ny-1)*(ny-1)))

      if(plot_switch > 0) then
         call point_plot(plt,num,nx,ny,nt,ib,jb,ie,je, &
                             xpb,xpe,ypb,ype)
      else
         call line_plot(plt,num,nx,ny,nt,ib,jb,ie,je, &
                            xpb,xpe,ypb,ype)
      end if

      call gsplci(red)
      call gspmci(red)
      call gstxci(red)

      call line(xpb,0.0,xpe,0.0)

      call frame

   end subroutine plot_it

   subroutine point_plot(plt,num,nx,ny,nt,ib,jb,ie,je, &
                             xpb,xpe,ypb,ype)
      
      implicit none

      integer,                   intent(in)  :: num,nx,ny,nt, &
                                                ib,jb,ie,je
      real, dimension(nx,ny,nt), intent(in)  :: plt
      real,                      intent(in)  :: xpb,xpe
      real,                      intent(out) :: ypb,ype


      real :: radius, value

      integer :: i, j, m, n, mm, nn

      real, dimension(nx,ny,nt) :: pltsqr

      character(len=1), parameter :: symbol='.'

      do n=1,num
      do j=1,ny
      do i=1,nx
         pltsqr(i,j,n)=plt(i,j,n)*plt(i,j,n)
      end do
      end do
      end do

      ype=maxval(pltsqr(ib:ie, jb:je, 1:num))*1.25
      ypb=-ype

      if(abs(ype - ypb) < 1.0e-5) then 
         call frame
         stop 'ype - ypb is too small.'
      end if
      
      call set(xfb,xfe,yfb,yfe,xpb,xpe,ypb,ype,plot_style)

      call line(xpb,ypb,xpe,ypb)
 
      call line(xpb,ypb,xpb,ype)

      i=int(xpe) + 1
      j=2

      m=int(ype-ypb) + 1
      n=m/10

!     call perim(i,j,m,m)
      
      value = ypb+0.02*(ype-ypb)

      do m=2,i,2
         radius=real(m)
         call line(radius,ypb,radius,value)
      end do

      call gsplci(blue)
      call gspmci(blue)
      call gstxci(blue)

      do j=jb,je,2
      do i=ib,ie,2
         do n=j,je,2
         do m=i,ie,2
            radius=sqrt(real((m-i)*(m-i)+(n-j)*(n-j)))

            do nn=1,num
               value=plt(m,n,nn)*plt(i,j,nn)

               call pwritx(radius,value,symbol,1,1,0,0)
            end do
         end do
         end do
      end do
      end do

   end subroutine point_plot

   subroutine line_plot(plt,num,nx,ny,nt,ib,jb,ie,je, &
                          xpb,xpe,ypb,ype)
      
      implicit none

      integer,                   intent(in)  :: num,nx,ny,nt, &
                                                ib,jb,ie,je
      real, dimension(nx,ny,nt), intent(in)  :: plt
      real,                      intent(in)  :: xpb,xpe
      real,                      intent(out) :: ypb,ype

      real, dimension(nx+ny) :: avg, sum

      real :: radius, value

      integer :: i, j, m, n, mm, nn


      sum = 0.0
      avg = 0.0


      do j=jb,je
      do i=ib,ie
         do n=j,je
         do m=i,ie
            radius=sqrt(real((m-i)*(m-i)+(n-j)*(n-j)))
            mm=int(radius+0.5) + 1

            do nn=1,num
               value=plt(m,n,nn)*plt(i,j,nn)

               avg(mm)=avg(mm)+value
               sum(mm)=sum(mm)+1.0
            end do
         end do
         end do
      end do
      end do

      n = 0

      do i=1,nx+ny
         if(sum(i) < 0.5) exit

         avg(i)=avg(i)/sum(i)
         n=i
      end do
         
      ypb=minval(avg)*1.25
      ype=maxval(avg)*1.25

      call set(xfb,xfe,yfb,yfe,xpb,xpe,ypb,ype,plot_style)

      call line(xpb,ypb,xpe,ypb)
 
      call line(xpb,ypb,xpb,ype)

      i=int(xpe) + 1
      j=2

      m=int(ype-ypb) + 1
      n=m/10

      value = ypb+0.02*(ype-ypb)

      do m=2,i,2
         radius=real(m)
         call line(radius,ypb,radius,value)
      end do

      call gsplci(blue)
      call gspmci(blue)
      call gstxci(blue)

      do i=2,nx+ny
         if(sum(i) < 0.5) exit

         call line(real(i-2), avg(i-1), real(i-1), avg(i))
      end do

   end subroutine line_plot

   subroutine plot_sl(yr,r,nm,nn,slnt,cnst,k,varname)
      
      implicit none

      integer,                       intent(in) :: nm,nn,k
      real(kind=8), dimension(0:nn), intent(in) :: yr,r
      real(kind=8),                  intent(in) :: slnt,cnst
      character(len=7),              intent(in) :: varname

      real :: x,y,xpb,xpe,ypb,ype

      integer :: i

      character(len=1), parameter :: symbol='.'
      character(len=9)            :: label

      call set(xwb,xwe,ywb,ywe,xwb,xwe,ywb,ywe,plot_style)

      call gsplci(red)
      call gspmci(red)
      call gstxci(red)

      write(label(1:9), fmt='(2a, i3)') varname(1:3), ' M=', k

      call pwritx(xlb,ylb,label,9,1,0,0)

      xpb=0.0
      xpe=r(nm)*1.05

      write(unit=*, fmt='(a, i5, f18.8)') &
           'nm,r(nm)=', nm,r(nm)

      ype=maxval(yr(0:nm))*1.05
      ypb=0.0


      write(unit=*, fmt='(a, 2f18.8)') &
           'xpe,ype=',xpe,ype

      call set(xfb,xfe,yfb,yfe,xpb,xpe,ypb,ype,plot_style)

      call line(xpb,ypb,xpe,ypb)
 
      call line(xpb,ypb,xpb,ype)

      y = ypb+0.02*(ype-ypb)

      do i=2,nm,2
         x=real(i)
         call line(x,ypb,x,y)
      end do

      x = xpb+0.02*(xpe-xpb)

      y=0.0

      do
         y=y+1.0
         if(y > ype) exit
         call line(xpb,y,x,y)
      end do

      call gsplci(blue)
      call gspmci(blue)
      call gstxci(blue)

      do i=1,nm
         x= r(i)
         y=yr(i)
         call pwritx(x,y,symbol,1,1,0,0)

         write(unit=*, fmt='(a,i3,2(f8.4,f18.8))') &
              'i,x,y,r,yr=',i,x,y,r(i),yr(i)
      end do

      xpb=0.0
      ypb=cnst

      do i=1,nm
         x=real(i)
         y=slnt*x+cnst

         if(y   > ype) exit

         call line(xpb,ypb,x,y)

         xpb=x
         ypb=y
      end do

      call frame

   end subroutine plot_sl
