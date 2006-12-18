   subroutine plot_it(plt,num,k,varname,nx,ny,nt)
      
      implicit none

      integer,                   intent(in)    :: num,k,nx,ny,nt
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
