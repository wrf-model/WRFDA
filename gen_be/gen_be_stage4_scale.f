program scale_length

   use da_constants

   implicit none

   integer, parameter  :: nt=1000                    ! maximum time series
   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name
   character*3         :: be_method                  ! Be method ('NMC', or 'ENS')
   character*80        :: filename                   ! Input filename.
   character*1         :: k_label                    ! = 'k' if model level, 'm' if mode output.
   character*2         :: ck                         ! Level index -> character.
   character*3         :: ce                         ! Member index -> character.
   integer             :: nk                         ! Dimensions read in.
   integer             :: ni0, nj0                   ! Dimensions read in.
   integer             :: vertical_level
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Period between dates (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: i, j, k, k1, k2, b, member ! Loop counters.

   real, allocatable   :: field(:,:,:)          ! Field projected into EOF space.

   real                      :: cut_dist_km,resolution_km

   namelist / scale_length_nl / start_date, end_date, interval, variable, &
                                be_method, ne, &
                                nk ,vertical_level, &
                                cut_dist_km, resolution_km
   logical             :: use_global_eofs, data_on_levels
   integer             :: num
!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Initialize namelist variables and other scalars.'
!---------------------------------------------------------------------------------------------

   vertical_ip = 0

   start_date = '2004030312'
   end_date = '2004033112'
   interval = 24
   variable = 'psi'
   be_method = 'NMC'
   ne = 1

   use_global_eofs = .false.
   data_on_levels = .false.

   open(unit=namelist_unit, file='scale_length_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, scale_length_nl)
   close(namelist_unit)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing vertical error statistics for dates ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne

   date = start_date
   cdate = sdate

!----------------------------------------------------------------------------------------
   num=0
   do while ( cdate <= edate )
      do member = 1, ne
      num=num+1
         write(6,'(5a,2I4)')'    Date = ', date, ', variable ', trim(variable), &
                           ' and member ', member,num

         write(ce,'(i3)')member
         if ( member < 10 ) ce = '00'//ce(3:3)
         if ( member >= 10 .and. member < 100 ) ce = '0'//ce(2:3)

!        Output fields (split into 2D files to allow parallel horizontal treatment):
            write(ck,'(i2)') nk

            if ( nk < 10 ) ck = '0'//ck(2:2)
            k_label = 'm'

!           Assumes variable directory has been created by script:
            filename = trim(variable)//'/'//date(1:10)//'.'//trim(variable)
            filename = trim(filename)//'.'//trim(be_method)//'.e'//ce//'.'//ck

            open (iunit, file = filename, form='unformatted')
            read(iunit) ni0, nj0, k

            if (num.eq.1) allocate(field(1:ni0, 1:nj0, 1:nt))

            read(iunit) data_on_levels, use_global_eofs
            read(iunit)field(1:ni0,1:nj0,num)
      end do ! End loop over members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do

   print*,'num ',num

   call process_single_variable(field,variable,cut_dist_km,&
                      resolution_km,num,ni0,nj0,ck,nt)

   if (nk.eq.vertical_level) then
   call merge_scale_length(variable, nk)
   endif

   deallocate (field) 

end program scale_length


   subroutine process_single_variable(var,varname,cut_dist_km,resolution_km,&
                                  num,ix,jy,kz,nt)

      implicit none

      integer,                      intent(in) :: num,ix,jy,nt
      real, dimension(ix,jy,nt),    intent(in) :: var
      character(len=10),            intent(in) :: varname
      character(len=2),             intent(in) :: kz
      real,                         intent(in) :: cut_dist_km,resolution_km

      integer :: i, j, k, n, output_unit, cut_grid

      character(len=80) :: flnm

      output_unit = 20

      cut_grid = cut_dist_km/resolution_km

!      write(*,'(I4,2e13.5)') cut_grid,cut_dist_km,resolution_km
      flnm=trim(varname)//'/'//'sl_print.'//trim(varname)//'.'//kz
      open(unit=output_unit, &
           file=trim(flnm), &
           form='formatted', &
         action='write', &
         access='sequential', &
         status='replace')

      if (kz.eq.'1') then
      write(unit=output_unit, fmt='(2a/a)') &
            trim(varname), ' scale length', &
           ' Lvl            m            scale-length'
      endif

         call make_scale_length(var,num,kz,varname,cut_grid, &
                                ix,jy,nt,output_unit)


      close(unit=output_unit, status='keep')

   end subroutine process_single_variable

   subroutine make_scale_length(plt,num,k,varname,cut_grid,  &
                                  nx,ny,nt,output_unit)

      implicit none

      integer,                   intent(in)    :: num,nx,ny,nt, &
                                                  output_unit,cut_grid
      real, dimension(nx,ny,nt), intent(in)    :: plt
      character(len=7),          intent(in)    :: varname
      character(len=2),          intent(in)    :: k
      real :: radius

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
      integer      :: squr_cut_grid

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

      squr_cut_grid = cut_grid*cut_grid  

      ib = 4             ; jb = 6 
      ie = nx - 4        ; je = ny - 6         

!      j_loop: do j=jb,je
!      i_loop: do i=ib,ie
      j_loop: do j=1,ny
      i_loop: do i=1,nx

!         n_loop: do n=jb, min((j+cut_grid),je)
!         m_loop: do m=ib, min((i+cut_grid),ie)
         n_loop: do n=1,ny
         m_loop: do m=1,nx

            radius=      real((m-i)*(m-i)+(n-j)*(n-j))
!      if( radius > squr_cut_grid ) exit 
           radius = sqrt(radius)
            nl = int(radius+0.5)
         t_loop: do nn=1,num
               bb(nl)=bb(nl) + plt(m,n,nn) * plt(i,j,nn)
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

         if(bb(n) <= 0.0) then
           print*,nl,' negative corr ',bb(n)
           exit
          endif

         bb(n)=bb(n)/nr(n)
         radius=r(n)/nr(n)
         if(bb(n) < bb(0)) then
            yr(n)=sqrt(8.0*(log(bb(0)/bb(n))))
         else
            yr(n)=0.0
         end if

         r(n)=radius

         nl=nl+1

        write(unit=*, fmt='(i4,f12.0,f20.4,3e13.5)') &
               n, nr(n), radius, yr(n),r(n),bb(n)

         if(nl > 1 .and. yr(n) > 3.0) exit
      end do

      d=0.0
      e=0.0

      do n=1,nl
         d=d+nr(n)* r(n)*yr(n)
         e=e+nr(n)* r(n)* r(n)
      end do

      ml=d/e

      sl=1.0/ml

      write(unit=*,  fmt='(/2a,a4,3e30.8/)') varname(1:3), &
           ' scale-length at mode:', k, ml, sl

      write(unit=output_unit, fmt='(a4,2e20.8)') k, ml, sl

   end subroutine make_scale_length

   subroutine merge_scale_length(variable,nk)

   implicit none
   character(len=80)   :: flnm
   character(len=10)   :: variable           
   character(len=2)    :: ck
   integer             :: nk,k
   real                :: ml,sl

   flnm=trim(variable)//'/'//'sl_print.'//trim(variable)
   open(16,file=trim(flnm),status='unknown')

   write(16, fmt='(2a/a)') &
            trim(variable), ' scale length', &
           ' Lvl            m            scale-length'
   
   do k=1,nk
   write(ck,'(i2)') k

   if ( k < 10 ) ck = '0'//ck(2:2)

   flnm = trim(variable)//'/'//'sl_print.'//trim(variable)//'.'//ck  
   open(15,file=trim(flnm),status='old')

   read(15,'(4x,2e20.8)') ml, sl
   write(16,'(i4,2e20.8)') k,ml,sl
   enddo

   close(15)
   close(16)
   end subroutine merge_scale_length

