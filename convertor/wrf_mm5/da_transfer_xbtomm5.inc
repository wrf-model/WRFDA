subroutine da_transfer_xbtomm5( xp, xb )

!------------------------------------------------------------------------------
!  PURPOSE: Compute and write out final analysis in MM5 format.
!
!  METHOD: 1) Read MM5 format first guess.
!          2) Interpolate u, v to MM5 B-grid.
!          3) Write out 3DVAR header information.
!          4) Write out 3DVAR analysis (and increments).
!
!  HISTORY: 07/21/2004
!           Wei Huang
!
!  PARENT_MODULE: DA_Setup_Structures
!------------------------------------------------------------------------------

   implicit none

   type (xpose_type), intent(in) :: xp          ! Domain decomposition vars.
   type (xb_type), intent(inout) :: xb          ! Analysis.
   
   type (mm5_model_type)         :: xb_mm5      ! MM5 model structure.
   integer                       :: ier         ! Input error flag.

   real, dimension(1:xb%mix,1:xb%mjy,1:xb%mkz)     :: gbuf ! Buffer for values on global-domain.

   real, dimension(1:xb%mjy+1,1:xb%mix+1,1:xb%mkz) :: u_bgrid, v_bgrid

   integer        :: len, index, seconds, nv, n
   integer        :: size3d

   integer        :: is, ie        ! i range of processor subdomain.
   integer        :: js, je        ! j range of processor subdomain.
   integer        :: ks, ke        ! k range of processor subdomain.
   integer        :: k,i,j         ! Loop counter.

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   integer :: anl_unit

   logical :: print_info, need_write

!----------------------------------------------------------------------------

!--Force output in IEEE32

   integer(kind=4) :: flag, big_head_flag
   real(kind=4), dimension(:,:,:,:), allocatable :: data

   real, dimension(:,:,:), allocatable :: u_global, v_global, w_global, &
                                          t_global, p_global, q_global

!  real, dimension(:,:,:), allocatable :: qcw_global, qrn_global

!---------------------------------------------------------------------------
!  [1.0] Read original MM5 format first guess:
!---------------------------------------------------------------------------

   IF ( wrf_dm_on_monitor() ) THEN

   anl_unit = jpout+10

   if(PRINT_DETAIL > 0) then
      print_info  = .true.
   else
      print_info  = .false.
   endif

   seconds = 86400

   nv = 1

   rewind(jpin)
   
!--Loop through headers to find the right time.

   time_loop: do
      read(jpin, iostat=ier) flag

      if(ier/=0) then
         write(unit=0,fmt='(a)') 'Error reading header flag'
         call abort()
      endif

      if (flag == 0) then
         read(jpin, iostat=ier) xb_mm5 % big_header % bhi, &
                                xb_mm5 % big_header % bhr,&
                                xb_mm5 % big_header % bhic, &
                                xb_mm5 % big_header % bhrc
         if(ier/=0) then
            write(0,'("Error reading big header")')
            call abort()
         endif

         if(print_info) &
            call DA_print_big_header(xb_mm5%big_header%bhi, xb_mm5%big_header%bhr, &
                                     xb_mm5%big_header%bhic,xb_mm5%big_header%bhrc)
         big_head_flag = flag
      elseif (flag == 1) then

         read(jpin,iostat=ier) &
              xb_mm5%sub_header(nv)%ndim, &
              xb_mm5%sub_header(nv)%start_index, &
              xb_mm5%sub_header(nv)%end_index, &
              xb_mm5%sub_header(nv)%current_time, &
              xb_mm5%sub_header(nv)%staggering, &
              xb_mm5%sub_header(nv)%ordering, &
              xb_mm5%sub_header(nv)%current_date, &
              xb_mm5%sub_header(nv)%name, &
              xb_mm5%sub_header(nv)%units, &
              xb_mm5%sub_header(nv)%description

         if(ier/=0) then
            write(0,'("Error reading subheader")')
            call abort()
         endif

         if(print_info) then
            call DA_print_sub_header(xb_mm5%sub_header(nv))
         endif

         index = xb_mm5 % big_header % bhi(1,1)

!        if(index /= 5) then
            seconds = DA_Diff_Seconds(ANALYSIS_DATE, xb_mm5%sub_header(1)%current_date)
!        else
!           seconds = 0
!        end if

         if ( seconds <= ANALYSIS_ACCU ) exit

         allocate(data(xb_mm5%sub_header(nv)%start_index(1):xb_mm5%sub_header(nv)%end_index(1), &
                       xb_mm5%sub_header(nv)%start_index(2):xb_mm5%sub_header(nv)%end_index(2), &
                       xb_mm5%sub_header(nv)%start_index(3):xb_mm5%sub_header(nv)%end_index(3), &
                       xb_mm5%sub_header(nv)%start_index(4):xb_mm5%sub_header(nv)%end_index(4)))

         read(jpin,iostat=ier) data

         deallocate(data)

      elseif (flag == 2) then
         cycle
      else
         stop
      endif
   enddo time_loop

   if(index /= 5) then
      len = len_trim(ANALYSIS_DATE)

      xb_mm5 % big_header % bhi( 1, 1) = 5

!-----Four-digit year of start time

      read(ANALYSIS_DATE( 1:04), fmt='(I4)') xb_mm5 % big_header % bhi( 5, 5)

!-----Month of the year of the start time (1-12)

      read(ANALYSIS_DATE( 6:07), fmt='(I2)') xb_mm5 % big_header % bhi( 6, 5)

!-----Day of the month of the start time (1-31)

      read(ANALYSIS_DATE( 9:10), fmt='(I2)') xb_mm5 % big_header % bhi( 7, 5)

!-----Hour of the day of the start time (0-23)

      read(ANALYSIS_DATE(12:13), fmt='(I2)') xb_mm5 % big_header % bhi( 8, 5)

!-----Minute of the start time (0-59)

      read(ANALYSIS_DATE(15:16), fmt='(I2)') xb_mm5 % big_header % bhi( 9, 5)

!-----Second of the start time (0-59)

      read(ANALYSIS_DATE(18:19), fmt='(I2)') xb_mm5 % big_header % bhi(10, 5)

!-----Ten thousandths of a second of the start time (0-9999)

      read(ANALYSIS_DATE(21:24), fmt='(I4)') xb_mm5 % big_header % bhi(11, 5)

!     do len=1,xb_mm5 % num_of_var
!        xb_mm5 % sub_header(len) % current_time = 0.0           ! Forecast time=0
!        xb_mm5 % sub_header(len) % current_date = ANALYSIS_DATE ! Forecast date
!     end do
!  else
!     xb_mm5 % big_header % bhi(11,5) = 0   ! TEN THOUSANDTHS OF SECONDS
   end if

   xb_mm5 % big_header % bhic(1,1) = 'MM5 3DVAR Analysis                          '

   open(unit=anl_unit, form='unformatted', status='unknown')

   write(anl_unit) big_head_flag

   write(anl_unit) xb_mm5%big_header%bhi,  xb_mm5%big_header%bhr, &
                   xb_mm5%big_header%bhic, xb_mm5%big_header%bhrc

      ALLOCATE ( u_global (1:xb%mjy+1,1:xb%mix+1,1:xb%mkz) )
      ALLOCATE ( v_global (1:xb%mjy+1,1:xb%mix+1,1:xb%mkz) )
      ALLOCATE ( w_global (1:xb%mjy+1,1:xb%mix+1,1:xb%mkz+1) )
      ALLOCATE ( t_global (1:xb%mjy+1,1:xb%mix+1,1:xb%mkz) )
      ALLOCATE ( p_global (1:xb%mjy+1,1:xb%mix+1,1:xb%mkz) )
      ALLOCATE ( q_global (1:xb%mjy+1,1:xb%mix+1,1:xb%mkz) )
!     ALLOCATE ( qcw_global (1:xb%mjy+1,1:xb%mix+1,1:xb%mkz) )
!     ALLOCATE ( qrn_global (1:xb%mjy+1,1:xb%mix+1,1:xb%mkz) )

   END IF   !end if(wrf_dm_on_monitor())

!------------------------------------------------------------------------------      
!  [2.0] Wind increments must be interpolated back to B-grid:
!------------------------------------------------------------------------------

   is = xp%its
   ie = xp%ite
   js = xp%jts
   je = xp%jte
   ks = xp%kts
   ke = xp%kte
   size3d = xb%mjy*xb%mix*xb%mkz

!------------------------------------------------------------------------------
! Convert u from a to b grid:
!------------------------------------------------------------------------------

!  Collect U component of wind increments into global buffer.
   call local_to_global( xp, xb%u, gbuf, 3 ) 
   call wrf_dm_bcast_real( gbuf, size3d )

   IF ( wrf_dm_on_monitor() ) THEN

     do k=1, xb%mkz
        do j=2, xb%mjy
           do i=2, xb%mix
              u_global(j,i,k) = 0.25*(gbuf(i,j  ,k)+gbuf(i-1,j,  k) &
                                    + gbuf(i,j-1,k)+gbuf(i-1,j-1,k))
           enddo

!          Interpolate boundary

           i=1
           u_global(j,i,k) = 0.5*(gbuf(i,j,k)+gbuf(i,j-1,k))

           i=xb%mix+1
           u_global(j,i,k) = 0.5*(gbuf(i-1,j,k)+gbuf(i-1,j-1,k))
        enddo
 
        do i=2, xb%mix
           j=1
           u_global(j,i,k) = 0.5*(gbuf(i,j,k)+gbuf(i-1,j,k))

           j=xb%mjy+1
           u_global(j,i,k) = 0.5*(gbuf(i,j-1,k)+gbuf(i-1,j-1,k))
        enddo

        u_global(1,1,k) = 2.0*u_global(1,2,k)-u_global(1,3,k)

        u_global(1,xb%mix+1,k) = 2.0*u_global(2,xb%mix+1,k)-u_global(3,xb%mix+1,k)

        u_global(xb%mjy+1,1,k) = 2.0*u_global(xb%mjy+1,2,k)-u_global(xb%mjy+1,3,k)

        u_global(xb%mjy+1,xb%mix+1,k) = 2.0*u_global(xb%mjy+1,xb%mix,k)-u_global(xb%mjy+1,xb%mix-1,k)
     enddo

   END IF   !end if(wrf_dm_on_monitor())

!  Collect V component of wind increments into global buffer.
   call local_to_global( xp, xb % v, gbuf, 3 )
   call wrf_dm_bcast_real( gbuf, size3d )
  
   IF ( wrf_dm_on_monitor() ) THEN

     do k=1, xb%mkz
        do j=2, xb%mjy
           do i=2, xb%mix
              v_global(j,i,k) = 0.25*(gbuf(i,j  ,k)+gbuf(i-1,j,  k) &
                                    + gbuf(i,j-1,k)+gbuf(i-1,j-1,k))
           enddo

!          Interpolate boundary

           i=1
           v_global(j,i,k) = 0.5*(gbuf(i,j,k)+gbuf(i,j-1,k))

           i=xb%mix+1
           v_global(j,i,k) = 0.5*(gbuf(i-1,j,k)+gbuf(i-1,j-1,k))
        enddo

        do i=2, xb%mix
           j=1
           v_global(j,i,k) = 0.5*(gbuf(i,j,k)+gbuf(i-1,j,k))

           j=xb%mjy+1
           v_global(j,i,k) = 0.5*(gbuf(i,j-1,k)+gbuf(i-1,j-1,k))
        enddo

        v_global(1,1,k) = 2.0*v_global(1,2,k)-v_global(1,3,k)

        v_global(1,xb%mix+1,k) = 2.0*v_global(2,xb%mix+1,k)-v_global(3,xb%mix+1,k)

        v_global(xb%mjy+1,1,k) = 2.0*v_global(xb%mjy+1,2,k)-v_global(xb%mjy+1,3,k)

        v_global(xb%mjy+1,xb%mix+1,k) = 2.0*v_global(xb%mjy+1,xb%mix,k)-v_global(xb%mjy+1,xb%mix-1,k)
     enddo

   END IF   !end if(wrf_dm_on_monitor())

!------------------------------------------------------------------------------
!  calculate 3DVAR analysis increments:
!------------------------------------------------------------------------------

!w
     call local_to_global( xp, xb % w, gbuf, 3 )
     IF ( wrf_dm_on_monitor() ) THEN
        do k=1, xb%mkz+1
        do j=1, xb%mjy
        do i=1, xb%mix
           w_global(j,i,k) = gbuf(i,j,k)
           w_global(j,i,k) = 0.0
        enddo
        enddo
        enddo
     END IF

!t
     call local_to_global( xp, xb % t, gbuf, 3 )
     IF ( wrf_dm_on_monitor() ) THEN
        do k=1, xb%mkz
        do j=1, xb%mjy
        do i=1, xb%mix
           t_global(j,i,k) = gbuf(i,j,k)
        enddo
        enddo
        enddo
     END IF

     call local_to_global( xp, xb % p, gbuf, 3 )
     IF ( wrf_dm_on_monitor() ) THEN
        do k=1, xb%mkz
        do j=1, xb%mjy
        do i=1, xb%mix
           p_global(j,i,k) = gbuf(i,j,k) - (xb%psac(i,j)*xb%znu(k) + xb%ptop)
        enddo
        enddo
        enddo
     END IF

!q
     call local_to_global( xp, xb % q, gbuf, 3 )
     IF ( wrf_dm_on_monitor() ) THEN
        do k=1, xb%mkz
        do j=1, xb%mjy
        do i=1, xb%mix
           q_global(j,i,k) = gbuf(i,j,k)
        enddo
        enddo
        enddo
     END IF

!qcw
!    call local_to_global( xp, xb % qcw, gbuf, 3 )
!    IF ( wrf_dm_on_monitor() ) THEN
!       do k=1, xb%mkz
!       do j=1, xb%mjy
!       do i=1, xb%mix
!          qcw_global(j,i,k) = gbuf(i,j,k)
!       enddo
!       enddo
!       enddo
!    END IF

!qrn
!    call local_to_global( xp, xb % qrn, gbuf, 3 )
!    IF ( wrf_dm_on_monitor() ) THEN
!       do k=1, xb%mkz
!       do j=1, xb%mjy
!       do i=1, xb%mix
!          qrn_global(j,i,k) = gbuf(i,j,k)
!       enddo
!       enddo
!       enddo
!    END IF

!------------------------------------------------------------------------------
!--Loop over variables.
!------------------------------------------------------------------------------

   IF ( wrf_dm_on_monitor() ) THEN

!     print_info  = .false.

!------------------------------------------------------------------------------      
!--loop over each variable
!------------------------------------------------------------------------------

   var_loop: do

      n = nv
 
      if(index /= 5) then
         xb_mm5 % sub_header(n) % current_time = 0.0           ! Forecast time=0
         xb_mm5 % sub_header(n) % current_date = ANALYSIS_DATE ! Forecast date
      end if

      if(print_info) then
         call DA_print_sub_header(xb_mm5%sub_header(n))
      endif

      write(anl_unit) flag

      write(anl_unit) &
            xb_mm5%sub_header(n)%ndim, &
            xb_mm5%sub_header(n)%start_index, &
            xb_mm5%sub_header(n)%end_index, &
            xb_mm5%sub_header(n)%current_time, &
            xb_mm5%sub_header(n)%staggering, &
            xb_mm5%sub_header(n)%ordering, &
            xb_mm5%sub_header(n)%current_date, &
            xb_mm5%sub_header(n)%name, &
            xb_mm5%sub_header(n)%units, &
            xb_mm5%sub_header(n)%description

      allocate(data(xb_mm5%sub_header(nv)%start_index(1):xb_mm5%sub_header(nv)%end_index(1), &
                    xb_mm5%sub_header(nv)%start_index(2):xb_mm5%sub_header(nv)%end_index(2), &
                    xb_mm5%sub_header(nv)%start_index(3):xb_mm5%sub_header(nv)%end_index(3), &
                    xb_mm5%sub_header(nv)%start_index(4):xb_mm5%sub_header(nv)%end_index(4)))

      read(jpin,iostat=ier) data

      if (xb_mm5%sub_header(nv)%name == 'U        ') then
          print *, 'xb%mjy, xb%mix, xb%mkz=', xb%mjy, xb%mix, xb%mkz
          print *, 'size(data, dim=1), size(data, dim=2), size(data, dim=3)=', &
                    size(data, dim=1), size(data, dim=2), size(data, dim=3)
          data(1:xb%mjy+1,1:xb%mix+1,1:xb%mkz,1) = u_global(1:xb%mjy+1,1:xb%mix+1,1:xb%mkz)
      elseif (xb_mm5%sub_header(nv)%name == 'V        ') then
          data(1:xb%mjy+1,1:xb%mix+1,1:xb%mkz,1) = v_global(1:xb%mjy+1,1:xb%mix+1,1:xb%mkz)
      elseif (xb_mm5%sub_header(nv)%name == 'T        ') then
         data(1:xb%mjy,1:xb%mix,1:xb%mkz,1) = t_global(1:xb%mjy,1:xb%mix,1:xb%mkz)
!     elseif (xb_mm5%sub_header(nv)%name == 'CLW      ') then
!        data(1:xb%mjy,1:xb%mix,1:xb%mkz,1) = qcw_global(1:xb%mjy,1:xb%mix,1:xb%mkz)
!     elseif (xb_mm5%sub_header(nv)%name == 'RNW      ') then
!        data(1:xb%mjy,1:xb%mix,1:xb%mkz,1) = qrn_global(1:xb%mjy,1:xb%mix,1:xb%mkz)
      elseif (xb_mm5%sub_header(nv)%name == 'Q        ') then
         data(1:xb%mjy,1:xb%mix,1:xb%mkz,1) = q_global(1:xb%mjy,1:xb%mix,1:xb%mkz)

         where (data < 1.0e-6)
            data = 1.0e-6
         end where

      elseif (xb_mm5%sub_header(nv)%name == 'W        ') then
         data(1:xb%mjy,1:xb%mix,1:xb%mkz+1,1) = w_global(1:xb%mjy,1:xb%mix,1:xb%mkz+1)
      elseif (xb_mm5%sub_header(nv)%name == 'PP       ') then
         data(1:xb%mjy,1:xb%mix,1:xb%mkz,1) = p_global(1:xb%mjy,1:xb%mix,1:xb%mkz)
      elseif (xb_mm5%sub_header(nv)%name == 'GROUND T ') then
         data(1:xb%mjy,1:xb%mix,1,1) = t_global(1:xb%mjy,1:xb%mix,xb%mkz)
      end if

      write(anl_unit) data

      deallocate(data)

!-----Read new flag

      read(jpin, iostat=ier) flag

      if(ier/=0) then
         write(0,'("Error reading header flag")')
         call abort()
      endif

      if (flag == 0) then
         read(jpin, iostat=ier) xb_mm5 % big_header % bhi, &
                                xb_mm5 % big_header % bhr,&
                                xb_mm5 % big_header % bhic, &
                                xb_mm5 % big_header % bhrc
         if(ier/=0) then
            write(0,'("Error reading big header")')
            call abort()
         endif

         if(print_info) &
            call DA_print_big_header(xb_mm5%big_header%bhi, xb_mm5%big_header%bhr, &
                                     xb_mm5%big_header%bhic,xb_mm5%big_header%bhrc)
      elseif (flag == 1) then

         nv = nv + 1

         if( nv > max_num_of_var) then
           write(unit=*, fmt='(2(/a))') &
                'WARNING  WARNING  WARNING  WARNING  WARNING', &
                'No. of vars exceeded max_num_of_var set in DA_Constants/DA_Constants.F'
           write(unit=*, fmt='(/a,i4)') &
                'nv = ', nv, &
                'max_num_of_var = ', max_num_of_var

           write(unit=*, fmt='(/a/)') &
                'The New IC written out MAY LOSE variables for MM5.'

           nv = max_num_of_var
         endif

         read(jpin,iostat=ier) &
              xb_mm5%sub_header(nv)%ndim, &
              xb_mm5%sub_header(nv)%start_index, &
              xb_mm5%sub_header(nv)%end_index, &
              xb_mm5%sub_header(nv)%current_time, &
              xb_mm5%sub_header(nv)%staggering, &
              xb_mm5%sub_header(nv)%ordering, &
              xb_mm5%sub_header(nv)%current_date, &
              xb_mm5%sub_header(nv)%name, &
              xb_mm5%sub_header(nv)%units, &
              xb_mm5%sub_header(nv)%description

         if(ier/=0) then
            write(0,'("Error reading subheader")')
            call abort()
         endif

         if(print_info) then
            call DA_print_sub_header(xb_mm5%sub_header(nv))
         endif

      elseif (flag == 2) then
         exit
      else
         stop
      endif

   enddo var_loop

!------------------------------------------------------------------------------      
!--[3.0] Check if missed terrain file variables
!------------------------------------------------------------------------------

   nv = nv + 1

   ter_loop: do

      read(terrain_unit, iostat=ier) flag

      if(ier/=0) then
         write(0,'("Error reading terrain header flag")')
         exit ter_loop
      endif

      if (flag == 0) then
         read(terrain_unit, iostat=ier) xb_mm5 % big_header % bhi, &
                                        xb_mm5 % big_header % bhr,&
                                        xb_mm5 % big_header % bhic, &
                                        xb_mm5 % big_header % bhrc
         if(ier/=0) then
            write(0,'("Error reading terrain header flag")')
            exit ter_loop
         endif

         if(print_info) &
            call DA_print_big_header(xb_mm5%big_header%bhi, xb_mm5%big_header%bhr, &
                                     xb_mm5%big_header%bhic,xb_mm5%big_header%bhrc)
      elseif (flag == 1) then

         read(terrain_unit,iostat=ier) &
              xb_mm5%sub_header(nv)%ndim, &
              xb_mm5%sub_header(nv)%start_index, &
              xb_mm5%sub_header(nv)%end_index, &
              xb_mm5%sub_header(nv)%current_time, &
              xb_mm5%sub_header(nv)%staggering, &
              xb_mm5%sub_header(nv)%ordering, &
              xb_mm5%sub_header(nv)%current_date, &
              xb_mm5%sub_header(nv)%name, &
              xb_mm5%sub_header(nv)%units, &
              xb_mm5%sub_header(nv)%description

         if(ier/=0) then
            write(0,'("Error reading subheader")')
            call abort()
         endif

         if(print_info) then
            call DA_print_sub_header(xb_mm5%sub_header(nv))
         endif

         allocate(data(xb_mm5%sub_header(nv)%start_index(1):xb_mm5%sub_header(nv)%end_index(1), &
                       xb_mm5%sub_header(nv)%start_index(2):xb_mm5%sub_header(nv)%end_index(2), &
                       xb_mm5%sub_header(nv)%start_index(3):xb_mm5%sub_header(nv)%end_index(3), &
                       xb_mm5%sub_header(nv)%start_index(4):xb_mm5%sub_header(nv)%end_index(4)))

         read(terrain_unit,iostat=ier) data

         need_write = .true.

         chk_loop: do n=1,nv-1
            if(xb_mm5%sub_header(nv)%name == xb_mm5%sub_header(n)%name ) then
               need_write = .false.

               exit
            end if
         end do chk_loop

         if ( need_write ) then

            xb_mm5%sub_header(n) % current_time = 0.0           ! Forecast time=0
            xb_mm5%sub_header(n) % current_date = ANALYSIS_DATE ! Forecast date

            if(print_info) &
               call DA_print_big_header(xb_mm5%big_header%bhi, xb_mm5%big_header%bhr, &
                                        xb_mm5%big_header%bhic,xb_mm5%big_header%bhrc)

            write(anl_unit) flag

            write(anl_unit) &
                  xb_mm5%sub_header(n)%ndim, &
                  xb_mm5%sub_header(n)%start_index, &
                  xb_mm5%sub_header(n)%end_index, &
                  xb_mm5%sub_header(n)%current_time, &
                  xb_mm5%sub_header(n)%staggering, &
                  xb_mm5%sub_header(n)%ordering, &
                  xb_mm5%sub_header(n)%current_date, &
                  xb_mm5%sub_header(n)%name, &
                  xb_mm5%sub_header(n)%units, &
                  xb_mm5%sub_header(n)%description

            write(anl_unit) data

         end if

         deallocate(data)

      elseif (flag == 2) then
         write(anl_unit) flag

         exit
      else
         stop
      end if

   enddo ter_loop

   if ( ier /= 0 ) then
      flag = 2

      write(anl_unit) flag
   end if
   END IF

!------------------------------------------------------------------------------      
!  [5.0] Tidy up:
!------------------------------------------------------------------------------  

   IF ( wrf_dm_on_monitor() ) THEN
      DEALLOCATE ( u_global )
      DEALLOCATE ( v_global )
      DEALLOCATE ( t_global )
      DEALLOCATE ( q_global )
!     DEALLOCATE ( qcw_global )
!     DEALLOCATE ( qrn_global )
      DEALLOCATE ( p_global )
      DEALLOCATE ( w_global )
   END IF

end subroutine da_transfer_xbtomm5

