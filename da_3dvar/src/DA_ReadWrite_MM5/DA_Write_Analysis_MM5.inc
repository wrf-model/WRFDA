subroutine da_write_analysis_mm5( ix, jy, kz, xp, xb, xa, it )

!------------------------------------------------------------------------------
!  PURPOSE: Compute and write out final analysis in MM5 format.
!
!  METHOD: 1) Read MM5 format first guess.
!          2) Interpolate u, v to MM5 B-grid.
!          3) Write out WRFVAR header information.
!          4) Write out WRFVAR analysis (and increments).
!
!  HISTORY: 10/06/2001 - Creation of F90 version.           Dale Barker
!
!  PARENT_MODULE: DA_ReadWrite_MM5
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in)           :: it          ! outer-loop index
   integer, intent(in)           :: ix          ! 1st dimension of xa.
   integer, intent(in)           :: jy          ! 2nd dimension of xa.
   integer, intent(in)           :: kz          ! 3rd dimension of xa.
   type (xpose_type), intent(in) :: xp          ! Domain decomposition vars.
   type (xb_type), intent(inout) :: xb          ! Analysis.
   type (x_type), intent(in)     :: xa          ! Analysis Increments.
   
   type (mm5_model_type)         :: xb_mm5      ! MM5 model structure.
   integer                       :: ier         ! Input error flag.

   real           :: u_bgrid(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme) ! u interpolated to MM5 B-grid.
   real           :: v_bgrid(xp%ims:xp%ime,xp%jms:xp%jme,xp%kms:xp%kme) ! v interpolated to MM5 B-grid.
   real           :: ref_p(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte)   ! Reference pressure.
   real           :: ref_h(xp%its:xp%ite,xp%jts:xp%jte,xp%kts:xp%kte)   ! Reference height.

   real           :: bbuf(1:ix+1,1:jy+1)      ! Buffer for wind increments on B-grid.
   real           :: gbuf(1:ix,1:jy,1:kz)     ! Buffer for values on global-domain.
   real           :: ub_yrow(1:jy,1:kz)       ! Buffer for last row of u on B-grid.
   real           :: vb_yrow(1:jy,1:kz)       ! Buffer for last row of v on B-grid.
   real           :: ub_xcol(1:ix,1:kz)       ! Buffer for last column of u on B-grid.
   real           :: vb_xcol(1:ix,1:kz)       ! Buffer for last column of v on B-grid.
   real           :: ub_corner(1:kz)          ! Buffer for corner point of u on B-grid.
   real           :: vb_corner(1:kz)          ! Buffer for corner point of v on B-grid.

   integer        :: len, index, seconds, nv, n
   integer        :: size3d

   integer        :: is, ie        ! i range of processor subdomain.
   integer        :: js, je        ! j range of processor subdomain.
   integer        :: ks, ke        ! k range of processor subdomain.
   integer        :: k,i,j         ! Loop counter.

!  integer        :: mix, mjy, mkz

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   integer :: anl_unit, inc_unit

   logical :: print_info, need_write

!----------------------------------------------------------------------------

!--Force output in IEEE32

   integer(kind=4) :: flag, big_head_flag
   real(kind=4), dimension(:,:,:,:), allocatable :: data

   real, dimension(:,:,:), allocatable :: u_global, v_global, w_global, &
                                          t_global, p_global, q_global

   real, dimension(:,:,:), allocatable :: qcw_global, qrn_global,  &
                                          qci_global, qsn_global, qgr_global

   real, dimension(:,:),   allocatable :: tgglobal

!---------------------------------------------------------------------------
!  [1.0] Read original MM5 format first guess:
!---------------------------------------------------------------------------

   IF ( wrf_dm_on_monitor() ) THEN

   inc_unit = jpout - max_ext_its + it
   anl_unit = jpout+10 - max_ext_its + it

   if(PRINT_DETAIL > 0) then
      print_info  = .true.
   else
      print_info  = .false.
   endif

   seconds = 86400

   nv = 1

   rewind(UNIT=jpin)
   
!--Loop through headers to find the right time.

   time_loop: do
      read(UNIT=jpin, iostat=ier) flag

      if(ier/=0) then
        CALL wrf_error_fatal3(__FILE__,__LINE__, &
          "Error reading header flag")
      endif

      if (flag == 0) then
         read(UNIT=jpin, iostat=ier) xb_mm5 % big_header % bhi, &
                                xb_mm5 % big_header % bhr,&
                                xb_mm5 % big_header % bhic, &
                                xb_mm5 % big_header % bhrc
         if(ier/=0) then
           CALL wrf_error_fatal3(__FILE__,__LINE__, &
             "Error reading big header")
         endif

         if(print_info) &
            call DA_print_big_header(xb_mm5%big_header%bhi, xb_mm5%big_header%bhr, &
                                     xb_mm5%big_header%bhic,xb_mm5%big_header%bhrc)
         big_head_flag = flag
      elseif (flag == 1) then

         read(UNIT=jpin,iostat=ier) &
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
           CALL wrf_error_fatal3(__FILE__,__LINE__, &
             "Error reading subheader")
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

         read(UNIT=jpin,iostat=ier) data

         deallocate(data)

      elseif (flag == 2) then
         cycle
      else
        CALL wrf_error_fatal3(__FILE__,__LINE__, "??")
      endif
   enddo time_loop

   if(index /= 5) then
      len = len_trim(ANALYSIS_DATE)

      xb_mm5 % big_header % bhi( 1, 1) = 5

!-----Four-digit year of start time

      read(UNIT=ANALYSIS_DATE( 1:04), fmt='(I4)') xb_mm5 % big_header % bhi( 5, 5)

!-----Month of the year of the start time (1-12)

      read(UNIT=ANALYSIS_DATE( 6:07), fmt='(I2)') xb_mm5 % big_header % bhi( 6, 5)

!-----Day of the month of the start time (1-31)

      read(UNIT=ANALYSIS_DATE( 9:10), fmt='(I2)') xb_mm5 % big_header % bhi( 7, 5)

!-----Hour of the day of the start time (0-23)

      read(UNIT=ANALYSIS_DATE(12:13), fmt='(I2)') xb_mm5 % big_header % bhi( 8, 5)

!-----Minute of the start time (0-59)

      read(UNIT=ANALYSIS_DATE(15:16), fmt='(I2)') xb_mm5 % big_header % bhi( 9, 5)

!-----Second of the start time (0-59)

      read(UNIT=ANALYSIS_DATE(18:19), fmt='(I2)') xb_mm5 % big_header % bhi(10, 5)

!-----Ten thousandths of a second of the start time (0-9999)

      read(UNIT=ANALYSIS_DATE(21:24), fmt='(I4)') xb_mm5 % big_header % bhi(11, 5)

!     do len=1,xb_mm5 % num_of_var
!        xb_mm5 % sub_header(len) % current_time = 0.0           ! Forecast time=0
!        xb_mm5 % sub_header(len) % current_date = ANALYSIS_DATE ! Forecast date
!     end do
!  else
!     xb_mm5 % big_header % bhi(11,5) = 0   ! TEN THOUSANDTHS OF SECONDS
   end if

   if ( write_increments ) then
      xb_mm5 % big_header % bhic(1,1) = 'MM5 WRFVAR Analysis Increments               '

      open(unit=inc_unit, form='unformatted', status='unknown')

      write(UNIT=inc_unit) big_head_flag

      write(UNIT=inc_unit) xb_mm5%big_header%bhi,  xb_mm5%big_header%bhr, &
                      xb_mm5%big_header%bhic, xb_mm5%big_header%bhrc
   end if

   xb_mm5 % big_header % bhic(1,1) = 'MM5 WRFVAR Analysis                          '

   open(unit=anl_unit, form='unformatted', status='unknown')

   write(UNIT=anl_unit) big_head_flag

   write(UNIT=anl_unit) xb_mm5%big_header%bhi,  xb_mm5%big_header%bhr, &
                   xb_mm5%big_header%bhic, xb_mm5%big_header%bhrc

      ALLOCATE ( u_global (1:ix+1,1:jy+1,1:kz) )
      ALLOCATE ( v_global (1:ix+1,1:jy+1,1:kz) )
      ALLOCATE ( w_global (1:ix+1,1:jy+1,1:kz+1) )
      ALLOCATE ( t_global (1:ix+1,1:jy+1,1:kz) )
      ALLOCATE ( p_global (1:ix+1,1:jy+1,1:kz) )
      ALLOCATE ( q_global (1:ix+1,1:jy+1,1:kz) )
      ALLOCATE ( qcw_global (1:ix+1,1:jy+1,1:kz) )
      ALLOCATE ( qrn_global (1:ix+1,1:jy+1,1:kz) )
      ALLOCATE ( qci_global (1:ix+1,1:jy+1,1:kz) )
      ALLOCATE ( qsn_global (1:ix+1,1:jy+1,1:kz) )
      ALLOCATE ( qgr_global (1:ix+1,1:jy+1,1:kz) )
      ALLOCATE ( tgglobal (1:ix+1,1:jy+1) )

      u_global(:,:,:) = 0.0
      v_global(:,:,:) = 0.0
      w_global(:,:,:) = 0.0
      t_global(:,:,:) = 0.0
      p_global(:,:,:) = 0.0
      q_global(:,:,:) = 0.0
      tgglobal(:,:) = 0.0

      qcw_global(:,:,:) = 0.0
      qrn_global(:,:,:) = 0.0
      qci_global(:,:,:) = 0.0
      qsn_global(:,:,:) = 0.0
      qgr_global(:,:,:) = 0.0
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
   size3d = ix*jy*kz

!------------------------------------------------------------------------------
! Convert u from a to b grid:
!------------------------------------------------------------------------------

!  Collect U component of wind increments into global buffer.
   call local_to_global( xp, xa % u, gbuf, 3 ) 
   call wrf_dm_bcast_real( gbuf, size3d )

!  Convert increments (global 2D) B-grid buffer and store into (local) u_bgrid.
   do k = ks,ke
     call a2b( gbuf(1:ix,1:jy,k), ix, jy, 1, bbuf(1:ix,1:jy) )
     u_bgrid(is:ie,js:je,k) = bbuf(is:ie,js:je)
   end do

! Gather u_bgrid across processors into global buffer.
   call local_to_global( xp, u_bgrid, gbuf, 3 ) 

   IF ( wrf_dm_on_monitor() ) THEN

!    B-grid u wind analysis = First guess + interpolated analysis increments:

     u_global(1:ix,1:jy,ks:ke) = gbuf(1:ix,1:jy,ks:ke)

!    Interpolate last row, last column, and corner point:

     ub_yrow(1:jy,1:kz) = 2.0 * gbuf(ix,1:jy,1:kz) - gbuf(ix-1,1:jy,1:kz)
     ub_xcol(1:ix,1:kz) = 2.0 * gbuf(1:ix,jy,1:kz) - gbuf(1:ix,jy-1,1:kz)
     ub_corner(1:kz) = 2.0 * (2.0 * gbuf(ix,jy,1:kz) - gbuf(ix-1,jy,1:kz)) - &
                       2.0 * gbuf(ix,jy-1,1:kz) + gbuf(ix-1,jy-1,1:kz)

     u_global(ix+1,1:jy,1:kz) = ub_yrow(1:jy,1:kz)
     u_global(1:ix,jy+1,1:kz) = ub_xcol(1:ix,1:kz)
     u_global(ix+1,jy+1,1:kz) = ub_corner(1:kz)

   END IF   !end if(wrf_dm_on_monitor())

!  Collect V component of wind increments into global buffer.
   call local_to_global( xp, xa % v, gbuf, 3 )
   call wrf_dm_bcast_real( gbuf, size3d )
  
!  Convert increments (global 2D) B-grid buffer and store into (local) v_bgrid.
   do k = ks,ke
     call a2b( gbuf(1:ix,1:jy,k), ix, jy, 1, bbuf(1:ix,1:jy) )
     v_bgrid(is:ie,js:je,k) = bbuf(is:ie,js:je)
   end do

! Gather v_bgrid across processors into global buffer.
   call local_to_global( xp, v_bgrid, gbuf, 3 )

   IF ( wrf_dm_on_monitor() ) THEN

!    B-grid v wind analysis = First guess + interpolated analysis increments:

     v_global(1:ix,1:jy,ks:ke) = gbuf(1:ix,1:jy,ks:ke)

!    Interpolate last row, last column, and corner point:

     vb_yrow(1:jy,1:kz) = 2.0 * gbuf(ix,1:jy,1:kz) - gbuf(ix-1,1:jy,1:kz)
     vb_xcol(1:ix,1:kz) = 2.0 * gbuf(1:ix,jy,1:kz) - gbuf(1:ix,jy-1,1:kz)
     vb_corner(1:kz) = 2.0 * (2.0 * gbuf(ix,jy,1:kz) - gbuf(ix-1,jy,1:kz)) - &
                       2.0 * gbuf(ix,jy-1,1:kz) + gbuf(ix-1,jy-1,1:kz)

     v_global(ix+1,1:jy,1:kz) = vb_yrow(1:jy,1:kz) 
     v_global(1:ix,jy+1,1:kz) = vb_xcol(1:ix,1:kz)
     v_global(ix+1,jy+1,1:kz) = vb_corner(1:kz)

   END IF   !end if(wrf_dm_on_monitor())

!------------------------------------------------------------------------------
!  calculate WRFVAR analysis increments:
!------------------------------------------------------------------------------

!w
     call local_to_global( xp, xa % w, gbuf, 3 )
     IF ( wrf_dm_on_monitor() ) THEN
       w_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz)
!      w_global(1:ix,1:jy,1:kz+1) = gbuf(1:ix,1:jy,1:kz+1)
     END IF

!t
     call local_to_global( xp, xa % t, gbuf, 3 )
     IF ( wrf_dm_on_monitor() ) THEN
       t_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz)
       tgglobal (1:ix,1:jy)  = gbuf(1:ix,1:jy,kz)
     END IF

     call local_to_global( xp, xa % p, gbuf, 3 )
     IF ( wrf_dm_on_monitor() ) THEN
       p_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz)
     END IF

!q
     call local_to_global( xp, xa % q, gbuf, 3 )
     IF ( wrf_dm_on_monitor() ) THEN
       q_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz)
     END IF

!qcw
     if ( write_qcw ) then
     call local_to_global( xp, xa % qcw, gbuf, 3 )
     IF ( wrf_dm_on_monitor() ) THEN
       qcw_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz)
     END IF
     END IF

!qrn
     if ( write_qrn ) then
     call local_to_global( xp, xa % qrn, gbuf, 3 )
     IF ( wrf_dm_on_monitor() ) THEN
       qrn_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz)
     END IF
     END IF

!qci
     if ( write_qci ) then
     call local_to_global( xp, xa % qci, gbuf, 3 )
     IF ( wrf_dm_on_monitor() ) THEN
       qci_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz)
     END IF
     END IF

!qsn
     if ( write_qsn ) then
     call local_to_global( xp, xa % qsn, gbuf, 3 )
     IF ( wrf_dm_on_monitor() ) THEN
       qsn_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz)
     END IF
     END IF

!qgr
     if ( write_qgr ) then
     call local_to_global( xp, xa % qgr, gbuf, 3 )
     IF ( wrf_dm_on_monitor() ) THEN
       qgr_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz)
     END IF
     END IF

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

      write(UNIT=anl_unit) flag

      write(UNIT=anl_unit) &
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

      if ( write_increments ) then
         write(UNIT=inc_unit) flag

         write(UNIT=inc_unit) &
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
      end if

      allocate(data(xb_mm5%sub_header(nv)%start_index(1):xb_mm5%sub_header(nv)%end_index(1), &
                    xb_mm5%sub_header(nv)%start_index(2):xb_mm5%sub_header(nv)%end_index(2), &
                    xb_mm5%sub_header(nv)%start_index(3):xb_mm5%sub_header(nv)%end_index(3), &
                    xb_mm5%sub_header(nv)%start_index(4):xb_mm5%sub_header(nv)%end_index(4)))

      read(UNIT=jpin,iostat=ier) data

      if (xb_mm5%sub_header(nv)%name == 'U        ') then
         call add_3d(data, u_global, ix+1, jy+1, kz)
      elseif (xb_mm5%sub_header(nv)%name == 'V        ') then
         call add_3d(data, v_global, ix+1, jy+1, kz)
      elseif (xb_mm5%sub_header(nv)%name == 'T        ') then
         call add_3d(data, t_global, ix+1, jy+1, kz)
      elseif (xb_mm5%sub_header(nv)%name == 'CLW      ') then
         call add_3d(data, qcw_global, ix+1, jy+1, kz)
         where (data < 0.0)
            data = 0.0
         end where
      elseif (xb_mm5%sub_header(nv)%name == 'RNW      ') then
         call add_3d(data, qrn_global, ix+1, jy+1, kz)
         where (data < 0.0)
            data = 0.0
         end where
      elseif (xb_mm5%sub_header(nv)%name == 'ICE      ') then
         call add_3d(data, qci_global, ix+1, jy+1, kz)
         where (data < 0.0)
            data = 0.0
         end where
      elseif (xb_mm5%sub_header(nv)%name == 'SNOW     ') then
         call add_3d(data, qsn_global, ix+1, jy+1, kz)
         where (data < 0.0)
            data = 0.0
         end where
      elseif (xb_mm5%sub_header(nv)%name == 'GRAUPEL  ') then
         call add_3d(data, qgr_global, ix+1, jy+1, kz)
         where (data < 0.0)
            data = 0.0
         end where
      elseif (xb_mm5%sub_header(nv)%name == 'Q        ') then
         call add_3d(data, q_global, ix+1, jy+1, kz)

         where (data < 1.0e-6)
            data = 1.0e-6
         end where

      elseif (xb_mm5%sub_header(nv)%name == 'W        ') then
         call add_3d(data, w_global, ix+1, jy+1, kz+1)
      elseif (xb_mm5%sub_header(nv)%name == 'PP       ') then
         call add_3d(data, p_global, ix+1, jy+1, kz)
      elseif (xb_mm5%sub_header(nv)%name == 'GROUND T ') then
         call add_2d(data, tgglobal, ix+1, jy+1)
      end if

      write(UNIT=anl_unit) data

      if ( write_increments ) then
         if (xb_mm5%sub_header(nv)%name == 'U        ') then
            data = 0.0
            call add_3d(data, u_global, ix+1, jy+1, kz)
         elseif (xb_mm5%sub_header(nv)%name == 'V        ') then
            data = 0.0
            call add_3d(data, v_global, ix+1, jy+1, kz)
         elseif (xb_mm5%sub_header(nv)%name == 'T        ') then
            data = 0.0
            call add_3d(data, t_global, ix+1, jy+1, kz)
         elseif (xb_mm5%sub_header(nv)%name == 'Q        ') then
            data = 0.0
            call add_3d(data, q_global, ix+1, jy+1, kz)
         elseif (xb_mm5%sub_header(nv)%name == 'CLW      ') then
            data = 0.0
            call add_3d(data, qcw_global, ix+1, jy+1, kz)
         elseif (xb_mm5%sub_header(nv)%name == 'RNW        ') then
            data = 0.0
            call add_3d(data, qrn_global, ix+1, jy+1, kz)
         elseif (xb_mm5%sub_header(nv)%name == 'ICE        ') then
            data = 0.0
            call add_3d(data, qci_global, ix+1, jy+1, kz)
         elseif (xb_mm5%sub_header(nv)%name == 'SNOW       ') then
            data = 0.0
            call add_3d(data, qsn_global, ix+1, jy+1, kz)
         elseif (xb_mm5%sub_header(nv)%name == 'GRAUPEL    ') then
            data = 0.0
            call add_3d(data, qgr_global, ix+1, jy+1, kz)
         elseif (xb_mm5%sub_header(nv)%name == 'W        ') then
            data = 0.0
            call add_3d(data, w_global, ix+1, jy+1, kz+1)
         elseif (xb_mm5%sub_header(nv)%name == 'PP       ') then
            data = 0.0
            call add_3d(data, p_global, ix+1, jy+1, kz)
         elseif (xb_mm5%sub_header(nv)%name == 'GROUND T ') then
            call add_2d(data, tgglobal, ix+1, jy+1)
         end if

         write(UNIT=inc_unit) data
      end if

      deallocate(data)

!-----Read new flag

      read(UNIT=jpin, iostat=ier) flag

      if(ier/=0) then
        CALL wrf_error_fatal3(__FILE__,__LINE__, &
          "Error reading header flag")
      endif

      if (flag == 0) then
         read(UNIT=jpin, iostat=ier) xb_mm5 % big_header % bhi, &
                                xb_mm5 % big_header % bhr,&
                                xb_mm5 % big_header % bhic, &
                                xb_mm5 % big_header % bhrc
         if(ier/=0) then
           CALL wrf_error_fatal3(__FILE__,__LINE__, &
             "Error reading big header")
         endif

         if(print_info) &
            call DA_print_big_header(xb_mm5%big_header%bhi, xb_mm5%big_header%bhr, &
                                     xb_mm5%big_header%bhic,xb_mm5%big_header%bhrc)
      elseif (flag == 1) then

         nv = nv + 1

         if( nv > max_num_of_var) then
           write(unit=stdout, fmt='(2(/a))') &
                'WARNING  WARNING  WARNING  WARNING  WARNING', &
                'No. of vars exceeded max_num_of_var set in DA_Constants/DA_Constants.F'
           write(unit=stdout, fmt='(/a,i4)') &
                'nv = ', nv, &
                'max_num_of_var = ', max_num_of_var

           write(unit=stdout, fmt='(/a/)') &
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
           CALL wrf_error_fatal3(__FILE__,__LINE__, &
             "Error reading subheader")
         endif

         if(print_info) then
            call DA_print_sub_header(xb_mm5%sub_header(nv))
         endif

      elseif (flag == 2) then
!        write(anl_unit) flag

!        if ( write_increments ) write(inc_unit) flag

         exit
      else
        CALL wrf_error_fatal3(__FILE__,__LINE__,"??")
      endif

   enddo var_loop

!------------------------------------------------------------------------------      
!--[3.0] Check if missed terrain file variables
!------------------------------------------------------------------------------

   nv = nv + 1

   ter_loop: do

      read(terrain_unit, iostat=ier) flag

      if(ier/=0) then
         write(UNIT=stderr) "Error reading terrain header flag"
         exit ter_loop
      endif

      if (flag == 0) then
         read(terrain_unit, iostat=ier) xb_mm5 % big_header % bhi, &
                                        xb_mm5 % big_header % bhr,&
                                        xb_mm5 % big_header % bhic, &
                                        xb_mm5 % big_header % bhrc
         if(ier/=0) then
            write(UNIT=stderr) "Error reading terrain header flag"
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
           CALL wrf_error_fatal3(__FILE__,__LINE__, &
             "Error reading subheader")
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

            write(UNIT=anl_unit) flag

            write(UNIT=anl_unit) &
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

            write(UNIT=anl_unit) data

            if ( write_increments ) then
               write(UNIT=inc_unit) flag
      
               write(UNIT=inc_unit) &
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

               write(UNIT=inc_unit) data
            end if
         end if

         deallocate(data)

      elseif (flag == 2) then
         write(UNIT=anl_unit) flag

         if ( write_increments ) write(UNIT=inc_unit) flag

         exit
      else
        CALL wrf_error_fatal3(__FILE__,__LINE__,"??")
      end if

   enddo ter_loop

   if ( ier /= 0 ) then
      flag = 2

      write(UNIT=anl_unit) flag

      if ( write_increments ) write(UNIT=inc_unit) flag
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
      DEALLOCATE ( qcw_global )
      DEALLOCATE ( qrn_global )
      DEALLOCATE ( qci_global )
      DEALLOCATE ( qsn_global )
      DEALLOCATE ( qgr_global )
      DEALLOCATE ( p_global )
      DEALLOCATE ( w_global )
      DEALLOCATE ( tgglobal)
   END IF

contains

subroutine add_3d(data, v_wrf, ix, jy, kz)

   implicit none

   integer,                                   intent(in)    :: ix, jy, kz
   real, dimension(1:ix,1:jy,1:kz),           intent(in)    :: v_wrf
   real(kind=4), dimension(1:jy,1:ix,1:kz,1), intent(inout) :: data

   integer :: i,j,k,n

   do k=1,kz
      n=kz+1-k
   do j=1,jy
   do i=1,ix
      data(j,i,k,1) = data(j,i,k,1) + v_wrf(i,j,n)
   enddo
   enddo
   enddo

end subroutine add_3d

subroutine add_2d(data, v_wrf, ix, jy)

   implicit none

   integer,                                intent(in)    :: ix, jy
   real, dimension(1:ix,1:jy),             intent(in)    :: v_wrf
   real(kind=4), dimension(1:jy,1:ix,1,1), intent(inout) :: data

   integer :: i,j

   do j=1,jy
   do i=1,ix
      data(j,i,1,1) = data(j,i,1,1) + v_wrf(i,j)
   enddo
   enddo

end subroutine add_2d

end subroutine da_write_analysis_mm5

