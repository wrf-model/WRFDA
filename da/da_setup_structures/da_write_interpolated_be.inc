subroutine da_write_interpolated_be ( be_big_header, pb_vert_reg,     &
                                      e1, e2, e3, e4, l1, l2, l3, l4, &
                                      s1, s2, s3, s4 )

   !----------------------------------------------------------------------------
   !
   ! This subroutine is used to write out the interpolated background error (BE)
   ! statistics file.
   !  
   ! 1) Argumnets:
   !
   !   be_big_header provides the big_headr%bhi(*,10), characteristics of BE;
   !   e1, e2, e3, e4 are the interpolated eigenvectors of psi, chi, p_u, and q;
   !   l1, l2, l3, l4 are the interpolated eigenvalues of psi, chi, p_u, and q;
   !   s1, s2, s3, s4 are the interpolated scale-length of psi, chi, p_u, and q;
   !   pb_vert_reg is the interpolated regression coefficients of balanced pressure. 
   !
   ! 2) The output "interpolated BE file" is fort.411, which is the standard MM5V3
   !    format(i4r4), and can be used as an BE input file with the namelist variable:
   !    interpolate_stats =.false. in 3dvar run and as an input in plotting program.
   !
   ! 3) Note: a) Since the current "interpolate_stats" is only implemented with the 
   !             GLOBAL Eigenvectors/values, the output interpolated BE only contains
   !             the GLOBAL Eigenvectors/values, no LOCAL ones.
   !
   !          b) The moisture control variable depends on "cv_options_hum" setting.
   !             If cv_options_hum = 1, only Q  statistics are written out;
   !             If cv_options_hum = 2, only RH statistics are written out.
   !             When you want to use this interpolated BE file to run wrfvar, you
   !             must check if cv_options_hum setting is consistent.
   !
   !------------------------------------------------------------------------------
      
   IMPLICIT NONE

   TYPE (big_record_header), INTENT(IN)   :: be_big_header  ! old BE big_header
   REAL, dimension(:,:,:)  , INTENT(IN)   :: pb_vert_reg    ! regression coeff.
   REAL, dimension(:,:)    , INTENT(IN)   :: e1, e2, e3, e4 ! eigenvectors 
   REAL, dimension(:)      , INTENT(IN)   :: l1, l2, l3, l4, s1, s2, s3, s4
                                           ! eigenvalues     scake-length
   
   Type (sub_record_header)                       :: sub_header
   TYPE (mm5_model_type)                          :: x_mm5   ! mm5
   real (kind=4), dimension(:,:,:,:), allocatable :: data
   integer, parameter                             :: flag0 = 0, flag1 = 1, &
                                                     flag2 = 2, ibe_unit = 411
   integer                                        :: n, nv, ier, flag, &
                                                     mix, mjy, mkz
   real                                           :: ds
   logical                                        :: print_info
   character (len=2) :: hh, dd, mm, yy

   integer  :: ib1, ib2, ib3, id1, id2

   IF (trace_use) CALL da_trace_entry("da_write_interpolated_be")

   nv = 1
   n  = nv
   print_info = .false.

   ! -------------------------------------------------------------------------
   ! 1. Write out the big_header and constant constant fields for BE file:
   ! -------------------------------------------------------------------------

   rewind(jpin)

   mm5_loop: do

      read(jpin, iostat=ier) flag

      if(ier/=0) then
         CALL wrf_error_fatal3(__FILE__,__LINE__, &
            'Error reading header flag')
      endif

      if (flag == 0) then
         read(jpin, iostat=ier) x_mm5 % big_header % bhi, &
                                x_mm5 % big_header % bhr,&
                                x_mm5 % big_header % bhic, &
                                x_mm5 % big_header % bhrc
         if(ier/=0) then
            CALL wrf_error_fatal3(__FILE__,__LINE__, &
              "Error reading big header")
         endif

         if(print_info) &
            call DA_print_big_header(x_mm5%big_header%bhi, &
                                     x_mm5%big_header%bhr, &
                                     x_mm5%big_header%bhic,&
                                     x_mm5%big_header%bhrc)

         !  1.1 Write out the big header flag
         !  ---------------------------------

         write(UNIT=ibe_unit) flag

         !  1.1.1 Modifty the Index

         x_mm5 % big_header % bhi ( 1, 1) = 10

         !  The global eigenvectors/values: vert_evalue = 1
         !  The local  eigenvectors/values: vert_evalue = 2

         if (vert_evalue == 1) then
            x_mm5 % big_header % bhic( 1, 1) = &
               ' INTP. GLOBAL SDV INC:: E VAL VEC 24 AND 12 HOUR FORECAST'
         else if  (vert_evalue == 2) then
            x_mm5 % big_header % bhic( 1, 1) = &
               ' INTP. LOCALI SDV INC:: E VAL VEC 24 AND 12 HOUR FORECAST'
         endif

         ! 1.1.2 Year, month, day, hour, minute, second,....

         read(UNIT=ANALYSIS_DATE( 1:04), fmt='(I4)') x_mm5 % big_header % bhi( 5, 5)
         read(UNIT=ANALYSIS_DATE( 6:07), fmt='(I2)') x_mm5 % big_header % bhi( 6, 5)
         read(UNIT=ANALYSIS_DATE( 9:10), fmt='(I2)') x_mm5 % big_header % bhi( 7, 5)
         read(UNIT=ANALYSIS_DATE(12:13), fmt='(I2)') x_mm5 % big_header % bhi( 8, 5)
         read(UNIT=ANALYSIS_DATE(15:16), fmt='(I2)') x_mm5 % big_header % bhi( 9, 5)
         read(UNIT=ANALYSIS_DATE(18:19), fmt='(I2)') x_mm5 % big_header % bhi(10, 5)
         read(UNIT=ANALYSIS_DATE(21:24), fmt='(I4)') x_mm5 % big_header % bhi(11, 5)

         sub_header%char_date = ANALYSIS_DATE( 3:04)//ANALYSIS_DATE( 6:07)// &
                                ANALYSIS_DATE( 9:10)//ANALYSIS_DATE(12:13)
         ! WRITE(UNIT=stdout,FMT='(2A)') "sub_header%char_date =",sub_header%char_date 

         ! 1.1.3 Characteristics of the Background Error Statistics form the OLD BE

         x_mm5 % big_header % bhi(:,10) = be_big_header % bhi(:,10)
         x_mm5 % big_header % bhr(:,10) = be_big_header % bhr(:,10)

         x_mm5 % big_header % bhic(:,10) = be_big_header % bhic(:,10)
         x_mm5 % big_header % bhrc(:,10) = be_big_header % bhrc(:,10)

         ! 1.2 Write out the big_header
         !  ----------------------------

         write(UNIT=ibe_unit) x_mm5 % big_header % bhi,  &
                              x_mm5 % big_header % bhr,  &
                              x_mm5 % big_header % bhic, &
                              x_mm5 % big_header % bhrc

         ! 1.3 To keep the grid-size for converting back Scale length back to Grid-points

         if (x_mm5 % big_header % bhi(30,10) /= 1 ) THEN
            ! scale length not available:
            ds = 2.0
         else
            ! scale length available:
            ds = x_mm5%big_header%bhr(9,1)
         endif

      elseif (flag == 1) then

         read(UNIT=jpin,iostat=ier) &
              x_mm5%sub_header(n)%ndim, &
              x_mm5%sub_header(n)%start_index, &
              x_mm5%sub_header(n)%end_index, &
              x_mm5%sub_header(n)%current_time, &
              x_mm5%sub_header(n)%staggering, &
              x_mm5%sub_header(n)%ordering, &
              x_mm5%sub_header(n)%current_date, &
              x_mm5%sub_header(n)%name, &
              x_mm5%sub_header(n)%units, &
              x_mm5%sub_header(n)%description

         if(ier/=0) then
            CALL wrf_error_fatal3(__FILE__,__LINE__, &
               "Error reading subheader")
         endif

         if(print_info) then
            call da_print_sub_header(x_mm5%sub_header(n))
         endif

         allocate(  data( &
            x_mm5%sub_header(n)%start_index(1):x_mm5%sub_header(n)%end_index(1),&
            x_mm5%sub_header(n)%start_index(2):x_mm5%sub_header(n)%end_index(2),&
            x_mm5%sub_header(n)%start_index(3):x_mm5%sub_header(n)%end_index(3),&
            x_mm5%sub_header(n)%start_index(4):x_mm5%sub_header(n)%end_index(4)))

         read(UNIT=jpin,iostat=ier) data

         ! 1.3 To write out the select the necessary constant fields:
         ! ---------------------------------------------------------

         if (x_mm5%sub_header(n)%name == 'PSTARCRS' .or. &
             x_mm5%sub_header(n)%name == 'GROUND T' .or. &
             x_mm5%sub_header(n)%name == 'TERRAIN ' .or. &
             x_mm5%sub_header(n)%name == 'MAPFACCR' .or. &
             x_mm5%sub_header(n)%name == 'MAPFACDT' .or. &
             x_mm5%sub_header(n)%name == 'CORIOLIS' .or. &
             x_mm5%sub_header(n)%name == 'LATITCRS' .or. &
             x_mm5%sub_header(n)%name == 'LONGICRS' .or. &
             x_mm5%sub_header(n)%name == 'LAND USE' .or. &
             x_mm5%sub_header(n)%name == 'SNOWCOVR' .or. &
             x_mm5%sub_header(n)%name == 'SIGMAH  ' ) then

            write(UNIT=ibe_unit) flag

            write(UNIT=ibe_unit) &
               x_mm5%sub_header(n)%ndim, &
               x_mm5%sub_header(n)%start_index, &
               x_mm5%sub_header(n)%end_index, &
               x_mm5%sub_header(n)%current_time, &
               x_mm5%sub_header(n)%staggering, &
               x_mm5%sub_header(n)%ordering, &
               x_mm5%sub_header(n)%current_date, &
               x_mm5%sub_header(n)%name, &
               x_mm5%sub_header(n)%units, &
               x_mm5%sub_header(n)%description

            write(UNIT=ibe_unit) data
            ! print '(I2," name=",a9," flag=",i2)', n,x_mm5%sub_header(n)%name,flag
            n = n + 1
         endif
         deallocate(data)

      elseif (flag == 2) then
         ! End of the one time period input file
        exit
      endif
   end do mm5_loop

   nv = n - 1
   mix = x_mm5 % big_header % bhi(16,1)
   mjy = x_mm5 % big_header % bhi(17,1)
   mkz = x_mm5 % big_header % bhi(12,5)
   
   ! -------------------------------------------------------------------------
   ! 2. Initialize the sub_header
   ! -------------------------------------------------------------------------
   sub_header%start_index  = (/1, 1, 1, 1/)
   sub_header%current_date = ANALYSIS_DATE
   sub_header%current_time = 0.0
   sub_header%staggering   = 'MODE'
   sub_header%ordering     = '    ' 
   sub_header%units        = '                         '
   sub_header%end_index(4) = 1

   n = 0

   ! -------------------------------------------------------------------------
   ! 3. Write out the fileds
   ! -------------------------------------------------------------------------
   !
   !    Note: In the current code of da_setup_background_errors.inc, 
   !          the "interpolate_stats" is only applied to the GLOBAL 
   !          Eigenvectors/values. So there are only GLOBAL interpolated 
   !          Eigenvectors/values could be written out.

   ! 3.1.1 GLOBAL EIGEN VECTORS for PSI
  
   sub_header%name         = 'PSI_EVECG'
   sub_header%description  = 'PSI GLOBAL EIGEN VECTOR'
   sub_header%ordering     = 'KM  ' 
   sub_header%units        = 'PSI                      '
   sub_header%ndim         = 2 
   sub_header%end_index(1) = mkz  
   sub_header%end_index(2) = mkz
   sub_header%end_index(3) = 1
  
   write(UNIT=ibe_unit) flag1
   call sub_write 
   call data_allocate
   data(:,:,1,1) = e1(:,:)
   write(UNIT=ibe_unit) data
   deallocate (data)
   n = n + 1
   WRITE(UNIT=stdout,FMT='(I2," Wrote field:",a)') n, sub_header%name

   ! 3.1.2 GLOBAL EIGEN VALUES  for PSI
  
   sub_header%name         = 'PSI_EVALG'
   sub_header%description  = 'PSI GLOBAL EIGEN VALUE '
   sub_header%ordering     = 'M   ' 
   sub_header%units        = 'PSI                      '
   sub_header%ndim         = 1 
   sub_header%end_index(1) = mkz  
   sub_header%end_index(2) = 1
   sub_header%end_index(3) = 1      
  
   write(UNIT=ibe_unit) flag1
   call sub_write 
   call data_allocate
   data(:,1,1,1) = l1(:)
   write(UNIT=ibe_unit) data
   deallocate (data)
   n = n + 1
   WRITE(UNIT=stdout,FMT='(I2," Wrote field:",a)') n, sub_header%name

   ! 3.2.1 GLOBAL EIGEN VECTORS for CHI
  
   sub_header%name         = 'CHI_EVECG'
   sub_header%description  = 'CHI GLOBAL EIGEN VECTOR'
   sub_header%ordering     = 'KM  ' 
   sub_header%units        = 'CHI                      '
   sub_header%ndim         = 2 
   sub_header%end_index(1) = mkz  
   sub_header%end_index(2) = mkz
   sub_header%end_index(3) = 1
  
   write(UNIT=ibe_unit) flag1
   call sub_write 
   call data_allocate
   data(:,:,1,1) = e2(:,:)
   write(UNIT=ibe_unit) data
   deallocate (data)
   n = n + 1
   WRITE(UNIT=stdout,FMT='(I2," Wrote field:",a)') n, sub_header%name

   ! 3.2.2 GLOBAL EIGEN VALUES  for CHI
  
   sub_header%name         = 'CHI_EVALG'
   sub_header%description  = 'CHI GLOBAL EIGEN VALUE '
   sub_header%ordering     = 'M   ' 
   sub_header%units        = 'CHI                      '
   sub_header%ndim         = 1 
   sub_header%end_index(1) = mkz  
   sub_header%end_index(2) = 1
   sub_header%end_index(3) = 1      
  
   write(UNIT=ibe_unit) flag1
   call sub_write 
   call data_allocate
   data(:,1,1,1) = l2(:)
   write(UNIT=ibe_unit) data
   deallocate (data)
   n = n + 1
   WRITE(UNIT=stdout,FMT='(I2," Wrote field:",a)') n, sub_header%name

   ! 3.3.1 GLOBAL EIGEN VECTORS for P_U
  
   sub_header%name         = 'P_U_EVECG'
   sub_header%description  = 'P_U GLOBAL EIGEN VECTOR'
   sub_header%ordering     = 'KM  ' 
   sub_header%units        = 'P_U                      '
   sub_header%ndim         = 2 
   sub_header%end_index(1) = mkz  
   sub_header%end_index(2) = mkz
   sub_header%end_index(3) = 1
  
   write(UNIT=ibe_unit) flag1
   call sub_write 
   call data_allocate
   data(:,:,1,1) = e3(:,:)
   write(UNIT=ibe_unit) data
   deallocate (data)
   n = n + 1
   WRITE(UNIT=stdout,FMT='(I2," Wrote field:",a)') n, sub_header%name

   ! 3.3.4 GLOBAL EIGEN VALUES  for P_U
  
   sub_header%name         = 'P_U_EVALG'
   sub_header%description  = 'P_U GLOBAL EIGEN VALUE '
   sub_header%ordering     = 'M   ' 
   sub_header%units        = 'P_U                      '
   sub_header%ndim         = 1 
   sub_header%end_index(1) = mkz  
   sub_header%end_index(2) = 1
   sub_header%end_index(3) = 1      
  
   write(UNIT=ibe_unit) flag1
   call sub_write 
   call data_allocate
   data(:,1,1,1) = l3(:)
   write(UNIT=ibe_unit) data
   deallocate (data)
   n = n + 1
   WRITE(UNIT=stdout,FMT='(I2," Wrote field:",a)') n, sub_header%name

   ! 3.4.1 GLOBAL EIGEN VECTORS for Q or RH
  
   if (cv_options_hum == 1 .or. cv_options_hum == 3 ) then
      sub_header%name         = 'Q_EVECG'
      sub_header%description  = 'Q GLOBAL EIGEN VECTOR'
      sub_header%units        = 'Q                        '
   else if ( cv_options_hum == 2 ) then
      sub_header%name         = 'RH_EVALG'
      sub_header%description  = 'RH GLOBAL EIGEN VECTOR'
      sub_header%units        = 'RH                       '
   endif
   sub_header%ordering     = 'KM  ' 
   sub_header%ndim         = 2 
   sub_header%end_index(1) = mkz  
   sub_header%end_index(2) = mkz
   sub_header%end_index(3) = 1
  
   write(UNIT=ibe_unit) flag1
   call sub_write 
   call data_allocate
   data(:,:,1,1) = e4(:,:)
   write(UNIT=ibe_unit) data
   deallocate (data)
   n = n + 1
   WRITE(UNIT=stdout,FMT='(I2," Wrote field:",a)') n, sub_header%name

   ! 3.4.4 GLOBAL EIGEN VALUES  for Q
  
   if (cv_options_hum == 1 .or. cv_options_hum == 3 ) then
      sub_header%name         = 'Q_EVALG'
      sub_header%description  = 'Q GLOBAL EIGEN VALUE '
      sub_header%units        = 'Q                        '
   else if ( cv_options_hum == 2 ) then
      sub_header%name         = 'RH_EVALG'
      sub_header%description  = 'RH GLOBAL EIGEN VALUE '
      sub_header%units        = 'RH                       '
   endif
   sub_header%ordering     = 'M   ' 
   sub_header%ndim         = 1 
   sub_header%end_index(1) = mkz  
   sub_header%end_index(2) = 1
   sub_header%end_index(3) = 1      
  
   write(UNIT=ibe_unit) flag1
   call sub_write 
   call data_allocate
   data(:,1,1,1) = l4(:)
   write(UNIT=ibe_unit) data
   deallocate (data)
   n = n + 1
   WRITE(UNIT=stdout,FMT='(I2," Wrote field:",a)') n, sub_header%name

   ! -------------------------------------------------------------------------
   ! 4. Write out the vertical regression coefficients
   ! -------------------------------------------------------------------------
 
   sub_header%name         = 'PB_VERREG' 
   sub_header%description  = 'BALANCE PRESSURE VERT.  REGRESSION'
   sub_header%staggering   = 'D   '
   sub_header%ordering     = 'YKM ' 
   sub_header%units        = '                         '
   sub_header%ndim         = 3 
   sub_header%end_index(1) = mix  
   sub_header%end_index(2) = mkz
   sub_header%end_index(3) = mkz
  
   write(UNIT=ibe_unit) flag1
   call sub_write 
   call data_allocate

   data(1:mix-1,:,:,1) = pb_vert_reg(1:mix-1,:,:)
   data(mix,:,:,1) = pb_vert_reg(mix-1,:,:)

   write(UNIT=ibe_unit) data
   deallocate (data)
   n = n + 1
   WRITE(UNIT=stdout,FMT='(I2," Wrote field:",a)') n, sub_header%name

   ! -------------------------------------------------------------------------
   ! 5. Write out Scale length                
   ! -------------------------------------------------------------------------

   ! 5.1 initialize the common part of the sub header
   ! ------------------------------------------------
 
   sub_header%staggering   = 'D   '
   sub_header%ordering     = 'MODE' 
   sub_header%units        = 'Number of Grid Points    '
   sub_header%ndim         = 1 
   sub_header%end_index(1) = mkz   
   sub_header%end_index(2) = 1      
   sub_header%end_index(3) = 1      

   call data_allocate

   ! 5.2.1 PSI scale length
   ! ----------------------
  
   sub_header%name         = 'PSI_S_LEN' 
   sub_header%description  = 'PSI SCALE LENGTH '
  
   write(UNIT=ibe_unit) flag1
   call sub_write 
   data(:,1,1,1) = s1(:) / ds
   write(UNIT=ibe_unit) data
   n = n + 1
   WRITE(UNIT=stdout,FMT='(I2," Wrote field:",a)') n, sub_header%name

   ! 5.2.2 CHI scale length
   ! ----------------------
 
   sub_header%name         = 'CHI_S_LEN' 
   sub_header%description  = 'CHI SCALE LENGTH '
  
   write(UNIT=ibe_unit) flag1
   call sub_write 
   data(:,1,1,1) = s2(:) / ds
   write(UNIT=ibe_unit) data
   n = n + 1
   WRITE(UNIT=stdout,FMT='(I2," Wrote field:",a)') n, sub_header%name

   ! 5.2.3 P_U scale length
   ! ----------------------
 
   sub_header%name         = 'P_U_S_LEN' 
   sub_header%description  = 'P_U SCALE LENGTH '
  
   write(UNIT=ibe_unit) flag1
   call sub_write 
   data(:,1,1,1) = s3(:) / ds
   write(UNIT=ibe_unit) data
   n = n + 1
   WRITE(UNIT=stdout,FMT='(I2," Wrote field:",a)') n, sub_header%name

   ! 5.2.4 Q scale length
   ! --------------------
 
   if (cv_options_hum == 1 .or. cv_options_hum == 3 ) then
      sub_header%name         = 'Q_S_LEN  ' 
      sub_header%description  = 'Q SCALE LENGTH '
   else if (cv_options_hum == 2 ) then
      sub_header%name         = 'RH_S_LEN  ' 
      sub_header%description  = 'RH SCALE LENGTH '
   endif
  
   write(UNIT=ibe_unit) flag1
   call sub_write 
   data(:,1,1,1) = s4(:) / ds
   write(UNIT=ibe_unit) data
   n = n + 1
   WRITE(UNIT=stdout,FMT='(I2," Wrote field:",a)') n, sub_header%name
   deallocate (data)

   ! --------------------------------------------------------------------------
   ! 6. Write out the last record  
   ! --------------------------------------------------------------------------

   write(UNIT=ibe_unit) flag2

   WRITE(UNIT=stdout,FMT='(/A)') "Interpolated Background Error Statistics file written out"
   WRITE(UNIT=stdout,FMT='(I2," constant fields, and ",I3," BE fields."/)') nv, n

   IF (trace_use) CALL da_trace_exit("da_write_interpolated_be")

contains

   subroutine sub_write 

      write(UNIT=ibe_unit) &
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

   end subroutine sub_write

   subroutine data_allocate

      allocate(data(sub_header%start_index(1):sub_header%end_index(1), &
                    sub_header%start_index(2):sub_header%end_index(2), &
                    sub_header%start_index(3):sub_header%end_index(3), &
                    sub_header%start_index(4):sub_header%end_index(4)))

   end subroutine data_allocate

END subroutine da_write_interpolated_be

