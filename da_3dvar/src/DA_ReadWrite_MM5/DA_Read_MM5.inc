subroutine DA_Read_MM5(input_unit, new, error)

   implicit none

   integer,               intent(in)    :: input_unit
   type (mm5_model_type), intent(inout) :: new
   integer,               intent(out)   :: error

   integer(kind=4) :: flag

   integer :: l, ier, n, i, j, k, nv

   logical :: print_info

   real :: sample

   real(kind=4), dimension(:,:,:,:), allocatable :: data

!---------------------------------------------------------------------------
!  Read in fields:
!---------------------------------------------------------------------------

   print_info  = .false.
!  print_info  = .true.

   new%num_of_var = 0

   do

      read(input_unit, iostat=ier) flag

      if(ier/=0) then
         write(0,'("Error reading header flag")')
         call abort()
      endif

      if (flag == 0) then
         read(input_unit,iostat=ier) new%big_header%bhi,  new%big_header%bhr, &
                                     new%big_header%bhic, new%big_header%bhrc

         if(ier/=0) then
            write(0,'("Error reading big header")')
            call abort()
         endif

         if(print_info) &
         call DA_print_big_header(new%big_header%bhi, new%big_header%bhr, &
                                  new%big_header%bhic,new%big_header%bhrc)
      elseif (flag == 1) then
         nv=new%num_of_var+1
     
         read(input_unit,iostat=ier) &
              new%sub_header(nv)%ndim, &
              new%sub_header(nv)%start_index, &
              new%sub_header(nv)%end_index, &
              new%sub_header(nv)%current_time, &
              new%sub_header(nv)%staggering, &
              new%sub_header(nv)%ordering, &
              new%sub_header(nv)%current_date, &
              new%sub_header(nv)%name, &
              new%sub_header(nv)%units, &
              new%sub_header(nv)%description

         if(ier/=0) then
            write(0,'("Error reading subheader")')
            call abort()
         endif

         if(print_info) then
            call DA_print_sub_header(new%sub_header(nv))
         endif

         allocate(data(new%sub_header(nv)%start_index(1):new%sub_header(nv)%end_index(1), &
                       new%sub_header(nv)%start_index(2):new%sub_header(nv)%end_index(2), &
                       new%sub_header(nv)%start_index(3):new%sub_header(nv)%end_index(3), &
                       new%sub_header(nv)%start_index(4):new%sub_header(nv)%end_index(4)))

         read(input_unit,iostat=ier) data
         
         if(ier/=0) then
            write(0,'("Error reading data")')
            call abort()
         endif

         call DA_cleanrows(data, new%sub_header(nv))
                        
         if (new%sub_header(nv)%name ==  'U        ') then
            new%u = data(:,:,:,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'V        ') then
            new%v = data(:,:,:,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'T        ') then
            new%t = data(:,:,:,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'Q        ') then
            new%q = data(:,:,:,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'W        ') then
            new%w = data(:,:,:,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'PP       ') then
            new%pp_c = data(:,:,:,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'CLW      ') then
            new%qcw = data(:,:,:,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'RNW      ') then
            new%qrn = data(:,:,:,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'ICE      ') then
            new%qci = data(:,:,:,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'SNOW     ') then
            new%qsn = data(:,:,:,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'GRAUPEL  ') then
            new%qgr = data(:,:,:,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'PSTARCRS ') then
            new%psac = data(:,:,1,1)
            call DA_crs_to_dot(new%psac, new%psad)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'GROUND T ') then
            new%tgrn = data(:,:,1,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'TERRAIN  ') then
            new%terr = data(:,:,1,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'MAPFACCR ') then
            new%msfc = data(:,:,1,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'MAPFACDT ') then
            new%msfd = data(:,:,1,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'CORIOLIS ') then
            new%cori = data(:,:,1,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'LATITCRS ') then
            new%latc = data(:,:,1,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'LONGICRS ') then
            new%lonc = data(:,:,1,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'LATITDOT ') then
            new%latd = data(:,:,1,1)
         elseif (new%sub_header(nv)%name ==  'LONGIDOT ') then
            new%lond = data(:,:,1,1)
         elseif (new%sub_header(nv)%name ==  'LAND USE ') then
            new%lanu = data(:,:,1,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'LANDMASK ') then
            new%landmask = data(:,:,1,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'SNOWCOVR ') then
            new%snow = data(:,:,1,1)

            new%num_of_var = new%num_of_var + 1
!        elseif (new%sub_header(nv)%name ==  'RES TEMP ') then
!           new%restemp = data(:,:,1,1)

!           new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'SIGMAH   ') then
            new%sigmah(:) = data(:,1,1,1)

            new%num_of_var = new%num_of_var + 1
         elseif (new%sub_header(nv)%name ==  'CHI      ') then
            new%chi = data(:,:,:,1)
         elseif (new%sub_header(nv)%name ==  'PSI      ') then
            new%psi = data(:,:,:,1)
         elseif (new%sub_header(nv)%name ==  'P_U      ') then
            new%blnc_p = data(:,:,:,1)
         elseif (new%sub_header(nv)%name ==  'RH       ') then
            new%rh = data(:,:,:,1)

         elseif (new%sub_header(nv)%name ==  'T_EVALG  ') then
            new%t_eval_glo = data(:,1,1,1)
         elseif (new%sub_header(nv)%name ==  'Q_EVALG  ') then
            IF(cv_options_hum == 1 .or. cv_options_hum == 3) new%q_eval_glo = data(:,1,1,1)
         elseif (new%sub_header(nv)%name ==  'CHI_EVALG') then
            new%chi_eval_glo = data(:,1,1,1)
         elseif (new%sub_header(nv)%name ==  'PSI_EVALG') then
            new%psi_eval_glo = data(:,1,1,1)
         elseif (new%sub_header(nv)%name ==  'P_U_EVALG') then
            new%blnc_p_eval_glo = data(:,1,1,1)
         elseif (new%sub_header(nv)%name ==  'RH_EVALG ') then
            IF(cv_options_hum == 2) new%rh_eval_glo = data(:,1,1,1)

         elseif (new%sub_header(nv)%name ==  'T_EVECG  ') then
            new%t_evec_glo = data(:,:,1,1)
         elseif (new%sub_header(nv)%name ==  'Q_EVECG  ') then
            IF(cv_options_hum == 1 .or. cv_options_hum == 3) new%q_evec_glo = data(:,:,1,1)
         elseif (new%sub_header(nv)%name ==  'CHI_EVECG') then
            new%chi_evec_glo = data(:,:,1,1)
         elseif (new%sub_header(nv)%name ==  'PSI_EVECG') then
            new%psi_evec_glo = data(:,:,1,1)
         elseif (new%sub_header(nv)%name ==  'P_U_EVECG') then
            new%blnc_p_evec_glo = data(:,:,1,1)
         elseif (new%sub_header(nv)%name ==  'RH_EVECG ') then
            IF(cv_options_hum == 2) new%rh_evec_glo = data(:,:,1,1)

         elseif (new%sub_header(nv)%name ==  'T_EVALL  ') then
            new%t_eval_loc = data(:,:,1,1)
         elseif (new%sub_header(nv)%name ==  'Q_EVALL  ') then
            IF(cv_options_hum == 1 .or. cv_options_hum == 3) new%q_eval_loc = data(:,:,1,1)
         elseif (new%sub_header(nv)%name ==  'CHI_EVALL') then
            new%chi_eval_loc = data(:,:,1,1)
         elseif (new%sub_header(nv)%name ==  'PSI_EVALL') then
            new%psi_eval_loc = data(:,:,1,1)
         elseif (new%sub_header(nv)%name ==  'P_U_EVALL') then
            new%blnc_p_eval_loc = data(:,:,1,1)
         elseif (new%sub_header(nv)%name ==  'RH_EVALL ') then
            IF(cv_options_hum == 2) new%rh_eval_loc = data(:,:,1,1)

         elseif (new%sub_header(nv)%name ==  'T_EVECL  ') then
            new%t_evec_loc(:,:,:) = data(:,:,:,1)
         elseif (new%sub_header(nv)%name ==  'Q_EVECL  ') then
            IF(cv_options_hum == 1 .or. cv_options_hum == 3) new%q_evec_loc(:,:,:) = data(:,:,:,1)
         elseif (new%sub_header(nv)%name ==  'CHI_EVECL') then
            new%chi_evec_loc(:,:,:) = data(:,:,:,1)
         elseif (new%sub_header(nv)%name ==  'PSI_EVECL') then
            new%psi_evec_loc(:,:,:) = data(:,:,:,1)
         elseif (new%sub_header(nv)%name ==  'P_U_EVECL') then
            new%blnc_p_evec_loc(:,:,:) = data(:,:,:,1)
         elseif (new%sub_header(nv)%name ==  'RH_EVECL ') then
            IF(cv_options_hum == 2) new%rh_evec_loc(:,:,:) = data(:,:,:,1)

         elseif (new%sub_header(nv)%name ==  'T_S_LEN  ') then
            new%t_sl = data(:,1,1,1)
         elseif (new%sub_header(nv)%name ==  'Q_S_LEN  ') then
            IF(cv_options_hum == 1 .or. cv_options_hum == 3) new%q_sl = data(:,1,1,1)
         elseif (new%sub_header(nv)%name ==  'CHI_S_LEN') then
            new%chi_sl = data(:,1,1,1)
         elseif (new%sub_header(nv)%name ==  'PSI_S_LEN') then
            new%psi_sl = data(:,1,1,1)
         elseif (new%sub_header(nv)%name ==  'P_U_S_LEN') then
            new%blnc_p_sl = data(:,1,1,1)
         elseif (new%sub_header(nv)%name ==  'RH_S_LEN ') then
            IF(cv_options_hum == 2) new%rh_sl = data(:,1,1,1)

         elseif (new%sub_header(nv)%name ==  'PB_VERREG') then
            new%pb_vert_reg(:,:,:) = data(:,:,:,1)

         else
            if(new%sub_header(nv)%ordering(1:2) /= 'CA') then
!               write(unit=*, fmt='(a)') &
!                    'Warning!!! A variable is not saved.'

!               call DA_print_sub_header(new%sub_header(nv))
            endif

            if(print_info) then
               write(6, fmt='(a,4(i2,a),e24.14)') &
                    'Sample value(', &
                     new%sub_header(nv)%start_index(1),',', &
                     new%sub_header(nv)%start_index(2),',', &
                     new%sub_header(nv)%start_index(3),',', &
                     new%sub_header(nv)%start_index(4),'):', &
                data(new%sub_header(nv)%start_index(1), &
                     new%sub_header(nv)%start_index(2), &
                     new%sub_header(nv)%start_index(3), &
                     new%sub_header(nv)%start_index(4))
            endif

            deallocate(data)

            cycle
         endif

         deallocate(data)

      elseif (flag == 2) then
         error = 0

         return
      else
         stop
      endif

   enddo

   error = 1

   write(0,'(/,"Hit the end of file of unit ", I3)') &
         input_unit

END SUBROUTINE DA_Read_MM5
