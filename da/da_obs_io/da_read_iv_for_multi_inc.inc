subroutine da_read_iv_for_multi_inc(file_index, iv)

   !-----------------------------------------------------------------------
   ! Purpose: Read for Multi-incremental 
   !-----------------------------------------------------------------------

   !-------------------------------------------------------------------------
   ! read iv=O-B structure written by WRFVAR
   !-------------------------------------------------------------------------

   implicit none

   type (ob_type), intent(inout)    :: iv      ! O-B structure.
   integer,  intent(in)             :: file_index
   integer                      :: unit_in
   character(len=filename_len)   :: filename

   integer      :: num_obs, ios 
   integer      :: global_num_obs
   character*20 :: ob_type_string               
   
   type (ob_type)                   :: iv_tmp  ! O-B structure.
   character*5  :: stn_id               
   integer      :: n, gn, k, kk, l, levels
   logical      :: found_flag

   if (trace_use) call da_trace_entry("da_read_iv_for_multi_inc")

   !-------------------------------------------------------------------------
   ! Fix input unit
   !-------------------------------------------------------------------------

   call da_get_unit(unit_in)

   write(unit=filename, fmt='(a,i3.3)') 'gts_omb.', file_index

   ! [1] Transfer surface obs:

   if (iv%ob_numb(iv%current_ob_time)%synop-iv%ob_numb(iv%current_ob_time-1)%synop > 0) then

       open(unit=unit_in,file=trim(filename)//'.synop',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'synop' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find synop marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%synop + 1, &
              iv%ob_numb(iv%current_ob_time)%synop
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find synop obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%synop-iv%ob_numb(iv%current_ob_time-1)%synop) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [2] Transfer metar obs:

   if (iv%ob_numb(iv%current_ob_time)%metar-iv%ob_numb(iv%current_ob_time-1)%metar > 0) then

       open(unit=unit_in,file=trim(filename)//'.metar',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'metar' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find metar marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%metar + 1, &
              iv%ob_numb(iv%current_ob_time)%metar
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find metar obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%metar-iv%ob_numb(iv%current_ob_time-1)%metar) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [3] Transfer ships obs:

   if (iv%ob_numb(iv%current_ob_time)%ships-iv%ob_numb(iv%current_ob_time-1)%ships > 0) then

       open(unit=unit_in,file=trim(filename)//'.ships',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'ships' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ships marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%ships + 1, &
              iv%ob_numb(iv%current_ob_time)%ships
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ships obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%ships-iv%ob_numb(iv%current_ob_time-1)%ships) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [4] Transfer sonde_sfc obs:

   if (iv%ob_numb(iv%current_ob_time)%sound-iv%ob_numb(iv%current_ob_time-1)%sound > 0) then

       open(unit=unit_in,file=trim(filename)//'.sonde_sfc',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'sonde_sfc' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find sonde_sfc marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%sound + 1, &
              iv%ob_numb(iv%current_ob_time)%sound
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find sonde_sfc obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%sound-iv%ob_numb(iv%current_ob_time-1)%sound) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [5] Transfer sound obs:

   if (iv%ob_numb(iv%current_ob_time)%sound-iv%ob_numb(iv%current_ob_time-1)%sound > 0) then

       open(unit=unit_in,file=trim(filename)//'.sound',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'sound' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find sound marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%sound + 1, &
              iv%ob_numb(iv%current_ob_time)%sound
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find sound obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%sound-iv%ob_numb(iv%current_ob_time-1)%sound) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [6] Transfer buoy obs:

   if (iv%ob_numb(iv%current_ob_time)%buoy-iv%ob_numb(iv%current_ob_time-1)%buoy > 0) then

       open(unit=unit_in,file=trim(filename)//'.buoy',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'buoy' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find buoy marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%buoy + 1, &
              iv%ob_numb(iv%current_ob_time)%buoy
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find buoy obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%buoy-iv%ob_numb(iv%current_ob_time-1)%buoy) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [7] Transfer Geo AMV obs:

   if (iv%ob_numb(iv%current_ob_time)%geoamv-iv%ob_numb(iv%current_ob_time-1)%geoamv > 0) then

       open(unit=unit_in,file=trim(filename)//'.geoamv',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'geoamv' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find geoamv marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%geoamv + 1, &
              iv%ob_numb(iv%current_ob_time)%geoamv
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find geoamv obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%geoamv-iv%ob_numb(iv%current_ob_time-1)%geoamv) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [8] Transfer gpspw obs:

   if (iv%ob_numb(iv%current_ob_time)%gpspw-iv%ob_numb(iv%current_ob_time-1)%gpspw > 0) then

       open(unit=unit_in,file=trim(filename)//'.gpspw',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'gpspw' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find gpspw marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%gpspw + 1, &
              iv%ob_numb(iv%current_ob_time)%gpspw
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find gpspw obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%gpspw-iv%ob_numb(iv%current_ob_time-1)%gpspw) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [9] Transfer SSM/I obs:

   if (iv%ob_numb(iv%current_ob_time)%ssmi_retrieval-iv%ob_numb(iv%current_ob_time-1)%ssmi_retrieval > 0) then

       open(unit=unit_in,file=trim(filename)//'.ssmir',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'ssmir' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ssmir marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%ssmi_retrieval + 1, &
              iv%ob_numb(iv%current_ob_time)%ssmi_retrieval
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ssmir obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%ssmi_retrieval-iv%ob_numb(iv%current_ob_time-1)%ssmi_retrieval) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [10] Transfer airep obs:

   if (iv%ob_numb(iv%current_ob_time)%airep-iv%ob_numb(iv%current_ob_time-1)%airep > 0) then

       open(unit=unit_in,file=trim(filename)//'.airep',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'airep' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find airep marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%airep + 1, &
              iv%ob_numb(iv%current_ob_time)%airep
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find airep obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%airep-iv%ob_numb(iv%current_ob_time-1)%airep) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [11] Transfer polaramv obs:

   if (iv%ob_numb(iv%current_ob_time)%polaramv-iv%ob_numb(iv%current_ob_time-1)%polaramv > 0) then

       open(unit=unit_in,file=trim(filename)//'.polaramv',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'polaramv' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find polaramv marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%polaramv + 1, &
              iv%ob_numb(iv%current_ob_time)%polaramv
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find polaramv obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%polaramv-iv%ob_numb(iv%current_ob_time-1)%polaramv) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [12] Transfer pilot obs:

   if (iv%ob_numb(iv%current_ob_time)%pilot-iv%ob_numb(iv%current_ob_time-1)%pilot > 0) then

       open(unit=unit_in,file=trim(filename)//'.pilot',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'pilot' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find pilot marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%pilot + 1, &
              iv%ob_numb(iv%current_ob_time)%pilot
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find pilot obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%pilot-iv%ob_numb(iv%current_ob_time-1)%pilot) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [13] Transfer ssmi_tb obs:

   if (iv%ob_numb(iv%current_ob_time)%ssmi_tb-iv%ob_numb(iv%current_ob_time-1)%ssmi_tb > 0) then

       open(unit=unit_in,file=trim(filename)//'.ssmi_tb',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'ssmi_tb' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ssmi_tb marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%ssmi_tb + 1, &
              iv%ob_numb(iv%current_ob_time)%ssmi_tb
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ssmi_tb obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%ssmi_tb-iv%ob_numb(iv%current_ob_time-1)%ssmi_tb) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [14] Transfer satem obs:

   if (iv%ob_numb(iv%current_ob_time)%satem-iv%ob_numb(iv%current_ob_time-1)%satem > 0) then

       open(unit=unit_in,file=trim(filename)//'.satem',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'satem' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find satem marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%satem + 1, &
              iv%ob_numb(iv%current_ob_time)%satem
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find satem obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%satem-iv%ob_numb(iv%current_ob_time-1)%satem) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [15] Transfer ssmt1 obs:

   if (iv%ob_numb(iv%current_ob_time)%ssmt1-iv%ob_numb(iv%current_ob_time-1)%ssmt1 > 0) then

       open(unit=unit_in,file=trim(filename)//'.ssmt1',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'ssmt1' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ssmt1 marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%ssmt1 + 1, &
              iv%ob_numb(iv%current_ob_time)%ssmt1
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ssmt1 obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%ssmt1-iv%ob_numb(iv%current_ob_time-1)%ssmt1) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [16] Transfer ssmt2 obs:

   if (iv%ob_numb(iv%current_ob_time)%ssmt2-iv%ob_numb(iv%current_ob_time-1)%ssmt2 > 0) then

       open(unit=unit_in,file=trim(filename)//'.ssmt2',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'ssmt2' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ssmt2 marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%ssmt2 + 1, &
              iv%ob_numb(iv%current_ob_time)%ssmt2
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ssmt2 obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%ssmt2-iv%ob_numb(iv%current_ob_time-1)%ssmt2) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [17] Transfer scatterometer obs:

   if (iv%ob_numb(iv%current_ob_time)%qscat-iv%ob_numb(iv%current_ob_time-1)%qscat > 0) then

       open(unit=unit_in,file=trim(filename)//'.qscat',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'qscat' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find qscat marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%qscat + 1, &
              iv%ob_numb(iv%current_ob_time)%qscat
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find qscat obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%qscat-iv%ob_numb(iv%current_ob_time-1)%qscat) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [18] Transfer profiler obs:

   if (iv%ob_numb(iv%current_ob_time)%profiler-iv%ob_numb(iv%current_ob_time-1)%profiler > 0) then

       open(unit=unit_in,file=trim(filename)//'.profiler',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'profiler' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find profiler marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%profiler + 1, &
              iv%ob_numb(iv%current_ob_time)%profiler
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find profiler obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%profiler-iv%ob_numb(iv%current_ob_time-1)%profiler) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [19] Transfer TC bogus obs:

   if (iv%ob_numb(iv%current_ob_time)%bogus-iv%ob_numb(iv%current_ob_time-1)%bogus > 0) then

       open(unit=unit_in,file=trim(filename)//'.bogus',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'bogus' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find bogus marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%bogus + 1, &
              iv%ob_numb(iv%current_ob_time)%bogus
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find bogus obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%bogus-iv%ob_numb(iv%current_ob_time-1)%bogus) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [20] Transfer AIRS retrievals:

   if (iv%ob_numb(iv%current_ob_time)%airsr-iv%ob_numb(iv%current_ob_time-1)%airsr > 0) then

       open(unit=unit_in,file=trim(filename)//'.airsr',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'airsr' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find airsr marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%airsr + 1, &
              iv%ob_numb(iv%current_ob_time)%airsr
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find airsr obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%airsr-iv%ob_numb(iv%current_ob_time-1)%airsr) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [21] Transfer gpsref obs:

   if (iv%ob_numb(iv%current_ob_time)%gpsref-iv%ob_numb(iv%current_ob_time-1)%gpsref > 0) then

       open(unit=unit_in,file=trim(filename)//'.gpsref',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//trim(filename)/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'gpsref' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find gpsref marker. "/))
       gn = 0
       do n = iv%ob_numb(iv%current_ob_time-1)%gpsref + 1, &
              iv%ob_numb(iv%current_ob_time)%gpsref
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find gpsref obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%ob_numb(iv%current_ob_time)%gpsref-iv%ob_numb(iv%current_ob_time-1)%gpsref) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

999 continue
   close (unit_in)
   call da_free_unit(unit_in)

   if (trace_use) call da_trace_exit("da_read_iv_for_multi_inc")
   return

1000 continue
   write(unit=message(1), fmt='(a,i3)') &
      'read error on unit: ',unit_in
   call da_warning(__FILE__,__LINE__,message(1:1))

end subroutine da_read_iv_for_multi_inc
