subroutine da_qc_rad (ob, iv)

   !---------------------------------------------------------------------------
   ! PURPOSE: perform quality control for radiance data.
   !
   ! METHOD:  seperated QC for each sensor
   !---------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (y_type),  INTENT(IN)      :: ob         ! Observation structure.
   TYPE (ob_type), INTENT(INOUT)   :: iv         ! O-B structure.

   integer :: inst, nchan,i,j
   logical   :: amsua, amsub, hirs, msu,airs,hsb

   integer, allocatable :: index(:)
   integer :: num_tovs_avg
   integer, allocatable :: excess_count(:)
   integer, allocatable :: excess_start(:)
   integer, allocatable :: spare_count(:)
   integer, allocatable :: spare_start(:)
   integer :: transfer
   logical :: copy_found

#ifdef RTTOV
   if (trace_use) call da_trace_entry("da_qc_rad")

!   if (tovs_redistribute) then
if (.false.) then
      allocate (num_tovs_before(iv%num_inst,num_procs))
      allocate (num_tovs_after(iv%num_inst,num_procs))

      ! Cannot be more total send,receives than combination of processors
      allocate (tovs_copy_count(iv%num_inst))
      allocate (tovs_send_pe(iv%num_inst,num_procs*num_procs))
      allocate (tovs_recv_pe(iv%num_inst,num_procs*num_procs))
      allocate (tovs_send_start(iv%num_inst,num_procs*num_procs))
      allocate (tovs_send_count(iv%num_inst,num_procs*num_procs))
      allocate (tovs_recv_start(iv%num_inst,num_procs*num_procs))

      call da_trace("da_qc_rad", message="allocated tovs redistibution arrays")

      allocate (index(num_procs))
      allocate (excess_count(num_procs))
      allocate (excess_start(num_procs))
      allocate (spare_count(num_procs))
      allocate (spare_start(num_procs))
   end if


   do inst = 1, iv%num_inst
      WRITE(UNIT=stdout,FMT='(A,A)') 'performing QC for ', trim(iv%instid(inst)%rttovid_string) 
      nchan    = iv%instid(inst)%nchan

      amsua = trim(inst_name(rtminit_sensor(inst))) == 'amsua'
      amsub = trim(inst_name(rtminit_sensor(inst))) == 'amsub'
      hirs  = trim(inst_name(rtminit_sensor(inst))) == 'hirs'
      msu   = trim(inst_name(rtminit_sensor(inst))) == 'msu'
      airs  = trim(inst_name(rtminit_sensor(inst))) == 'airs'
      hsb   = trim(inst_name(rtminit_sensor(inst))) == 'hsb'

      if (hirs) then
         ! 1.0 QC for HIRS
         ! call da_qc_hirs(inst,nchan,ob,iv)
         WRITE(UNIT=stderr,FMT='(A)') 'QC Not impemented for HIRS' 
      else if (airs) then
         ! call da_qc_airs(inst,nchan,ob,iv)
         WRITE(UNIT=stderr,FMT='(A)') 'QC Not impemented for AIRS'
      else if ( hsb ) then
         ! call da_qc_hsb(inst,nchan,ob,iv)
         WRITE(UNIT=stderr,FMT='(A)') 'QC Not impemented for HSB'
      else if (amsua) then
         !  2.0 QC for AMSU-A
         call da_qc_amsua(inst,nchan,ob,iv)
      else if ( amsub ) then
         ! 3.0 QC for AMSU-B
         call da_qc_amsub(inst,nchan,ob,iv)
      else if (msu) then
         ! call da_qc_msu(inst,nchan, ob,iv)
         WRITE(UNIT=stderr,FMT='(A)') 'QC Not impemented for MSU'
      else
         WRITE(UNIT=errmsg(1),FMT='(A,A)') &
            "Unrecognized Instrument",TRIM(inst_name(rtminit_sensor(inst)))
         call da_error(__FILE__,__LINE__,errmsg(1:1))
      end if

!      if (tovs_redistribute) then
if (.false.) then
         ! Report number of observations to other processors via rootproc

         write(unit=stdout,fmt='(A,I2)') "For instrument", inst

         num_tovs_before(inst,:) = 0
         num_tovs_before(inst,myproc+1)=iv%instid(inst)%num_rad
         call da_proc_sum_ints(num_tovs_before(inst,:))
#ifdef DM_PARALLEL
         call wrf_dm_bcast_integer(num_tovs_before(inst,:),num_procs)
#endif

         num_tovs_after(inst,:) = num_tovs_before(inst,:)

!         if (rootproc .AND. print_detail_radiance) then
            write(unit=stdout,fmt='(A)') "Initial tovs observation distribution, pe, num"
            do i=1,num_procs
               write(unit=stdout,fmt='(I4,I10)') i-1,num_tovs_before(inst,i)
            end do
!         end if

         ! Decide how to reallocate observations

         num_tovs_avg=sum(num_tovs_before(inst,:))/num_procs

         call da_trace_int_sort(num_tovs_before(inst,:),num_procs,index)

         do i=1,num_procs
            excess_count(i)=num_tovs_before(inst,index(i))-num_tovs_avg
            spare_count(i)=num_tovs_avg-num_tovs_before(inst,index(i))
            excess_start(i)=num_tovs_avg+1
            spare_start(i)=num_tovs_before(inst,index(i))+1
         end do

         ! if (rootproc .AND. print_detail_radiance) then
         !    write(unit=stdout,fmt='(A)') "After sort, pe, num,excess, spare, excess_start, spare_start"
         !    do i=1,num_procs
         !       write(unit=stdout,fmt='(2I4,5I10)') i,index(i)-1, &
         !          num_tovs_before(inst,index(i)), excess_count(i),spare_count(i), &
         !          excess_start(i),spare_start(i)
         !    end do
         ! end if

         tovs_copy_count(inst) = 0
         tovs_send_start(inst,:) = 0
         tovs_send_count(inst,:) = 0

         do
            copy_found = .false.
            do i=1,num_procs
               if (spare_count(i) > tovs_min_transfer) then
                  do j=num_procs,1,-1
                     if (excess_count(j) > tovs_min_transfer ) then
                        copy_found = .true.
                        tovs_copy_count(inst)=tovs_copy_count(inst)+1
                        tovs_send_pe(inst,tovs_copy_count(inst)) = index(j)-1
                        tovs_recv_pe(inst,tovs_copy_count(inst)) = index(i)-1
                        tovs_send_start(inst,tovs_copy_count(inst)) = excess_start(j)
                        tovs_recv_start(inst,tovs_copy_count(inst)) = spare_start(i)
                        transfer=MIN(spare_count(i),excess_count(j))
                        tovs_send_count(inst,tovs_copy_count(inst)) = transfer
                        num_tovs_after(inst,index(i))=num_tovs_after(inst,index(i))+transfer
                        num_tovs_after(inst,index(j))=num_tovs_after(inst,index(j))-transfer
                        spare_count(i)=spare_count(i)-transfer
                        spare_start(i)=spare_start(i)+transfer
                        excess_count(j)=excess_count(j)-transfer
                        excess_start(j)=excess_start(j)+transfer
                        exit
                     end if   
                  end do
               end if
            end do
            if (.not. copy_found) exit
         end do   

         if (print_detail_radiance) then
            do i=1,tovs_copy_count(inst)
               write (unit=stdout,fmt='(5(A,I5))') "Would send ",tovs_send_count(inst,i), &
                  " from ",tovs_send_pe(inst,i),":",tovs_send_start(inst,i)," to ", &
                  tovs_recv_pe(inst,i),":",tovs_recv_start(inst,i)
            end do
            ! Need to sort back to PE order
            write(unit=stdout,fmt='(A)') "Final tovs observation distribution, pe, num"
            do i=1,num_procs
               do j=1,num_procs
                  if (index(j)==i) then
                     write(unit=stdout,fmt='(I4,I10)') i-1,num_tovs_after(inst,i)
                  end if
               end do
            end do
         end if
      end if
   end do

!   if (tovs_redistribute) then
if (.false.) then
      deallocate (index)
      deallocate (excess_start)
      deallocate (excess_count)
      deallocate (spare_start)
      deallocate (spare_count)
   end if

   if (print_detail_radiance) call da_status_rad(iv,__FILE__,__LINE__)

   if (trace_use) call da_trace_exit("da_qc_rad")
#endif

end subroutine da_qc_rad


