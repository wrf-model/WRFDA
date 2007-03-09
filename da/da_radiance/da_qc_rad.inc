subroutine da_qc_rad (ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: perform quality control for radiance data.
   !
   ! METHOD:  seperated QC for each sensor
   !---------------------------------------------------------------------------

   implicit none

   type (y_type),  intent(in)      :: ob         ! Observation structure.
   type (ob_type), intent(inout)   :: iv         ! O-B structure.

   integer :: i, nchan,p,j
   logical   :: amsua, amsub, hirs, msu,airs,hsb

   integer, allocatable :: index(:)
   integer :: num_tovs_avg
   integer, allocatable :: excess_count(:)
   integer, allocatable :: excess_start(:)
   integer, allocatable :: spare_count(:)
   integer, allocatable :: spare_start(:)
   integer :: transfer
   logical :: copy_found
   integer :: temp(num_procs)

#ifdef RTTOV
   if (trace_use) call da_trace_entry("da_qc_rad")

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

   do i = 1, iv%num_inst
      nchan    = iv%instid(i)%nchan

      amsua = trim(inst_name(rtminit_sensor(i))) == 'amsua'
      amsub = trim(inst_name(rtminit_sensor(i))) == 'amsub'
      hirs  = trim(inst_name(rtminit_sensor(i))) == 'hirs'
      msu   = trim(inst_name(rtminit_sensor(i))) == 'msu'
      airs  = trim(inst_name(rtminit_sensor(i))) == 'airs'
      hsb   = trim(inst_name(rtminit_sensor(i))) == 'hsb'

      if (hirs) then
         ! 1.0 QC for HIRS
         call da_qc_hirs(i,nchan,ob,iv)
         !call da_warning(__FILE__,__LINE__,(/'QC Not implemented for HIRS'/))
      else if (airs) then
         call da_qc_airs(i,nchan,ob,iv)
         !call da_warning(__FILE__,__LINE__,(/'QC Not implemented for AIRS'/))
      else if ( hsb ) then
         ! call da_qc_hsb(i,nchan,ob,iv)
         call da_warning(__FILE__,__LINE__,(/'QC Not implemented for HSB'/))
      else if (amsua) then
         call da_qc_amsua(i,nchan,ob,iv)
      else if ( amsub ) then
         call da_qc_amsub(i,nchan,ob,iv)
      else if (msu) then
         ! call da_qc_msu(i,nchan, ob,iv)
         call da_warning(__FILE__,__LINE__,(/'QC Not implemented for MSU'/))
      else
         write(unit=message(1),fmt='(A,A)') &
            "Unrecognized instrument",trim(inst_name(rtminit_sensor(i)))
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

      ! Report number of observations to other processors via rootproc

      num_tovs_before(i,:) = 0
      num_tovs_before(i,myproc+1)=iv%instid(i)%num_rad
      temp(:)= num_tovs_before(i,:)
      call da_proc_sum_ints(temp(:))

#ifdef DM_PARALLEL
      call wrf_dm_bcast_integer(temp(:),num_procs)
#endif
      num_tovs_before(i,:) = temp(:)

      num_tovs_after(i,:) = num_tovs_before(i,:)

      if (rootproc .and. print_detail_rad) then
         write(unit=message(1),fmt='(A,I1,A)') "Instrument ",i, &
            " initial tovs distribution"
         write(unit=message(2),fmt=*) num_tovs_before(i,:)
         call da_message(message(1:2))
      end if

      ! Decide how to reallocate observations

      num_tovs_avg=sum(num_tovs_before(i,:))/num_procs

      call da_trace_int_sort(num_tovs_before(i,:),num_procs,index)

      do p=1,num_procs
         excess_count(p)=num_tovs_before(i,index(p))-num_tovs_avg
         spare_count(p)=num_tovs_avg-num_tovs_before(i,index(p))
         excess_start(p)=num_tovs_avg+1
         spare_start(p)=num_tovs_before(i,index(p))+1
      end do

      tovs_copy_count(i) = 0
      tovs_send_start(i,:) = 0
      tovs_send_count(i,:) = 0

      do
         copy_found = .false.
         do p=1,num_procs
            if (spare_count(p) > tovs_min_transfer) then
               do j=num_procs,1,-1
                  if (excess_count(j) > tovs_min_transfer ) then
                     copy_found = .true.
                     tovs_copy_count(i)=tovs_copy_count(i)+1
                     tovs_send_pe(i,tovs_copy_count(i)) = index(j)-1
                     tovs_recv_pe(i,tovs_copy_count(i)) = index(p)-1
                     tovs_send_start(i,tovs_copy_count(i)) = excess_start(j)
                     tovs_recv_start(i,tovs_copy_count(i)) = spare_start(p)
                     transfer=min(spare_count(p),excess_count(j))
                     tovs_send_count(i,tovs_copy_count(i)) = transfer
                     num_tovs_after(i,index(p))=num_tovs_after(i,index(p))+transfer
                     num_tovs_after(i,index(j))=num_tovs_after(i,index(j))-transfer
                     spare_count(p)=spare_count(p)-transfer
                     spare_start(p)=spare_start(p)+transfer
                     excess_count(j)=excess_count(j)-transfer
                     excess_start(j)=excess_start(j)+transfer
                     exit
                  end if   
               end do
            end if
         end do
         if (.not. copy_found) exit
      end do   

      if (print_detail_rad) then
         write(unit=message(1),fmt='(A,I1,A)') "Instrument ",i," final tovs distribution"
         write(unit=message(2),fmt=*) num_tovs_after(i,:)
         call da_message(message(1:2))
      end if

      iv % instid(i) % num_rad_glo = sum(num_tovs_after(i,:))
   end do

   deallocate (index)
   deallocate (excess_start)
   deallocate (excess_count)
   deallocate (spare_start)
   deallocate (spare_count)

!   if (print_detail_rad) call da_status_rad(iv,__FILE__,__LINE__)

   if (trace_use) call da_trace_exit("da_qc_rad")
#endif

end subroutine da_qc_rad


