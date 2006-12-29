subroutine da_status_rad(iv,str,line)

   implicit none

   type(ob_type),    intent(in) :: iv
   character(len=*), intent(in) :: str
   integer,          intent(in) :: line

   integer :: i
   integer :: isum(10)
   real    :: rsum(40)

   if (trace_use) call da_trace_entry("da_status_rad")

   isum(:) = 0
   rsum(:) = 0.0

   isum(3) = -1
   rsum(3) = -1.0
   rsum(6) = -1.0
   rsum(7) = -1.0

   write (unit=stderr,fmt='(A,I6)') str,line
   do i=1,rtminit_nsensor
      write (unit=stderr,fmt='(A,I2)') "For instrument ",i 
      isum(1) = sum(iv%instid(i)%loc_i(:))
      isum(2) = sum(iv%instid(i)%loc_j(:))
      ! isum(3) = sum(iv%instid(i)%loc_k(:))
      isum(4) = sum(iv%instid(i)%scanpos(:))
      isum(5) = sum(iv%instid(i)%scanline(:))
      isum(6) = sum(iv%instid(i)%ifgat(:))
      isum(7) = sum(iv%instid(i)%landsea_mask(:))
      isum(8) = sum(iv%instid(i)%cloud_flag(:,:))
      isum(9) = sum(iv%instid(i)%isflg(:))

      rsum(1)  = sum(iv%instid(i)%loc_dx(:))
      rsum(2)  = sum(iv%instid(i)%loc_dy(:))
      ! rsum(3)  = sum(iv%instid(i)%loc_dz(:))
      rsum(4)  = sum(iv%instid(i)%loc_dxm(:))
      rsum(5)  = sum(iv%instid(i)%loc_dym(:))
      ! rsum(6)  = sum(iv%instid(i)%loc_dzm(:))
      ! rsum(7)  = sum(iv%instid(i)%zk(:,:))
      rsum(11) = sum(iv%instid(i)%tb_xb(:,:))
      rsum(12) = sum(iv%instid(i)%satzen(:))
      rsum(13) = sum(iv%instid(i)%satazi(:))
      rsum(14) = sum(iv%instid(i)%solzen(:))
      rsum(15) = sum(iv%instid(i)%solazi(:))
      rsum(16) = sum(iv%instid(i)%emiss(:,:))
      rsum(17) = sum(iv%instid(i)%pm(:,:))
      rsum(18) = sum(iv%instid(i)%tm(:,:))
      rsum(19) = sum(iv%instid(i)%qm(:,:))
      rsum(20) = sum(iv%instid(i)%qrn(:,:))
      rsum(21) = sum(iv%instid(i)%qcw(:,:))
      rsum(22) = sum(iv%instid(i)%qci(:,:))
      rsum(23) = sum(iv%instid(i)%qsn(:,:))
      rsum(24) = sum(iv%instid(i)%qgr(:,:))
      rsum(25) = sum(iv%instid(i)%ps(:))
      rsum(26) = sum(iv%instid(i)%ts(:))
      rsum(27) = sum(iv%instid(i)%t2m(:))
      rsum(28) = sum(iv%instid(i)%mr2m(:))
      rsum(29) = sum(iv%instid(i)%u10(:))
      rsum(30) = sum(iv%instid(i)%v10(:))
      rsum(31) = sum(iv%instid(i)%clwp(:))
      rsum(32) = sum(iv%instid(i)%smois(:))
      rsum(33) = sum(iv%instid(i)%tslb(:))
      rsum(34) = sum(iv%instid(i)%snowh(:))
      rsum(35) = sum(iv%instid(i)%elevation(:))
      rsum(36) = sum(iv%instid(i)%soiltyp(:))
      rsum(37) = sum(iv%instid(i)%vegtyp(:))
      rsum(38) = sum(iv%instid(i)%vegfra(:))

      isum(10) = sum(iv%instid(i)%tb_qc(:,:))
      rsum(39) = sum(iv%instid(i)%tb_inv(:,:))
      rsum(40) = sum(iv%instid(i)%tb_error(:,:))

      write(unit=stderr,fmt='(A,10I7)')         "1:10 ",isum(1:10)
      write(unit=stderr,fmt='(A,10(1X,E12.6))') "1:5  ",rsum(1:5)
      write(unit=stderr,fmt='(A,10(1X,E12.6))') "6:10 ",rsum(6:10)
      write(unit=stderr,fmt='(A,10(1X,E12.6))') "11:15",rsum(11:15)
      write(unit=stderr,fmt='(A,10(1X,E12.6))') "16:20",rsum(16:20)
      write(unit=stderr,fmt='(A,10(1X,E12.6))') "21:25",rsum(21:25)
      write(unit=stderr,fmt='(A,10(1X,E12.6))') "26:30",rsum(26:30)
      write(unit=stderr,fmt='(A,10(1X,E12.6))') "31:35",rsum(31:35)
      write(unit=stderr,fmt='(A,10(1X,E12.6))') "36:40",rsum(36:40)
   end do

   if (trace_use) call da_trace_exit("da_status_rad")

end subroutine da_status_rad


