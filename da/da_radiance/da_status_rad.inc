subroutine da_status_rad(iv,str,line)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(ob_type),    intent(in) :: iv
   character(len=*), intent(in) :: str
   integer,          intent(in) :: line


   integer :: i,n,j
   integer :: isum(10)
   real    :: rsum(40)

   if (trace_use) call da_trace_entry("da_status_rad")

   write (unit=stderr,fmt='(A,I6)') str,line

   do i=1,rtminit_nsensor
      isum(:) = 0
      rsum(:) = 0.0

      isum(3) = -1
      rsum(3) = -1.0
      rsum(6) = -1.0
      rsum(7) = -1.0
      write (unit=stderr,fmt='(A,I2)') "For instrument ",i 
      do n=1,iv%instid(i)%num_rad
         isum(1) = isum(1) + iv%instid(i)%rad(n)%loc%i
         isum(2) = isum(2) + iv%instid(i)%rad(n)%loc%j
         ! isum(3) = isum(3) + iv%instid(i)%rad(n)%loc%k
         isum(4) = isum(4) + iv%instid(i)%rad(n)%scanpos
         isum(5) = isum(5) + iv%instid(i)%rad(n)%scanline
         isum(6) = isum(6) + iv%instid(i)%rad(n)%ifgat
         isum(7) = isum(7) + iv%instid(i)%rad(n)%landsea_mask
         isum(8) = isum(8) + sum(iv%instid(i)%rad(n)%cloud_flag(:))
         isum(9) = isum(9) + iv%instid(i)%rad(n)%isflg

         rsum(1)  = rsum(1)   + iv%instid(i)%rad(n)%loc%dx
         rsum(2)  = rsum(2)   + iv%instid(i)%rad(n)%loc%dy
         ! rsum(3)  = rsum(3)   + iv%instid(i)%rad(n)%loc%dz
         rsum(4)  = rsum(4)   + iv%instid(i)%rad(n)%loc%dxm
         rsum(5)  = rsum(5)   + iv%instid(i)%rad(n)%loc%dym
         ! rsum(6)  = rsum(6)   + iv%instid(i)%rad(n)%loc%dzm
         ! rsum(7)  = rsum(7)   + sum(iv%instid(i)%rad(n)%zk(:))
         rsum(11) = rsum(11) + sum(iv%instid(i)%rad(n)%tb_xb(:))
         rsum(12) = rsum(12) + iv%instid(i)%rad(n)%satzen 
         rsum(13) = rsum(13) + iv%instid(i)%rad(n)%satazi 
         rsum(14) = rsum(14) + iv%instid(i)%rad(n)%solzen 
         rsum(15) = rsum(15) + iv%instid(i)%rad(n)%solazi 
         rsum(16) = rsum(16) + sum(iv%instid(i)%rad(n)%emiss(:))
         rsum(17) = rsum(17) + sum(iv%instid(i)%rad(n)%pm(:))
         rsum(18) = rsum(18) + sum(iv%instid(i)%rad(n)%tm(:))
         rsum(19) = rsum(19) + sum(iv%instid(i)%rad(n)%qm(:))
         rsum(20) = rsum(20) + sum(iv%instid(i)%rad(n)%qrn(:))
         rsum(21) = rsum(21) + sum(iv%instid(i)%rad(n)%qcw(:))
         rsum(22) = rsum(22) + sum(iv%instid(i)%rad(n)%qci(:))
         rsum(23) = rsum(23) + sum(iv%instid(i)%rad(n)%qsn(:))
         rsum(24) = rsum(24) + sum(iv%instid(i)%rad(n)%qgr(:))
         rsum(25) = rsum(25) + iv%instid(i)%rad(n)%ps
         rsum(26) = rsum(26) + iv%instid(i)%rad(n)%ts
         rsum(27) = rsum(27) + iv%instid(i)%rad(n)%t2m
         rsum(28) = rsum(28) + iv%instid(i)%rad(n)%mr2m
         rsum(29) = rsum(29) + iv%instid(i)%rad(n)%u10
         rsum(30) = rsum(30) + iv%instid(i)%rad(n)%v10
         rsum(31) = rsum(31) + iv%instid(i)%rad(n)%clwp
         rsum(32) = rsum(32) + iv%instid(i)%rad(n)%smois
         rsum(33) = rsum(33) + iv%instid(i)%rad(n)%tslb
         rsum(34) = rsum(34) + iv%instid(i)%rad(n)%snowh
         rsum(35) = rsum(35) + iv%instid(i)%rad(n)%elevation
         rsum(36) = rsum(36) + iv%instid(i)%rad(n)%soiltyp
         rsum(37) = rsum(37) + iv%instid(i)%rad(n)%vegtyp
         rsum(38) = rsum(38) + iv%instid(i)%rad(n)%vegfra


         do j = 1,iv%instid(i)%nchan
            isum(10) = isum(10) + iv%instid(i)%rad(n)%tb(j)%qc
            rsum(39) = rsum(39) + iv%instid(i)%rad(n)%tb(j)%inv
            rsum(40) = rsum(40) + iv%instid(i)%rad(n)%tb(j)%error
         end do
      end do
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


