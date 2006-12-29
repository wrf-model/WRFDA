subroutine da_oi( str,line,y1,y2,ob1,ob2)

   !-----------------------------------------------------------------------
   ! Purpose: quick way of showing location
   !-----------------------------------------------------------------------

   use da_define_structures

   implicit none

   character*(*) str
   integer, intent(in) :: line
   type(y_type), optional :: y1
   type(y_type), optional :: y2
   type(ob_type), optional, intent(in) :: ob1
   type(ob_type), optional, intent(in) :: ob2

   integer :: i,k,n
   real :: sum_y1
   real :: sum_y2
   real :: sum_ob1
   real :: sum_ob2

   sum_y1=-1.0
   sum_y2=-1.0
   sum_ob1=-1.0
   sum_ob2=-1.0

   if (.not. oi_use) return

   if (.not. use_radiance) goto 100

      if (present(y1)) then
         sum_y1=0.0
         do i=1,y1%num_inst
            if (associated(y1%instid(i)%tb)) &
               sum_y1=sum_y1+sum(y1%instid(i)%tb(:,:))
         end do
      else
         sum_y1=-1.0
      end if

      if (present(y2)) then
         sum_y2=0.0
         do i=1,y2%num_inst
            if (associated(y2%instid(i)%tb)) &
               sum_y2=sum_y2+sum(y2%instid(i)%tb(:,:))
         end do
      else
         sum_y2=-1.0
      end if

      if (present(ob1)) then
         sum_ob1=0.0
         do i=1,ob1%num_inst
            if (associated(ob1%instid(i)%tb_xb)) then
!write (0,*) "sum(ob1%instid(i)%tb_xb(:,:))",sum(ob1%instid(i)%tb_xb(:,:))
               sum_ob1=sum_ob1+sum(ob1%instid(i)%tb_xb(:,:)) 
endif

            if (associated(ob1%instid(i)%tb_inv)) then
!write (0,*) "sum(ob1%instid(i)%tb_inv(:,:))",sum(ob1%instid(i)%tb_inv(:,:))
               sum_ob1=sum_ob1+sum(ob1%instid(i)%tb_inv(:,:)) 
end if

            if (associated(ob1%instid(i)%tb_qc)) then
               sum_ob1=sum_ob1+sum(ob1%instid(i)%tb_qc(:,:)) 
!write (0,*) "sum(ob1%instid(i)%tb_qc(:,:))",sum(ob1%instid(i)%tb_qc(:,:))
end if

            if (associated(ob1%instid(i)%tb_error)) then
!write (0,*) "sum(ob1%instid(i)%tb_error(:,:))",sum(ob1%instid(i)%tb_error(:,:))
               sum_ob1=sum_ob1+sum(ob1%instid(i)%tb_error(:,:)) 
end if

            if (associated(ob1%instid(i)%t)) then
!write (0,*) "sum(ob1%instid(i)%t(:,:))",sum(ob1%instid(i)%t(:,:))
               sum_ob1=sum_ob1+sum(ob1%instid(i)%t(:,:)) 
end if
            if (associated(ob1%instid(i)%mr)) then
!write (0,*) "sum(ob1%instid(i)%mr(:,:))",sum(ob1%instid(i)%mr(:,:))
               sum_ob1=sum_ob1+sum(ob1%instid(i)%mr(:,:)) 
end if
            if (associated(ob1%instid(i)%t2m)) then
!write (0,*) "sum(ob1%instid(i)%t2m(:))",sum(ob1%instid(i)%t2m(:))
               sum_ob1=sum_ob1+sum(ob1%instid(i)%t2m(:)) 
end if
            if (associated(ob1%instid(i)%u10)) then
               sum_ob1=sum_ob1+sum(ob1%instid(i)%u10(:)) 
!write (0,*) "ob1%instid(i)%u10(:)",ob1%instid(i)%u10(:)
end if
            if (associated(ob1%instid(i)%v10)) then
               sum_ob1=sum_ob1+sum(ob1%instid(i)%v10(:)) 
!write (0,*) "ob1%instid(i)%v10(:)",ob1%instid(i)%v10(:)
end if
            if (associated(ob1%instid(i)%mr2m)) then
               sum_ob1=sum_ob1+sum(ob1%instid(i)%mr2m(:)) 
!write (0,*) "ob1%instid(i)%mr2m(:)",ob1%instid(i)%mr2m(:)
end if
            if (associated(ob1%instid(i)%ps)) then
               sum_ob1=sum_ob1+sum(ob1%instid(i)%ps(:)) 
!write (0,*) "ob1%instid(i)%ps(:)",ob1%instid(i)%ps(:)
end if

            if (associated(ob1%instid(i)%emiss)) then
               sum_ob1=sum_ob1+sum(ob1%instid(i)%emiss(:,:)) 
!write (0,*) "ob1%instid(i)%emiss(:)",ob1%instid(i)%emiss(:,:)
end if

            if (associated(ob1%instid(i)%ts)) then
               sum_ob1=sum_ob1+sum(ob1%instid(i)%ts(:)) 
!write (0,*) "ob1%instid(i)%ts(:)",ob1%instid(i)%ts(:)
end if

            if (associated(ob1%instid(i)%elevation)) then
               sum_ob1=sum_ob1+sum(ob1%instid(i)%elevation(:)) 
!write (0,*) "ob1%instid(i)%elevation(:)",ob1%instid(i)%elevation(:)
end if

            if (associated(ob1%instid(i)%clwp)) then
               sum_ob1=sum_ob1+sum(ob1%instid(i)%clwp(:)) 
!write (0,*) "ob1%instid(i)%clwp(:)",ob1%instid(i)%clwp(:)
end if

            if (associated(ob1%instid(i)%pm)) &
               sum_ob1=sum_ob1+sum(ob1%instid(i)%pm(:,:)) 

            if (associated(ob1%instid(i)%tm)) &
               sum_ob1=sum_ob1+sum(ob1%instid(i)%tm(:,:)) 

            if (associated(ob1%instid(i)%qm)) &
               sum_ob1=sum_ob1+sum(ob1%instid(i)%qm(:,:)) 

            if (associated(ob1%instid(i)%cloud_flag)) &
               sum_ob1=sum_ob1+sum(ob1%instid(i)%cloud_flag(:,:)) 
         end do
      else
         sum_ob1=-1.0
      end if

100 continue

   write(unit=stderr,fmt='(A,3(E18.10,1X),A,I6)') "oi:", sum_y1, sum_y2, sum_ob1, str, line

end subroutine da_oi


