subroutine da_oi( str,line,y1,y2,ob1,ob2)

   !-----------------------------------------------------------------------
   ! Purpose: quick way of showing location
   !-----------------------------------------------------------------------

   implicit none

   character(len=*), intent(in)           :: str
   integer,          intent(in)           :: line
   type(y_type),     intent(in), optional :: y1
   type(y_type),     intent(in), optional :: y2
   type(ob_type)   , intent(in), optional :: ob1
   type(ob_type),    intent(in), optional :: ob2

   integer :: i
   real :: sum_y1
   real :: sum_y2
   real :: sum_ob1
   real :: sum_ob2

   sum_y1=-1.0
   sum_y2=-1.0
   sum_ob1=-1.0
   sum_ob2=-1.0

   if (.not. oi_use) return

   if (.not. use_rad) goto 100

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
            sum_ob1=sum_ob1+sum(ob1%instid(i)%tb_xb(:,:)) 
         endif

         if (associated(ob1%instid(i)%tb_inv)) then
            sum_ob1=sum_ob1+sum(ob1%instid(i)%tb_inv(:,:)) 
         end if

         if (associated(ob1%instid(i)%tb_qc)) then
            sum_ob1=sum_ob1+sum(ob1%instid(i)%tb_qc(:,:)) 
         end if

         if (associated(ob1%instid(i)%tb_error)) then
            sum_ob1=sum_ob1+sum(ob1%instid(i)%tb_error(:,:)) 
         end if

         if (associated(ob1%instid(i)%t)) then
            sum_ob1=sum_ob1+sum(ob1%instid(i)%t(:,:)) 
         end if
         if (associated(ob1%instid(i)%mr)) then
            sum_ob1=sum_ob1+sum(ob1%instid(i)%mr(:,:)) 
         end if
         if (associated(ob1%instid(i)%t2m)) then
            sum_ob1=sum_ob1+sum(ob1%instid(i)%t2m(:)) 
         end if
         if (associated(ob1%instid(i)%u10)) then
            sum_ob1=sum_ob1+sum(ob1%instid(i)%u10(:)) 
         end if
         if (associated(ob1%instid(i)%v10)) then
            sum_ob1=sum_ob1+sum(ob1%instid(i)%v10(:)) 
         end if
         if (associated(ob1%instid(i)%mr2m)) then
            sum_ob1=sum_ob1+sum(ob1%instid(i)%mr2m(:)) 
         end if
         if (associated(ob1%instid(i)%ps)) then
            sum_ob1=sum_ob1+sum(ob1%instid(i)%ps(:)) 
         end if

         if (associated(ob1%instid(i)%emiss)) then
            sum_ob1=sum_ob1+sum(ob1%instid(i)%emiss(:,:)) 
         end if

         if (associated(ob1%instid(i)%ts)) then
            sum_ob1=sum_ob1+sum(ob1%instid(i)%ts(:)) 
         end if

         if (associated(ob1%instid(i)%elevation)) then
            sum_ob1=sum_ob1+sum(ob1%instid(i)%elevation(:)) 
         end if

         if (associated(ob1%instid(i)%clwp)) then
            sum_ob1=sum_ob1+sum(ob1%instid(i)%clwp(:)) 
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

   if (present(ob2)) then
      sum_ob2=0.0
      do i=1,ob2%num_inst
         if (associated(ob2%instid(i)%tb_xb)) then
            sum_ob2=sum_ob2+sum(ob2%instid(i)%tb_xb(:,:)) 
         endif

         if (associated(ob2%instid(i)%tb_inv)) then
            sum_ob2=sum_ob2+sum(ob2%instid(i)%tb_inv(:,:)) 
         end if

         if (associated(ob2%instid(i)%tb_qc)) then
            sum_ob2=sum_ob2+sum(ob2%instid(i)%tb_qc(:,:)) 
         end if

         if (associated(ob2%instid(i)%tb_error)) then
            sum_ob2=sum_ob2+sum(ob2%instid(i)%tb_error(:,:)) 
         end if

         if (associated(ob2%instid(i)%t)) then
            sum_ob2=sum_ob2+sum(ob2%instid(i)%t(:,:)) 
         end if
         if (associated(ob2%instid(i)%mr)) then
            sum_ob2=sum_ob2+sum(ob2%instid(i)%mr(:,:)) 
         end if
         if (associated(ob2%instid(i)%t2m)) then
            sum_ob2=sum_ob2+sum(ob2%instid(i)%t2m(:)) 
         end if
         if (associated(ob2%instid(i)%u10)) then
            sum_ob2=sum_ob2+sum(ob2%instid(i)%u10(:)) 
         end if
         if (associated(ob2%instid(i)%v10)) then
            sum_ob2=sum_ob2+sum(ob2%instid(i)%v10(:)) 
         end if
         if (associated(ob2%instid(i)%mr2m)) then
            sum_ob2=sum_ob2+sum(ob2%instid(i)%mr2m(:)) 
         end if
         if (associated(ob2%instid(i)%ps)) then
            sum_ob2=sum_ob2+sum(ob2%instid(i)%ps(:)) 
         end if

         if (associated(ob2%instid(i)%emiss)) then
            sum_ob2=sum_ob2+sum(ob2%instid(i)%emiss(:,:)) 
         end if

         if (associated(ob2%instid(i)%ts)) then
            sum_ob2=sum_ob2+sum(ob2%instid(i)%ts(:)) 
         end if

         if (associated(ob2%instid(i)%elevation)) then
            sum_ob2=sum_ob2+sum(ob2%instid(i)%elevation(:)) 
         end if

         if (associated(ob2%instid(i)%clwp)) then
            sum_ob2=sum_ob2+sum(ob2%instid(i)%clwp(:)) 
         end if

         if (associated(ob2%instid(i)%pm)) &
            sum_ob2=sum_ob2+sum(ob2%instid(i)%pm(:,:)) 

         if (associated(ob2%instid(i)%tm)) &
            sum_ob2=sum_ob2+sum(ob2%instid(i)%tm(:,:)) 

         if (associated(ob2%instid(i)%qm)) &
            sum_ob2=sum_ob2+sum(ob2%instid(i)%qm(:,:)) 

         if (associated(ob2%instid(i)%cloud_flag)) &
            sum_ob2=sum_ob2+sum(ob2%instid(i)%cloud_flag(:,:)) 
      end do
   else
      sum_ob2=-1.0
   end if

100 continue

   write(unit=stderr,fmt='(A,3(E18.10,1X),A,I6)') "oi:", sum_y1, sum_y2, sum_ob1, str, line

end subroutine da_oi


