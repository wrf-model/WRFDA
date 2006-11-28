subroutine da_read_y_unit(filename,unit_out,num,obs_type_in, nc)

   !-------------------------------------------------------------------------  
   ! Program to read diagnostics written on yp_unit or y_unit by WRF-Var
   ! Author:   Syed RH Rizvi          MMM/NCAR       05/16/2006
   !-------------------------------------------------------------------------   

   implicit none

   integer      ,intent (in)    :: unit_out
   integer      ,intent (inout) :: num
   character*(*),intent (in)    :: obs_type_in, filename                 
   integer      ,intent (in)    :: nc      

   integer      :: num_obs , unit_in, ios
   character*9  :: dummy_c
   character*20 :: ob_type               
   logical      :: if_write
   
   real         :: fld(7), fld_rad                          
   integer      :: n, k, n1,n2, levels

   ob_type="Unknown"

   call da_get_unit(unit_in)
   open(unit=unit_in,file=trim(filename),form='formatted',iostat=ios,status='old')
   if (ios /= 0 ) Then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot open random observation error file"//trim(filename)/))
   Endif

1  continue

   read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type,num_obs                    
   
   if_write = .false.
   if (index(ob_type,OBS_type_in(1:nc)) > 0) if_write = .true.

!    If radiance data treat differently
   if (index(ob_type,'noaa') > 0  .and. index(ob_type,'amsu') > 0 ) then             
     do n = 1, num_obs        
      if (if_write) num = num + 1
       read(unit_in,'(2i8,e15.7)')n1, n2, fld_rad                              
       if(if_write)write(unit_out,'(2i8,e15.7)')num,n2, fld_rad                              
     end do
   else    
     do n = 1, num_obs        
      if (if_write) num = num + 1
      if (index( ob_type,'bogus') > 0) then
         read(unit_in,'(i8)', err=1000)levels
         if (if_write) write(unit_out,'(i8)')levels
         read(unit_in,'(2i8,7e15.7)', err= 1000) n1, n2, fld
         if (if_write) write(unit_out,'(2i8,7e15.7)') num, levels, fld  
      end if
      read(unit_in,'(i8)', err=1000)levels
      if (if_write) write(unit_out,'(i8)')levels
      do k = 1, levels
         read(unit_in,'(2i8,7e15.7)', err= 1000) n1, n2, fld  
         if (if_write) write(unit_out,'(2i8,7e15.7)') num, k, fld
      end do
     end do
   end if
   if (if_write) go to 999
   go to 1

999  continue
   close (unit_in)
   call da_free_unit(unit_in)
   return
1000  continue 
   write(unit=message(1), fmt='(a,i3,a,a)') &
      'read error on unit: ',unit_in, ' for ob_type', trim(ob_type)
   call da_warning(__FILE__,__LINE__,message(1:1))

end subroutine da_read_y_unit
