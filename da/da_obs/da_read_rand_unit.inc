subroutine da_read_rand_unit(filename, unit_in,num,obs_type_in, nc)
!----------------------------------------------------------------------------   
!  Program to read diagnostics written on rand_unit by WRF-Var
!  Author:   Syed RH Rizvi          MMM/NCAR       05/16/2006
!    
!----------------------------------------------------------------------------   
   IMPLICIT NONE

   INTEGER      ,intent (IN)    :: unit_in
   INTEGER      ,intent (inout) :: num
   character*(*),intent (IN)    :: obs_type_in 
   character*(*),intent (INOUT) :: filename                    
   INTEGER      ,intent (IN)    :: nc      
!
   integer      :: num_obs 
   character*9  :: dummy_c
   character*20 :: ob_type               
   logical   :: if_write
   
   real         :: fld(10), fld1_rad , fld2_rad                         
   integer      :: n, k, n1,n2, levels, ios
!----------------------------------------------------------------------------   
   open(unit=unit_in,   file=trim(filename), status='old',iostat=ios)
   if (ios /= 0 ) Then
      call da_error(__FILE__,__LINE__, &
         (/"Canot open file"//trim(filename)/))
   Endif
1  continue
!----------------------------------------------------------------------------   

   read(unit_in,'(a20,i8)', end = 999, err = 1000)ob_type,num_obs                    
   
   if_write = .FALSE.
   if( index(ob_type,OBS_TYPE_IN(1:nc)) > 0 ) if_write = .TRUE.

!    If radiance data treat differently
   if (index(ob_type,'noaa') > 0  .and. index(ob_type,'amsu') > 0 ) then
     DO n = 1, num_obs
      if (if_write) num = num + 1
       read(unit_in,'(2i8,2e15.7)')n1, n2, fld1_rad,fld2_rad
       if(if_write)write(rand_unit,'(2i8,2e15.7)')num,n2, fld1_rad,fld2_rad
     ENDDO
   else
   
      DO n = 1, num_obs        
        if( if_write) num = num + 1
        if( index( ob_type,'bogus')     > 0 )  then
         read(unit_in,'(i8)', err=1000)levels
         if(if_write) write(rand_unit,'(i8)')levels
         read(unit_in,'(2i8,10e15.7)', err= 1000) n1, n2, fld
         if(if_write) write(rand_unit,'(2i8,10e15.7)') num, levels, fld  
        endif
       read(unit_in,'(i8)', err=1000)levels
         if(if_write) write(rand_unit,'(i8)')levels
         DO k = 1, levels
         read(unit_in,'(2i8,10e15.7)', err= 1000) n1, n2, fld  
         if(if_write) write(rand_unit,'(2i8,10e15.7)') num, k, fld
         END DO
      END DO
   endif
!--------------------------------------------------------------
   if( if_write )  go to 999
   go to 1
!--------------------------------------------------------------
999  continue
     close (unit_in)
     return
1000  continue 
   write(unit=errmsg(1), fmt='(/a,i3/a/)') &
              'read error on unit: ',unit_in, &
              'in da_read_rand_unit'
   call da_error(__FILE__,__LINE__,errmsg(1:1))

end subroutine da_read_rand_unit
