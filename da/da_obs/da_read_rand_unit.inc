subroutine da_read_rand_unit(unit_in,unit_out,num,obs_type_in, nc)
!----------------------------------------------------------------------------   
!  Program to read diagnostics written on yp_unit or y_unit by WRF-Var
!  Author:   Syed RH Rizvi          MMM/NCAR       05/16/2006
!    
!----------------------------------------------------------------------------   
   IMPLICIT NONE

   INTEGER      ,intent (IN) :: unit_in, unit_out
   INTEGER      ,intent (inout) :: num
   character*(*),intent (IN) :: obs_type_in                 
   INTEGER      ,intent (IN) :: nc      
!
   integer      :: num_obs 
   character*9  :: dummy_c
   character*20 :: ob_type               
   logical   :: if_write
   
   real         :: fld(10)                          
   integer      :: n, k, n1,n2, levels
!----------------------------------------------------------------------------   
1  continue
!----------------------------------------------------------------------------   

   read(unit_in,'(a20,i8)', end = 999, err = 1000)ob_type,num_obs                    
   if( ob_type(1:5) =='*****') return
   
   if_write = .FALSE.
   if( index(ob_type,OBS_TYPE_IN(1:nc)) > 0 ) if_write = .TRUE.

   
      DO n = 1, num_obs        
        if( if_write) num = num + 1
        if( index( ob_type,'bogus')     > 0 )  then
         read(unit_in,'(i8)', err=1000)levels
         if(if_write) write(unit_out,'(i8)')levels
         read(unit_in,'(2i8,10e15.7)', err= 1000) n1, n2, fld
         if(if_write) write(unit_out,'(2i8,10e15.7)') num, levels, fld  
        endif
       read(unit_in,'(i8)', err=1000)levels
         if(if_write) write(unit_out,'(i8)')levels
         DO k = 1, levels
         read(unit_in,'(2i8,10e15.7)', err= 1000) n1, n2, fld  
         if(if_write) write(unit_out,'(2i8,10e15.7)') num, k, fld
         END DO
      END DO
!--------------------------------------------------------------
   if( if_write ) return
   go to 1
!--------------------------------------------------------------
999  continue
   write(unit=stdout, fmt='(/a,i3/a/)') &
              'unexpected end of file on unit: ',unit_in, &                       
              'in da_read_rand_unit'
      stop
1000  continue 
   write(unit=stdout, fmt='(/a,i3/a/)') &
              'read error on unit: ',unit_in, &
              'in da_read_rand_unit'
      stop
end subroutine da_read_rand_unit
