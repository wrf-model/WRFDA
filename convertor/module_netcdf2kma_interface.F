MODULE module_netcdf2kma_interface

   use module_wave2grid_kma
!  implicit none

CONTAINS

SUBROUTINE netcdf2kma_interface ( grid, config_flags ) 

   USE module_domain
   USE module_timing
   USE module_driver_constants
   USE module_configure

!  IMPLICIT NONE
   real    :: DPSE(imaxe,jmaxe)
   real    :: DUE(imaxe,jmaxe,kmax),DVE(imaxe,jmaxe,kmax)
   real    :: DTE(imaxe,jmaxe,kmax),DQE(imaxe,jmaxe,kmax)
   real    :: PSB(imax,jmax)
   real    :: UB(imax,jmax,kmax),VB(imax,jmax,kmax)
   real    :: TB(imax,jmax,kmax),QB(imax,jmax,kmax)
   real    :: PSG(imax,jmax)
   real    :: UG(imax,jmax,kmax),VG(imax,jmax,kmax)
   real    :: TG(imax,jmax,kmax),QG(imax,jmax,kmax)
   integer :: i,j,k      !shcimsi
   real    :: dum(imax,jmax,kmax)  !shcimsi

!--Input data.

   TYPE(domain) , INTENT(INOUT)  :: grid
   TYPE (grid_config_rec_type)   :: config_flags
   integer                       :: USE_INCREMENT      !shc
   integer     :: incre,back,ID(5),KT,IM,JM,KM         !shc
! we have to convert in equal lat/lon data 
!           to Gaussian latitude
!
!   First the Equal lat/lon data
! set Field as per KMA order (North top South and 0 to 360 east)
!shc-wei start
!  back = 102                    !shc start
   back = 48                     !shc start
!shc-wei end
   read(back) ID,KT,IM,JM,KM
   read(back)       !topo
   read(back) PSB
   read(back)       !psea
   read(back) TB 
   read(back) UB 
   read(back) VB 
   read(back) QB 
   read(back)       !rh
   read(back)       !z           !shc end
   USE_INCREMENT=1     !shc start
   if (USE_INCREMENT.eq.1) then
!shc-wei start
!  incre = 101                   
   incre = 47                   
!shc-wei end
   read(incre) DPSE
   read(incre) DUE
   read(incre) DVE
   read(incre) DTE
   read(incre) DQE     !shc end
!  DPSE=20.0; DUE=3.0; DVE=3.0; DTE=5.0;  DQE=0.001    !shcimsi
!  imaxe=grid%ed31-grid%sd31                    !shc start
!  jmaxe=grid%ed32-grid%sd32
!  kmaxe=grid%ed33-grid%sd33
!  imaxg=imaxe;  jmaxg=jmaxe-1; kmaxg=kmaxe    
   call reorder_for_kma(DPSE,imaxe,jmaxe,1)        
   call reorder_for_kma(DUE,imaxe,jmaxe,kmax)
   call reorder_for_kma(DVE,imaxe,jmaxe,kmax)
   call reorder_for_kma(DTE,imaxe,jmaxe,kmax)
   call reorder_for_kma(DQE,imaxe,jmaxe,kmax)  !shc end
   DPSE=DPSE*0.01                              !shchPa 
   call Einc_to_Ganl(DPSE,DUE,DVE,DTE,DQE,&    !shc start
                      PSB, UB, VB, TB, QB,&
                      PSG, UG, VG, TG, QG)             
9001 format(10e15.7)     !shcimsi start
   dum = 0.0
   write(901,9001) ((dum(i,j,1),i=1,imax),j=1,jmax)
   write(901,9001) ((PSG(i,j),i=1,imax),j=1,jmax)
   write(901,9001) ((dum(i,j,1),i=1,imax),j=1,jmax)
   do k=1,kmax
   write(901,9001) ((TG(i,j,k),i=1,imax),j=1,jmax)
   enddo
   do k=1,kmax
   write(901,9001) ((UG(i,j,k),i=1,imax),j=1,jmax)
   enddo
   do k=1,kmax
   write(901,9001) ((VG(i,j,k),i=1,imax),j=1,jmax)
   enddo
   do k=1,kmax
   write(901,9001) ((QG(i,j,k),i=1,imax),j=1,jmax)
   enddo
   do k=1,kmax
   write(901,9001) ((dum(i,j,k),i=1,imax),j=1,jmax)
   enddo
   do k=1,kmax
   write(901,9001) ((dum(i,j,k),i=1,imax),j=1,jmax)   !shcimsi end
   enddo
   call PREGSM1(PSG,TG,UG,VG,QG,PSB,TB,UB,VB,QB) !shc end

   else          !shc

   call reorder_for_kma(grid%ht(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1),&
                         grid%ed31-grid%sd31  ,grid%ed32-grid%sd32,1)
   call reorder_for_kma(grid%psfc(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1),&
                           grid%ed31-grid%sd31  ,grid%ed32-grid%sd32,1)
   call reorder_for_kma(grid%em_u_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,&
                             grid%sd33:grid%ed33-1),&
                             grid%ed31-grid%sd31  ,grid%ed32-grid%sd32   ,&
                             grid%ed33-grid%sd33)
   call reorder_for_kma(grid%em_v_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,&
                             grid%sd33:grid%ed33-1),&
                             grid%ed31-grid%sd31  ,grid%ed32-grid%sd32   ,&
                             grid%ed33-grid%sd33)
   call reorder_for_kma(grid%em_t_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,&
                             grid%sd33:grid%ed33-1),&
                             grid%ed31-grid%sd31  ,grid%ed32-grid%sd32   ,&
                             grid%ed33-grid%sd33)
   call reorder_for_kma(grid%moist_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,&
                             grid%sd33:grid%ed33-1,P_qv:P_qv),&
                             grid%ed31-grid%sd31  ,grid%ed32-grid%sd32   ,&
                             grid%ed33-grid%sd33)
!
! convert xb-psfc pressure in hPa
  grid%psfc(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1) = 0.01 *  &
  grid%psfc(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1)
  write(*,*) 'shcimsi num of gird',grid%ed31,grid%ed32,grid%ed33
  write(*,*) 'shcimsi grid',grid%ed31-grid%sd31,grid%ed32-grid%sd32,&
             grid%ed33-grid%sd33

  CALL PREGSM(grid%psfc(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1),&
        grid%em_t_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1),&
        grid%em_u_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1),&
        grid%em_v_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1),&
    grid%moist_2(grid%sd31:grid%ed31-1,grid%sd32:grid%ed32-1,grid%sd33:grid%ed33-1,P_qv),&                   !shc
    PSB,TB,UB,VB,QB)         !shc

    endif         !shc

    
END SUBROUTINE netcdf2kma_interface


SUBROUTINE reorder_for_kma(wrf,n1,n2,n3)

!IMPLICIT none                
 integer, intent(in) :: n1,n2,n3
 real, intent(inout) :: wrf(n1,n2,n3)

 real, dimension(n1,n2,n3)   :: kma
 integer                     :: i,j,k, n1half
!
    n1half = n1/2 + 0.5
    do k=1,n3
      do j= 1,n2
        do i=1,n1
         if( i <= n1half)then
         kma(n1half+i,n2-j+1,k) = wrf(i,j,k)
         else   
         kma(i-n1half,n2-j+1,k) = wrf(i,j,k)
         end if
        end do
      end do
    end do
      wrf = kma
END SUBROUTINE reorder_for_kma

END MODULE module_netcdf2kma_interface

