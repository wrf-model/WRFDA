program update_bc

   use record_header
   use module_io

   implicit none

   integer, parameter :: ic_unit = 10, &
                         bc_unit = 12, &
                     output_unit = 20

   integer(kind=4) :: flag

   integer :: ier, i, j, k, n, counter

   real(kind=4), dimension(:,:,:,:), allocatable :: data

   logical :: print_info

   print_info = .true.


   call read_mm5_ic(ic_unit, ier)

   if(ier /= 0) then
      write(unit=*, fmt='(a)') &
           'Error to read new IC.', &
           'Program stoped.'
      stop 'Wrong IC file'
   end if

   nb=5

   counter = 0

   do
      read(bc_unit, iostat=ier) flag

      if(ier /= 0) exit

      write(output_unit) flag

      if (flag == 0) then
         read(bc_unit,iostat=ier) big_header%bhi,  big_header%bhr, &
                                  big_header%bhic, big_header%bhrc

         if(ier/=0) then
            write(*,'("Error reading big header large domain")')
            call abort()
         endif

         if(print_info) &
         call print_big_header(big_header%bhi,  big_header%bhr, &
                               big_header%bhic, big_header%bhrc)

         time_interval=big_header%bhr(1,7)

         write(output_unit) big_header%bhi,  big_header%bhr, &
                            big_header%bhic, big_header%bhrc

         new_variable = .false.

         kl=mkx
         cb=1
         ce=2
         nl=1
      elseif (flag == 1) then
         read(bc_unit,iostat=ier) &
              sub_header%ndim, &
              sub_header%start_index, &
              sub_header%end_index, &
              sub_header%current_time, &
              sub_header%staggering, &
              sub_header%ordering, &
              sub_header%current_date, &
              sub_header%name, &
              sub_header%units, &
              sub_header%description

         allocate(data(sub_header%start_index(1):sub_header%end_index(1), &
                       sub_header%start_index(2):sub_header%end_index(2), &
                       sub_header%start_index(3):sub_header%end_index(3), &
                       sub_header%start_index(4):sub_header%end_index(4)))

         read(bc_unit) data

!        if(print_info) &
         call print_sub_header(sub_header)

         write(output_unit) &
               sub_header%ndim, &
               sub_header%start_index, &
               sub_header%end_index, &
               sub_header%current_time, &
               sub_header%staggering, &
               sub_header%ordering, &
               sub_header%current_date, &
               sub_header%name, &
               sub_header%units, &
               sub_header%description

         var_name = sub_header%name

         kl=sub_header%end_index(2)
         nb=sub_header%end_index(3)


         counter = counter + 1

         if(counter > 8) counter = counter - 8

         if(counter == 1) then
            if(new_variable) call deallocate_bc

            new_variable = .true.

            cb=2
            ce=4
            kl=mkx
            nl=1
        
            if(var_name(1:1) == 'W') kl=mkx+1

            call allocate_bc(mix,mjx,kl,nb)

            if(var_name(1:1) == 'U') then
               call set_bc_val(mix,mjx,kl,nb,uuu,wbn,ebn,sbn,nbn,dot)
            else if(var_name(1:1) == 'V') then
               call set_bc_val(mix,mjx,kl,nb,vvv,wbn,ebn,sbn,nbn,dot)
            else if(var_name(1:1) == 'W') then
               call set_bc_val(mix,mjx,kl,nb,www,wbn,ebn,sbn,nbn,crs)
            else if(var_name(1:1) == 'T') then
               call set_bc_val(mix,mjx,kl,nb,ttt,wbn,ebn,sbn,nbn,crs)
            else if(var_name(1:1) == 'Q') then
               call set_bc_val(mix,mjx,kl,nb,qqq,wbn,ebn,sbn,nbn,crs)
            else if(var_name(1:1) == 'P') then
               cb=3
               ce=5
               call set_bc_val(mix,mjx,kl,nb,ppp,wbn,ebn,sbn,nbn,crs)
            end if
         end if

         if(var_name(cb:ce) == 'WB ') then
            wbo(:,:,:)=data(:,:,:,1)

            data(:,:,:,1)=wbn(:,:,:)
         else if(var_name(cb:ce) == 'EB ') then
            ebo(:,:,:)=data(:,:,:,1)

            data(:,:,:,1)=ebn(:,:,:)
         else if(var_name(cb:ce) == 'SB ') then
            sbo(:,:,:)=data(:,:,:,1)

            data(:,:,:,1)=sbn(:,:,:)
         else if(var_name(cb:ce) == 'NB ') then
            nbo(:,:,:)=data(:,:,:,1)

            data(:,:,:,1)=nbn(:,:,:)
         else if(var_name(cb:ce) == 'WBT') then
            wbt(:,:,:)=data(:,:,:,1)

            call set_bc_ten(wbn,wbo,wbt)

            data(:,:,:,1)=wbt(:,:,:)
         else if(var_name(cb:ce) == 'EBT') then
            ebt(:,:,:)=data(:,:,:,1)

            call set_bc_ten(ebn,ebo,ebt)

            data(:,:,:,1)=ebt(:,:,:)
         else if(var_name(cb:ce) == 'SBT') then
            sbt(:,:,:)=data(:,:,:,1)

            call set_bc_ten(sbn,sbo,sbt)

            data(:,:,:,1)=sbt(:,:,:)
         else if(var_name(cb:ce) == 'NBT') then
            nbt(:,:,:)=data(:,:,:,1)

            call set_bc_ten(nbn,nbo,nbt)

            data(:,:,:,1)=nbt(:,:,:)
         end if

         write(output_unit) data

         deallocate(data)

      elseif (flag == 2) then

         exit

      endif

   enddo

   do
      read(bc_unit, iostat=ier) flag

      if(ier /= 0) exit

      write(output_unit) flag

      if (flag == 0) then
         read(bc_unit,iostat=ier) big_header%bhi,  big_header%bhr, &
                                  big_header%bhic, big_header%bhrc

         if(ier/=0) then
            write(*,'("Error reading big header large domain")')
            call abort()
         endif

         if(print_info) &
         call print_big_header(big_header%bhi,  big_header%bhr, &
                               big_header%bhic, big_header%bhrc)

         write(output_unit) big_header%bhi,  big_header%bhr, &
                            big_header%bhic, big_header%bhrc

      elseif (flag == 1) then
         read(bc_unit,iostat=ier) &
              sub_header%ndim, &
              sub_header%start_index, &
              sub_header%end_index, &
              sub_header%current_time, &
              sub_header%staggering, &
              sub_header%ordering, &
              sub_header%current_date, &
              sub_header%name, &
              sub_header%units, &
              sub_header%description

         allocate(data(sub_header%start_index(1):sub_header%end_index(1), &
                       sub_header%start_index(2):sub_header%end_index(2), &
                       sub_header%start_index(3):sub_header%end_index(3), &
                       sub_header%start_index(4):sub_header%end_index(4)))

         read(bc_unit) data

!        if(print_info) &
         call print_sub_header(sub_header)

         write(output_unit) &
               sub_header%ndim, &
               sub_header%start_index, &
               sub_header%end_index, &
               sub_header%current_time, &
               sub_header%staggering, &
               sub_header%ordering, &
               sub_header%current_date, &
               sub_header%name, &
               sub_header%units, &
               sub_header%description

         write(output_unit) data

         deallocate(data)

      endif

   enddo

CONTAINS

   subroutine allocate_bc(iy,jx,kz,nb)

      implicit none

      integer, intent(in) :: iy,jx,kz,nb

      allocate(wbn(iy,kz,nb))
      allocate(wbo(iy,kz,nb))
      allocate(wbt(iy,kz,nb))

      allocate(ebn(iy,kz,nb))
      allocate(ebo(iy,kz,nb))
      allocate(ebt(iy,kz,nb))

      allocate(sbn(jx,kz,nb))
      allocate(sbo(jx,kz,nb))
      allocate(sbt(jx,kz,nb))

      allocate(nbn(jx,kz,nb))
      allocate(nbo(jx,kz,nb))
      allocate(nbt(jx,kz,nb))

   end subroutine allocate_bc

   subroutine deallocate_bc

      implicit none

      deallocate(wbn)
      deallocate(wbo)
      deallocate(wbt)

      deallocate(ebn)
      deallocate(ebo)
      deallocate(ebt)

      deallocate(sbn)
      deallocate(sbo)
      deallocate(sbt)

      deallocate(nbn)
      deallocate(nbo)
      deallocate(nbt)

   end subroutine deallocate_bc

   subroutine set_bc_val(iy,jx,kz,nb,u,wbv,ebv,sbv,nbv,dot_crs)

      implicit none

      integer,                intent(in)  :: iy,jx,kz,nb,dot_crs
      real, dimension(:,:,:), intent(in)  :: u
      real, dimension(:,:,:), intent(out) :: wbv,ebv,sbv,nbv

      integer :: i,j,k,n,ie,je

      ie=iy+1-dot_crs
      je=jx+1-dot_crs

      do k=1,kz
      do n=1,nb
         do i=1,iy
            wbv(i,k,n)=u(i,   n,k)
            ebv(i,k,n)=u(i,je-n,k)
         end do
        
         do j=1,jx
            sbv(j,k,n)=u(   n,j,k)
            nbv(j,k,n)=u(ie-n,j,k)
         end do
      end do
      end do

   end subroutine set_bc_val

   subroutine set_bc_ten(bn,bo,bt)

      implicit none

      real, dimension(:,:,:), intent(in)    :: bn, bo
      real, dimension(:,:,:), intent(inout) :: bt

      integer :: i,k,n,ie,ke,ne

      real    :: rti

      rti = 1.0/time_interval

      ie=size(bn, dim=1)
      ke=size(bn, dim=2)
      ne=size(bn, dim=3)

      do k=1,ke
      do n=1,ne
      do i=1,ie
         bt(i,k,n)=bt(i,k,n)+rti*(bo(i,k,n)-bn(i,k,n))
      end do
      end do
      end do

   end subroutine set_bc_ten

end program update_bc

