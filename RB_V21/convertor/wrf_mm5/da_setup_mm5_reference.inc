SUBROUTINE da_setup_mm5_reference(xb)

!------------------------------------------------------------------------------
!  PURPOSE: Define/allocate components of MM5 model state.
!
!  METHOD:
!
!  HISTORY: 08/09/2004 - Creation of F90 version.
!           Wei Huang
!
!  PARENT_MODULE: wrf_mm5
!------------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE (xb_type), INTENT(inout) :: xb     ! First guess state
 
   INTEGER                       :: ier         ! error index
   integer                       :: i, j

   integer(kind=4) :: flag, big_head_flag
   real(kind=4), dimension(:,:,:,:), allocatable :: data

!  LOGICAL, EXTERNAL :: wrf_dm_on_monitor

   TYPE (big_record_header) :: big_header
   TYPE (sub_record_header) :: sub_header

!---------------------------------------------------------------------------
!  [1.0] Read original MM5 format first guess:
!---------------------------------------------------------------------------

   write(unit=*, fmt='(2a,2x,a,i6)') &
        'file:', __FILE__, 'line:', __LINE__

   rewind(jpin)

   read_loop: do
      READ(jpin, iostat=ier) flag

      if( ier /= 0) then
          write(0, fmt='(a, i4, a)') &
               'Error in read flag from unit:', jpin, &
               'Stopped here.'
          call abort()
      end if

      if(ier/=0) then
         write(unit=0,fmt='(a)') 'Error reading header flag'
         call abort()
      endif

      if (flag == 0) then
         read(jpin, iostat=ier) big_header % bhi,  big_header % bhr,&
                                big_header % bhic, big_header % bhrc
         if(ier/=0) then
            write(0,'("Error reading big header")')
            call abort()
         endif
      elseif (flag == 1) then

         read(jpin,iostat=ier) &
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

         if(ier/=0) then
            write(0,'("Error reading subheader")')
            call abort()
         endif
         allocate(data(sub_header%start_index(1):sub_header%end_index(1), &
                       sub_header%start_index(2):sub_header%end_index(2), &
                       sub_header%start_index(3):sub_header%end_index(3), &
                       sub_header%start_index(4):sub_header%end_index(4)))

         read(jpin,iostat=ier) data

         if(sub_header%name ==  'PSTARCRS ') then
            do j=xb%jts,xb%jte
            do i=xb%its,xb%ite
               xb%psac(i,j)  = data(j,i,1,1)
            enddo
            enddo

            close(jpin)
            exit read_loop
         endif

         deallocate(data)

      elseif (flag == 2) then
         cycle
      else
         stop
      endif
   enddo read_loop

   write(unit=*, fmt='(2a,2x,a,i6)') &
        'file:', __FILE__, 'line:', __LINE__
 
   ptop = big_header%bhr(2,2)
   ps0  = big_header%bhr(2,5)
   ts0  = big_header%bhr(3,5)
   tlp  = big_header%bhr(4,5)
   tis0 = big_header%bhr(5,5)

   xb%ptop = big_header%bhr(2,2)
   xb%ps0  = big_header%bhr(2,5)
   xb%ts0  = big_header%bhr(3,5)
   xb%tlp  = big_header%bhr(4,5)
   xb%tis0 = big_header%bhr(5,5)
   
END SUBROUTINE da_setup_mm5_reference

