subroutine da_recursive_filter_1d(pass, alpha, field, n)

   !---------------------------------------------------------------------------
   ! Purpose: Perform one pass of recursive filter on 1D array.
   !
   ! Method:  Perform right-moving filter followed by left-moving filter.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: pass           ! Current pass of filter.
   real,    intent(in)    :: alpha          ! Alpha coefficient for RF.
   real,    intent(inout) :: field(:)       ! Array to be filtered.
   integer, intent(in)    :: n              ! Size of field array.

   integer :: j              ! Loop counter.
   real    :: one_alpha      ! 1 - alpha.
   real    :: a(1:n)         ! Input field.
   real    :: b(1:n)         ! Field after left-right pass.
   real    :: c(1:n)         ! Field after right-left pass.

   if (trace_use_dull) call da_trace_entry("da_recursive_filter_1d")
   
   !-------------------------------------------------------------------------
   ! [1.0] Initialise:
   !-------------------------------------------------------------------------

   one_alpha = 1.0 - alpha
   
   a(1:n) = field(1:n)

   !-------------------------------------------------------------------------
   ! [2.0] Perform right-moving filter:
   !-------------------------------------------------------------------------

   ! use turning conditions as in the appendix of Hayden & Purser (1995):

   if (pass == 1) then
      b(1) = one_alpha * a(1)
   else if (pass == 2) then
      b(1) = a(1) / (1.0 + alpha)
   else
      b(1) = one_alpha * (a(1) - alpha**3 * a(2)) / (1.0 - alpha**2)**2
   end if

   ! [2.2] Perform pass left to right:

   do j = 2, n
      b(j) = alpha * b(j-1) + one_alpha * a(j)
   end do

   !-------------------------------------------------------------------------
   ! [3.0] Perform left-moving filter:
   !-------------------------------------------------------------------------

   ! use turning conditions as in the appendix of Hayden & Purser (1995):

   if (pass == 1) then
      c(n) = b(n) / (1.0 + alpha)
   else
      c(n) = one_alpha * (b(n) - alpha**3 * b(n-1)) / (1.0 - alpha**2)**2
   end if

   ! [3.2] Perform pass left to right:

   do j = n-1, 1, -1
      c(j) = alpha * c(j+1) + one_alpha * b(j)
   end do
        
   field(1:n) = c(1:n)

   if (trace_use_dull) call da_trace_exit("da_recursive_filter_1d")
   
end subroutine da_recursive_filter_1d


 !--------------------------------------------------------------------------

   ! [2.1] Print out global eigenvectors:

   write(unit=stdout,fmt='(2A)') 'Domain eigenvectors for ', trim(name)

   write(unit=stdout,fmt='(50i13)')(m, m=1,kz)
   do k = 1, kz      
      write(unit=stdout,fmt='(I3,50e13.5)')k, (be_eigenvec(k,m), m=1,kz)
   end do
   write(unit=stdout,fmt='(A)') " "

   ! [2.2]: Test eigenvector orthogonality: sum_k (e_m(k) e_n(k)) = delta_mn:

   allocate(array_mask(1:kz))
   allocate(matrix2(1:kz,1:kz))
      
   write(unit=stdout,fmt='(2A)') &
      'Eigenvector orthogonality check for ', trim(name)
   write(unit=stdout,fmt='(A)')' Mode     Diagonal         Maximum off-diagonal'
   do k1 = 1, kz
      do k2 = 1, kz
         matrix2(k1,k2) = sum(be_eigenvec(:,k1) * be_eigenvec(:,k2))
      end do
         
      array_mask(1:kz) =.true.
      array_mask(k1) = .false.
      max_off_diag = MAXVAL(ABS(matrix2(k1,:)),mask=array_mask(:))
      write(unit=stdout,fmt='(I4,4x,1pe12.4,10x,1pe12.4)')k1, matrix2(k1,k1), &
         max_off_diag
   end do
   write(unit=stdout,fmt=*) ' '

   ! [2.3] Test eigenvectors completeness - sum_m (e_m(k1) e_m(k2)) = delta_k1k2

   write(unit=stdout,fmt='(2A)') &
      'Eigenvector completeness check for ', trim(name)
   write(unit=stdout,fmt='(A)')' Level    Diagonal         Maximum off-diagonal'
   do k1 = 1, kz
      do k2 = 1, kz
         matrix2(k1,k2) = sum(be_eigenvec(k1,:) * be_eigenvec(k2,:))
      end do
         
      array_mask(1:kz) =.true.
      array_mask(k1) = .false.
      max_off_diag = MAXVAL(ABS(matrix2(k1,:)),mask=array_mask(:))
      write(unit=stdout,fmt='(I4,4x,1pe12.4,10x,1pe12.4)')k1, matrix2(k1,k1), &
         max_off_diag
   end do
   write(unit=stdout,fmt=*) ' '

   !-------------------------------------------------------------------------
   ! [3.0]: Tidy up:
   !-------------------------------------------------------------------------  

   deallocate(matrix2)
   deallocate(array_mask)

   if (trace_use) call da_trace_exit("da_check_eof_decomposition")
       
end subroutine da_check_eof_decomposition


