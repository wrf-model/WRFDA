subroutine trace_real_sort(key, n, index)

!-----------------------------------------------------------------------
!--Purpose: Sort reals for tracing
!-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: n      ! The number of items to be sorted. 
   real,    intent(in)  :: key(1:n)
   integer, intent(out) :: index(1:n)

   integer :: head       ! heaps are tree structures: head and child refer
   integer :: child      ! to related items within the tree 
   integer :: i          
   integer :: dum        ! used to swap index items

!--initialise index:
   do i=1,n
      index(i)=i
   end do

#if 0
   do head = 1, n-1
      do i = head+1, n
         if(key(index(i)) > key(index(head))) then
            dum=index(i)
            index(i)=index(head)
            index(head)=dum
         endif
      enddo
   enddo

!  do head = 1, n
!     write(unit=0, fmt='(a,i4,a,f12.3)') &
!          'key(', index(head), ')=', key(index(head))
!  enddo
#else
!--Do heapsort: Create the heap...
   makeheap : do i=n/2,1,-1
      head=i
      sift1 : do
         !-find the largest out of the head and its two children...
         child=head*2
         if (child>n) exit sift1
         if (child<n) then
            if (key(index(child+1))>key(index(child))) child=child+1
         end if

         !-if the head is the largest, then sift is done...
         if (key(index(head))>=key(index(child))) exit sift1

         !-otherwise swap to put the largest child at the head,
         !-and prepare to repeat the procedure for the head in its new
         !-subordinate position.
         dum=index(child)
         index(child)=index(head)
         index(head)=dum
         head=child
      end do sift1
   end do makeheap

!--Retire heads of the heap, which are the largest, and
!--stack them at the end of the array.

   retire : do i=n,2,-1
      dum=index(1)
      index(1)=index(i)
      index(i)=dum
      head=1
      !-second sift is similar to first...
      sift2: do
         child=head*2
         if (child>(i-1)) exit sift2
         if (child<(i-1)) then
            if (key(index(child+1))>key(index(child))) child=child+1
         end if

         if (key(index(head))>=key(index(child))) exit sift2
         dum=index(child)  
         index(child)=index(head)
         index(head)=dum
         head=child
      end do sift2  
   end do retire
#endif

end subroutine trace_real_sort

