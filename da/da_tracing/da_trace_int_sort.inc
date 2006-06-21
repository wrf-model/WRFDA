subroutine da_trace_int_sort( &
   key, &
   n, &
   index)

   IMPLICIT NONE

   INTEGER, INTENT(IN)          :: n      ! The number of items to be sorted. 
   INTEGER, INTENT(IN)          :: key(:)
   INTEGER, INTENT(OUT) :: index(:)

   INTEGER :: head       ! heaps are tree structures: head and child refer
   INTEGER :: child      ! to related items within the tree 
   INTEGER :: i          
   INTEGER :: dum        ! used to swap index items


   ! initialise index:
   DO i=1,n
      index(i)=i
   END DO 

   ! Do heapsort: Create the heap...
   makeheap : DO i=n/2,1,-1
      head=i
      sift1 : DO
         ! find the largest out of the head and its two children...
         child=head*2
         IF (child>n) EXIT sift1
         IF (child<n) THEN
            IF (key(index(child+1))>key(index(child))) child=child+1
         END IF
         ! if the head is the largest, then sift is done...
         IF (key(index(head))>=key(index(child))) EXIT sift1
         ! otherwise swap to put the largest child at the head,
         ! and prepare to repeat the procedure for the head in its new
         ! subordinate position.
         dum=index(child)
         index(child)=index(head)
         index(head)=dum
         head=child
      END DO sift1
   END DO makeheap

   ! Retire heads of the heap, which are the largest, and
   ! stack them at the end of the array.
   retire : DO i=n,2,-1
      dum=index(1)
      index(1)=index(i)
      index(i)=dum
      head=1
         ! second sift is similar to first...
      sift2: DO
         child=head*2
         IF (child>(i-1)) EXIT sift2
         IF (child<(i-1)) THEN
            IF (key(index(child+1))>key(index(child))) child=child+1
         END IF
         IF (key(index(head))>=key(index(child))) EXIT sift2
         dum=index(child)
         index(child)=index(head)
         index(head)=dum
         head=child
      END DO sift2  
   END DO retire

end subroutine da_trace_int_sort
