SUBROUTINE DA_Recursive_Filter_1D( pass, alpha, field, n )

!---- -------------------------------------------------------------------------
!  PURPOSE: Perform one pass of recursive filter on 1D array.
!
!  METHOD:  Perform right-moving filter followed by left-moving filter.
!
!  HISTORY: 08/14/2000 - Creation of F90 version.           Dale Barker
!
!  PARENT MODULE: DA_Recursive_Filter
!------------------------------------------------------------------------------

   INTEGER, INTENT(IN)    :: pass           ! Current pass of filter.
   REAL, INTENT(IN)       :: alpha          ! Alpha coefficient for RF.
   REAL, INTENT(INOUT)    :: field(:)       ! Array to be filtered.
   INTEGER, INTENT(IN)    :: n              ! Size of field array.

   INTEGER                :: j, p           ! Loop counters.
   REAL                   :: one_alpha      ! 1 - alpha.
   REAL                   :: a(1:n)         ! Input field.
   REAL                   :: b(1:n)         ! Field after left-right pass.
   REAL                   :: c(1:n)         ! Field after right-left pass.
   
!----------------------------------------------------------------------------
!  [1.0] Initialise:
!----------------------------------------------------------------------------

   one_alpha = 1.0 - alpha
   
   a(1:n) = field(1:n)

!----------------------------------------------------------------------------
!  [2.0] Perform right-moving filter:
!----------------------------------------------------------------------------

!  Use turning conditions as in the appendix of Hayden & Purser (1995):

   IF ( pass == 1 ) THEN
      b(1) = one_alpha * a(1)
   ELSE IF ( pass == 2 ) THEN
      b(1) = a(1) / ( 1.0 + alpha )
   ELSE
      b(1) = one_alpha * ( a(1) - alpha**3 * a(2) ) / ( 1.0 - alpha**2)**2
   END IF

!  [2.2] Perform pass left to right:

   DO j = 2, n
      b(j) = alpha * b(j-1) + one_alpha * a(j)
   END DO

!----------------------------------------------------------------------------
!  [3.0] Perform left-moving filter:
!----------------------------------------------------------------------------

!  Use turning conditions as in the appendix of Hayden & Purser (1995):

   IF ( pass == 1 ) THEN
      c(n) = b(n) / ( 1.0 + alpha )
   ELSE
      c(n) = one_alpha * ( b(n) - alpha**3 * b(n-1) ) / ( 1.0 - alpha**2)**2
   END IF

!  [3.2] Perform pass left to right:

   DO j = n-1, 1, -1
      c(j) = alpha * c(j+1) + one_alpha * b(j)
   END DO
        
   field(1:n) = c(1:n)
   
END SUBROUTINE DA_Recursive_Filter_1D







