SUBROUTINE DA_Recursive_Filter_1D_Adj( pass, alpha, field, n )

!---- -------------------------------------------------------------------------
!  PURPOSE: Perform one pass of recursive filter on 1D array - adjoint.
!
!  METHOD:  Standard adjoint coding.
!
!  HISTORY: 08/14/2000 - Creation of F90 version.           Dale Barker
!
!  PARENT MODULE: DA_Recursive_Filter
!------------------------------------------------------------------------------

   INTEGER, INTENT(IN)    :: pass           ! Current pass of filter.
   REAL, INTENT(IN)       :: alpha          ! Alpha coefficient for RF.
   REAL, INTENT(INOUT)    :: field(:)       ! Array to be filtered.
   INTEGER, INTENT(IN)    :: n              ! Size of field array.
   
   INTEGER                :: j, p           ! Loop counter.
   REAL                   :: one_alpha      ! 1 - alpha.
   REAL                   :: a(1:n)         ! Input field.
   REAL                   :: b(1:n)         ! Field after left-right pass.
   REAL                   :: c(1:n)         ! Field after right-left pass.

!----------------------------------------------------------------------------
!  [1.0] Initialise:
!----------------------------------------------------------------------------

   one_alpha = 1.0 - alpha

!----------------------------------------------------------------------------
!  [4.0] Copy and tidy up:
!----------------------------------------------------------------------------

   c(1:n) = field(1:n)
   
!----------------------------------------------------------------------------
!  [3.0] Perform left-moving filter:
!----------------------------------------------------------------------------

!  [3.2] Perform pass left to right:

   b(1:n) = 0.0   

   DO j = 1, n-1
      c(j+1) = c(j+1) + alpha * c(j)
      b(j) = one_alpha * c(j)
   END DO

!  Use turning conditions as in the appendix of Hayden & Purser (1995):

   IF ( pass == 1 ) THEN
      b(n) = b(n) + c(n) / ( 1.0 + alpha )
   ELSE
      b(n) = b(n) + one_alpha * c(n) / ( 1.0 - alpha**2)**2
      b(n-1) = b(n-1) - one_alpha * alpha**3 * c(n) / ( 1.0 - alpha**2)**2
   END IF

!----------------------------------------------------------------------------
!  [2.0] Perform right-moving filter:
!----------------------------------------------------------------------------

   a(1:n) = 0.0

!  [2.2] Perform pass left to right:

   DO j = n, 2, -1
      b(j-1) = b(j-1) + alpha * b(j)
      a(j) = a(j) + one_alpha * b(j)
   END DO

!  Use turning conditions as in the appendix of Hayden & Purser (1995):

   IF ( pass == 1 ) THEN
      a(1) = a(1) + one_alpha * b(1)
   ELSE IF ( pass == 2 ) THEN
      a(1) = a(1) + b(1) / ( 1.0 + alpha )
   ELSE
       a(1) = a(1) + one_alpha * b(1) / ( 1.0 - alpha**2)**2
       a(2) = a(2) - one_alpha * alpha**3 * b(1) / ( 1.0 - alpha**2)**2
   END IF

   field(1:n) = a(1:n)
   
END SUBROUTINE DA_Recursive_Filter_1D_Adj




