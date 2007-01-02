      SUBROUTINE DA_Array_Print(direction, a, ch)

      IMPLICIT NONE

      INTEGER, INTENT(IN)            :: direction
      REAL, INTENT(IN)               :: a(:,:)
      CHARACTER (len=*), INTENT(IN)  :: ch

      REAL                           :: amax
      INTEGER                        :: i, j
      INTEGER                        :: len1, len2
      INTEGER                        :: jstart
      INTEGER                        :: jend
      INTEGER                        :: jump

      len1 = SIZE(a(:,:),dim=1)
      len2 = SIZE(a(:,:),dim=2)
      
!     Writes the scalar field a

      amax = MAXVAL(abs(a))
      WRITE(unit=*, fmt='(a, 1pe15.8, 4i8)') &
           'max(a)=', amax, shape(a)

      WRITE(unit=*,fmt='(a, 1pe15.8, a)') &
           'max(a)=', amax, ', i down, j horiz.'

      WRITE(unit=*,fmt='(6x,288i3)') (i,i=1,len2)

!     Direction indicates the order of the rows of the print out:

      IF ( direction == 1 ) THEN
         jstart = 1
         jend = len1
         jump = 1
      ELSE
         jstart = len1
         jend = 1
         jump = -1
      END IF

      if (amax.ne.0.)then
         do j=jstart,jend,jump
            WRITE(unit=*,fmt='(1x,i5,288i3)') j, (INT(a(j,i)/amax*99.),i=1,len2)
         end do
      endif

END SUBROUTINE DA_Array_Print

