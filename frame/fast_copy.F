      SUBROUTINE fast_copy_lint ( outbuf, inbuf, n )
        IMPLICIT NONE
        INTEGER n
        INTEGER*8 inbuf(*), outbuf(*)
        ! Local
        INTEGER i
        DO i = 1,n
           outbuf(i) = inbuf(i)
        ENDDO
        RETURN
      END SUBROUTINE fast_copy_lint

      SUBROUTINE fast_zero_lint ( outbuf, n )
        IMPLICIT NONE
        INTEGER n
        INTEGER*8 outbuf(*)
        ! Local
        INTEGER i
        DO i = 1,n
           outbuf(i) = 0
        ENDDO
        RETURN
      END SUBROUTINE fast_zero_lint

