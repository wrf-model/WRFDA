SUBROUTINE crs_2_dot (crs, dot)

   implicit none

   real, dimension(:,:) :: crs, dot


   integer :: i, j, ix, jx

   ix = size(crs, dim=1)
   jx = size(crs, dim=2)

   do j = 2, jx-1
      do i = 2, ix-1
         dot (i,j) = 0.25 * ( crs (i,j  ) + crs (i-1,j  ) &
                            + crs (i,j-1) + crs (i-1,j-1))
      enddo

      dot (1, j) = 0.5 * (crs (   1,j) + crs (   1,j-1))
      dot (ix,j) = 0.5 * (crs (ix-1,j) + crs (ix-1,j-1))
   enddo

   do i = 2, ix-1
      dot (i, 1) = 0.5 * (crs (i,   1) + crs (i-1,   1))
      dot (i,jx) = 0.5 * (crs (i,jx-1) + crs (i-1,jx-1))
   enddo

   dot ( 1, 1) = crs (   1,   1)
   dot ( 1,jx) = crs (   1,jx-1)
   dot (ix,jx) = crs (ix-1,jx-1)
   dot (ix, 1) = crs (ix-1,   1)

END SUBROUTINE crs_2_dot
