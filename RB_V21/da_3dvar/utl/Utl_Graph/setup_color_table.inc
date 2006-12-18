subroutine setup_color_table

   implicit none

   call gscr(1,  0, 1.00, 1.00, 1.00) ! White
   call gscr(1,  1, 0.00, 0.00, 0.00) ! BLACK
   call gscr(1,  2, 1.00, 0.00, 0.00) ! Red
   call gscr(1,  3, 0.00, 1.00, 0.00) ! Green
   call gscr(1,  4, 0.00, 0.00, 1.00) ! Blue
   call gscr(1,  5, 0.60, 0.00, 0.80) ! Dark violet
   call gscr(1,  6, 0.00, 1.00, 1.00) ! Cyran
   call gscr(1,  7, 1.00, 0.00, 1.00) ! Magenta
   call gscr(1,  8, 0.90, 0.25, 0.00) ! FreshRed
   call gscr(1,  9, 0.40, 0.30, 0.20) ! Tan
   call gscr(1, 10, 1.00, 1.00, 0.00) ! Yellow
   call gscr(1, 11, 0.60, 0.60, 0.60) ! Gray

end subroutine setup_color_table
