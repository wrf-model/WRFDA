subroutine da_convertor_v_interp( v_in, z_in, n_in, &
                                  v_out, z_out, n_out )

   implicit none

   integer,                intent(in)  :: n_in, n_out
   real, dimension(n_in),  intent(in)  :: v_in, z_in
   real, dimension(n_out), intent(in)  :: z_out
   real, dimension(n_out), intent(out) :: v_out

   integer :: k, kabv, kblw, i
   real    :: w1, w2

   logical :: increase_in_vertical

!--does vertical coordinate increase or decrease with increasing k?
!  set offset appropriately

   if(z_in(n_in) > z_in(1)) then
      increase_in_vertical = .true.

#if 0
      if(print_detail > 0) then
         write(unit=*, fmt='(2a,2x,a,i4/a)') &
              'file:', __FILE__, 'line:', __LINE__, &
              'monotonic increase in vertical interpolation.'
      endif
#endif
   else
      increase_in_vertical = .false.

#if 0
      if(print_detail > 0) then
         write(unit=*, fmt='(2a,2x,a,i4/a)') &
              'file:', __FILE__, 'line:', __LINE__, &
              'monotonic decrease in vertical interpolation.'
      endif
#endif
   endif

   if(increase_in_vertical) then
      do k=1, n_out
         if(z_out(k) <= z_in(1)) then
            kabv = 2
         else if(z_out(k) >= z_in(n_in)) then
            kabv = n_in
         else
            i_loop: do i=2, n_in
               if(z_out(k) <= z_in(i)) then
                  kabv = i
                  exit i_loop
               endif
            enddo i_loop
         endif

         kblw = kabv - 1
         w2 = (z_in(kabv)-z_out(k))/(z_in(kabv)-z_in(kblw))
         w1 = 1.0-w2
         v_out(k) = w1*v_in(kabv) + w2*v_in(kblw)

#if 0
         if(print_detail > 0) then
            write(unit=*, fmt='(a,2i4,2x,a,2f8.4,2x,a,3f18.10)') &
                 'index:', kabv, k, 'weight:', w1, w2, &
                 'value:', v_in(kabv), v_in(kblw), v_out(k)
         endif
#endif
      enddo
   else
      do k=1, n_out
         if(z_out(k) >= z_in(1)) then
            kabv = 2
         else if(z_out(k) <= z_in(n_in)) then
            kabv = n_in
         else
            d_loop: do i=2, n_in
               if(z_out(k) >= z_in(i)) then
                  kabv = i
                  exit d_loop
               endif
            enddo d_loop
         endif

         kblw = kabv - 1
         w2 = (z_in(kabv)-z_out(k))/(z_in(kabv)-z_in(kblw))
         w1 = 1.0-w2
         v_out(k) = w1*v_in(kabv) + w2*v_in(kblw)

#if 0
         if(print_detail > 0) then
            write(unit=*, fmt='(a,2i4,2x,a,2f8.4,2x,a,3f18.10)') &
                 'index:', kabv, k, 'weight:', w1, w2, &
                 'value:', v_in(kabv), v_in(kblw), v_out(k)
         endif
#endif
      enddo
   endif

end subroutine da_convertor_v_interp

