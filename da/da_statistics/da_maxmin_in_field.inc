subroutine da_maxmin_in_field(field, max, min)

   implicit none

   real, intent(in)         :: field(is:ie,js:je,ks:ke)

   type (maxmin_field_type), intent(out) :: max(xp%kts:xp%kte)
   type (maxmin_field_type), intent(out) :: min(xp%kts:xp%kte)

   do k = ks, ke
      max(k)%value = -1.E20
      min(k)%value =  1.E20
      do j = js, je
         do i = is, ie
            if (field(i,j,k) > max(k)%value) then
               max(k)%value = field(i,j,k)
               max(k)%i     = i
               max(k)%j     = j
            end if
            if (field(i,j,k) < min(k)%value) then
               min(k)%value = field(i,j,k)
               min(k)%i     = i
               min(k)%j     = j
            end if
         end do
      end do
   end do

end subroutine da_maxmin_in_field
