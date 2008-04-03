subroutine trace_set(my_id, my_html, my_csv, my_url)

   implicit none

   integer, optional, intent(in) :: my_id
   logical, optional, intent(in) :: my_html, my_csv
   character(len=*), optional, intent(in) :: my_url

   if(present(my_id)) then
      myproc = my_id
   endif

   if(present(my_html)) then
      use_html = my_html
   endif

   if(present(my_csv)) then
      use_csv = my_csv
   endif

   if(present(my_url)) then
      documentation_url = my_url
   endif

end subroutine trace_set

