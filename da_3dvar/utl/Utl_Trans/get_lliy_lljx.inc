   subroutine get_lliy_lljx(big_header_CAA_domain, big_header_current, &
                            lliy, lljx)

      implicit none

      type (big_record_header), intent(in)  :: big_header_CAA_domain, big_header_current
      integer,                  intent(out) :: lliy, lljx



      integer :: miy_current, mjx_current, &
                 miy_CAA_domain, mjx_CAA_domain, i, j

!  For CAA domain

!  lliy_CAA_domain: low-left corner i-direction (y-direction) in its mother domain
!  lljx_CAA_domain: low-left corner j-direction (x-direction) in its mother domain
!  miy_CAA_domain_coarse: The most coarse domain size in i-direction (y-direction)
!  mjx_CAA_domain_coarse: The most coarse domain size in j-direction (x-direction)

      integer :: lliy_CAA_domain, lljx_CAA_domain, miy_CAA_domain_coarse, mjx_CAA_domain_coarse

!  For 3DVAR domain

!  lliy_current: low-left corner i-direction (y-direction) in its mother domain
!  lljx_current: low-left corner j-direction (x-direction) in its mother domain
!  miy_current_coarse: The most coarse domain size in i-direction (y-direction)
!  mjx_current_coarse: The most coarse domain size in j-direction (x-direction)

      integer :: lliy_current,    lljx_current,    miy_current_coarse,    mjx_current_coarse

      real :: dx, dy

!     Current 3DVAR Domain size

      miy_current=big_header_current%bhi(16,1)
      mjx_current=big_header_current%bhi(17,1)

!     Calcuclate distance of the low-left corner to its
!     mother domain's low-left corner (3DVAR domain)

      dy = 0.5+(big_header_current%bhr(10,1)-1.0) &
         * real(big_header_current%bhi(20,1))
      dx = 0.5+(big_header_current%bhr(11,1)-1.0) &
         * real(big_header_current%bhi(20,1))

!     transfer to grid number (low-left corner's grid index)

      lliy_current=int(dy) + 1
      lljx_current=int(dx) + 1

!     the coarsest domain size: 3DVAR domain

      miy_current_coarse=big_header_current%bhi(5,1)
      mjx_current_coarse=big_header_current%bhi(6,1)

!     CAA original domain size

      miy_CAA_domain=big_header_CAA_domain%bhi(16,1)
      mjx_CAA_domain=big_header_CAA_domain%bhi(17,1)

!     the coarsest domain size: CAA original domain

      miy_CAA_domain_coarse=big_header_CAA_domain%bhi(5,1)
      mjx_CAA_domain_coarse=big_header_CAA_domain%bhi(6,1)

!     Calcuclate distance of the low-left corner to its 
!     mother domain's low-left corner (CAA Domain)

      dy = 0.5+(big_header_CAA_domain%bhr(10,1)-1.0) &
         * real(big_header_CAA_domain%bhi(20,1))
      dx = 0.5+(big_header_CAA_domain%bhr(11,1)-1.0) &
         * real(big_header_CAA_domain%bhi(20,1))

!     grid numbers difference between 3DVAR and CAA domain

      lliy=(miy_current_coarse - miy_CAA_domain_coarse)/2
      lljx=(mjx_current_coarse - mjx_CAA_domain_coarse)/2

      lliy_CAA_domain=big_header_CAA_domain%bhi(20,1)*lliy+int(dy)+1
      lljx_CAA_domain=big_header_CAA_domain%bhi(20,1)*lljx+int(dx)+1

!     low-left corner index of CAA original domain in 3DVAR domain.
!     (3DVAR is extended CAA domain, &
!      so 3DVAR domain is bigger than CAA domain)

      lliy=lliy_CAA_domain - lliy_current + 1
      lljx=lljx_CAA_domain - lljx_current + 1

      write(unit=*, fmt='(2(a,i4,2x)/)') &
              'current%bhi(20,1)=', big_header_current%bhi(20,1), &
              'CAA_dmn%bhi(20,1)=', big_header_CAA_domain%bhi(20,1)

      write(unit=*, fmt='(2(a,f8.2,2x)/)') &
              'current%bhr(10,1)=', big_header_current%bhr(10,1), &
              'CAA_dmn%bhr(10,1)=', big_header_CAA_domain%bhr(10,1)

      write(unit=*, fmt='(2(a,f8.2,2x)/)') &
              'current%bhr(11,1)=', big_header_current%bhr(11,1), &
              'CAA_dmn%bhr(11,1)=', big_header_CAA_domain%bhr(11,1)

      write(unit=*, fmt='(3(2(a,i4,2x)/))') &
              'lliy_current=',lliy_current, &
              'lljx_current=',lljx_current, &
              'lliy_CAA_domain=',lliy_CAA_domain, &
              'lljx_CAA_domain=',lljx_CAA_domain, &
              'lliy=',lliy,'lljx=',lljx

   end subroutine get_lliy_lljx
