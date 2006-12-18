subroutine da_obs_diagnostics( num_sound, ob, iv, re )

   implicit none

   integer, intent(in)                    :: num_sound
   type (residual_sound_type), intent(in) :: ob(:)
   type (sound_type), intent(in)          :: iv(:)
   type (residual_sound_type), intent(in) :: re(:)
   
   integer                                :: unit
   integer                                :: n, k

   unit = 35

   do n = 1, num_sound
      do k = 1, iv(n) % info % levels
         if ( iv(n) % u(k) % qc >= obs_qc_pointer ) &
         write(unit,'(a5,2f9.3,5f17.7,i8)')iv(n) % info % id, &   ! Station
                                           iv(n) % info % lat, &  ! Latitude
                                           iv(n) % info % lon, &  ! Longitude
                                           iv(n) % p(k), &        ! Obs Pressure
                                           ob(n) % u(k), &        ! O
                                           iv(n) % u(k) % inv, &  ! O-B
                                           re(n) % u(k), &        ! O-A
                                           iv(n) % u(k) % error, &! Obs error
                                           iv(n) % u(k) % qc      ! QC flag

         if ( iv(n) % v(k) % qc >= obs_qc_pointer ) &
         write(unit+1,'(a5,2f9.3,5f17.7,i8)')iv(n) % info % id, & ! Station
                                           iv(n) % info % lat, &  ! Latitude
                                           iv(n) % info % lon, &  ! Longitude
                                           iv(n) % h(k), &        ! Obs Pressure
                                           ob(n) % v(k), &        ! O
                                           iv(n) % v(k) % inv, &  ! O-B
                                           re(n) % v(k), &        ! O-A
                                           iv(n) % v(k) % error, &! Obs error
                                           iv(n) % v(k) % qc      ! QC flag

         if ( iv(n) % t(k) % qc >= obs_qc_pointer ) &
         write(unit+2,'(a5,2f9.3,5f17.7,i8)')iv(n) % info % id, & ! Station
                                           iv(n) % info % lat, &  ! Latitude
                                           iv(n) % info % lon, &  ! Longitude
                                           iv(n) % h(k), &        ! Obs Pressure
                                           ob(n) % t(k), &        ! O
                                           iv(n) % t(k) % inv, &  ! O-B
                                           re(n) % t(k), &        ! O-A
                                           iv(n) % t(k) % error, &! Obs error
                                           iv(n) % t(k) % qc      ! QC flag
                                           
         if ( iv(n) % q(k) % qc >= obs_qc_pointer ) &
         write(unit+4,'(a5,2f9.3,5f17.7,i8)')iv(n) % info % id, & ! Station
                                           iv(n) % info % lat, &  ! Latitude
                                           iv(n) % info % lon, &  ! Longitude
                                           iv(n) % h(k), &        ! Obs Pressure
                                           ob(n) % q(k), &        ! O
                                           iv(n) % q(k) % inv, &  ! O-B (kg/kg)
                                           re(n) % q(k), &        ! O-A
                                           iv(n) % q(k) % error, &! Obs error (kg/kg)
                                           iv(n) % q(k) % qc      ! QC flag               
      end do
   end do
      
!  End of file markers:
   write(unit,  '(a5,2f9.3,5f17.7,i8)')'00000',0.0,0.0,0.0,0.0,0.0,0.0,0.0,0
   write(unit+1,'(a5,2f9.3,5f17.7,i8)')'00000',0.0,0.0,0.0,0.0,0.0,0.0,0.0,0
   write(unit+2,'(a5,2f9.3,5f17.7,i8)')'00000',0.0,0.0,0.0,0.0,0.0,0.0,0.0,0
   write(unit+3,'(a5,2f9.3,5f17.7,i8)')'00000',0.0,0.0,0.0,0.0,0.0,0.0,0.0,0

end subroutine da_obs_diagnostics

