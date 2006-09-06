program gen_be_ep2
!
!---------------------------------------------------------------------- 
!  Purpose : To convert WRF ensemble to format required for use as 
!  flow-dependent perturbations in WRF-Var (alpha control variable, 
!  alphacv_method = 2).
!
!  Owner: Dale Barker (NCAR/MMM)
!  Please acknowledge author/institute in work that uses this code.
!
!----------------------------------------------------------------------

#ifdef crayx1
#define iargc ipxfargc
#endif

   use da_constants
   use da_gen_be
   use da_tracing

   implicit none

   character (len=200)   :: filestub                  ! General filename stub.
   character (len=200)   :: input_file                ! Input file. 
   character (len=200)   :: output_file               ! Output file. 
   character (len=10)    :: date                      ! Character date.
   character (len=10)    :: var                       ! Variable to search for.
   character (len=3)     :: cne                       ! Ensemble size.
   character (len=3)     :: ce                        ! Member index -> character.

   integer, external     :: iargc
   integer               :: numarg
   integer               :: ne                        ! Ensemble size.
   integer               :: i, j, k, member           ! Loop counters.
   integer               :: dim1                      ! Dimensions of grid (T points).
   integer               :: dim1s                     ! Dimensions of grid (vor/psi pts).
   integer               :: dim2                      ! Dimensions of grid (T points).
   integer               :: dim2s                     ! Dimensions of grid (vor/psi pts).
   integer               :: dim3                      ! Dimensions of grid (T points).
   real                  :: member_inv                ! 1 / member.
   real                  :: ds                        ! Grid resolution.
   logical               :: remove_mean               ! Remove mean from standard fields.

   real, allocatable     :: u(:,:,:)                  ! u-wind.
   real, allocatable     :: v(:,:,:)                  ! v-wind.
   real, allocatable     :: temp(:,:,:)               ! Temperature.
   real, allocatable     :: q(:,:,:)                  ! Specific humidity.
   real, allocatable     :: psfc(:,:)                 ! Surface pressure.
   real, allocatable     :: u_store(:,:,:)            ! u-wind.
   real, allocatable     :: v_store(:,:,:)            ! v-wind.
   real, allocatable     :: temp_store(:,:,:)         ! Temperature.
   real, allocatable     :: q_store(:,:,:)            ! Specific humidity.
   real, allocatable     :: psfc_store(:,:)           ! Surface pressure.

   real, allocatable     :: utmp(:,:)                 ! u-wind.
   real, allocatable     :: vtmp(:,:)                 ! v-wind.
   real, allocatable     :: ttmp(:,:)                 ! temperature.
   real, allocatable     :: dummy(:,:)                ! dummy.

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [1] Initialize information.'
!---------------------------------------------------------------------------------------------

   if (trace_use) call da_trace_init
   if (trace_use) call da_trace_entry("gen_be_ep2")

   remove_mean = .true.

   numarg = iargc()
   if ( numarg /= 3 )then
      write(UNIT=6,FMT='(a)') &
        "Usage: gen_be_ep2 date ne <wrf_file_stub> Stop"
      stop
   end if

   call getarg( 1, date )
   call getarg( 2, cne )
   read(cne,'(i3)')ne
   call getarg( 3, filestub )

   if ( remove_mean ) then
      write(6,'(a,a)')' Computing gen_be ensemble perturbation files for date ', date
   else
      write(6,'(a,a)')' Computing gen_be ensemble forecast files for date ', date
   end if
   write(6,'(a)')' Perturbations are in MODEL SPACE (u, v, t, q, ps)'
   write(6,'(a,i4)')' Ensemble Size = ', ne
   write(6,'(a,a)')' Input filestub = ', trim(filestub)

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [2] Set up data dimensions and allocate arrays:' 
!---------------------------------------------------------------------------------------------

!  Get grid dimensions from first T field:
   var = "T"
   input_file = trim(filestub)//'.e001'
   call da_stage0_initialize( input_file, var, dim1, dim2, dim3, ds )
   dim1s = dim1+1 ! u i dimension is 1 larger.
   dim2s = dim2+1 ! v j dimension is 1 larger.

!  Allocate arrays in output fields:
   allocate( u(1:dim1,1:dim2,1:dim3) ) ! Note - interpolated to mass pts for output.
   allocate( v(1:dim1,1:dim2,1:dim3) ) ! Note - interpolated to mass pts for output.
   allocate( temp(1:dim1,1:dim2,1:dim3) )
   allocate( q(1:dim1,1:dim2,1:dim3) )
   allocate( psfc(1:dim1,1:dim2) )
   allocate( u_store(1:dim1,1:dim2,1:dim3) ) ! Note - interpolated to chi pts for output.
   allocate( v_store(1:dim1,1:dim2,1:dim3) )
   allocate( temp_store(1:dim1,1:dim2,1:dim3) )
   allocate( q_store(1:dim1,1:dim2,1:dim3) )
   allocate( psfc_store(1:dim1,1:dim2) )
   u_store = 0.0
   v_store = 0.0
   temp_store = 0.0
   q_store = 0.0
   psfc_store = 0.0

!  Temporary arrays:
   allocate( utmp(1:dim1s,1:dim2) ) ! u on Arakawa C-grid.
   allocate( vtmp(1:dim1,1:dim2s) ) ! v on Arakawa C-grid.
   allocate( ttmp(1:dim1,1:dim2) )
   allocate( dummy(1:dim1,1:dim2) )

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [3] Extract necessary fields from input NETCDF files and output.'
!---------------------------------------------------------------------------------------------

   do member = 1, ne

      write(UNIT=ce,FMT='(i3.3)')member
      input_file = trim(filestub)//'.e'//ce  

      do k = 1, dim3

         ! Read u, v:
         var = "U"
         call da_get_field( input_file, var, 3, dim1s, dim2, dim3, k, utmp )
         var = "V"
         call da_get_field( input_file, var, 3, dim1, dim2s, dim3, k, vtmp )

!        Interpolate u to mass pts:
         do j = 1, dim2
            do i = 1, dim1
               u(i,j,k) = 0.5 * ( utmp(i,j) + utmp(i+1,j) )
               v(i,j,k) = 0.5 * ( vtmp(i,j) + vtmp(i,j+1) )
            end do
         end do

!        Read theta, and convert to temperature:
         call da_get_trh( input_file, dim1, dim2, dim3, k, ttmp, dummy )
         temp(:,:,k) = ttmp(:,:)

!        Read mixing ratio, and convert to specific humidity:
         var = "QVAPOR"
         call da_get_field( input_file, var, 3, dim1, dim2, dim3, k, dummy )
         q(:,:,k) = dummy(:,:) / ( 1.0 + dummy(:,:) )

      end do

!     Finally, extract surface pressure:
      var = "PSFC"
      call da_get_field( input_file, var, 2, dim1, dim2, dim3, 1, psfc )

!     Write out ensemble forecasts for this member:
      output_file = 'tmp.e'//ce  
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)date, dim1, dim2, dim3
      write(gen_be_ounit)u
      write(gen_be_ounit)v
      write(gen_be_ounit)temp
      write(gen_be_ounit)q
      write(gen_be_ounit)psfc
      close(gen_be_ounit)

!     Optionally calculate accumulating mean:
      if ( remove_mean ) then
         member_inv = 1.0 / real(member)
         u_store = ( real( member-1 ) * u_store + u ) * member_inv
         v_store = ( real( member-1 ) * v_store + v ) * member_inv
         temp_store = ( real( member-1 ) * temp_store + temp ) * member_inv
         q_store = ( real( member-1 ) * q_store + q ) * member_inv
         psfc_store = ( real( member-1 ) * psfc_store + psfc ) * member_inv
      end if

   end do

   deallocate( utmp )
   deallocate( vtmp )
   deallocate( ttmp )
   deallocate( dummy )

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [4] Compute perturbations and output' 
!---------------------------------------------------------------------------------------------

   if ( remove_mean ) then
      write(6,'(a)') "    Calculate ensemble perturbations"
   else
      write(6,'(a)') "    WARNING: Not removing ensemble mean (outputs are full-fields)"
   end if

   do member = 1, ne
      write(UNIT=ce,FMT='(i3.3)')member

!     Re-read ensemble member standard fields:
      input_file = 'tmp.e'//ce  
      open (gen_be_iunit, file = input_file, form='unformatted')
      read(gen_be_iunit)date, dim1, dim2, dim3
      read(gen_be_iunit)u
      read(gen_be_iunit)v
      read(gen_be_iunit)temp
      read(gen_be_iunit)q
      read(gen_be_iunit)psfc
      close(gen_be_iunit)

      if ( remove_mean ) then
         u = u - u_store
         v = v - v_store
         temp = temp - temp_store
         q = q - q_store
         psfc = psfc - psfc_store
      end if

!     Write out perturbations for this member:

      output_file = 'u/u.e'//trim(ce) ! Output u.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)u
      close(gen_be_ounit)

      output_file = 'v/v.e'//trim(ce) ! Output v.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)v
      close(gen_be_ounit)

      output_file = 't/t.e'//trim(ce) ! Output T.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)temp
      close(gen_be_ounit)

      output_file = 'q/q.e'//trim(ce) ! Output q.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit)q
      close(gen_be_ounit)

      output_file = 'ps/ps.e'//trim(ce) ! Output ps.
      open (gen_be_ounit, file = output_file, form='unformatted')
      write(gen_be_ounit)dim1, dim2, dim3
      write(gen_be_ounit).false., .false.
      write(gen_be_ounit)psfc
      close(gen_be_ounit)

   end do

   if (trace_use) call da_trace_exit("gen_be_ep2")
   if (trace_use) call da_trace_report

end program gen_be_ep2

