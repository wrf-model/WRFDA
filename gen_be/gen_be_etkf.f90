program gen_be_etkf
!
!---------------------------------------------------------------------- 
!  Purpose : Perform an Ensemble Transformn Kalman Filter (ETKF) rescaling
!  of WRF ensemble forecast data.
!
!  Owner: Dale Barker (NCAR/MMM) - WRF wrappper, Xuguang Wang (NOAA) - ETKF algorithm.
!  Please acknowledge author/institute in work that uses this code.
!
!----------------------------------------------------------------------

#ifdef crayx1
#define iargc ipxfargc
#endif

   use da_control
   use da_gen_be
   use da_etkf
   use da_tracing

   implicit none

   integer, parameter    :: nv3d = 4                  ! #3D variables (u, v, T, q).
   integer, parameter    :: nv2d = 1                  ! #2D variables (ps).
   integer, parameter    :: unit = 100                ! Unit number.

   character (len=200)   :: filestub                  ! General filename stub.
   character (len=200)   :: input_file                ! Input file. 
   character (len=200)   :: output_file               ! Output file. 
   character (len=10)    :: var                       ! Variable to search for.
   character (len=3)     :: ce                        ! Member index -> character.

   integer               :: num_members               ! Ensemble size.
   integer               :: num_obs                   ! Number of observations.
   integer               :: naccumt1                  ! Number of previous cycles.
   integer               :: naccumt2                  ! Number of previous cycles.
   integer               :: nstartaccum1              ! Cycle from which naccumt1 cycle starts.
   integer               :: nstartaccum2              ! Cycle from which naccumt2 cycle starts.
   integer               :: nout                      ! Output record for inn. vec./ob. error var.
   integer               :: ni                        ! 1st dimension size.
   integer               :: niu                       ! 1st dimension size (u)
   integer               :: nj                        ! 2nd dimension size.
   integer               :: njv                       ! 2nd dimension size (v)
   integer               :: nk                        ! 3rd dimension size.
   integer               :: nv                        ! Total number of 2D/3D variables.
   integer               :: nij, nijk, nijkv          ! Dimensions.
   integer               :: o, i, j, k, v             ! Loop counters.
   integer               :: member, ijkv              ! Loop counters.
   integer               :: index                     ! Array index.
   integer               :: xend 
   real                  :: num_members_inv           ! 1 / ensemble size.
   real                  :: tainflatinput             ! Pre-specified inflation, if not using adaptive inflation.
   real                  :: rhoinput                  ! Pre-specified inflation, if not using adaptive rho factor.
   real                  :: ds                        ! Grid resolution (m).
   real                  :: x_mean                    ! Ensemble mean.

   character(len=10),pointer:: varr(:)                ! Variable name.
   integer, pointer      :: nkk(:)                    ! Vertical dimension of field.
   integer, pointer      :: xstart(:)                 ! Starting position of variable..
   real, pointer         :: xf(:,:)                   ! Ensemble perturbations.
   real, pointer         :: xf_stdv(:)                ! Ensemble perturbation standard deviation.
   real, pointer         :: y(:,:)                    ! H(xf).
   real, pointer         :: sigma_o2(:)               ! Ob error variance.
   real, pointer         :: yo(:)                     ! Observation.
   real, pointer         :: uc(:,:)                   ! u-wind (C grid).
   real, pointer         :: vc(:,:)                   ! v-wind (C grid).
   real, pointer         :: field(:,:)                ! Input field.
   real, pointer         :: dummy(:,:)                ! Dummy.
 
   namelist / gen_be_etkf_nl / filestub, num_members, &
                               naccumt1, naccumt2, nstartaccum1, nstartaccum2, &
                               tainflatinput, rhoinput, nout

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [1] Initialize information.'
!---------------------------------------------------------------------------------------------

   if (trace_use) call da_trace_init
   if (trace_use) call da_trace_entry("gen_be_etkf")

   filestub = 'test'
   num_members = 56
   naccumt1 = 0
   naccumt2 = 0
   nstartaccum1 = 0
   nstartaccum2 = 0
   tainflatinput = 0.0
   rhoinput = 0.0
   nout = 1

   open(unit=unit, file='gen_be_etkf_nl.nl', &
        form='formatted', status='old', action='read')
   read(unit, gen_be_etkf_nl)
   close(unit)

   write(6,'(a,a)')'   Filestub = ', trim(filestub)
   write(6,'(a,i4)')'   Number of ensemble members = ', num_members
   write(6,'(a,i4)')'   naccumt1 = ', naccumt1
   write(6,'(a,i4)')'   naccumt2 = ', naccumt2
   write(6,'(a,i4)')'   nstartaccum1 = ', nstartaccum1
   write(6,'(a,i4)')'   nstartaccum2 = ', nstartaccum2
   write(6,'(a,f15.5)')'   tainflatinput = ', tainflatinput
   write(6,'(a,f15.5)')'   rhoinput = ', rhoinput

   num_members_inv = 1.0 / real(num_members)

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [2] Set up data dimensions and allocate arrays:' 
!---------------------------------------------------------------------------------------------

!  Get grid dimensions from first T field:
   input_file = trim(filestub)//'.e001'
   var = 'T'
   call da_stage0_initialize( input_file, var, ni, nj, nk, ds )
   niu = ni+1 ! u i dimension is 1 larger.
   njv = nj+1 ! v j dimension is 1 larger.
   nij = ni * nj
   nijk = nij * nk
   nijkv = nv3d * nijk + nv2d * nij ! #3D + #2D variables.
   nv = nv3d + nv2d
print *, ni, nj, nk, nv, nij, nijk, nv3d, nv2d, nijkv
!  Allocate arrays:
   allocate ( varr(1:nv) )
   allocate ( nkk(1:nv) )
   allocate ( xstart(1:nv) )
   allocate( xf(1:nijkv,1:num_members) )
   allocate( xf_stdv(1:nijkv) )

   do v = 1, nv3d
      nkk(v) = nk
   end do
   do v = nv3d + 1, nv
      nkk(v) = 1 
   end do
   allocate( field(1:ni,1:nj) )
   allocate( uc(1:niu,1:nj) )
   allocate( vc(1:ni,1:njv) )
   allocate( dummy(1:ni,1:nj) )

!  Hardwired:
   varr(1) = 'U'
   varr(2) = 'V'
   varr(3) = 'T'
   varr(4) = 'QVAPOR'
   varr(5) = 'PSFC'

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [3] Read observation information.'
!---------------------------------------------------------------------------------------------

   do member = 1, num_members

      write(unit=ce,FMT='(i3.3)')member
      input_file = 'ob.e'//ce  
      open(unit, file = input_file, status='old')
      read(unit,*)num_obs

      if ( member == 1 ) then
         write(6,'(a,i10)')'   Number of observations = ', num_obs
         allocate( y(1:num_obs,1:num_members) )
         allocate( sigma_o2(1:num_obs) )
         allocate( yo(1:num_obs) )
      end if

      do o = 1, num_obs
         read(unit,'(3f17.7)')y(o,member), sigma_o2(o), yo(o)
!        Convert yo-H(xb) to H(xb):
         y(o,member) = yo(o) - y(o,member)
      end do
   end do

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [4] Extract necessary fields from WRF ensemble forecasts.'
!---------------------------------------------------------------------------------------------

   xstart(1) = 1
   do v = 2, nv
      xstart(v) = xstart(v-1) + ( ni * nj * nkk(v-1) )
   end do

   do member = 1, num_members

      write(UNIT=ce,FMT='(i3.3)')member
      input_file = trim(filestub)//'.e'//ce  

      do v = 1, nv
         do k = 1, nkk(v)

            if ( varr(v) == 'U' ) then
               call da_get_field( input_file, varr(v), 3, niu, nj, nk, k, uc )
               do j = 1, nj
                  do i = 1, ni
                     field(i,j) = 0.5 * ( uc(i,j) + uc(i+1,j) )
                  end do
               end do
            end if

            if ( varr(v) == 'V' ) then
               call da_get_field( input_file, varr(v), 3, ni, njv, nk, k, vc )
               do j = 1, nj
                  do i = 1, ni
                     field(i,j) = 0.5 * ( vc(i,j) + vc(i,j+1) )
                  end do
               end do
            end if

            if ( varr(v) == "T" ) then
!              Read theta, and convert to temperature:
               call da_get_trh( input_file, ni, nj, nk, k, field, dummy )
            end if

!           Read mixing ratio, and convert to specific humidity:
            if ( varr(v) == 'QVAPOR' ) then
               call da_get_field( input_file, varr(v), 3, ni, nj, nk, k, field )
               field(:,:) = field(:,:) / ( 1.0 + field(:,:) )
            end if

!           Read surface pressure:
            if ( varr(v) == 'PSFC' ) then
               call da_get_field( input_file, varr(v), 2, ni, nj, nk, k, field )
            end if

!           Fill 4D array:
            index = xstart(v) + (k-1) * nij
            do j = 1, nj
               do i = 1, ni
                  xf(index,member) = field(i,j)
                  index = index + 1
               end do
            end do
         end do ! k
      end do ! v
   end do !member

!  Convert ensemble forecasts to perturbations:
   do ijkv = 1, nijkv
      x_mean = sum(xf(ijkv,1:num_members)) * num_members_inv
      xf(ijkv,1:num_members) = xf(ijkv,1:num_members) - x_mean
!      x_mean = sum(xf(ijkv,1:num_members)) * num_members_inv
      xf_stdv(ijkv) = sqrt(sum(xf(ijkv,1:num_members)**2)) * num_members_inv
   end do

!  Output prior ensemble standard deviation:
   do v = 1, nv
      output_file = trim(varr(v))//'/'//trim(varr(v))//'.prior.stdv'
      open(unit, file = output_file, form='unformatted')
      write(unit)ni, nj, nkk(v)
      xend = xstart(v) + nij * nkk(v) - 1
      write(unit)xf_stdv(xstart(v):xend)
      close(unit)
   end do

   deallocate( uc )
   deallocate( vc )
   deallocate( field )
   deallocate( dummy )

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [5] Call ETKF:'
!---------------------------------------------------------------------------------------------

   call da_solve_etkf( nijkv, num_members, num_obs, xf, y, sigma_o2, yo, nout, &
                       naccumt1, naccumt2, nstartaccum1, nstartaccum2, tainflatinput, rhoinput )

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [6] Calculate diagnostics:' 
!---------------------------------------------------------------------------------------------

!  Recalculate and output posterior ensemble perturbation standard deviation:
   do ijkv = 1, nijkv
      xf_stdv(ijkv) = sqrt(sum(xf(ijkv,1:num_members)**2)) * num_members_inv
   end do

   do v = 1, nv
      output_file = trim(varr(v))//'/'//trim(varr(v))//'.posterior.stdv'
      open(unit, file = output_file, form='unformatted')
      write(unit)ni, nj, nkk(v)
      xend = xstart(v) + nij * nkk(v) - 1
      write(unit)xf_stdv(xstart(v):xend)
      close(unit)
   end do

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [7] Output EnSRF analysis ensemble:'
!---------------------------------------------------------------------------------------------

   do member = 1, num_members
      write(6,'(a,i4)')'   Writing ensemble perturbations for member', member
      write(UNIT=ce,FMT='(i3.3)')member

      do v = 1, nv

!        Output prior ensemble forecasts:
         output_file = trim(varr(v))//'/'//trim(varr(v))//'.prior.e'//trim(ce)
         open(unit, file = output_file, form='unformatted')
         write(unit)ni, nj, nkk(v)
         xend = xstart(v) + nij * nkk(v) - 1
         write(unit)xf(xstart(v):xend,member)
         close(unit)
      end do
   end do

   if (trace_use) call da_trace_exit("gen_be_etkf")
   if (trace_use) call da_trace_report

end program gen_be_etkf

