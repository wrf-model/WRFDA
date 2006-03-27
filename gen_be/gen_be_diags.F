program gen_be_diags

   use da_constants
   use da_gen_be

   implicit none

   character*10        :: variable                   ! Variable name
   character*3         :: be_method                  ! Be method (NMC, or ENS)   
   character*8         :: uh_method                  ! Uh_method (power, scale)
   integer             :: n_smth_sl                  ! Number of smoothing for scale-length
   character*80        :: filename                   ! Input filename.
   integer             :: nk,nk_3d                   ! Dimensions read in.
   integer             :: num_bins                   ! Number of bins (3D).
   logical             :: data_on_levels             ! False if data is projected onto EOFs.

   namelist / gen_be_diags_nl / be_method, uh_method, n_smth_sl

   be_method = 'NMC'
   n_smth_sl = 0

   open(unit=namelist_unit, file='gen_be_diags_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_diags_nl)
   close(namelist_unit)

   filename = 'gen_be.'//trim(be_method)//'.dat'
   open (ounit, file = filename, form='unformatted')
!----------------------------------------------------------------------------
   write(6,'(/a)')' [1] Gather regression coefficients.'
!----------------------------------------------------------------------------

   call da_readwrite_be_stage2( ounit, be_method, nk )

!----------------------------------------------------------------------------
   write(6,'(/a)')' [2] Gather vertical error eigenvectors, eigenvalues.'
!----------------------------------------------------------------------------

   variable = 'psi'
   call da_readwrite_be_stage3( ounit, nk, variable, be_method )

   variable = 'chi_u'
   call da_readwrite_be_stage3( ounit, nk, variable, be_method )

   variable = 't_u'
   call da_readwrite_be_stage3( ounit, nk, variable, be_method )

   variable = 'rh'
   call da_readwrite_be_stage3( ounit, nk, variable, be_method )

! To keep the dimension nk for 3d fields:
   nk_3d = nk

   if (uh_method /= 'power') then
     variable = 'ps_u'
     call da_readwrite_be_stage3( ounit,  1, variable, be_method )
   endif

!----------------------------------------------------------------------------
! yrg mods:
   if (uh_method == 'power') then
     write(6,'(/a)')' [3] Gather horizontal error power spectra.'
   else if (uh_method == 'scale') then
     write(6,'(/a)')' [3] Gather horizontal scale length.'
   endif
!----------------------------------------------------------------------------

! To assign the dimension nk for 3d fields:
   nk = nk_3d

   variable = 'psi'
   call da_readwrite_be_stage4( ounit, nk, be_method, uh_method, n_smth_sl, variable )

   variable = 'chi_u'
   call da_readwrite_be_stage4( ounit, nk, be_method, uh_method, n_smth_sl, variable )

   variable = 't_u'
   call da_readwrite_be_stage4( ounit, nk, be_method, uh_method, n_smth_sl, variable )

   variable = 'rh'
   call da_readwrite_be_stage4( ounit, nk, be_method, uh_method, n_smth_sl, variable )

   variable = 'ps_u'
   call da_readwrite_be_stage4( ounit, 1, be_method, uh_method, n_smth_sl, variable )

   close(ounit)

end program gen_be_diags
