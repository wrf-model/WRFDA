program gen_be_diags

   use da_constants
   use da_gen_be

   implicit none

   character*10        :: variable                   ! Variable name
   character*3         :: be_method                  ! Be method (NMC, or ENS)
   character*80        :: filename                   ! Input filename.
   integer             :: nk                         ! Dimensions read in.
   integer             :: num_bins                   ! Number of bins (3D).
   logical             :: data_on_levels             ! False if data is projected onto EOFs.

   namelist / gen_be_diags_nl / be_method, data_on_levels

   be_method = 'NMC'
   data_on_levels = .false.

   open(unit=namelist_unit, file='gen_be_diags_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_diags_nl)
   close(namelist_unit)

   filename = 'gen_be.'//trim(be_method)//'.dat'
   open (ounit, file = filename, form='unformatted')

!----------------------------------------------------------------------------
!  [1] Gather regression coefficients:
!----------------------------------------------------------------------------

   call da_readwrite_be_stage2( ounit, be_method, nk )

!----------------------------------------------------------------------------
!  [2] Gather vertical error eigenvectors, eigenvalues:
!----------------------------------------------------------------------------

   variable = 'psi'
   call da_readwrite_be_stage3( ounit, nk, variable, be_method )

   variable = 'chi_u'
   call da_readwrite_be_stage3( ounit, nk, variable, be_method )

   variable = 't_u'
   call da_readwrite_be_stage3( ounit, nk, variable, be_method )

   variable = 'rh'
   call da_readwrite_be_stage3( ounit, nk, variable, be_method )

!----------------------------------------------------------------------------
!  [3] Gather horizontal error power spectra:
!----------------------------------------------------------------------------

   variable = 'psi'
   call da_readwrite_be_stage4( ounit, nk, be_method, data_on_levels, variable )

   variable = 'chi_u'
   call da_readwrite_be_stage4( ounit, nk, be_method, data_on_levels, variable )

   variable = 't_u'
   call da_readwrite_be_stage4( ounit, nk, be_method, data_on_levels, variable )

   variable = 'rh'
   call da_readwrite_be_stage4( ounit, nk, be_method, data_on_levels, variable )

   variable = 'ps_u'
   call da_readwrite_be_stage4( ounit, 1, be_method, data_on_levels, variable )

   close(ounit)

end program gen_be_diags
