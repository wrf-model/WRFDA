Module mod_tstrad

  Use parkind1, Only : jpim     ,jprb
  Implicit None
  Real(Kind=jprb), pointer :: xkbav(:,:,:)
  Real(Kind=jprb), pointer :: xkradovu(:,:)
  Real(Kind=jprb), pointer :: xkradovd(:,:)
  Real(Kind=jprb), pointer ::  xkradov1(:,:)
  Real(Kind=jprb), pointer ::  xkradov2(:,:)
  Real(Kind=jprb), pointer ::  xkbsav(:,:)

  Real(Kind=jprb), pointer ::  xkbem(:)

End Module mod_tstrad
