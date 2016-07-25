
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin
  A = addfile("./fcst.nc","rw")
  B = addfile("./xref.nc","r")

 ist = istart
 jst = jstart
 ied = iend 
 jed = jend 
 kst = kstart
 ked = kend 
 vNames = (/ "U", "V", "T","P" /)
 nNames = dimsizes (vNames)   ; Number of variables on the file

 do n=0, nNames-1              ; Loop through each variable

    va = A->$vNames(n)$        ; Read the variable to memory
    vb = B->$vNames(n)$
    vc = new(dimsizes(va),typeof(va))
    vc!0 = va!0
    vc!1 = va!1
    vc!2 = va!2
    vc!3 = va!3
    dims = dimsizes(vc)
    vc = 0.
    vc@_FillValue = 0.
    copyatt(vc,va)
    
    do k = kst-1, ked-1
       do j = jst-1, jed-1
          do i = ist-1, ied-1
             vc(0,k,j,i) = va(0,k,j,i) - vb(0,k,j,i) 
          end do
       end do
    end do
    if ( vNames(n) .eq. "T" ) then
       vc = vc * (9.8/3)^2         ;(g/NT)**2, g=9.8m/s^2; N=10^-2 S^-2, T=300K
    end if
    if ( vNames(n) .eq. "P" ) then
       vc = vc * (1./300.)^2       ;(1/RHO*Cs)**2, RHO=1kg/m^3, Cs=300m/s
    end if
    
    gName = "A_"+vNames(n) 
    A->$gName$ = vc
    delete(va)
    delete(vb)
    delete(vc)

 end do

end
