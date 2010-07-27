load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

begin

 a = addfile("a.nc","rw")   ; Open input file "a"
 b = addfile("b.nc","r")    ; Open input file "b"

; vNames = getfilevarnames (a) ; Get names of all variables on file
 vNames = (/ "PH", "T", "U", "V", "W", "QVAPOR", "MU" /)
 nNames = dimsizes (vNames)   ; Number of variables on the file

 do n=0, nNames-1              ; Loop through each variable

    va = a->$vNames(n)$        ; Read the variable to memory
    vb = b->$vNames(n)$
    vc = a->$vNames(n)$
    
    vc = vb - va
    gName = "G_"+vNames(n) 
    a->$gName$ = vc

    delete(va)
    delete(vb)
    delete(vc)

 end do
 
end
