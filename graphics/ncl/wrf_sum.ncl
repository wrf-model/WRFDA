load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

begin

 a = addfile("a.nc","rw")   ; Open input file "a"
 b = addfile("b.nc","r")    ; Open input file "b"

 lbc = 3    ; Temporary fix for bug in WRF+ adjoint near boundaries

; vNames = getfilevarnames (a) ; Get names of all variables on file
 vNames = (/ "A_T", "A_U", "A_V", "A_W", "A_QVAPOR","A_PH", "A_MU" /)
 nNames = dimsizes (vNames)   ; Number of variables on the file

 do n=0, nNames-1              ; Loop through each variable

    va = a->$vNames(n)$        ; Read the variable to memory
    vb = b->$vNames(n)$
    vc = a->$vNames(n)$

    vc = 0.
;    do j = lbc, dims(2)-lbc-1
;       do i = lbc, dims(3)-lbc-1
;          vc(0,:,j,i) = va(0,:,j,i) + vb(0,:,j,i) 
;       end do
;    end do
    vc = va + vb
    a->$vNames(n)$ = vc

    delete(va)
    delete(vb)
    delete(vc)

 end do
 
end
 
