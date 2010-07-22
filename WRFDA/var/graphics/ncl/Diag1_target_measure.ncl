load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

begin

 ad    = addfile("ad.nc","rw")   ; Open input file "ad"
 xa    = addfile("xa.nc","r")   ; Open input file "Xa"
 xb    = addfile("xb.nc","r")   ; Open input file "Xb"
 xaf   = addfile("xaf.nc","r")  ; Open input file "Xaf"
 xbf   = addfile("xbf.nc","r")  ; Open input file "Xbf"
 xt    = addfile("xt.nc","r")   ; Open input file "Xt"

 ist = istart
 jst = jstart
 ied = iend
 jed = jend
 kst = kstart
 ked = kend
 
 delta_e_nl = 0.d
 vNames = (/ "U", "V", "T","P" /) 
 nNames = dimsizes (vNames)   ; Number of variables on the file
 do n=0, nNames-1             ; Loop through each variable
    vxbfxt = xbf->$vNames(n)$ - xt->$vNames(n)$  
    vxafxt = xaf->$vNames(n)$ - xt->$vNames(n)$ 
;    gName = "G_"+vNames(n) 
;    vxbfxt = xbfxt->$gName$ 
;    vxafxt = xafxt->$gName$  

    scal = 1.
    if ( vNames(n) .eq. "T" ) then
       scal = (9.8/3)^2   ;(g/NT)**2, g=9.8m/s^2; N=10^-2 S^-2, T=300K
    end if
    if ( vNames(n) .eq. "P" ) then
       scal = (1./300.)^2 ;(1/RHO*Cs)**2, RHO=1kg/m^3, Cs=300m/s
    end if
    
;    delta_e_nl = delta_e_nl + scal * (sum(vxafxt^2) - sum(vxbfxt^2))
    do k = kst-1, ked-1
       do j = jst-1, jed-1
          do i = ist-1, ied-1
             delta_e_nl = delta_e_nl + scal * ( vxafxt(0,k,j,i)^2 - vxbfxt(0,k,j,i)^2 ) 
          end do
       end do
    end do
 
    nlName = "NL_"+vNames(n) 
    ad->$nlName$ = scal * (vxafxt^2 - vxbfxt^2)
    
    delete(vxbfxt)
    delete(vxafxt)
 end do
 delete(vNames)

 delta_e_tl = 0.d
 vNames = (/ "U", "V", "T", "PH", "QVAPOR", "MU" /) 
 nNames = dimsizes (vNames)   ; Number of variables on the file
 do n=0, nNames-1             ; Loop through each variable
    vxa    = xa->$vNames(n)$  ; Read the variable to memory
    vxb    = xb->$vNames(n)$        
    aName  = "A_"+vNames(n) 
    vad    = ad->$aName$       

    delta_e_tl = delta_e_tl + sum((vxa - vxb) * vad)
    print("delta_e_tl "+vNames(n)+" :"+ sum((vxa - vxb) * vad) * 1E-5)
 
    tlName = "TL_"+vNames(n)
    aiName = "AI_"+vNames(n)
    ad->$tlName$ = (vxa - vxb) * vad
    ad->$aiName$ = vxa - vxb
    
    delete(vxa)
    delete(vxb)
    delete(vad)
 end do

 print ("---> Non-Linear Forecast Reduction = "+delta_e_nl * 1E-5)
 print ("---> Linear estimate               = "+delta_e_tl * 1E-5)
 print ("---> Percentage of Error           = "+ 100.0*(delta_e_nl-delta_e_tl)/delta_e_nl)
 end
