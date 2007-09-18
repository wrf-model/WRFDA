;********************************************************
; WRF: color height-x [west-east] cross-section
;********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"

begin
;********************************************************
; open file 
;********************************************************

  first_guess     = addfile(bakfile+".nc", "r")
  analysis        = addfile(analfile+".nc", "r")

  znu  = first_guess->ZNU(0,:)
  xlatt    = first_guess->XLAT(0,:,xlon-1)
  xlatt@units = "degrees_north"
  xlatt_u    = first_guess->XLAT_U(0,:,xlon-1)
  xlatt_u@units = "degrees_north"
  xlatt_v    = first_guess->XLAT_V(0,:,xlon-1)
  xlatt_v@units = "degrees_north"

  plong = first_guess->XLONG(0,xlat-1,xlon-1)
  plat = first_guess->XLAT(0,xlat-1,xlon-1)

  title="PSOT"+" - "+var
  subtitle="Date="+date+",Lon="+plong+",Lat="+plat+",Eta = "+znu(kl-1)+",Val="+omb+",Err="+err

;********************************************************
; Read W at Time=6, south_north=0, convert to cm/sec
;********************************************************
  fg1 = first_guess->U(0,:,:,xlon-1)              ; import data + meta info
  an1 = analysis->U(0,:,:,xlon-1)             ; import data + meta info    
  df1 = an1
  df1 = an1 - fg1
  df1!0 = "lev"
  df1&lev = znu
  df1!1 = "lat"
  df1&lat = xlatt_u
  print ("min U  = "+min(df1))
  print ("max U  = "+max(df1))

  fg2 = first_guess->V(0,:,:,xlon-1)              ; import data + meta info
  an2  = analysis->V(0,:,:,xlon-1)             ; import data + meta info   
  df2 = an2
  df2 = an2 - fg2
  df2!0 = "lev"
  df2&lev = znu
  df2!1 = "lat"
  df2&lat = xlatt_v
  print ("min V  = "+min(df2))
  print ("max V  = "+max(df2))

  fg3 = first_guess->T(0,:,:,xlon-1)              ; import data + meta info
  an3  = analysis->T(0,:,:,xlon-1)             ; import data + meta info
  df3 = an3
  df3 =  an3 - fg3
  df3!0 = "lev"
  df3&lev = znu
  df3!1 = "lat"
  df3&lat = xlatt
  print ("min T  = "+min(df3))
  print ("max T  = "+max(df3)) 
 
  fg4 = first_guess->QVAPOR(0,:,:,xlon-1)              ; import data + meta info
  an4  = analysis->QVAPOR(0,:,:,xlon-1)             ; import data + meta info
  df4 = an4
  df4 = an4 - fg4
  df4 = 1000.0 * df4
  df4@units = "g/kg"
  df4!0 = "lev"
  df4&lev = znu
  df4!1 = "lat"
  df4&lat = xlatt
  print ("min QV = "+min(df4))
  print ("max QV = "+max(df4))

;********************************************************
; create plots  
;********************************************************

  plts                      = new (4,"graphic")

  wks = gsn_open_wks(works,"yz_"+expt)          ; ps,pdf,x11,ncgm,eps
  gsn_define_colormap(wks,"gui_default")           ; select color map

  res                       = True            ; plot mods desired
  res@gsnMaximize           = True            ; uncomment to maximize size

  res@cnFillOn              = True            ; turn on color
  res@gsnSpreadColors       = True            ; use entire color map
  res@lbLabelAutoStride     = True            ; let NCL determine label spacing
  res@trYReverse            = True            ; reverse y axis
  res@tmXBTickSpacingF      = 10.              ; force labeling every 5 deg lon

  res@gsnDraw               = False            ; (a) do not draw
  res@gsnFrame              = False            ; (b) do not advance 'frame'


  res@gsnLeftString         = "U"
  plts(0)                   = gsn_csm_contour(wks,df1,res)

  res@gsnLeftString         = "V"
  plts(1)                   = gsn_csm_contour(wks,df2,res)

  res@gsnLeftString         = "Theta'"
  plts(2)                   = gsn_csm_contour(wks,df3,res)

  res@gsnLeftString         = "QV"
  plts(3)                   = gsn_csm_contour(wks,df4,res)

;************************************************
; create panel: panel plots have their own set of resources
;************************************************
  resP                  = True                   ; modify the panel plot
  resP@gsnFrame         = False                  ; don't advance panel plot
  resP@txString         = title+"~C~"+" "
  resP@gsnMaximize      = True                   ; maximize panel area
  resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
  gsn_panel(wks,plts,(/2,2/),resP)               ; now draw as one plot

  txres               = True
  txres@txFontHeightF = 0.012
  gsn_text_ndc(wks,subtitle,0.5,0.95,txres)
  frame(wks)

end