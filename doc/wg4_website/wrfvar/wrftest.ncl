;*************************************************
; WRF: panel three different variables at the same time step
;************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "~/ncl/WRF_contributed.ncl.test"     ; copied on 4/5/4 from /fs/cgd/data0/shea/nclGSUN. 

begin
;************************************************
; open file and read in data
;************************************************
  first_guess     = addfile("wrf_3dvar_input.nc", "r")
  analysis        = addfile("wrf_3dvar_output.nc", "r")

;************************************************
; Read vertical coordinate for plot labels
;************************************************
  znu   = first_guess->ZNU(0,:)                          ; (Time, bottom_top)
;************************************************
; Read fields
;************************************************
  fg = first_guess->T                  ; (Time, bottom_top, south_north, west_east)
  an = analysis->T                     ; (Time, bottom_top, south_north, west_east)
  df = an
  df = an - fg

;************************************************
; create plots  
;************************************************
  wks = gsn_open_wks(works ,"wrfvar_anlincs")           ; ps,pdf,x11,ncgm,eps
  gsn_define_colormap(wks ,"BlAqGrYeOrReVi200"); choose colormap

  res                       = True             ; plot mods desired
;;res@gsnMaximize           = True             ; uncomment to maximize size
  res@gsnSpreadColors       = True             ; use full range of colormap
  res@cnFillOn              = True             ; color plot desired
  res@cnLinesOn             = False            ; turn off contour lines
  res@cnLineLabelsOn        = False            ; turn off contour labels
  res@lbLabelAutoStride     = True             ; let NCL figure lb stride
  WRF_map_c(first_guess,res,0)                           ; set map resources    

;************************************************
; allocate array for 6 plots
;************************************************
  plts                      = new (2,"graphic")   
;************************************************
; Specify (arbitrarily chosen) subscripts 
; This could also be done in a do loop or explicitly specified
;************************************************
  nt                        = 0                ; last time step
  kl                        = 3      
;************************************************
; Tell NCL not to draw or advance frame for individual plots
;************************************************
  res@gsnDraw               = False            ; (a) do not draw 
  res@gsnFrame              = False            ; (b) do not advance 'frame'

  res@gsnLeftString         = "First Guess (FG)"
  plts(0)                   = gsn_csm_contour_map(wks,fg(nt,kl,:,:),res)

  res@gsnLeftString         = "FG - Analysis"
  plts(1)                   = gsn_csm_contour_map(wks,df(nt,kl,:,:),res)

;************************************************
; create panel: panel plots have their own set of resources
;************************************************
  resP                  = True                   ; modify the panel plot
  resP@txString         = fg@description+": znu="+znu(kl)
  resP@gsnMaximize      = True                   ; maximize panel area
  resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level  
  gsn_panel(wks,plts,(/1,2/),resP)               ; now draw as one plot

end

