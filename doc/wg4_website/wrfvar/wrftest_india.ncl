;*************************************************
; WRF: panel three different variables at the same time step
;************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "~/ncl/wrf/WRF_contributed.ncl.test"     ; copied on 4/5/4 from /fs/cgd/data0/shea/nclGSUN. 

begin
;************************************************
; open file and read in data
;************************************************

  directory = "/ocotillo1/dmbarker/data/afwa_stats/2006020312/"
  directory = "/ocotillo1/dmbarker/data/afwa_stats/2006020400/"
  file1 = "/ocotillo1/dmbarker/data/india.30km/wrfinput_d01.2005072400"
  file2 = "/ocotillo1/dmbarker/data/india.30km/wrfvar-tutorial/2005072400/wrf_3dvar_output"
  first_guess     = addfile(file1+".nc", "r")
  analysis     = addfile(file2+".nc", "r")

;************************************************
; Read vertical coordinate for plot labels
;************************************************

  znu   = first_guess->ZNU(0,:)                          ; (Time, bottom_top)

;************************************************
; Read fields
;************************************************

  kl = 26
  var = "U"

 fg = first_guess->U ;Theta-T0
 an = analysis->U ;Theta-T0
 df = an
 df = an - fg

;************************************************
; create plots  
;************************************************
  wks = gsn_open_wks("pdf" , var)          ; ps,pdf,x11,ncgm,eps
  gsn_define_colormap(wks ,"BlAqGrYeOrReVi200"); choose colormap

  res                       = True             ; plot mods desired
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
;************************************************
; Tell NCL not to draw or advance frame for individual plots
;************************************************
 res@cnLevelSelectionMode = "ManualLevels" ; set manual contour levels

 res@gsnDraw               = False            ; (a) do not draw
 res@gsnFrame              = False            ; (b) do not advance 'frame'

 res@gsnLeftString         = "First Guess"
 res@cnMinLevelValF = -50               ; set mimimum contour level
 res@cnMaxLevelValF =   0              ; set maximum contour level
 res@cnLevelSpacingF = 2               ; set contour spacing
 plts(0)                   = gsn_csm_contour_map(wks,fg(nt,kl,:,:),res)

 res@gsnLeftString         = "Analysis Increment"
 res@cnMinLevelValF = -10               ; set mimimum contour level
 res@cnMaxLevelValF = 10               ; set maximum contour level
 res@cnLevelSpacingF = 2               ; set contour spacing
 plts(1)                   = gsn_csm_contour_map(wks,df(nt,kl,:,:),res)

;************************************************
; create panel: panel plots have their own set of resources
;************************************************
  resP                  = True                   ; modify the panel plot
  resP@txString         = file1 + var+"("+kl+")"
  resP@gsnMaximize      = True                   ; maximize panel area
  resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
  gsn_panel(wks,plts,(/2,2/),resP)               ; now draw as one plot

end

