; ***********************************************
; xy_1.ncl
; ***********************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
;************************************************
begin
;************************************************
; read in data
;************************************************

 domain = "con200"
 nrow = 27
 resolution = 200.0

 domain = "t4b"
 nrow = 41
 resolution = 15.0

 dir = "/ocotillo1/dmbarker/data/"+domain+"/noobs/gen_be/"

 ncol = 3

 var1="psi"
 filename=var1+"/sl_print."+var1
 data1=asciiread(dir+filename,(/nrow,ncol/),"float")
 var2="chi_u"
 filename=var2+"/sl_print."+var2
 data2=asciiread(dir+filename,(/nrow,ncol/),"float")
 var3="t_u"
 filename=var3+"/sl_print."+var3
 data3=asciiread(dir+filename,(/nrow,ncol/),"float")
 var4="rh"
 filename=var4+"/sl_print."+var4
 data4=asciiread(dir+filename,(/nrow,ncol/),"float")

 y1 = new((/nrow/),float)
 y2 = new((/nrow/),float)
 y3 = new((/nrow/),float)
 y4 = new((/nrow/),float)
 x  = new((/nrow/),float)

 x = ispan(1,nrow,1)
 y1(:) = resolution * data1(:,2)
 y2(:) = resolution * data2(:,2)
 y3(:) = resolution * data3(:,2)
 y4(:) = resolution * data4(:,2)

;************************************************
; plotting parameters
;************************************************
 wks   = gsn_open_wks ("pdf","lengthscales."+domain)  ; open workstation

 res                  = True                     ; plot mods desired
 res@trXMinF  =  1.0                  ; min value on x-axis
 res@trXMaxF  =  nrow                 ; max value on x-axis
 res@trYMinF  =  0.0                   ; min value on y-axis
 res@tiXAxisString   = "Vertical Mode"                  ; Label for the X axis
 res@tiYAxisString   = "Lengthscale (km)"             ; Label for the Y axis
 res@xyLineThicknesses = (/4.0,4.0,4.0,4.0/)             ; make 2nd lines thicker
 res@xyLineColors      = (/"black","red","blue","green","purple"/)          ; change line color
 res@xyDashPatterns    = (/0,1,2,3,4/)                         ; choose dash patterns
 res@tiMainFont      = "Helvetica"               ; Font for title
 res@tiXAxisFont     = "Helvetica"               ; Font for X axis label
 res@tiYAxisFont     = "Helvetica"               ; Font for Y axis label
 res@xyMarkLineModes = (/"MarkLines","MarkLines","MarkLines","MarkLines","MarkLines","MarkLines"/)
 res@xyMarkers       = (/1,2,3,4,5/)                 ; (none, dot, asterisk)
 res@xyMarkerColors   = res@xyLineColors         ; Marker colors
 res@xyMarkerSizeF   = 0.02                      ; Marker size (default is 0.01)
 res@tiXAxisFontHeightF     = 0.03               ; Change the font size.
 res@tiYAxisFontHeightF     = 0.03
; res@xyLabelMode            = "Custom"    ; Label XY curves.
 res@xyLineLabelFontHeightF = 0.03        ; Font size and color
 res@xyLineLabelFontColor   = 2           ; for line labels

 res@lgPerimOn              = False               ; turn off box around
 res@lgLabelFontHeightF     = .02                 ; label font height
 res@xyExplicitLegendLabels = (/"","100km","50km","m=5"/)         ; create explicit labels
 res@pmLegendDisplayMode    = "Always"            ; turn on legend
 res@pmLegendSide           = "Bottom"               ; Change location of 
 res@pmLegendParallelPosF   = 0.75                ; move units right
 res@pmLegendOrthogonalPosF = -1.2                 ; move units down
 res@pmLegendWidthF         = 0.2                 ; Change width and
 res@pmLegendHeightF        = 0.2                 ; height of legend.

 plts                      = new (4,"graphic")

 res@gsnDraw               = False            ; (a) do not draw
 res@gsnFrame              = False            ; (b) do not advance 'frame'

 res@tiMainString     = var1         ; add title
 res@trYMaxF  =  max(y1)           ; max value on y-axis
 plts(0) = gsn_csm_xy (wks,x,y1,res) ; create plot

 res@tiMainString     = var2         ; add title
 res@trYMaxF  =  max(y2)           ; max value on y-axis
 plts(1) = gsn_csm_xy (wks,x,y2,res) ; create plot

 res@tiMainString     = var3         ; add title
 res@trYMaxF  =  max(y3)           ; max value on y-axis
 plts(2) = gsn_csm_xy (wks,x,y3,res) ; create plot

 res@tiMainString     = var4         ; add title
 res@trYMaxF  =  max(y4)           ; max value on y-axis
 plts(3) = gsn_csm_xy (wks,x,y4,res) ; create plot

;************************************************
; create panel: panel plots have their own set of resources
;************************************************
  resP                  = True                   ; modify the panel plot
;  resP@txString         = directory
 ; resP@gsnMaximize      = True                   ; maximize panel area
  resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
  gsn_panel(wks,plts,(/2,2/),resP)               ; now draw as one plot

end
