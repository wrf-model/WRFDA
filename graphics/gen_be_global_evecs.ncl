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

   works = getenv("GRAPHIC_WORKS")

   region = getenv("REGION")
   dir    = getenv("WORK_DIR")
   nrow   = stringtointeger(getenv("BE_NROW"))

   ncol = 6
   y = new((/nrow/),float)
   x = new((/5,nrow/),float)

   var1="psi"
   filename1="fort.174"

   var2="chi_u"
   filename2="fort.178"

   var3="t_u"
   filename3 = "fort.182"

   var4="rh"
   filename4 = "fort.186"

   var5="qcloud"
   filename5="fort.190"

   var6="qrain"
   filename6="fort.194"

   var7="qice"
   filename7 = "fort.198"

   var8="qsnow"
   filename8 = "fort.202"

   data1=asciiread(dir+"/"+filename1,(/nrow,ncol/),"float")
   data2=asciiread(dir+"/"+filename2,(/nrow,ncol/),"float")
   data3=asciiread(dir+"/"+filename3,(/nrow,ncol/),"float")
   data4=asciiread(dir+"/"+filename4,(/nrow,ncol/),"float")
   data5=asciiread(dir+"/"+filename5,(/nrow,ncol/),"float")
   data6=asciiread(dir+"/"+filename6,(/nrow,ncol/),"float")
   data7=asciiread(dir+"/"+filename7,(/nrow,ncol/),"float")
   data8=asciiread(dir+"/"+filename8,(/nrow,ncol/),"float")

   ;************************************************
   ; plotting parameters
   ;************************************************
   wks   = gsn_open_wks (works,"gen_be_global_evecs_"+region)  ; open workstation

   res                  = True                     ; plot mods desired
   res@trXMinF  =  -1.0                  ; min value on x-axis
   res@trXMaxF  =  1.0                   ; max value on x-axis
   res@trYMinF  =  1.0                   ; min value on y-axis
   res@trYMaxF  =  nrow                  ; max value on y-axis
   res@tiXAxisString   = "Eigenvector"                  ; Label for the X axis
   res@tiYAxisString   = "Model Level"             ; Label for the Y axis
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
   res@xyExplicitLegendLabels = (/"m=1","m=2","m=3","m=4","m=5"/)         ; create explicit labels
   res@pmLegendDisplayMode    = "Always"            ; turn on legend
   res@pmLegendSide           = "Bottom"               ; Change location of 
   res@pmLegendParallelPosF   = 0.15                ; move units right
   res@pmLegendOrthogonalPosF = -0.55                ; move units down
   res@pmLegendWidthF         = 0.2                 ; Change width and
   res@pmLegendHeightF        = 0.2                 ; height of legend.

   plts                      = new (8,"graphic")

   res@gsnDraw               = False            ; (a) do not draw
   res@gsnFrame              = False            ; (b) do not advance 'frame'

   y(:)   = data1(:,0)
   x(0,:) = data1(:,1)
   x(1,:) = data1(:,2)
   x(2,:) = data1(:,3)
   x(3,:) = data1(:,4)
   x(4,:) = data1(:,5)
   res@tiMainString     = var1         ; add title
   plts(0) = gsn_csm_xy (wks,x,y,res) ; create plot

   y(:)   = data2(:,0)
   x(0,:) = data2(:,1)
   x(1,:) = data2(:,2)
   x(2,:) = data2(:,3)
   x(3,:) = data2(:,4)
   x(4,:) = data2(:,5)
   res@tiMainString     = var2         ; add title
   plts(1) = gsn_csm_xy (wks,x,y,res) ; create plot

   y(:)   = data3(:,0)
   x(0,:) = data3(:,1)
   x(1,:) = data3(:,2)
   x(2,:) = data3(:,3)
   x(3,:) = data3(:,4)
   x(4,:) = data3(:,5)
   res@tiMainString     = var3         ; add title
   plts(2) = gsn_csm_xy (wks,x,y,res) ; create plot

   y(:)   = data4(:,0)
   x(0,:) = data4(:,1)
   x(1,:) = data4(:,2)
   x(2,:) = data4(:,3)
   x(3,:) = data4(:,4)
   x(4,:) = data4(:,5)
   res@tiMainString     = var4         ; add title
   plts(3) = gsn_csm_xy (wks,x,y,res) ; create plot


   y(:)   = data5(:,0)
   x(0,:) = data5(:,1)
   x(1,:) = data5(:,2)
   x(2,:) = data5(:,3)
   x(3,:) = data5(:,4)
   x(4,:) = data5(:,5)
   res@tiMainString     = var5         ; add title
   plts(4) = gsn_csm_xy (wks,x,y,res) ; create plot

   y(:)   = data6(:,0)
   x(0,:) = data6(:,1)
   x(1,:) = data6(:,2)
   x(2,:) = data6(:,3)
   x(3,:) = data6(:,4)
   x(4,:) = data6(:,5)
   res@tiMainString     = var6         ; add title
   plts(5) = gsn_csm_xy (wks,x,y,res) ; create plot

   y(:)   = data7(:,0)
   x(0,:) = data7(:,1)
   x(1,:) = data7(:,2)
   x(2,:) = data7(:,3)
   x(3,:) = data7(:,4)
   x(4,:) = data7(:,5)
   res@tiMainString     = var7         ; add title
   plts(6) = gsn_csm_xy (wks,x,y,res) ; create plot

   y(:)   = data8(:,0)
   x(0,:) = data8(:,1)
   x(1,:) = data8(:,2)
   x(2,:) = data8(:,3)
   x(3,:) = data8(:,4)
   x(4,:) = data8(:,5)
   res@tiMainString     = var8         ; add title
   plts(7) = gsn_csm_xy (wks,x,y,res) ; create plot

   ;************************************************
   ;************************************************
   ; create panel: panel plots have their own set of resources
   ;************************************************
    resP1                  = True                   ; modify the panel plot
   ;  resP1@txString         = directory
   ; resP1@gsnMaximize      = True                   ; maximize panel area
   resP1@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts(0:3),(/2,2/),resP1)               ; now draw as one plot

    resP2                  = True                   ; modify the panel plot
   ;  resP2@txString         = directory
   ; resP2@gsnMaximize      = True                   ; maximize panel area
   resP2@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
   gsn_panel(wks,plts(4:7),(/2,2/),resP2)               ; now draw as one plot

end
