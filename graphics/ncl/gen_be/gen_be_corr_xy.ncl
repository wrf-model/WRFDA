;================================================;
;  conwomap_2.ncl
;================================================;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
; ================================================;
begin
;=================================================;
; open file and read in data
;=================================================;

;AFWA T4B:
 dir =  "/ocotillo1/bray/reg_test/afwa_gen_be_2006020312/run/gen_be_xlf"
 nj = 237
 nk = 41

 var1 = "chi"
 var2 = "t"

 filename1 = var1+"_u."+var1+"."+"NMC"
 filename2 = var2+"_u."+var2+"."+"NMC"

 data1=asciiread(dir+filename1,(/nk,nj/),"float")
 data2=asciiread(dir+filename2,(/nk,nj/),"float")

 avg1 = dim_sum(data1) / nj
 avg2 = dim_sum(data2) / nj

 x = new((/2,nk/),float)
 x(0,:) = 1.0 - avg1
 x(1,:) = 1.0 - avg2
; x(0,:) = 1.0 - data1(:,7)
; x(1,:) = 1.0 - data2(:,7)

 y = new((/nk/),float)
 y = ispan(1,nk,1)

;************************************************
; plotting parameters
;************************************************
 wks   = gsn_open_wks ("pdf",dir+var1+var2+"regcoeff")      ; open workstation

 res                  = True                     ; plot mods desired
 res@trXMinF  =  0.0                   ; min value on x-axis
 res@trXMaxF  =  1.0                  ; max value on x-axis
 res@trYMinF  =  1.0                   ; min value on y-axis
 res@trYMaxF  =  nk                    ; max value on y-axis

 res@tiXAxisString   = "Correlation"                 ; Label for the X axis
 res@xyLineThicknesses = (/4.0,4.0,4.0,4.0/)             ; make 2nd lines thicker
 res@xyLineColors      = (/"black","red","blue","green","purple"/)          ; change line color
; res@trYReverse        = True                       ; reverse Y-axis
 res@xyDashPatterns    = (/0,0,1,2,3,4/)                         ; choose dash patterns
 res@tiMainFont      = "Helvetica"               ; Font for title
 res@tiXAxisFont     = "Helvetica"               ; Font for X axis label
 res@tiYAxisFont     = "Helvetica"               ; Font for Y axis label
 res@xyMarkLineModes = (/"MarkLines","MarkLines","MarkLines","MarkLines","MarkLines","MarkLines"/)
 res@xyMarkers       = (/2,3,4,5/)                 ; (none, dot, asterisk)
 res@xyMarkerColors   = res@xyLineColors         ; Marker colors
 res@xyMarkerSizeF   = 0.02                      ; Marker size (default is 0.01)
 res@tiXAxisFontHeightF     = 0.02               ; Change the font size.
 res@tiYAxisFontHeightF     = 0.02
; res@xyLabelMode            = "Custom"    ; Label XY curves.
 res@xyLineLabelFontHeightF = 0.02        ; Font size and color
 res@xyLineLabelFontColor   = 2           ; for line labels

 res@lgPerimOn              = False               ; turn off box around
 res@lgLabelFontHeightF     = .015                ; label font height
 res@xyExplicitLegendLabels = (/var1,var2,"m=3","m=4","m=5"/)         ; create explicit labels
 res@pmLegendDisplayMode    = "Always"            ; turn on legend
 res@pmLegendSide           = "Bottom"               ; Change location of
 res@pmLegendParallelPosF   = 0.85                ; move units right
 res@pmLegendOrthogonalPosF = -1.15                ; move units down
 res@pmLegendWidthF         = 0.15                ; Change width and
 res@pmLegendHeightF        = 0.18                ; height of legend.

; res@tiMainString     = var         ; add title
 res@tiYAxisString   = "Model Level"              ; Label for the Y axis
 plot = gsn_csm_xy (wks,x,y,res) ; create plot

  end
