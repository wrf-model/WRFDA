;===========================================================================================;
;  verif_anal_time_series.ncl
; Author : Syed RH Rizvi  MMM/NCAR   Date: 12/25/2008
; Purpose: Generates desired plots for verification for
;          surface parametrs (PSfc, U10, V10, T2 & Q2)
;          desired Upper levels (U, V , T, Q, Wind vector or Geopotential)
;=========================================================;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
;===========================================================================================;
begin
   ;===========================================================================================;
   ; open file and read in data
   ;-----------------------------------------------------------
   run_dir=getenv("PWD")+"/"
   plot_dir = run_dir

   ;********************************************************************************************
   ; Data Format:  In each row total (1 + 16 x 4 = 65) values for Upper-air
   ; Data Format:  In each row total (1 + 1 x 4 = 5) values for Surface
   ;
   ; Date,  For each level triplets for NUM, BIAS Absolute BIAS & RMSE 
   ; levels:  1000 925 850 700 500 400 300 250 200 150 100 70 50 30 20 10  5  3  2  1
   ; index      1   2   3   4   5   6   7   8   9  10  11  12 13 14 15 16 17 18 19 20
   ;
   ;   Type :   1     2       3
   ;          BIAS  RMSE     ABIAS
   ;********************************************************************************************
       plevels=(/1000,925,850,700,500,400,300,250,200,150,100,70,50,30,20, 10, 5, 3, 2, 1/)
   ;********************************************************************************************
    diag_types=(/"BIAS","RMSE","ABIAS"/)	
    diag_names=(/"Bias","RMSE","Absolute Bias"/) 
    Fill_ValueF = -99.99
   ;-----------------------------------------------------------
   read_headers = asciiread(run_dir+"header_main",-1,"string")
   Xaxis_title = read_headers(0)
   num_expt = stringtointeger(read_headers(1))
   pdat_dirs = (/read_headers(2 : 2+num_expt)/)
   ;-----------------------------------------------------------
   legend_line_cols=new((/num_expt/),"string")
   legend_line_cols(0:num_expt-1)=exp_line_cols(0:num_expt-1)
   ;-----------------------------------------------------------
   num_d_scor=dimsizes(select_scores)
   ;-----------------------------------------------------------
   num_obs = dimsizes(read_headers) - (num_expt + 2)
   fnames_obs = new((/num_obs/),"string")
   do iob = 1, num_obs
   fnames_obs(iob-1)= read_headers(num_expt+2+iob-1)
   end do
   ;-----------------------------------------------------------
   do iob = 1, num_obs
   
   num_d_levs= 1
   if( iob .eq. 1) then
   num_d_levs=dimsizes(select_levs)
   end if
   ;------------------
   varheads = asciiread(run_dir+fnames_obs(iob-1),-1,"string")
   nrow = stringtointeger(varheads(0))
   ncol = stringtointeger(varheads(1))
   varnames = (/varheads(2 : :)/) 
   
   num_vars = dimsizes(varnames)/2
   if( iob .eq. 1 .and. num_vars .gt. 4 ) then
   print(" Currently Upper air variables are "+num_vars)               
   ; print(" Cannot plot more than four variables in one panel")
   ; exit
   end if
   if( iob .eq. 2 .and. num_vars .gt. 6 ) then
   print(" Currently Surface variables are "+num_vars)               
   print(" Cannot plot more than six variables in one panel")
   exit
   end if
   varfiles = new((/num_vars/),"string")
   varunits = new((/num_vars/),"string")

   do iv = 1, num_vars
     varfiles(iv-1)= varnames(2*(iv-1))
     varunits(iv-1)= varnames(2*(iv-1)+1)
   end do
   ;----------------------------------------------------------
   dfile=pdat_dirs(0)+varfiles(0)
   dates = new((/nrow/),"float")
   dates = stringtofloat(systemfunc("cut -c8-9 " + dfile)) 
   ;----------------------------------------------------------
   if( iob .eq. 1) then
    nlev = 20
    q_levels = 8
    out_name="Time_Series_UPA_"
   end if
   if( iob .eq. 2) then
    nlev = 1
    q_levels = 1
    out_name="Time_Series_SFC_"
   end if
   ;--------------------------------------------
   nexp = num_expt
   ;
   data = new ((/num_vars,nexp,nrow,ncol/),"float")

   do iexp = 1, nexp
   do iv = 1, num_vars
     data((iv-1),(iexp-1),:,:)=asciiread(pdat_dirs(iexp-1)+varfiles(iv-1),(/nrow,ncol/),"float")
   end do
   end do
   ;----------------------------------------------------------
   do idtype =1, num_d_scor  
     do ii = 1, 3
       if ( select_scores(idtype-1) .eq. diag_types(ii-1) ) then
         Type = ii
       end if
     end do
     diag_name=diag_names(Type-1)
   ;----------------------------------------------------------
   first = True
   do idlev = 1, num_d_levs 
     plevel = "SFC"
     level=1     
     if( iob .eq. 1 ) then
      do ll = 1, nlev
        if ( select_levs(idlev-1) .eq. plevels(ll-1) ) then
           ilev = ll
           level=ilev
           plevel = plevels(level-1)+" hPa  "
        end if
      end do
     end if
   ;----------------------------------------------------------
   if (first) then
     if( iob .eq. 1 ) then
     wks   = gsn_open_wks (wksdev,plot_dir+out_name+select_scores(idtype-1)+"-"+select_levs(idlev-1)+"-hr"+verify_hour)   ; open workstation
     end if
     if( iob .eq. 2 ) then
     wks   = gsn_open_wks (wksdev,plot_dir+out_name+select_scores(idtype-1)+"-hr"+verify_hour)   ; open workstation
     end if
     first = False
   end if
   ;----------------------------------------------------------
   
   ;----------------------------------------------------------
    plts                      = new (num_vars,"graphic")   ; define number of panels
    x = new((/nrow/),"integer")
    y = new((/num_vars,nexp,nrow/),"float")

    x = ispan(1,nrow,1)
   ;----------------------------------------------------------
   do iv = 1, num_vars
     var=stringtocharacter(varfiles(iv-1)) 
      do iexp = 1, nexp
          do irow = 1, nrow
          if( (iob.eq.1.and.var(0:5) .eq. "QVAPOR") .or. (iob.eq.2.and.var(0:2) .eq. "Q2M") ) then
          y(iv-1,iexp-1,irow-1) = 1000.0*data(iv-1,iexp-1,irow-1,1+(level-1)*4+Type)
          else
          y(iv-1,iexp-1,irow-1) = data(iv-1,iexp-1,irow-1,1+(level-1)*4+Type)
          end if
          end do
      end do
     delete (var)
   end do
   y@_FillValue = Fill_ValueF

   ;----------------------------------------------------------
   res          = True                              ; plot mods desired
   res@gsnDraw               = False                ; (a) do not draw
   res@gsnFrame              = False                ; (b) do not advance 'frame'
   res@xyLineThicknesses = (/1.7,1.7,1.7,1.7,1.7,1.7,1.7,1.7,1.7,1.7/)    ; make lines thicker
   res@xyLineColors      = legend_line_cols                              ; change line color
   res@xyDashPatterns    = (/0,0,0,0,0,0,0,0,0,0/)                        ; choose dash patterns
   res@xyMarkers       = (/2,2,2,2,2,2,2,2,2,2/)                          ; (none, dot, asterisk)
   res@xyMarkerColors   = res@xyLineColors          ; Marker colors
   res@xyMarkerSizeF   = 0.01                       ; Marker size (default is 0.01)
   res@xyLineLabelFontHeightF = 0.02                ; Font size and color
   res@xyLineLabelFontColor   = 2                   ; for line labels
   res@tmXTBorderOn     = True                      ; set up the top border
   res@tmXTOn           = False                     ; set the top tickmark
   res@tmYRBorderOn     = True                      ; set up the Right border
   res@tmYROn           = False                     ; set the Right tickmark
   res@tiXAxisFont     = "Times-roman"              ; Font for X axis label
   res@tiYAxisFont     = "Times-roman"              ; Font for Y axis label
   res@tiXAxisFontThicknessF  = 2.5
   res@tiYAxisFontThicknessF  = 2.5
   res@tiXAxisFontHeightF     = 0.03                ; Change the font size.
   res@tiYAxisFontHeightF     = 0.03
   res@tiXAxisFontAspectF     = 1.5
   res@tiYAxisFontAspectF     = 1.5
   res@tiXAxisString   = Xaxis_title                ; Label for the X axis
   res@gsnMaximize     = True                    ; maximize the space for the plot
  ;----------------------------------------------------------
   res@trXMaxF = nrow
   res@trXMinF = 1
    do iv = 1, num_vars
     res@tiYAxisString   = varunits(iv-1)             ; Label for the Y axis
     plts(iv-1) = gsn_csm_xy(wks,x,y(iv-1,:,:),res)
    end do
   ;--------------------------------------------------------------------------------------
   ; create panel: panel plots have their own set of resources
   ;--------------------------------------------------------------------------------------
   resP                  = True                    ; modify the panel plot
   resP@gsnMaximize      = True                    ; maximize panel area
   resP@gsnFrame         = False                   ; don't advance the frame
   resP@gsnPanelXWhiteSpacePercent = 4.	 	  ; increase spacing along Y-axis between panel plots
   resP@gsnPanelYWhiteSpacePercent = 4.	 	  ; increase spacing along Y-axis between panel plots
   resP@gsnPanelRowSpec  = False                   ; specify 1 top, 2 lower level
   resP@lbLabelFontHeightF = 0.008		  ; set font height of Label Bar labels
   resP@gsnPanelBottom   = 0.25			  ; shrink panel plot by setting bottom edge of plot
   resP@gsnPanelTop      = 1.00  		  ; shrink panel plot by setting top edge of plot
   resP@gsnPaperOrientation = "portrait"
   resP@txString   = plevel+"  "+diag_name

   ;-------------------------------------------------------------------------------------
   legend = create "Legend" legendClass wks
    "vpXF"                     : 0.20                    ; position on virtual page
    "vpYF"                     : 0.15
    "vpWidthF"                 : 0.6                     ; width
    "vpHeightF"                : 0.1                     ; height
    "lgPerimOn"                : False                   ; no perimeter
    "lgItemCount"              : (nexp)                 ; how many
    "lgLabelsOn"               : False                   ; no default lables
    "lgLineLabelFontHeightF"   : 0.012                   ; font height
    "lgMonoLineThickness"      : True
    "lgLineThicknessF"         : 1.7
    "lgDashIndexes"            : (/0,0,0,0,1/)             ; line paters
    "lgLineLabelStrings"       : exp_legends
    "lgLineColors"             : legend_line_cols
    "lgMonoLineLabelFontColor" : True                    ; one label color
   end create

   draw(legend)
   ;-------------------------------------------------------------------------------------
    if( num_vars .eq. 1) then
     gsn_panel(wks,plts,(/1,1/),resP)                ; now draw as one plot
    end if
    if( num_vars .eq. 2) then
     gsn_panel(wks,plts,(/1,2/),resP)                ; now draw as one plot
    end if 
    if( num_vars .eq. 3 .or. num_vars .eq. 4 ) then
     gsn_panel(wks,plts,(/2,2/),resP)       ; now draw as one plot
    end if
    if( num_vars .gt. 4 ) then
     gsn_panel(wks,plts,(/2,3/),resP)       ; now draw as one plot
    end if
   ;-------------------------------------------------------------------------------------
 

   frame(wks)
  
   end do                            ; level loop over
   delete(wks)
   delete(x)
   delete(y)
   end do                            ; Type loop over

   delete(varheads)
   delete(varnames)
   delete(varfiles)
   delete(varunits)
   delete(data)
   delete(plts)

   end do                            ; iob  loop over

   end
