;===========================================================================================;
;  verif_grid_vert_profile.ncl
; Author: Syed RH Rizvi  NCAR/ESSL/MMM
;         Hui Shao       NCAR/RAL/DATC:
; Date  : 10/12/2007
; 
; Udates:
;      Syed RH Rizvi     NCAR/ESSL/MMM         06/05/2009
;        a) Added verification for wind vector & geopotentials 
;        b) For vertical profiles, added the choice to display the desired
;           number of pressure levels 
;=============================================================;
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"   
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"   
;=============================================================;
begin
   ;=============================================================;
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
   ;----------------------------------------------------------
   
   ; Fix top_plot_level
     levels=dimsizes(plevels)
    top_level=levels
    do n = 1,levels-1          
    if( plevels(levels-n) .lt. p_top ) then     
       top_level= top_level - 1
    end if
    end do
 
   ;----------------------------------------------------------
   read_headers = asciiread(run_dir+"header_main",-1,"string")
    Title = read_headers(0)
    num_expt = stringtointeger(read_headers(1))
    pdat_dirs = (/read_headers(2 : 2+num_expt)/)
   ;----------------------------------------------------------
   num_legend=num_expt
   legend_line_cols=new((/num_legend/),"string")
   legend_line_cols(0:num_legend-1)=exp_line_cols(0:num_legend-1)

   delete(num_legend)
   num_legend=num_expt
   new_legends=new((/num_legend/),"string")
   new_legends(0:num_legend-1)=exp_legends(0:num_legend-1)
   ;----------------------------------------------------------
   num_d_scor=dimsizes(select_scores)
   ;----------------------------------------------------------
    num_obs = dimsizes(read_headers) - (num_expt + 2)
    fnames_obs = new((/num_obs/),"string")

    upper_air_file="fnames_upr"
    do iob = 1, num_obs
     fnames_obs(iob-1)= read_headers(num_expt+2+iob-1)
     if ( fnames_obs(iob-1) .eq. upper_air_file ) then
      file_names =  fnames_obs(iob-1)
     end if 
    end do
   ;----------------------------------------------------------
    varheads = asciiread(run_dir+file_names,-1,"string")
    nrow = stringtointeger(varheads(0))
    ncol = stringtointeger(varheads(1))
    varnames = (/varheads(2 : :)/)

    num_vars = dimsizes(varnames)/2
    varfiles = new((/num_vars/),"string")
    varunits = new((/num_vars/),"string")

    do iv = 1, num_vars
      varfiles(iv-1)= varnames(2*(iv-1))
      varunits(iv-1)= varnames(2*(iv-1)+1)
    end do
   ;-----------------------------------------------------------
    out_name = "Profile_"
    q_level = 8 
    if( top_level .lt. q_level ) then
    q_level = top_level - 1
    end if
    nexp = num_expt
    data = new ((/num_vars,nexp,nrow,ncol/),"float")

    do iexp = 1, nexp
    do iv = 1, num_vars
    data((iv-1),(iexp-1),:,:)=asciiread(pdat_dirs(iexp-1)+varfiles(iv-1),(/nrow,ncol/),"float")
    end do
    end do
       
   ;-----------------------------------------------------------
   do idtype =1, num_d_scor
     do ii = 1, 3
       if ( select_scores(idtype-1) .eq. diag_types(ii-1) ) then
         Type = ii
       end if
     end do
     diag_name=diag_names(Type-1)
   ;-----------------------------------------------------------
    xavg = new((/num_vars,nexp,top_level/),"float")
    ob_tot = new((/num_vars,top_level/),"float")
    data@_FillValue = Fill_ValueF
   
   do iv = 1, num_vars
     var=stringtocharacter(varfiles(iv-1))
     do iexp = 1, nexp
       do ilev = 1 , top_level
        if (var(0:5) .eq. "QVAPOR") then
        xavg((iv-1),(iexp-1),(ilev-1)) = 1000.0*avg(data((iv-1),(iexp-1),:,(1+(ilev-1)*4+Type)))
        else
        xavg((iv-1),(iexp-1),(ilev-1)) = avg(data((iv-1),(iexp-1),:,(1+(ilev-1)*4+Type)))
        end if
       end do
     end do
    delete (var)
   end do
   data@_FillValue = -99
   do iv = 1, num_vars
      do ilev = 1 , top_level
        ob_tot((iv-1),(ilev-1)) = sum(data((iv-1),0,:,(1+(ilev-1)*4)))
      end do
   end do
   ob_tot@_FillValue = -999
   ;----------------------------------------------------------------------------------------
    wks   = gsn_open_wks (wksdev,plot_dir+out_name+diag_types(Type-1)+"-hr"+verify_hour)     ; open workstation
   ;----------------------------------------------------------------------------------------
    plts                      = new (num_vars,"graphic")
   
    x = new((/num_vars,nexp,top_level/),"float")
    y = new((/top_level/),"integer")
    y = ispan(0,top_level-1,1)

   do iv =1, num_vars
   ;----------------------------------------------------------------------------------------
    res          = True                              ; plot mods desired
   ;----------------------------------------------------------------------------------------
    res@xyLineThicknesses = (/2.0,2.0,2.0,2.0,4.0,4.0,4.0,4.0/)            ; make 2nd lines thicker
    res@xyLineColors      = legend_line_cols                               ; change line color
    res@xyDashPatterns    = (/0,0,0,0,2,2,3,3,4,4/)                        ; choose dash patterns
    res@xyMarkers         = (/2,2,2,2,4,4,5,5/)                            ; (none, dot, asterisk)
    res@xyMarkerColors    = res@xyLineColors             ; Marker colors
    res@xyMarkerSizeF          = 0.01                    ; Marker size (default is 0.01)
    res@xyLineLabelFontHeightF = 0.02                    ; Font size and color
    res@xyLineLabelFontColor   = 2                       ; for line labels
   
    res@tmXTBorderOn           = True                    ; set up the top border
    res@tmXTOn                 = False                   ; set the top tickmark
   
   ;res@tmYRBorderOn           = False                   ; set up the Right border
   ;res@tmYRLabelsOn           = True    
    res@tmYROn                 = False                   ; set the Right tickmark
    res@tmYLMode               = "Explicit"
    res@tmLabelAutoStride      = True 
    res@tmYUseLeft             = False

    res@tiXAxisFont            = "Times-roman"           ; Font for X axis label
    res@tiYAxisFont            = "Times-roman"           ; Font for Y axis label
    res@tiXAxisFontThicknessF  = 3.0
    res@tiYAxisFontThicknessF  = 3.0
    res@tiXAxisFontHeightF     = 0.03                    ; Change the font size.
    res@tiYAxisFontHeightF     = 0.03
    res@tiXAxisFontAspectF     = 1.5
    res@tiYAxisFontAspectF     = 1.5
    res@tiYAxisString   = " Pressure (hPa)"              ; Label for the X axis

    res@gsnDraw               = False                    ; (a) do not draw
    res@gsnFrame              = False                    ; (b) do not advance 'frame'
   ;--------------------------------------------------------------------
    res@tmYLValues  = y                              ; Assign y-axis labels
    res@tmYLLabels  = plevels
    res@gsnMaximize = True                           ; maximize page area

    res@tiXAxisString   = varunits(iv-1)             ; Label for the Y axis
    var=stringtocharacter(varfiles(iv-1))
    if (var(0:5)  .eq. "QVAPOR") then
     res@trYMaxF = q_level
    else
     res@trYMaxF = top_level-1
    end if
    delete (var) 
    x(iv-1,:,:) = xavg(iv-1,:,:)
    plts(iv-1) = gsn_csm_xy(wks,x(iv-1,:,:),y,res)
   
   ;----------------------------------------------
   end do
   ;--------------------------------------------------------------------
   ;  panel: panel plots have their own set of resources
   ;--------------------------------------------------------------------
     resP                  = True                   ; modify the panel plot
    resP@gsnMaximize      = True                    ; maximize panel area
    resP@gsnFrame         = False
    resP@gsnPanelRowSpec  = True                   ; specify 1 top, 2 lower level
    resP@gsnPaperOrientation = "auto"
    resP@gsnPanelXWhiteSpacePercent = 4.           ; increase spacing along Y-axis between panel plots
    resP@gsnPanelYWhiteSpacePercent = 4.           ; increase spacing along Y-axis between panel plots
    resP@lbLabelFontHeightF = 0.008                ; set font height of Label Bar labels
    resP@gsnPanelBottom   = 0.16                   ; shrink panel plot by setting bottom edge of plot
    ; resP@gsnPanelTop      = 1.0                    ; shrink panel plot by setting top edge of plot

    resP@txString   = diag_name+"  "+Title
   ;-------------------------------------------------------------------------------------
   ;  Do paneling
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
     gsn_panel(wks,plts,(/3,3/),resP)       ; now draw as one plot
    end if
   ;-------------------------------------------------------------------------------------


   ;-------------------------------------------------------------------------------------
   legend = create "Legend" legendClass wks
    "vpXF"                     : 0.20                    ; position on virtual page
    "vpYF"                     : 0.15
    "vpWidthF"                 : 0.6                     ; width
    "vpHeightF"                : 0.1                     ; height
    "lgPerimOn"                : False                   ; no perimeter
    "lgItemCount"              : nexp                    ; how many
    "lgLabelsOn"               : False                   ; no default lables
    "lgLineLabelFontHeightF"   : 0.012                   ; font height
    "lgMonoLineThickness"      : True
    "lgLineThicknessF"         : 2.0
    "lgDashIndexes"            : (/0,0,0,0,1/)             ; line paters
    "lgLineLabelStrings"       : new_legends
    "lgLineColors"             : legend_line_cols
    "lgMonoLineLabelFontColor" : True                    ; one label color
    end create

   draw(legend)
   ;-------------------------------------------------------------------------------------
   frame(wks)
   delete(wks)

   end do
   ;-----------------------
   delete(varheads)
   delete(varnames)
   delete(varfiles)
   delete(varunits)
   delete(x)
   delete(ob_tot)
   delete(y)
   delete(data)
   delete(plts)
   ;-----------------------

   end
