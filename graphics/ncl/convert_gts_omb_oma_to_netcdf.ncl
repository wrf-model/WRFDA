;
; This code converts gts_omb_oma_01 output files from WRF-VAR into netCDF files.
; There is one file per observation type (specified in $obs_types, below)
;  Will only work with NCL version 5.1.1 or later
;
;         Craig Schwartz NCAR/MMM Jan 2011
;

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

begin
system("date")
;;;;;;

gts_fullname = "./gts_omb_oma_01"   ; Full path to the gts_omb_oma_file you want to read-in

obs_types = (/ "synop","metar","sound","sonde_sfc","buoy","ships","geoamv","polaramv","pilot","profiler","qscat","airep","gpsref" /) ; Names of the observation-types you want to convert


;;;;;;;; NOTHING BELOW HERE SHOULD NEED MODIFICATION (hopefully...) ;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;; FUNCTIONS BEGIN HERE ;;;;;;;;;;;

undef("pull_data")
function pull_data(nobs,field,data_locs,cdata,nlevels,ob_type,units)
   local max_levels,data,ibeg,iend,jj,j,is,ie,idx,k,ob_type,units
   begin
   max_levels = max(nlevels) ; maximum number of levels for any observation

   data = new( (/nobs,max_levels,5/),"float", -888888)   
   data@missing_value = data@_FillValue

   if ( field.eq."u" .or. field.eq."gpsref" )  then
      ibeg = (/56,73,90,98,115/)
      iend = (/72,89,97,114,131/)
   end if
   if ( field .eq. "v" )  then
      ibeg = (/132,149,166,174,191/)
      iend = (/148,165,173,190,207/)
   end if
   if ( field .eq. "t" )  then
      ibeg = (/208,225,242,250,267/)
      iend = (/224,241,249,266,283/)
   end if
   if ( field .eq. "p" )  then
      ibeg = (/284,301,318,326,343/)
      iend = (/300,317,325,342,359/)
   end if
   if ( field .eq. "q" )  then
      ibeg = (/360,377,394,402,419/)
      iend = (/376,393,401,418,435/)
   end if

   if ( field.eq."q" .and. ob_type.eq."sound" ) then
      ibeg = (/284,301,318,326,343/)
      iend = (/300,317,325,342,359/)
   end if
     
   jj = nobs - 1 ; Get counter into NCL space
   is = 0
   do j = 0,jj   ; LOOP over Number of obs
      if (nlevels(j).le.0) then
         data(j,:,:) = data@_FillValue
         continue
      else
         ie = nlevels(j) - 1   ; Subtract 1 for NCL indexing
         idx = data_locs(j)
         do k = 0,4          ; Loop over different parameters in file (i.e, omb, oma, qc)
            data(j,is:ie,k) = stringtofloat(charactertostring(cdata(idx:idx+ie,ibeg(k):iend(k))))
         end do
      end if
   end do

   data@units = units
   data!0 = "site"
   data!1 = "nlevels_max"

   return(data)

end  ; END function

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

undef("get_p_for_sound")
function get_p_for_sound(nobs,data_locs,cdata,nlevels,units)
   local max_levels,data,jj,j,is,ie,idx,units
   begin
   max_levels = max(nlevels)  ; maximum number of levels for any observation
   data = new( (/nobs,max_levels/),"float", -888888)   
   data@missing_value = data@_FillValue

   jj = nobs - 1 ; Get counter into NCL space
   is = 0
   do j = 0,jj   ; LOOP over Number of obs
      if(nlevels(j).le.0) then
         data(j,:) = data@_FillValue
         continue
      else
         ie = nlevels(j) - 1     ; Subtract 1 for NCL indexing
         idx = data_locs(j)
         data(j,is:ie) = stringtofloat(charactertostring(cdata(idx:idx+ie,39:55)))
      end if
   end do

   data@units = units
   data!0 = "site"
   data!1 = "nlevels_max"

   return(data)

end ; END function

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

undef("write_nc")
procedure write_nc( n:integer, nobs:integer, type:string, cdata:character, sdata:string)
   local udata,vdata,tdata,qdata,pdata
   begin

   ;;;; Get information regarding lines in the file containing data and number of levels/ob ;;;;;;

   iobs = n                            ; Starting line index in gts_omb_oma file where the header for the observation type is located
   ilevel_obs = new(nobs,"integer", -888888 )    ; Line numbers in gts_omb_oma containing number of levels per ob
   nlevel_obs = new(nobs,"integer", -888888 )    ; Actual Number of levels for each ob

   ilevel_obs(0) = iobs+1
   nlevel_obs(0) = stringtointeger(charactertostring(cdata(ilevel_obs(0),0:7)))

   do k = 1, nobs -1
      ilevel_obs(k) = ilevel_obs(k-1)+nlevel_obs(k-1)+1
      nlevel_obs(k) = stringtointeger(charactertostring(cdata(ilevel_obs(k),0:7)))
   end do
   
   data_locs = ilevel_obs + 1    ;Location where omb_oma data BEGINS

   max_levels = max(nlevel_obs) ; Maximum number of levels for any observation
   char_len=5                   ; For the Station IDs...allow 5 characters

   ;;;;; Define netCDF file;;;;;;;

   outname = "gts_omb_oma_"+type+".nc"     ; Define name of the netCDF file you're creating
   system("rm -f "+outname)
   fout = addfile(outname,"c")
   setfileoption(fout,"DefineMode",True)   ; Tell NCL you're defining a netCDF file
   fileAtt = True                          ; Define global attributes
   fileAtt@creation_date = systemfunc("date")
   fileattdef(fout,fileAtt)

   dimNames = (/"site","char_len","nlevels_max"/)     ; Define dimension names and sizes in file you're creating
   dimSizes = (/nobs,char_len,max_levels/)
   dimUnlim = (/False,False,False/)
   filedimdef(fout,dimNames,dimSizes,dimUnlim)

   ;;;;; Get lat/lon/station-ID data ;;;;;;;;;

   lats = new(nobs,"float", -888888 )
   lons = new(nobs,"float", -888888 )
   stid = new((/nobs,char_len/),"character")

   stid@missing_value = stringtochar("-888")

   kk = nobs - 1     ; Pull station ID, lat, lon information from the text file
   do k = 0,kk
      idx = data_locs(k)
      if(str_fields_count(sdata(idx)," ").lt.3) then
	 print("the observation at line "+idx+" has zero levels")
         lats(k)=lats@_FillValue
         lons(k)=lons@_FillValue
         stid(k,:)=stid@missing_value
      else
         lats(k)=stringtofloat(charactertostring(cdata(idx,21:29)))
         lons(k)=stringtofloat(charactertostring(cdata(idx,30:38)))
         stid(k,:)=cdata(idx,16:20)
      end if
   end do

   nlevel_obs!0 = "site"     ; Name the dimensions...need to correspond to the "dimNames" entry above...sort of...
   lats!0 = "site"
   lons!0 = "site"
   stid!0 = "site"
   stid!1 = "char_len"

   filevardef(fout,"NLEV",typeof(nlevel_obs),getvardims(nlevel_obs))   ; Declare varialbes in netCDF file and assign dimensions of the variables
   filevardef(fout,"LATS",typeof(lats),getvardims(lats))   
   filevardef(fout,"LONS",typeof(lons),getvardims(lons))
   filevardef(fout,"STID",typeof(stid),getvardims(stid))

   filevarattdef(fout,"NLEV",nlevel_obs)    ; Assign attributes to netCDF variables
   filevarattdef(fout,"LATS",lats)
   filevarattdef(fout,"LONS",lons)
   filevarattdef(fout,"STID",stid)

   fout->NLEV = (/nlevel_obs/)       ; Write data to netCDF variables without meta-data
   fout->LATS = (/lats/)
   fout->LONS = (/lons/)
   fout->STID = (/stid/)

  ;;;;;; Now get the OMB/OMA data for U,V,T,Q,P ;;;;;;;;;; 

   uvars = (/ "UOBS","UOMB","UQCF","UERR","UOMA" /)   ; Names of the variables in netCDF file
   vvars = (/ "VOBS","VOMB","VQCF","VERR","VOMA" /)
   tvars = (/ "TOBS","TOMB","TQCF","TERR","TOMA" /)
   qvars = (/ "QOBS","QOMB","QQCF","QERR","QOMA" /)
   pvars = (/ "POBS","POMB","PQCF","PERR","POMA" /)

  ;;;;;; 

   if ( type.eq."synop" .or. type.eq."metar" .or. type .eq. "ships" .or. \
        type.eq."buoy"  .or. type.eq."sonde_sfc" ) then

      udata = pull_data(nobs,"u",data_locs,cdata,nlevel_obs,type,"m/s")   ; Call function to pull data values
      vdata = pull_data(nobs,"v",data_locs,cdata,nlevel_obs,type,"m/s")
      tdata = pull_data(nobs,"t",data_locs,cdata,nlevel_obs,type,"K")
      qdata = pull_data(nobs,"q",data_locs,cdata,nlevel_obs,type,"kg/kg")
      pdata = pull_data(nobs,"p",data_locs,cdata,nlevel_obs,type,"Pa")

      do x = 0,4   ; Loop over the different parameters (e.g., omb, oma, qc)
         filevardef(fout,uvars(x),typeof(udata(:,0,x)),getvardims(udata(:,0,x)))
         filevardef(fout,vvars(x),typeof(vdata(:,0,x)),getvardims(vdata(:,0,x)))
         filevardef(fout,tvars(x),typeof(tdata(:,0,x)),getvardims(tdata(:,0,x)))
         filevardef(fout,qvars(x),typeof(qdata(:,0,x)),getvardims(qdata(:,0,x)))
         filevardef(fout,pvars(x),typeof(pdata(:,0,x)),getvardims(pdata(:,0,x)))

         filevarattdef(fout,uvars(x),udata(:,0,x))
         filevarattdef(fout,vvars(x),vdata(:,0,x))
         filevarattdef(fout,tvars(x),tdata(:,0,x))
         filevarattdef(fout,qvars(x),qdata(:,0,x))
         filevarattdef(fout,pvars(x),pdata(:,0,x))

         fout->$uvars(x)$ = (/udata(:,0,x)/)
         fout->$vvars(x)$ = (/vdata(:,0,x)/)
         fout->$qvars(x)$ = (/qdata(:,0,x)/)
         fout->$tvars(x)$ = (/tdata(:,0,x)/)
         fout->$pvars(x)$ = (/pdata(:,0,x)/)
      end do
   end if

   if ( type.eq."geoamv" .or. type.eq."polaramv" .or.   \
        type.eq."pilot"  .or. type.eq."profiler" .or.   \
        type.eq."qscat"  .or. type.eq."bogus"    .or.   \
	type.eq."airep" ) then

      udata = pull_data(nobs,"u",data_locs,cdata,nlevel_obs,type,"m/s")   ; Call function to pull data values
      vdata = pull_data(nobs,"v",data_locs,cdata,nlevel_obs,type,"m/s")

      if (type.eq."airep") then
         tdata = pull_data(nobs,"t",data_locs,cdata,nlevel_obs,type,"K")
      end if

      pres = get_p_for_sound(nobs,data_locs,cdata,nlevel_obs,"Pa")
      filevardef(fout,"PRES",typeof(pres),getvardims(pres))
      filevarattdef(fout,"PRES",pres)
      fout->PRES = (/ pres /)

      do x = 0,4   ; Loop over the different parameters (e.g., omb, oma, qc)
         filevardef(fout,uvars(x),typeof(udata(:,:,x)),getvardims(udata(:,:,x)))
         filevardef(fout,vvars(x),typeof(vdata(:,:,x)),getvardims(vdata(:,:,x)))

         filevarattdef(fout,uvars(x),udata(:,:,x))
         filevarattdef(fout,vvars(x),vdata(:,:,x))

         fout->$uvars(x)$ = (/udata(:,:,x)/)
         fout->$vvars(x)$ = (/vdata(:,:,x)/)

         if(isvar("tdata")) then
            filevardef(fout,tvars(x),typeof(tdata(:,:,x)),getvardims(tdata(:,:,x)))
            filevarattdef(fout,tvars(x),tdata(:,:,x))
            fout->$tvars(x)$ = (/tdata(:,:,x)/)
         end if

      end do
   end if

   if (type.eq."sound") then
      udata = pull_data(nobs,"u",data_locs,cdata,nlevel_obs,type,"m/s")
      vdata = pull_data(nobs,"v",data_locs,cdata,nlevel_obs,type,"m/s")
      tdata = pull_data(nobs,"t",data_locs,cdata,nlevel_obs,type,"K")
      qdata = pull_data(nobs,"q",data_locs,cdata,nlevel_obs,type,"kg/kg")

      pres = get_p_for_sound(nobs,data_locs,cdata,nlevel_obs,"Pa")
      filevardef(fout,"PRES",typeof(pres),getvardims(pres))
      filevarattdef(fout,"PRES",pres)
      fout->PRES = (/ pres /)

      do x = 0,4
         filevardef(fout,uvars(x),typeof(udata(:,:,x)),getvardims(udata(:,:,x)))
         filevardef(fout,vvars(x),typeof(vdata(:,:,x)),getvardims(vdata(:,:,x)))
         filevardef(fout,tvars(x),typeof(tdata(:,:,x)),getvardims(tdata(:,:,x)))
         filevardef(fout,qvars(x),typeof(qdata(:,:,x)),getvardims(qdata(:,:,x)))

         filevarattdef(fout,uvars(x),udata(:,:,x))
         filevarattdef(fout,vvars(x),vdata(:,:,x))
         filevarattdef(fout,tvars(x),tdata(:,:,x))
         filevarattdef(fout,qvars(x),qdata(:,:,x))

         fout->$uvars(x)$ = (/udata(:,:,x)/)
         fout->$vvars(x)$ = (/vdata(:,:,x)/)
         fout->$tvars(x)$ = (/tdata(:,:,x)/)
         fout->$qvars(x)$ = (/qdata(:,:,x)/)
      end do
   end if

   if (type.eq."gpsref") then
      gps_vars = (/ "GPSREF_OBS","GPSREF_OMB","GPSREF_QCF","GPSREF_ERR","GPSREF_OMA" /)
      gpsref_data = pull_data(nobs,"gpsref",data_locs,cdata,nlevel_obs,type,"refractivity_units")  ; Refractivity

      height = get_p_for_sound(nobs,data_locs,cdata,nlevel_obs,"m")      ; Actually pull HEIGHT for gpsref
      filevardef(fout,"HEIGHT",typeof(height(:,0)),getvardims(height(:,0)))
      filevarattdef(fout,"HEIGHT",height(:,0))
      fout->HEIGHT = (/ height(:,0) /)

      do x = 0,4
         filevardef(fout,gps_vars(x),typeof(gpsref_data(:,0,x)),getvardims(gpsref_data(:,0,x)))
         filevarattdef(fout,gps_vars(x),gpsref_data(:,0,x))
         fout->$gps_vars(x)$ = (/gpsref_data(:,0,x)/)
      end do
   end if

   setfileoption(fout,"DefineMode",False)   ; Tell NCL we're exiting netCDF-creation mode

end ; END FUNCTION

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                               ;
;            CODE REALLY BEGINS HERE            ;
;                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if ( .not. isfilepresent(gts_fullname) ) then
   print("Can not find the file "+gts_fullname)
   exit 
else
   if ( stringtointeger(systemfunc("wc -l "+gts_fullname)) .eq. 0 ) then
      print("No data found in "+gts_fullname+"...Exiting")
      exit
   else
      print("Reading data from "+gts_fullname)
      sdata  = asciiread(gts_fullname, -1, "string")    ; -1 means read all rows.
      cdata = stringtochar(sdata)   ; Convert all data to "character" variable-type
      print (dimsizes(sdata)+" lines read from file")
      system("date")
   end if

   headers = ind(str_fields_count(sdata," ").eq.2)  ; Line numbers in NCL space that are headers(ie., "synop 1806")
   num_headers = dimsizes(headers) - 1 ; Subtract 1 to get into NCL space

   do i = 0,num_headers    ; LOOP over all lines in files corresponding to a header for new ob-type
      n = headers(i)       ; Line number in gts_omb_oma file corresponding to the header in NCL space

      ob_type = str_squeeze(charactertostring(cdata(n,0:19)))   ; Observation type
      if ( any(obs_types.eq.ob_type) ) then  ; Check to make sure this is one of the obs-types we wanted to convert
         nobs = stringtointeger(charactertostring(cdata(n,20:27))) ; number of obs
         print("---- Converting "+nobs +" observations of type "+str_upper(ob_type)+" to netCDF ----")
	 if (nobs.eq.0) then
	    print("no "+ob_type+" obs")
  	 else
	    write_nc(n,nobs,ob_type,cdata,sdata)   ; Call procedure to write netCDF file for the current obs-type
  	 end if
      end if

   end do   ; END LOOP OVER HEADERS (loop over "i")

end if

;;;;;;;;;;;;;;;;;;;

system("date")
end

exit
