<?php
include_once("../../../../libphp/connect_db.php");

# db_connect("mysql.ucar.edu", "essl_country", "rduser");
$mysqli = db_connect("tea.mmm.ucar.edu", "wrfweb", "rduser");

if ($_POST["form"]) {
  $form = db_escape($_POST["form"], false);
  foreach ($form as $k => $v) {
    $form[$k] = trim($v);
  }
}

$form["model"] = mysql_real_escape_string($_REQUEST["model"]);

if ($form["model"] == "" ||
    $form["model"] == "NULL") {
  $form["model"] = "arw";
}

# $query = "select countryname from essl_country.country order by countryname asc";
$query = "select countryname from wrfweb.wrf_country order by countryname asc";

# $result = mysql_query($query);
$result = $mysqli->query($query);

$country_options = "<option value=\"United States\">United States</option>\n";
$citizenship_options = "<option value=\"United States\">United States</option>\n";
# while ($row = mysql_fetch_row($result)) {
while ($row = $result->fetch_array()) {
  $c = ucwords(strtolower($row[0]));
  if ($form["country"] == $c) {
    $selected = " SELECTED";
  } else {
    $selected = "";
  }
  $country_options .= "<option value=\"$c\"$selected>$c</option>\n";
  $c = ucwords(strtolower($row[0]));
  if ($form["citizenship"] == $c) {
    $selected = " SELECTED";
  } else {
    $selected = "";
  }
  $citizenship_options .= "<option value=\"$c\"$selected>$c</option>\n";
}

$affiliation_types = array("US Universities",
			   "US Government / State Agencies",
			   "US Private Companies",
			   "US Non-Profit",
			   "Non-US Universities",
			   "Non-US Governmental / Provincial Agencies",
			   "Non-US Private Companies",
			   "Non-US Non-Profit");

#$intended_usages = array("Numerical Weather Prediction",
#			 "Idealized Simulations",
#			 "Coupled Models",
#			 "Regional Climate",
#			 "Data Assimilation",
#			 "Atmospheric Chemistry / Transport / Air Pollution",
#			 "Military Purposes",
#			 "Teaching",
#			 "Non Meteorological Applications",
#			 "Other");

$intended_usages = array('WRFDA',
                         'WRFPLUS',
                         'MADIS2LITTLER',
                         'Numerical Weather Prediction',
			 'Idealized Simulations',
			 'Coupled Models',
			 'Regional Climate',
			 'Data Assimilation',
			 'Atmospheric Chemistry / Transport / Air Pollution',
			 'Military Purpose',
			 'Teaching',
			 'Non Meteorological Applications',
			 'Other');




?>
<html>
<head>
<title>WRF Data Assimilation System Download: New User Registration</title>
<meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
<meta name="KEYWORDS" content="meteorology, mesoscale, microscale, weather modeling, forecasting weather, weather prediction, predictability, cloud systems, boundary layers, surface-atmosphere interactions, convective cloud systems, atmospheric chemistry, precipitating weather systems, wildfire research, National Center for Atmospheric Research">
<meta name="DESCRIPTION" content="WRF Mesoscale Model Users homepage">
<meta name="ROBOTS" content="INDEX,FOLLOW">
<meta name="resource-type" content="document">
<meta http-equiv="expires" content="0">
<meta name="author" content="MMM Webmaster">
<meta name="copyright" content="Copyright (c) 2004 by UCAR">
<meta name="revisit-after" content="1 days">
<meta name="distribution" content="Global">
<meta name="rating" content="General">


<SCRIPT language=JavaScript src="../scripts/menu_array.js" type=text/javascript></SCRIPT>
<SCRIPT language=JavaScript src="../scripts/mmenu.js" type=text/javascript></SCRIPT>

<SCRIPT language=JavaScript src="/libjs/jquery.js" type=text/javascript></SCRIPT>
<script type="text/javascript">
<!--
var errors = new Array();
var miscerrors = new Array();
var title = new Array();
title["firstname"]              = "First Name";
title["lastname"]               = "Last Name";
title["email"]                  = "E-Mail Address";
title["affiliation"]            = "Affilication (Company Name)";
title["affiliation_type"]       = "Affilication Type";
title["city"]                   = "City";
title["country"]                = "Country Where You Work";
title["citizenship"]            = "Country of Citizenship";
title["intended_usage"]         = "Intended WRF Software Usage";
title["intended_usage_other"]   = "Intended WRF Software Usage: Other Usage";
// title["model"]                  = "Download Preference";

function is_valid_email() {

  var field_value = $("input[ @name='form[email]']").val();
  //  var field_label = "#email_label";

  //  if (field_value) {
    if ((field_value.indexOf(".") > 2) && (field_value.indexOf("@") > 0)) {

    $("#email_label").removeClass("required");
    return true;
  } else {
    $("#email_label").addClass("required");
    //    miscerrors.push(title["email"]);
    miscerrors.push("E-mail address is invalid");
    return false;
  }
}

function check_required_select(field) {
  if ($("#" + field + " option:selected").val()) {
    return true;
  } else {
    errors.push(field + ": " + title[field]);
    $("#" + field).addClass("required");
    return false;
  }
}

function check_required_text(field) {
  var field_value = $("input[ @name='form[" + field + "]']").val();
  var field_label = "#" + field + "_label";
  field_value.replace(/^\s+|\s+$/g,"");

  //  alert(field + ": " + field_value);
  if (field_value) {
    $(field_label).removeClass("required");
    return true;
  } else {
    $(field_label).addClass("required");
    errors.push(title[field]);
    return false;
  }
}

$(document).ready(function() {
  $("#submit").click(function(){
    errors.length = 0;
    miscerrors.length = 0;
    check_required_text("firstname");
    check_required_text("lastname");
    check_required_text("email");
    is_valid_email();
    check_required_text("affiliation");
    check_required_select("affiliation_type");
    check_required_text("city");
    check_required_select("country");
    check_required_select("citizenship");
    if (check_required_select("intended_usage")) {
      if ($("#intended_usage option:selected").val() == "Other") {
	check_required_text("intended_usage_other");
      }
    }
    errors_popup = "Please fill out the following required fields:\n\n";
    for ( var i=0, len=errors.length; i<len; ++i ){
      errors_popup += "   * " + errors[i] + "\n";
    }
    if (miscerrors.length > 0) {
      errors_popup += "\nPlease correct the following errors:\n";
      for ( var i=0, len=miscerrors.length; i<len; ++i ){
	errors_popup += "   * " + miscerrors[i] + "\n";
      }
    }      
    if (errors.length > 0) {
      alert(errors_popup);
      errors.length = 0;
      return false;
    } else {
      return true;
    }
  });

  $("input[ @name='form[firstname]']").blur(function(){
    check_required_text("firstname");
  });
  $("input[ @name='form[lastname]']").blur(function(){
    check_required_text("lastname");
  });
  $("input[ @name='form[email]']").blur(function(){
    check_required_text("email");
    is_valid_email();
  });
  $("input[ @name='form[affiliation]']").blur(function(){
    check_required_text("affiliation");
  });
  $("input[ @name='form[city]']").blur(function(){
    check_required_text("city");
  });
  $("input[ @name='form[intended_usage_other]']").blur(function(){
    if (check_required_select("intended_usage")) {
      check_required_text("intended_usage_other");
    }
  });

  if ($("#intended_usage").val() != 'Other') {
    //    alert($("#intended_usage").val());
    $("#intended_usage_other_span").hide();
  }

   $("#intended_usage").change(function() {
     if ($(this).val() == 'Other') {
       $("#intended_usage_other_span").slideDown('slow');
     } else {
       $("#intended_usage_other_span").slideUp();
     }
   });
});
-->
</script>


<link href="../images/wrfstyle.css" rel="stylesheet" type="text/css">

<style type="text/css">
<!--
.widthmedium6 {
 width: 300px;
 font-size: 6pt;
}
.widthmedium9 {
 width: 300px;
 font-size: 8pt;
}
.required {
 color: red;
}
-->
</style>

</head>

<body bgcolor="#FFFFFF" link="#000099" vlink="#000099" alink="#FF3300" leftmargin="0" topmargin="0" marginwidth="0" marginheight="0">
<table width="100%" border="0" cellspacing="0" cellpadding="0">
  <tr bgcolor="#000000">
    <td height="10" bgcolor="#000000"><div align="left"><img src="images/transgif.gif" width="1" height="1"></div>
    </td>
  </tr>
  <tr valign="top">
    <td height="10" bgcolor="#DDFFDD"><img src="../images/header_new_cropped.png" height="56" border="0"></td>
  </tr>
  <tr bgcolor="#FFCC33">
    <td height="6" background="../images/goldrule.jpg" bgcolor="#FFCC33"><img src="../images/goldrule.jpg" width="1" height="1"></td>
  </tr>
  <tr>
    <td height="34" bgcolor="#009933">&nbsp;</td>
  </tr>
  <tr>
    <td height="2" bgcolor="#ffcc66"><img src="../images/transgif.gif" width="1" height="1"></td>
  </tr>
</table>
<table width="100%" height="89%" border="0" cellpadding="0" cellspacing="0" class="background">
  <tr>
    <td width="100%" height="859" valign="top" bgcolor="#CBFFCD"><table style=max-width:1000px; border="0" cellpadding="0" cellspacing="0">
      <tr valign="top">
        <td width="117" height="910" bgcolor="#009933"><p>&nbsp;</p>

          <table width="117" border="0" align="center" cellpadding="0" cellspacing="0">
              <td><p align="center"><a href="../downloads.html" class="leftnavlink">Overview</a></p>
                <p align="center"><a href="get_source.html" class="leftnavlink">WRFDA</a></p>
                <p align="center"><a href="wrfplus.html" class="leftnavlink">WRFPLUS</a></p>
                <p align="center"><a href="testdata.html" class="leftnavlink">Test data</a></p>
                <p align="center"><a href="fso.html" class="leftnavlink">FSO</a></p>
                <p align="center"><a href="madis.html" class="leftnavlink">MADIS2LITTLER</a></p>
                <p align="center"><a href="tools.html" class="leftnavlink">Tools</a></p>
                <p align="center"><a href="free_data.html" class="leftnavlink">Free Input Data</a></p>
              </td>
          </table>

          <p>&nbsp;</p>          <p>&nbsp;</p>          <img src="../images/shim112.gif" width="112" height="1"></td>
          <td bgcolor="#CBFFCD"><p>&nbsp;</p>
            <p class="pagetitle">WRFDA Source Code Download: New User Registration</p>
            <p class="bodytext">
	    <!-- ################################################################ -->

<!--

	  <B>Notice (8/6/2008 10:30am):</B> The registration page is currently undergoing maintenance.  We should have it available again later today.  We regret any inconvenience this may cause!

-->

<?php
# exit; 
?>
<form method="post">

<?php
function db_escape($values, $quotes = true) {
    global $mysqli;
# print "<PRE>"; print_r($values); exit;
    if (is_array($values)) {
      foreach ($values as $key => $value) {
        $values[$key] = db_escape($value, $quotes);
      }
    } else if ($values === null) {
      $values = 'NULL';
    } else if (is_bool($values)) {
      $values = $values ? 1 : 0;
    } else if (!is_numeric($values)) {
      $values = mysqli_real_escape_string($mysqli, $values);
      if ($quotes) {
        $values = '"' . $values . '"';
      }
    }
    return $values;
}


function required($name) {
   global $form_errors;
   if ($form_errors["$name"] != "") {
     return " required";
   } else {
     return "";
   }
}
function check_form($form) {
  
  if ($form["firstname"] == "") {
    $errors["firstname"] = "First Name";
  }
  if ($form["lastname"] == "") {
    $errors["lastname"] = "Last Name";
  }
  if ($form["email"] == "") {
    $errors["email"] = "E-Mail Address";
  }
  if ($form["affiliation"] == "") {
    $errors["affiliation"] = "Affilication (Company Name)";
  }
  if ($form["affiliation_type"] == "") {
    $errors["affiliation_type"] = "Affilication Type";
  }
  if ($form["city"] == "") {
    $errors["city"] = "City";
  }
  if ($form["country"] == "") {
    $errors["country"] = "Country Where You Work";
  }
  if ($form["citizenship"] == "") {
    $errors["citizenship"] = "Country of Citizenship";
  }
  if ($form["intended_usage"] == "") {
    $errors["intended_usage"] = "Intended WRF Software Usage";
  }
  //  if ($form["model"] == "") {
  //    $errors["model"] = "Download Preference";
  //  }
  if ($form["intended_usage"] == "Other" &&
      $form["intended_usage_other"] == "") {
    $errors["intended_usage"] = "\"Other\" Intended Usage";
  }
  return $errors;
}

$form["submit"] = $_POST["submit"];


if ($form["submitted"]) {
  $form_errors = check_form($form);
}

if ($form_errors) {
  print "<p class=\"bodytext\">Please fill out the following required fields:</p>\n";
  print "<UL>\n";
  foreach ($form_errors as $form_error) {
    print "<LI class=\"detailtext\">$form_error</li>\n";
  }
  print "</UL>\n";
  $form["submit"] = "";
}

if ($form["submit"] == "") {
?>

<p class="bodytext">All fields below are required.  Information collected here is for internal use only. </p>

<p class="bodytext">
<table border="1" cellspacing="0" cellpadding="3">
  <tr>
   <?php $r = required("firstname"); ?>
	  <td align="left"><p class="detailtext<?php print $r; ?>" id="firstname_label">First Name</p></td>
	  <td><p class="detailtext" class="required"><input type="text" name="form[firstname]" size="30" maxlength="255" class="widthmedium9" value="<?php print $form["firstname"]; ?>"></p></td>
  </tr>
  <tr>
   <?php $r = required("lastname"); ?>
	  <td align="left"><p class="detailtext<?php print $r; ?>" id="lastname_label">Last Name</p></td>
	  <td><p class="detailtext"><input type="text" name="form[lastname]" size="30" maxlength="255" class="widthmedium9" value="<?php print $form["lastname"]; ?>"></p></td>
  </tr>
  <tr>
   <?php $r = required("email"); ?>
	  <td align="left"><p class="detailtext<?php print $r; ?>" id="email_label">E-Mail Address</p></td>
	  <td>
	  <p class="detailtext">
	  <input type="text" name="form[email]" size="30" maxlength="255" class="widthmedium9" value="<?php print $form["email"]; ?>">
	  <br />
	  <input type="checkbox" name="form[subscribe]" 
<?php
if ($form["submitted"] == "" || ($form["submitted"] == "yes" && $form["subscribe"] != "")) {
  print "checked";
}
?>
          > Subscribe to <a href="http://mailman.ucar.edu/mailman/listinfo/wrf-news">WRF-News</a> mailing list?	  </p>	  </td>
  </tr>
  <tr>
   <?php $r = required("affiliation"); ?>
	  <td align="left"><p class="detailtext<?php print $r; ?>" id="affiliation_label">Affiliation (Company Name)</p></td>
	  <td>
	  <p class="detailtext">
	  <input type="text" name="form[affiliation]" id="affiliation" size="30" maxlength="255" class="widthmedium9" value="<?php print $form["affiliation"]; ?>">
	  </p>	  </td>
  </tr>

  <tr>
   <?php $r = required("affiliation_type"); ?>
	  <td align="left"><p class="detailtext<?php print $r; ?>" id="affiliation_type_label">Affiliation Type</p></td>
	  <td>
	     <p class="detailtext">
	     <select name="form[affiliation_type]" id="affiliation_type" class="widthmedium9">

<?php
foreach ($affiliation_types as $affiliation_type) {
  if ($form["affiliation_type"] == $affiliation_type) {
    $selected = " SELECTED";
  } else {
    $selected = "";
  }
  print "<option value=\"$affiliation_type\"$selected>$affiliation_type</option>\n";
}

?>
	     </select>
	     </p>	  </td>
  </tr>

  <tr>
   <?php $r = required("city"); ?>
	  <td align="left"><p class="detailtext<?php print $r; ?>" id="city_label">City</p></td>
	  <td><p class="detailtext"><input type="text" name="form[city]" size="30" maxlength="255" class="widthmedium9" value="<?php print $form["city"]; ?>"></p></td>
  </tr>

  <tr>
   <?php $r = required("country"); ?>
	  <td align="left"><p class="detailtext<?php print $r; ?>" id="country_label">Country Where You Work</p></td>
	  <td>
	     <p class="detailtext">
	     <select name="form[country]" id="country" class="widthmedium9">
             <?php print $country_options; ?>
	     </select>
	     </p>	  </td>
  </tr>

  <tr>
   <?php $r = required("citizenship"); ?>
	  <td align="left"><p class="detailtext<?php print $r; ?>" id="citizenship_label">Country of Citizenship</p></td>
	  <td>
	     <p class="detailtext">
	     <select name="form[citizenship]" id="citizenship" class="widthmedium9">
             <?php print $citizenship_options; ?>
	     </select>
	     </p>	  </td>
  </tr>

  <tr>
   <?php $r = required("intended_usage"); ?>
	  <td align="left"><p class="detailtext<?php print $r; ?>">Intended WRF Software Usage</p></td>
	  <td>
	     <p class="detailtext">
	     <select name="form[intended_usage]" id="intended_usage" class="widthmedium9">
<?php
foreach ($intended_usages as $intended_usage) {
  if ($form["intended_usage"] == $intended_usage) {
    $selected = " SELECTED";
  } else {
    $selected = "";
  }
  print "<option value=\"$intended_usage\"$selected>$intended_usage</option>\n";
}

?>
	     </select>
	     </p>
	  <span id="intended_usage_other_span">
          <p class="detailtext<?php print $r; ?>" id="intended_usage_other_label">
	  Please specify other usage:<br />
	  <input type="text" name="form[intended_usage_other]" id="intended_usage_other" size="30" maxlength="255" class="widthmedium9" value="<?php print $form["intended_usage_other"]; ?>">
	  </p>
	  </span>	  </td>
  </tr>

<!--
  <tr>
   <?php $r = required("model"); ?>
	  <td align="left"><p class="detailtext<?php print $r; ?>" id="model_label">Download Preference</p></td>
	  <td><p class="detailtext">
		 <input type="radio" name="form[model]" value="arw"
<?php	     
if ($form["submitted"] != "yes"  || $form["model"] == "arw") {
    print " checked";
}
?>
	         >ARW Dynamic Core<br />
		 <input type="radio" name="form[model]" value="nmm"
<?php	     
if ($form["model"] == "nmm") {
    print " checked";
}
?>
	         >NMM Dynamic Core
	      </p>
	  </td>
  </tr>

-->

  <tr>
	  <td colspan="2" align="center">
<center>
<input name="form[submitted]" type="hidden" value="yes">
<input name="form[model]" type="hidden" value="<?php print $form["model"]; ?>">
<input name="submit" type="submit" value="Submit" style="font-size:8pt;" id="submit">
</center></td>
  </tr>
</table>

<?php
} else {
  $mysqli = db_connect("tea.mmm.ucar.edu", "wrfweb", "wtwrf");

  if ($form["intended_usage"] == "Other") {
    $form["intended_usage"] = $form["intended_usage_other"];
  }

  $query = <<<END
select user_id
from wrfweb.wrf_users
where email = '$form[email]'
order by user_id;
END;

#  print "<PRE>$query</PRE>"; 

#  $result = mysql_query($query);
  $result = $mysqli->query($query);

#  if (mysql_num_rows($result) > 0) {
  if ($result->num_rows > 0) {
#    $row = mysql_fetch_row($result);
    $row = $result->fetch_row();
    $user_id = $row[0];
    $query = <<<END
update wrfweb.wrf_users
set
   firstname        = '$form[firstname]',
   lastname         = '$form[lastname]',
   email            = '$form[email]',
   affiliation      = '$form[affiliation]',
   affiliation_type = '$form[affiliation_type]',
   city             = '$form[city]',
   country          = '$form[country]',
   citizenship      = '$form[citizenship]',
   intended_usage   = '$form[intended_usage]',
   model            = '$form[model]',
   last_login_dt = now(),
   last_registered_dt = now()
where
   user_id = $user_id;
END;

#    $result = mysql_query($query);
    $result = $mysqli->query($query);
  } else {
    $query = <<<END
insert into wrfweb.wrf_users
set
firstname        = '$form[firstname]',
lastname         = '$form[lastname]',
email            = '$form[email]',
affiliation      = '$form[affiliation]',
affiliation_type = '$form[affiliation_type]',
city             = '$form[city]',
country          = '$form[country]',
citizenship      = '$form[citizenship]',
intended_usage   = '$form[intended_usage]',
model            = '$form[model]',
last_login_dt = now(),
last_registered_dt = now(),
original_registered_dt = now()
END;
    # $result = mysql_query($query);
    $result = $mysqli->query($query);

    # $user_id = mysql_insert_id();
    $user_id = $mysqli->insert_id;
  }
 if ($user_id) {
   print '<p class="bodytext">User registration successful!</p>';

   if ($form["subscribe"] == "on") {
#     $to      = "akiyama@ucar.edu";
     $to      = "wrf-news-subscribe@ucar.edu";
     $subject = "subscribe " . $form["email"];
     $body    = $subject;
     $headers = "From: " . $form["email"] . "\r\n" .
       "X-Mailer: php";
     if (mail($to, $subject, $body, $headers)) {
       print '<p class="bodytext">A request to subscribe to WRF-News has been sent.</p>';       
       print '<p class="bodytext">Please check your inbox for subscription confirmation instructions.</p>';
     } else {
#       print "$to<br />$subject<br />$body<br />$headers";

       print '<p class="bodytext">A request to subscribe to WRF-News was not successfully.</p>';       
       print '<p class="bodytext">Please visit this <a href="http://mailman.ucar.edu/mailman/listinfo/wrf-news">WRF-News subscription page</a> to manually subscribe.</p>';
     }
   }

   if ($form["model"] == "arw") {
     $goto_url = "http://www2.mmm.ucar.edu/wrf/users/download/get_sources.html";
#     $goto_url = "http://www2.mmm.ucar.edu/wrf/users/download/get_source2.htm";
   } elseif ($form["model"] == "nmm") {
     $goto_url = "http://www.dtcenter.org/wrf-nmm/users/downloads/index.php?email=" . $form["email"];
   } else {
     $goto_url = "";
   }
     
   print '<p class="bodytext">Please <a href="' . $goto_url . '">click here</a> to proceed to the download page.</p>';
 } else {
   print '<p class="bodytext"><b>User registration unsuccessful.</b></p>';
   print '<p class="bodytext"><b>Please <a href="' . $_SERVER['PHP_SELF'] . '">click here</a> to try again.</b></p>';
 }
}

?>
</form>

</p>

	    <!-- ################################################################ -->  	    </p>
            <blockquote> 
              <blockquote>&nbsp;</blockquote>
            </blockquote>
            <p align="center" class="detailtextRed"> 
              <script language="JavaScript">

<!--
document.write("updated " + document.lastModified);
// -->
                        </script>
              <br>
            </p>            <img src="../images/transgif.gif" width="1" height="1"><img src="../images/shim162.gif" width="162" height="1"></td>
        </tr>
      
    </table></td>
  </tr>
  
  <tr>
    <td height="50" bgcolor="#009933" class="detailtext">&nbsp;</td>
  </tr>
</table>
</body>
</html>

