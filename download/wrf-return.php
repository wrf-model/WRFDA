<?php
# $form = db_escape($_POST["form"]);
if ($_POST["form"]) {
#  $form = db_escape($_POST["form"], false);
  $form = $_POST["form"];
}

include_once("../../../../libphp/connect_db.php");
?>
<html>
<head>
<title>WRF Data Assimilation System Download: Returning Users</title>
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
var title = new Array();
title["email"]                  = "E-Mail Address";

function check_required_text(field) {
  var field_value = $("input[ @name='form[" + field + "]']").val();
  var field_label = "#" + field + "_label";

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
    check_required_text("email");
    errors_popup = "Please fill out the following required fields:\n\n";
    for ( var i=0, len=errors.length; i<len; ++i ){
      errors_popup += "   * " + errors[i] + "\n";
    }
    if (errors.length > 0) {
      alert(errors_popup);
      errors.length = 0;
      return false;
    } else {
      return true;
    }
  });

  $("input[ @name='form[email]']").blur(function(){
    check_required_text("email");
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
            <p class="pagetitle">WRFDA Source Code Download: Returning Users</p>
            <p class="bodytext">
	    <!-- ################################################################ -->
<form method="post">

<?php
function db_escape($values, $quotes = true) {
  global $mysqli;
    if (is_array($values)) {
      foreach ($values as $key => $value) {
	$values[$key] = db_escape($value, $quotes);
      }
    }
    else if ($values === null) {
      $values = 'NULL';
    }
    else if (is_bool($values)) {
      $values = $values ? 1 : 0;
    }
    else if (!is_numeric($values)) {
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
  if ($form["email"] == "") {
    $errors["email"] = "E-Mail Address";
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

<p class="bodytext">Returning users: please fill in the E-mail Address Field and click on the "Submit" button below.</p>

<p class="bodytext">
<table border="1" cellspacing="0" cellpadding="3">
  <tr>
   <?php $r = required("email"); ?>
	  <td align="left"><p class="detailtext<?php print $r; ?>" id="email_label">E-Mail Address</p></td>
	  <td>
	  <p class="detailtext">
	  <input type="text" name="form[email]" size="30" maxlength="255" class="widthmedium9" value="<?php print $form["email"]; ?>">
	  </p>	  </td>
  </tr>

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

  $mysqli = db_connect("box.mmm.ucar.edu", "wrfweb", "rdwrf");

  $form = db_escape($form, false);

  $query = <<<END
select count(1) as count from wrfweb.wrf_users
where
    citizenship     != "" &&
    model           != "" &&
    email            = '$form[email]';
END;
  
#  $query = <<<END
#select user_id from wrfweb.wrf_users
#where
#    citizenship     != "" &&
#    model           != "" &&
#    email            = '$form[email]';
#END;
  
# print "<PRE>$query</PRE>";

# $result = mysql_query($query);
# $row = mysql_fetch_row($result);
$result = $mysqli->query($query);
$row = $result->fetch_array();


 if ($row[0] > 0) {
#   print "<!--\n$form[model]\n-->";
   
   db_connect("box.mmm.ucar.edu", "wrfweb", "wtwrf");
   $query = "update wrfweb.wrf_users
                set last_login_dt = now()
                where email = '$form[email]'";
   # mysql_query($query);
   $result = $mysqli->query($query);

  $goto_url = "http://www2.mmm.ucar.edu/wrf/users/download/get_sources.html#WRF-DA";
  print '<p class="bodytext"><b>User registration verified.</b></p>';
  print '<p class="bodytext">Please <a href="' . $goto_url . '">click here</a> to proceed to the download page.</p>';

#  db_connect("box.mmm.ucar.edu", "wrfweb", "wtwrf");
 } else {
   $goto_url = "http://www2.mmm.ucar.edu/wrf/users/download/wrf-regist.php";
   print '<p class="bodytext"><b>Unable to verify user registration.</b></p>';
   print '<p class="bodytext">You will need to register to download the software</p>';
   print '<p class="bodytext">Please <a href="' . $goto_url . '">click here</a> to proceed to the registration page.</p>';
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
            </p>            <img src="../images/transgif.gif" width="1" height="1"><img src="../images/shim112.gif" width="112" height="1"><img src="../images/shim162.gif" width="162" height="1"></td>
        </tr>
      
    </table></td>
  </tr>
  
  <tr>
    <td height="50" bgcolor="#009933" class="detailtext">&nbsp;</td>
  </tr>
</table>
</body>
</html>
