<?php
# FileName="Connection_php_mysql.htm"
# Type="MYSQL"
# HTTP="true"
$hostname_feedback_db = "sql.ucar.edu";
$database_feedback_db = "mmm_wrfda";
$username_feedback_db = "xinzhang";
$password_feedback_db = "xinzhang8955";
$feedback_db = mysql_pconnect($hostname_feedback_db, $username_feedback_db, $password_feedback_db) or trigger_error(mysql_error(),E_USER_ERROR); 
?>