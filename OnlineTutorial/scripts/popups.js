
function popUps(URL,sw,sh) {
day = new Date();
id = day.getTime();
eval("page" + id + " = window.open(URL, '" + id + "','toolbar=0,scrollbars=0,location=0,statusbar=0,menubar=0,resizable=0,width="+sw+",height="+sh+"');");
}