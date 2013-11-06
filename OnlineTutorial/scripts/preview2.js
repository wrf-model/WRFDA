
function showPreview(url, border, width, height, desc)
{

  border = typeof(border) == "undefined" ? 0 : border;
  width = typeof(width) == "undefined" ? 350 : width;
  height = typeof(height) == "undefined" ? 350 : height;
  desc = typeof(desc) == "undefined" ? "" : desc;

  document.getElementById("sp").innerHTML = '<img src="' + url + '" border="' + border + '" width="' + width + '" height="' + height + '>" <br><br><div align="left">' + desc;

}



