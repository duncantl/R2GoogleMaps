library(XML)
library(RCurl)

getCabs =
function(m = 10)
{
  x = getForm('http://cabspotting.org/cabs.xml.php',  m = m)
  doc = xmlParse(x, asText = TRUE)
  structure(as.integer(xmlSApply(xmlRoot(doc), xmlGetAttr, "updates")),
            names = xmlSApply(xmlRoot(doc), xmlGetAttr, "id"))
}

getCabInfo = 
function(m = 60, cabs = names(getCabs(m)), combine = TRUE)
{
  ans = lapply(cabs, getOneCab, m)

  if(combine)
    do.call("rbind", ans)
  else {
    names(ans) = cabs
    ans
  }
}

getOneCab =
function(id, m = 60)
{
  ans = getForm("http://cabspotting.org/cab.xml.php", cab = id, m = m)
  doc = xmlParse(ans)
  tmp = xmlSApply(xmlRoot(doc), xmlAttrs)
  tmp = as.data.frame(t(tmp), row.names = 1:ncol(tmp))
  names(tmp) = c("cab", "lat", "long", "status", "time")
  tmp$time = as.POSIXct(as.numeric(as.character(tmp$time)), origin = "1970-01-01")
  tmp
}
