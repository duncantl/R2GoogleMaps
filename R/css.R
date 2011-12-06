# From grz.R in SVGAnnotation

getStyle =
function(node)
   UseMethod("getStyle")

getStyle.XMLInternalNode =
function(node)
  getStyle(xmlGetAttr(node, "style"))

getStyle.character =
function(node)  
{
  els = strsplit(gsub(";[[:space:]]*$", "", node), ";")[[1]]
  tmp = strsplit(els, ":[ ]*")
  structure(sapply(tmp, `[`, 2), names = trim(sapply(tmp, `[`, 1)))
}


# From compressStyles.R in SVGAnnotation            
toCSS =
function(style, className = character(), ..., collapse = "; ")
{
   # Don't put the values in a { }
 #  paste(c(paste(className, "{") , <below>, "}"), collapse = gsub(";", "", collapse))

  paste(names(style),  style, sep = ": ", collapse = collapse)
}            
            
trim = 
function (x) 
gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
