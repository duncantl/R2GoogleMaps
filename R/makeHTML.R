getJavascriptFunctionName =
  #
  # Examine Javascript code and find the name of the function.
  #
function(code)
{
     # Assume it is a correct name!
  gsub("function[[:space:]]+([a-zA-Z0-9]+)[[:space:]]*\\(.*", "\\1", code)
}

gMapTypes =
c(
"G_NORMAL_MAP",
"G_SATELLITE_MAP",
"G_HYBRID_MAP",
"G_PHYSICAL_MAP",
"G_MAPMAKER_NORMAL_MAP",
"G_MAPMAKER_HYBRID_MAP",
"G_MOON_ELEVATION_MAP",
"G_MOON_VISIBLE_MAP",
"G_MARS_ELEVATION_MAP",
"G_MARS_VISIBLE_MAP",
"G_MARS_INFRARED_MAP",
"G_SKY_VISIBLE_MAP",
"G_DEFAULT_MAP_TYPES",
"G_MAPMAKER_MAP_TYPES",
"G_MOON_MAP_TYPES",
"G_MARS_MAP_TYPES",
"G_SKY_MAP_TYPES")

processMapOptions =
function(els)
{
  types = els$mapTypes
  if(!is.null(types)) {
     els$mapTypes = structure(lapply(types, as.name), names = NULL)
   }

  els
}


gControls =
  c(
"GSmallMapControl",
"GLargeMapControl",
"GSmallZoomControl",
"GLargeMapControl3D",
"GSmallZoomControl3D",
"GScaleControl",
"GMapTypeControl",
"GMenuMapTypeControl",
"GHierarchicalMapTypeControl",
"GOverviewMapControl",
"GNavLabelControl")

matchControl =
function(control)
{
  if(is.na(control) || control == "" || length(control) == 0)
     return(control)
  
  i = pmatch(tolower(control), tolower(gControls))
  if(any(is.na(i))) {
    warning("did not match control ", control[is.na(i)])
  }

  gControls[ i [!is.na(i)] ]
}

makeInitFunction =
  # Create the code for the onLoad event to initialize the Google maps.
function(code, center = c(0, 0), zoom = 5, control = "", mapOpts = c(),
          id = "map_canvas", name = "initialize", map = "map")
{
 txt = paste(code, collapse = "\n\t")

 if(inherits(code, "AsIs"))
    return(structure(I(txt), names = getJavascriptFunctionName(code)))

 if(length(control)) 
    control = sprintf("%s.addControl(new %s());", map, matchControl(control))
 
 tmp = 'function %s() {
           if (GBrowserIsCompatible()) {
               var %s = new GMap2(document.getElementById("%s"), %s);
               %s.setCenter(%s, %d);
               %s
               %s
           }
        }'

  if(length(mapOpts))
     mapOpts = toJSON(processMapOptions(mapOpts))
  else
     mapOpts = "{}"

  structure(I(sprintf(tmp, name, map, id, mapOpts,
                       map, latLong(center, prefix = ""), as.integer(zoom),
                       control, txt)),
            names = name)
}

googleMapsDoc =
function(code, center = c(0, 0), zoom = 1, file = NA, scripts = character(),
          title = character(), dims = c(750, 750),
          control = "GSmallZoomControl3D",
          mapOpts = NULL,
          key = getOption("GoogleMapsKey"), 
          version = 3,
          onload = "initialize()", canvas.id = "map_canvas",
          template = system.file("templates", "template.html", package = "R2GoogleMaps")
         )
{
   if(is.null(key))
     key = ""
   
   doc = htmlParse(template)

   body = getNodeSet(doc, "//body")[[1]]
   xmlAttrs(body) = c(onload = onload)

   div = getNodeSet(body, "./div")
   if(length(div)) {
         div = div[[1]]   

         if(!is.na(canvas.id)) 
           xmlAttrs(div) = c(id = canvas.id)

         if(length(dims) > 0 && !is.na(dims)) {
             sty = getStyle(div)
             if(is(dims, "numeric"))
                dims = paste(dims, "px", sep = "")
             if(length(names(dims)) == 0)
                 names(dims) = c("width", "height")[seq(along = dims)]
             sty[names(dims)] = dims
             xmlAttrs(div) = c(style = toCSS(sty))
         }
    }

   if(length(title)) {
     ttle = getNodeSet(doc, "/html/head/title")
     if(length(ttle))
       xmlValue(ttle[[1]]) = title
   }


   js.nodes = getNodeSet(doc, "/html/head/script[@type='text/javascript']")
   js.nodes = getNodeSet(doc, "/html/head/script[@type='text/javascript' and starts-with(@src, 'http://maps.google.com/maps?')]")
   src = sprintf("http://maps.google.com/maps?file=api&v=%d&key=%s", version, key)
   if(length(js.nodes) == 0) 
     newXMLNode("script", attrs = c(src = src,
                                    type = "text/javascript"), parent = xmlRoot(doc)[["html"]][["head"]])
   else
     xmlAttrs(js.nodes[[1]]) = c(src = src)
     

   
   addJavascript(doc, scripts)

   addJavascript(doc, makeInitFunction(code, center, zoom, control, mapOpts, canvas.id))   
   if(length(file) == 0 || is.na(file))
     doc
   else {
     saveXML(doc, file)
     # cat(saveXML(doc, file), file = file)
   }
   
}

if(FALSE) {
addJavaScript =
  #
  #
  # check if they are already there.
  #
function(doc, scripts, asIs = inherits())
{
  head = getNodeSet(doc, "/html/head")
  if(length(head) == 0)
    head = newXMLNode("head", parent = xmlRoot(doc))
  else
    head = head[[1]]


  node = newXMLNode("script", attrs = c(type = "text/javascript"))

  node
}
}
