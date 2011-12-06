
latLong =
function(lat, long, digits = 4,   fmt = sprintf("%%.%df", as.integer(digits)), prefix = "\t", collapse = ",\n")
{
   fmt = sprintf("new GLatLng(%s, %s)", fmt, fmt)

   if(missing(long)) {
     if(is.list(lat)) {
       long = lat[[2]]       
       lat = lat[[1]]
     } else if(is.matrix(lat)) {
       long = lat[,2]
       lat = lat[,1]       
     }  else if(is.vector(lat)) {
         long = lat[2]
         lat = lat[1]
     } else
         stop("unrecognized specification for lat and long")
   }

   ans = sprintf(fmt, lat, long)

   if(collapse == FALSE)
     return(ans)
   
   ans = paste(prefix, ans, collapse = collapse)
   if(length(lat) == 1)
     ans
   else
     paste(paste(prefix, "[", sep = ""), ans,
           paste(prefix, "]", sep = ""), sep = "\n")
}

gpolyline =
function(lat, long, color = NA, width = NA, var = character(), semiColon = FALSE, addOverlay =!semiColon)
{
   if(missing(long)) {
     if(is.list(lat)) {
       long = lat[[2]]       
       lat = lat[[1]]
     } else if(is.matrix(lat)) {
       long = lat[,2]
       lat = lat[,1]       
     } else
         stop("unrecognized specification for lat and long")
   }  

  tmp = latLong(lat, long)
  ans = paste("new GPolyline(\n", tmp, sep = "")

  if(length(var) && !is.na(var) && nchar(var)[1] > 0 )
     ans = paste(var, "=", ans)

  if(!is.na(width) && is.na(color))
    color = "#FFFFFF"

   if(!is.na(color))
      ans = paste(ans, sprintf(', "%s"', as(as(color, "RGBStrictHash"), "character")))

   if(!is.na(width))
      ans = paste(ans, sprintf(', %d', as.integer(width)))
   
  ans = paste(ans, ")", if(semiColon) ";", sep = "")

  if(addOverlay)
     addOverlay(ans)
  else
     ans   
}


gpolygon =
function(lat, long, border.color = "#FF0000", fill.color = "#FFFFFF", border.width = 1,
          border.alpha = 1, fill.alpha = 0,
          var = character(), semiColon = FALSE, addOverlay =!semiColon,
          close = lat[length(lat)] != lat[1] || long[length(long)] != long[1])
{
   if(missing(long)) {
     if(is.list(lat)) {
       long = lat[[2]]       
       lat = lat[[1]]
     } else if(is.matrix(lat)) {
       long = lat[,2]
       lat = lat[,1]       
     } else
         stop("unrecognized specification for lat and long")
   }  

  if(close) {
    lat = c(lat, lat[1])
    long = c(long, long[1])    
  }
   
  tmp = latLong(lat, long)
  ans = paste("new GPolygon(\n", tmp, sep = "")

  if(length(var) && !is.na(var) && nchar(var)[1] > 0 )
     ans = paste(var, "=", ans)

  params = sprintf('"%s", %d, %.4f', as(as(border.color, "RGBStrictHash"), "character"), border.width, border.alpha)
  params1 = sprintf('"%s", %.4f', as(as(fill.color, "RGBStrictHash"), "character"), fill.alpha)
  params = paste(params, params1, sep = ", ")
   
  ans = paste(ans, ",", params, ")", if(semiColon) ";", sep = "")

  if(addOverlay)
     addOverlay(ans)
  else
     ans   
}



addOverlay =
  #
  # This is vectorized.
  #
function(obj,  map = "map", semiColon = TRUE)
{
  sprintf("%s.addOverlay(%s)%s",
              map, obj, if(semiColon) ";" else "")
}


gmarker =
  #
  # create one or more marker;
  #
function(lat, long, icon, ..., var = character(), addOverlay = FALSE, call = "new GMarker")
{
   if(missing(long)) {
     if(is.list(lat)) {
       long = lat[[2]]       
       lat = lat[[1]]
     } else if(is.matrix(lat)) {
       long = lat[,2]
       lat = lat[,1]       
     }   else if(is.vector(lat)) {
         long = lat[2]
         lat = lat[1]
     } else
         stop("unrecognized specification for lat and long")
   }

  args = list(...)

  ans = if(length(args)) 
           sprintf("%s(%s, %s)",  call, latLong(lat, long, collapse = FALSE), toJSON(args))
        else
           sprintf("%s(%s)", call, latLong(lat, long, collapse = FALSE))
     

  if(addOverlay)
     addOverlay(ans)
  else
     ans

}

markerData =
  #
  # Spits the lat,long pairs out in XML as <markers><marker lat="" lon=""/><marker...></markers>
  #
function(lat, long, file = character())
{

   if(missing(long)) {
     if(is.list(lat)) {
       long = lat[[2]]       
       lat = lat[[1]]
     } else if(is.matrix(lat)) {
       long = lat[,2]
       lat = lat[,1]       
     } else
         stop("unrecognized specification for lat and long")
   }


   m = newXMLNode("markers")
   invisible(mapply(function(lat, lon) 
                      newXMLNode("marker", 
                                  attrs = c(lat = lat, lng = lon), 
                                  parent = m),
                 lat, long))

   if(!is.na(file) && length(file))
     saveXML(m, file)
   else
     m
 }
