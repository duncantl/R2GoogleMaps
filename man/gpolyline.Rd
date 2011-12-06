\name{gpolyline}
\alias{gpolyline}
\alias{gpolygon}
\alias{gmarker}
\title{Create JavaScript code for GPolyline and GMarker objects}
\description{
  These functions are used to create JavaScript code
  that will in turn create polyline paths and
  marker objects that are displayed on a Google Map.
  These objects are associated with one or more
  latitude and longitude pairs.

  A marker supports many optional settings such as draggable, title,
  clickable, bouncy, hide. 
}
\usage{
gpolyline(lat, long, color = NA, width = NA, var = character(), semiColon = FALSE, addOverlay = !semiColon)
gmarker(lat, long, icon, ..., var = character(), addOverlay = FALSE, call = "new GMarker")
gpolygon(lat, long, border.color = "#FF0000", fill.color = "#FFFFFF", border.width = 1,
          border.alpha = 1, fill.alpha = 0,
          var = character(), semiColon = FALSE, addOverlay =!semiColon,
          close = lat[length(lat)] != lat[1] || long[length(long)] != long[1])
}
\arguments{
  \item{lat}{this specifies the latitude of the locations.
    This can be a numeric vector, or alternatively it can be
    a matrix or list/data.frame where the first two columns
    are considered to be the latitude and longitude (in that order).}
  \item{long}{a numeric vector giving the longitude of the points.
     If this is not specified, the longitude is  extracted from the
     \code{lat} object.}  
  \item{color}{an RGB color specified in hexademical (e.g. \#FF0000 for
    red) that is used to color the polyline.
    }
  \item{width}{an integer giving the width of the polyline}
  \item{var}{a character string to which, if specified, the newly
  created JavaScript \code{GPolyLine} or \code{GMarker} will be
  assigned. This has nothing to do with R variables; the assignment
  takes place in JavaScript.
}
  \item{semiColon}{a logical value indicating whether to add a
    semi-colon to the end of the generated JavaScript command.
    One may want to suppress this if the result of the call is to be
    used directly within another call.
   }
  \item{addOverlay}{a logical value which if \code{TRUE} wraps the
    generated code in a call to \code{addOverlay}.}
  \item{\dots}{\code{name = value} pairs
    This won't work for all possible permissible options, e.g. icon,
    since that requires a GIcon object rather than a JavaScript
    primitive value. We might add a facility to allow code to be passed
    directly.}
  \item{icon}{currently ignored. It is intended to address the issue of
  specifying the GIcon for the marker.}
  \item{call}{a string that gives the call to create the GMarker
  object. This parameter allows the caller to specify the name of a
  JavaScript function that can be used instead of the regular "new
  GMarker".
   Unfortunately, one cannot specify additional arguments for those
   JavaScript calls, so this is less useful than originally intended.}
 \item{border.color,fill.color}{color specifications for the
   border and interior. These can be specified using
   hexadecimal format such as "\#FF0000" or the named equivalents
    "red".
  }
 \item{border.width}{an integer specifying the number of pixels
   for  the border of the polygon}
 \item{border.alpha,fill.alpha}{these are values between 0 and 1 that
  specify the opaqueness or alpha-level of the border color and the
  interior/fill color.}
\item{close}{a logical value that if \code{TRUE} causes \code{gpolygon}
   to ensure that the polygon is closed, i.e. loops back to the original point}
 
}
\value{
  Both functions return a character vector/string
  that contains the generated JavaScript code.
  \code{gpolyline} returns a string.
  \code{gmarker} returns a character vector with as many elements as
  there are (latitude, longitude) pairs given in the input.
}
\references{
  \url{http://code.google.com/apis/maps/documentation/reference.html}
 and  \url{http://code.google.com/apis/maps/documentation/index.html}
}
\seealso{
  \code{\link{googleMapsDoc}}
  \code{\link{latLong}}

  \url{http://www.omegahat.org/R2GoogleMaps/sampleDocs/sfcabs.html}  
}
\examples{

}
\keyword{IO}
\keyword{programming}
\concept{JavaScript}
\concept{Google Maps}
