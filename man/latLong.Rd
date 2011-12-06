\name{latLong}
\alias{latLong}
\title{Create JavaScript code for GLatLon objects}
\description{
  This function creates JavaScript code that
  will create \code{GLatLon} objects when the map
  is viewed in a browser. These represent a geographical
  position and are  used in various places.
  We use them to create (poly)lines to draw paths
  and for specifying the location of a marker on a map.
}
\usage{
latLong(lat, long, digits = 4,
        fmt = sprintf("\%\%.\%df", as.integer(digits)),
         prefix = "\t", collapse = ",\n")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{this specifies the latitude of the locations.
    This can be a numeric vector, or alternatively it can be
    a matrix or list/data.frame where the first two columns
    are considered to be the latitude and longitude (in that order).}
  \item{long}{a numeric vector giving the longitude of the points.
     If this is not specified, the longitude is  extracted from the
     \code{lat} object.}
   \item{digits}{the number of digits after the decimal place used to
     represent  the numbers in the JavaScript code}
   \item{fmt}{a character string used to map an individual number to
     JavaScript form. This is typically not specified in a call but does
     allow an advanced user to control the way numbers are presented to
     the JavaScript interpreter.
    }
  \item{prefix}{a character string used as a prefix before each
    generated call to \code{new GLatLon}. This is used to indent the
    code appropriately}
  \item{collapse}{a character string or logical value.
    If this is \code{FALSE}, the individual JavaScript calls are
    returned as a character vector with an element for each call.
    If this is a string, it is used as the value of the \code{collapse} argument
    in a call to \code{\link[base]{paste}}.}
}
\value{
  Depending on whether \code{collapse} is a string or \code{FALSE},
  the result is either a single character string containing the JavaScript
  code to create all the GLatLon objects, or
  a character vector with an element for each latitude and longitude
  pair.

}
\references{
  \url{http://code.google.com/apis/maps/documentation/reference.html}
  and
   \url{http://code.google.com/apis/maps/documentation/index.html}
}
\author{Duncan Temple Lang}
\seealso{
 \code{\link{gpolyline}}
 \code{\link{gmarker}}
 \code{\link{addOverlay}}
 \code{\link{googleMapsDoc}}

  \url{http://www.omegahat.org/R2GoogleMaps/sampleDocs/sfcabs.html}
}
\examples{
}
\keyword{programming}
\keyword{IO}
\concept{JavaScript}
\concept{Google Maps}
