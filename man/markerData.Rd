\name{markerData}
\alias{markerData}
\title{Generate an XML document for representing Google Maps maker objects}
\description{
  This is a simple function that allows us to generate an input file
  for specifying the locations of GMarker objects to be displayed
  on a Google Map. Rather than creating the JavaScript code
  to create the GMarker objects, we create the data that is
  read at run/display-time  and generic JavaScript
  code process the content and dynamically generates the
  markers then.  This can be useful in AJAX-style setups
  or when the inputs change or there are many different input files,
  but the HTML and JavaScript remains  constant.
}
\usage{
markerData(lat, long, file = character())
}
\arguments{
  \item{lat}{this specifies the latitude of the locations.
    This can be a numeric vector, or alternatively it can be
    a matrix or list/data.frame where the first two columns
    are considered to be the latitude and longitude (in that order).}
  \item{long}{a numeric vector giving the longitude of the points.
     If this is not specified, the longitude is  extracted from the
     \code{lat} object.}  
   \item{file}{a character string giving the name of the file
     to which the XML document should be written. If this is not
     specified is an empty character vector, the XML document is
     returned  directly and can be post-processed before it is
     stored, or can be sent to a server in a request without being
     written to a file.
     }
}
\value{
  If \code{file} is empty, the result is an object of class
  \code{XMLInternalDocument} which represents the generated XML
  content. Otherwise, the function creates the specified file
  and returns its name.
}
\references{
  \url{http://code.google.com/apis/maps/documentation/reference.html}
  and
   \url{http://code.google.com/apis/maps/documentation/index.html}
}
\author{Duncan Temple Lang}
\seealso{
  \code{\link{googleMapsDoc}}
  \code{genericMarkerData.js} in
  \code{system.file("javascript", "genericMakerData.js", package = "R2GoogleMaps")}.

  \url{http://www.omegahat.org/R2GoogleMaps/sampleDocs/sfcabs.html}  
}
\examples{
}
\keyword{programming}
\keyword{IO}
\concept{JavaScript}
\concept{Google Maps}

