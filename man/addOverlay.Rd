\name{addOverlay}
\alias{addOverlay}
\title{Creates JavaScript call to addOverlay}
\description{
  This function creates JavaScript code that
  adds the specified object as an overlay to the
  Google Map. This takes place when the JavaScript
  code is run; not in this call.

  This is used when we create (code for) overlay objects
  using functions such as \code{\link{gmarker}}
  and \code{\link{gpolyline}}. Having created
  such objects, we then add them to the map.

  These overlay creation functions (in R) support
  adding the overlay directly via their \code{addOverlay}
  parameter.
  
}
\usage{
addOverlay(obj, map = "map", semiColon = TRUE)
}
\arguments{
  \item{obj}{a character vector giving the code for the object(s)
     that are to be added to the map as an overlay(s)}
  \item{map}{a character string giving the name of the JavaScript
  variable used to reference the GMap object.}
  \item{semiColon}{a logical value indicating whether to add a
    semi-colon to the end of the generated JavaScript command.
    One may want to suppress this if the result of the call is to be
    used directly within another call.
   }
}
\value{
  A character vector giving the JavaScript call(s) that
  will add the objects to the map.
}
\references{
  \url{http://code.google.com/apis/maps/documentation/reference.html}
  and
  \url{http://code.google.com/apis/maps/documentation/index.html}
}
\author{Duncan Temple Lang}
\seealso{
 \code{\link{gpolyline}},  \code{\link{gmarker}}
 \code{\link{googleMapsDoc}}

 \url{http://www.omegahat.org/R2GoogleMaps/sampleDocs/sfcabs.html}
}
\examples{

}
\keyword{IO}
\keyword{programming}
\concept{JavaScript}
\concept{Google Maps}
