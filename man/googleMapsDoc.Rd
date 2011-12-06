\name{googleMapsDoc}
\alias{googleMapsDoc}
\title{Create an HTML document displaying a Google Map}
\description{
  This is  the high-level function for creating a
  document that can be displayed in a Web browser
  to show a Google Map.
  This allows us to display geographical data on a Google Map
  that is interactive. We can add many overlay objects
  such as lines, paths and polygons and interactive markers.

  In the future, we will be able to add images.
  We can also control different aspects of interaction.
}
\usage{
googleMapsDoc(code, center = c(0, 0), zoom = 1, file = NA,
               scripts = character(), title = character(),
                dims = c(750, 750), control = "GSmallZoomControl3D",
                mapOpts = NULL,
                key = getOption("GoogleMapsKey"), version = 3,
                 onload = "initialize()", canvas.id = "map_canvas",
                  template = system.file("templates", "template.html",
                                           package = "R2GoogleMaps"))
}
\arguments{
  \item{code}{a character string/vector that specifies the essential
    code for the function that creates the Google Map within the
    document. Typically, this code is appended to generic code that
    creates the map, centers it at the coordinates specified via
    \code{center} and optionally adds a control.
    If this has the class \code{AsIs} (e.g. using the \code{\link[base]{I}}
    function), it is treated as self-contained.
  }
  \item{center}{a numeric vector giving the latitude and longitude at
    which the map should be centered}
  \item{zoom}{an integer giving the zoom level for when the map is first
    displayed. This should be between 1 and 20 inclusive.  See the
    Google Maps API.}
  \item{file}{a character string giving the name of the file
    that is to be created. If this is \code{NA} or an empty character
    vector (\code{character()}), the HTML document is returned (as an
    \code{HTMLInternalDocument} object). 
  One can then modify this and add more content to it and then save it,
  or perhaps send it to a browser using, e.g. DCOM or AppleScript or a
  shell call.}
  \item{scripts}{a character vector giving either the names of
    JavaScript code files or the JavaScript code itself which are
    inserted into the HTML document.
    If these are file names, a reference to that file will be added.
    In the future, we will provide a mechanism that allows the caller of
    this function to  control whether a reference is used or the
    contents of the files are added.
   }
  \item{title}{a string that is used for the title of the HTML document,
  appearing in the window frame}
  \item{dims}{a numeric/integer vector giving the number of pixels in
    the horizontal and vertical orientations for the map canvas on which
    the map is drawn and displayed. One can specify an individual value
    but it must be a named vector, using either \code{width} or \code{height}.}
  \item{control}{a character vector giving the name of the type of
    GControl to display on the map. This can be a partial match (see
    \code{\link[base]{pmatch}}) to the class name from the Google Maps
    API. If no control is to appear, use the empty string - \code{""}.  
  }
  \item{mapOpts}{a named list providing named options that map to a
     GMapOptions constructor/initializer. This can be used to specify
  the \code{mapTypes}, the size, the background color, ...}
  \item{key}{a character string giving the Google Maps API key to use when connecting to the
    Google Maps server.  See registering an application in the Google Maps documentation.}
  \item{version}{an integer specifying the version of the Google Maps
    API to use. This currently should be either 2 or 3.}
  \item{onload}{the JavaScript call that is used to create the map when
    the document is loaded. This is used  as the value of the
    \code{onload} attribute of the HTML document's \code{body} element.}
  \item{canvas.id}{the name of the HTML element that is used to display the
  map. This is typically not specified, but if the HTML document is to
  have two or more maps displayed, one may want to control this.}
  \item{template}{the name of the file that contains the HTML code that
    acts as a template.}
}
\value{
  If \code{file} is a string, this is returned and the corresponding
  file is created/overwritten with the generated HTML document.
  Alternatively, if \code{file} is \code{NA} or an empty vector,
  the generated HTML document is returned as an \code{HTMLInternalDocument}
  object.
  
}
\references{
  \url{http://code.google.com/apis/maps/documentation/reference.html}
  and
   \url{http://code.google.com/apis/maps/documentation/index.html}
}
\seealso{
  \url{http://www.omegahat.org/R2GoogleMaps/sampleDocs/sfcabs.html}
  
  \code{\link{gpolyline}},  \code{\link{gmarker}}  
}

\examples{

 # Thanks to Markus Loecher's example.
plottingCode = "// ..."  # need to put something of substance
googleMapsDoc(c("map.setUIToDefault();",
                "map.enableGoogleBar();",
                "map.addControl(new GSmallZoomControl3D());",
                plottingCode
               ), mapOpts = list(googleBarOptions = c(style = "new")))
}
\keyword{IO}
\keyword{programming}
\concept{JavaScript}
\concept{Google Maps}
