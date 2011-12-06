library(R2GoogleMaps)

p = c(37.72008, -122.34073)

y = p[1] +  c(-.7, .7, .7, -.7)/40
x = p[2] +  c(-1, -1, 1, 1)/20

y1 = p[1] +  c(-.7, .7, -.7, .7)/40 + .1
x1 = p[2] +  c(-1, -1, 1, 1)/20 + .2

code = c(gpolygon(y1, x1, fill.alpha = .4),
         gpolygon(y, x, "#0000FF", "yellow", fill.alpha = 1),
         gmarker(p, addOverlay = TRUE))

z = googleMapsDoc(code, p, 11, "bar.html", title = "Polygon example",
                   mapOpts = c(mapTypes = list(mapTypes = c("G_HYBRID_MAP", "G_SATELLITE_MAP"))))

