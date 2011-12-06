#library(RGraphicsDevice)

# See Rjs.R in RGraphicsDevice.

setClass("RGBStrict", representation(alpha = "numeric"), contains = "RGB")


# For the moment, handle named colors manually and convert them to hexadecimal 0x....
RGB = c("black" = "0x000000", red = "0xFF0000", green = "0x00FF00", blue = "0x0000FF", transparent = "0x00000000")


toRGBHexInt =
function(from, prefix = "0x")
 paste(prefix, substring(from, 2, 7), sep = "" )

setAs("character", "RGBStrict",
      tmp <- function(from) {

         if(substring(from, 1, 1) == "#") {
           ans = new("RGBStrict", toRGBHexInt(from))
           ans@alpha = if(nchar(from) == 7)
                           1.0
                       else
                         hexToInteger(substring(from, 8))/255
           ans
         } else {
           v = col2rgb(from)
           
           new("RGBStrict", toRGBHexInt(rgb(v[1], v[2], v[3], maxColorValue = 255)),
                  alpha = if(from == "transparent") 0.0 else 1.0)
         }
      })

setAs("RGB", "RGBStrict", tmp)

setAs("integer", "RGBStrict",
      function(from) {
         as(as(from, "RGB"), "RGBStrict")
      })

hexToInteger =
function(x)
{
   els = strsplit(x, "")[[1]]
   i = match(tolower(els), c(0:9,letters))
   sum((i-1) * 16^rev((seq(along = i) - 1)))
}
