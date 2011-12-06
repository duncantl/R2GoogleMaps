setClass("RGBStrictHash", contains = "RGB")

setAs("character", "RGBStrictHash",
      function(from) {
         if(substring(from, 1, 1) == "0x") {
           ans = new("RGBStrictHash", toRGBHexInt(from, "#"))
         } else {
           v = col2rgb(from)
           new("RGBStrictHash", toRGBHexInt(rgb(v[1], v[2], v[3], maxColorValue = 255), "#"))
         }
      })
