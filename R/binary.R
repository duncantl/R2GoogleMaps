#
# This is the start of GPolyline encodin.
#

binaryRep =
function(x, y =  round(x * 100000))
{
  ans = integer()

  while(TRUE) {
     ans = c(ans, y%%2)
     y = as.integer(y/2)
     if(y < 2) {
       ans = c(ans, y)
       break
     }
  }

  paste(rev(ans), collapse = "")
}
