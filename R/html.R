checkbox =
function(label, checked = TRUE, name = NA, ..., parent = "li")
{
  attrs = list(...)
  mapply(function() {
            newXMLNode(parent, newXMLNode("input", attrs = c(checked = checked)))
         }, label)
}
