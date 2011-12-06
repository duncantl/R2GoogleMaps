# Copied and modified from ecmaScript.R in SVGAnnotation.

setGeneric("addJavascript",
   # Add the inclusion of the JavaScript code.  
function(doc, scripts, insertJS = inherits(scripts, "AsIs"), at = NA,
         ..., .jsvars = list(...), escapeFun = NULL)
            standardGeneric("addJavascript"))

setMethod("addJavascript", "character",
function(doc, scripts, insertJS = inherits(scripts, "AsIs"), at = NA,
         ..., .jsvars = list(...), escapeFun = NULL)
{
   addJavascript(htmlParse(doc), scripts, insertJS, at, ..., .jsvars = .jsvars, escapeFun = escapeFun)
 })

setMethod("addJavascript", "HTMLInternalDocument",
function(doc, scripts, insertJS = inherits(scripts, "AsIs"), at = NA,
         ..., .jsvars = list(...), escapeFun = NULL) # newXMLCommentNode)
{
   head = getNodeSet(doc, "/*/head", character())
   if(length(head) == 0)
      head = newXMLNode("head", parent = xmlRoot(doc), at = 0)
   else
      head = head[[1]]
  
   addJavascript(head, scripts, insertJS, at, ..., escapeFun = escapeFun, .jsvars = .jsvars)
})

setMethod("addJavascript", "XMLInternalDocument",
function(doc, scripts, insertJS = inherits(scripts, "AsIs"), at = NA,
         ..., .jsvars = list(...), escapeFun = NULL)
{
  addJavascript(xmlRoot(doc), scripts, insertJS, at, ..., .jsvars = .jsvars, escapeFun = escapeFun)
})

setMethod("addJavascript", "XMLInternalNode",
function(doc, scripts, insertJS = inherits(scripts, "AsIs"), at = NA,
         ..., isHTML = xmlName(doc) == "head", .jsvars = list(...), escapeFun = NULL)
{
   if(length(scripts) == 0 || all(scripts == ""))
     return()
   
   insertJS = rep(insertJS, length = length(scripts))
   
   if(length(.jsvars)) {
      library(RJSONIO, pos = length(search()) - 1)
      jsNode = newXMLNode("script", attrs = c(type = "text/ecmascript"),
                                    parent = doc, at = at)
      code = paste("var ", names(.jsvars), " = ", sapply(.jsvars, toJSON), ";", collapse = "\n\n", sep = "")
      if(!is.null(escapeFun))
         escapeFun(code, parent = jsNode)
      else
         addChildren(jsNode, code)
    }
   
   if(length(scripts) > 1) {
     sapply(seq(along = scripts), function(i) addJavascript(doc, scripts[i], insertJS[i], at, escapeFun = escapeFun, isHTML = isHTML))
     return(invisible(doc))
   }

      # Single script case
   
   jsNode = newXMLNode("script", attrs = c(type = "text/javascript"))


   if(!is(scripts, "AsIs"))
      scripts = findJScripts(scripts, ok = TRUE)

   if(insertJS)  {
      code = if(!file.exists(scripts))
                 paste(scripts, collapse = "\n")
             else
                 paste(readLines(scripts, warn = FALSE), collapse = "\n")

      if(!is.null(escapeFun))
         escapeFun(code, parent = jsNode)
      else
         addChildren(jsNode, code)

      if(!is(scripts, "AsIs"))
        addAttributes(jsNode, "originalSource" = scripts)
   } else 
      addAttributes(jsNode, "src" = scripts) # normalizePath(path.expand(scripts)))

     # If the caller has specified where to place this node, use that, otherwise
     # add it to the end of the existing script nodes, i.e. after the last existing <script>
   if(!is.na(at))
       addChildren(doc, jsNode, at = 0)
   else {
      others = getNodeSet(doc, "./script") 
      if(length(others))
         addSibling(others[[length(others)]], jsNode)
      else
         addChildren(doc, jsNode, at = 0)

        # put a separator between the nodes as the HTML dump won't do  it for us.
      addSibling(jsNode, newXMLTextNode("\n"), after = FALSE)
   }

   invisible(doc)
})


findJScripts =
function(x, ...)
{
 x
}
