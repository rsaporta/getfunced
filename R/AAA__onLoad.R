'.' <- "dummy variable so that R CMD check will not complain"

## hmmm... this doesn't work for some reason, so using the above solution instead
# #' @importFrom utils suppressForeignCheck
# utils::suppressForeignCheck(c("."))

## --------------------------------------------------------------------------------------------- ##
##                 This is a generic .onLoad function                                            ##
## --------------------------------------------------------------------------------------------- ##
# .onLoad <- function(libname, pkgname, verbose=getOption("verbose.onLoad", default=TRUE)) {
#   if (verbose)
#     cat(sprintf("Running .onLoad() from getfunced        ||   [turn off this message with options(verbose.onLoad = FALSE) ]\n", pkgname))
# 
#   ## -------------------- OPTIONS FOR PACKAGE ----------------------- ##
#   ## Options to Load.  
#   ##   All Values should be quoted strings
#   ##   Actual string values should have quotes inside the quotes
#   ##     eg  "\"an example string\"" or "'an example string'"
#   ## Code adapted from the data.table package
#   opts = c(  "getfunced.some_string"    = "'a string'"
#            , "getfunced.some_number"    = "Inf"
#            , "getfunced.some_integer"   = "100L"
#           )
#   for (i in setdiff(names(opts),names(options()))) {
#       eval(parse(text=paste("options(",i,"=",opts[i],")",sep="")))
#   }
#   ## ---------------------------------------------------------------- ##
# 
# 
#   ## What else should happen when this package loads? 
#   "... more stuff ..."
#   
#   return(invisible(TRUE))
# }
# 
# 
