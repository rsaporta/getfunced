framework borrowed from: 
https://gist.github.com/jefferys/b79fe87314b0dc72fec9

# ======================================================== #
#   use file name here   #
# ======================================================== #


### PARAM FORMAT

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### WITH DEFAULT VALUE
#' @param param_name param_description.  
#'        WHAT IS IT?
#' 
#'        Default is \code{as.character(substitute(arg_to_iterate_over))}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### WITH NO DEFAULT
#' @param param_name param_description.  
#'        WHAT IS IT?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#==========================================================
# $FILENAME$
#==========================================================

#' TITLE
#' 
#' ONE-LINER WHAT IT DOES
#' 
#' DETAILED DESCRIPTION
#'
#' @SECTION-NAME text
#' 
#' @name 
#' @aliases 
#' 
#' @param one  ## GRAB THE SHARED PARAMS HERE
#' @param two 
#'
#' @return 
#'
#' @examples
#' 
NULL

if (FALSE) {
  filename <- "example.file.R"
  file_full_path <- as.path("~/Development", filename)

  title=basename(file_full_path)
  left_pad = 3

  example_text <- "/Users/rsaporta/Development/rpkgs/getfunced/example_file.R"
}

# 

# #' @importFrom magrittr %<>%
# #' @importFrom magrittr %>%
make_group_documentation <- function(
    file_full_path
  , aliases         = c()
  , return          = "WHAT IS RETURNED??"
  , example_text    = "\n"
  , filename        = basename(file_full_path)
  , title           = filename
  , header          = filename
  , neat_box_title  = TRUE
) {
  if (length(file_full_path) != 1 || !is.character(file_full_path) || !nzchar(file_full_path))
    stop("invalid input for file_full_path --  It should be a character string of length 1")
  if (!file.exists(file_full_path))
    stop("file '", file_full_path, "' does not exist")


  if (neat_box_title)
    header %<>% neat_box

  if (missing(title) && grepl(pattern = "\\.", title)) {
    title %<>% strsplit("\\.")[[1L]] %>% head(-1L) %>% paste(collapse="_")
  }
  title %<>% gsub(pattern=" ", "_")

  name <- tolower(title)

  if (file_exists(example_text))
    example_text <- readLines(example_text)



}

neat_box <- function(x, min_width = 62L, left_pad=3L, collapse="\n") {
  if (!is.numeric(left_pad)) {
    warning("'left_pad' should be a number.  Will use 0")
    left_pad <- 0
  }
  if (!is.numeric(min_width)) {
    warning("'min_width' should be a number.  Will use 0")
    min_width <- 0
  }

  min_width %<>% as.integer()
  left_pad %<>% as.integer()

  width <- max(min_width, nchar(x) + 6L + left_pad)
  width <- width - 4L
  hr_bar <- rep("=", width) %>% paste0(collapse="")
  text_bar <- (width - left_pad) %>% paste0("%", left_pad, "s", "%-", ., "s") %>% sprintf("", x)
  
  ## RETURN
  c(hr_bar, text_bar, hr_bar) %>% paste0("# ", ., " #", collapse=collapse)
}

# filename
# filename %>% toproper

# aliases vector of strings. defaults to c()


#==========================================================
# %s
#==========================================================

#' Group of functions page title
#' 
#' Group of functions Description section
#' 
#' Group of functions Details paragraph.
#'
#' @section After Arguments and Value sections:
#' Despite its location, this actually comes after the Arguments and Value sections.
#' Also, don't need to use null, could annotate first function, and then
#' using function name as the groupBy name is more intuitive.
#' 
#' @param x a param for toBar and notToBar
#' @param y a param just for notToBar
#' @return Hard to have one return section for all functions,
#' might want to have a manual list here.
#' @name anyNameButFunctionNameIsUnique
NULL



#' collectArgs and iterateWithArgs
#' 
#' Functions to cleanly collect arguments from within one function or environment (to then pass to another or to iterate over)
#' 
#' \code{collectArgs()} colects objects from an envrionment into a single list. Generally, the list will then be passed to other functions (usually with \code{\link[base]{do.call}})
#' 
#' \code{iterateWithArgs()} similarly collects the objects in an environment, with the difference that one specific object is selected to iterate over. For each iteration, the given value is passed along with all the other objects to \code{FUNC}.
#'
# # ' @section After Arguments and Value sections:
# # ' Despite its location, this actually comes after the Arguments and Value sections.
# # ' Also, don't need to use null, could annotate first function, and then
# # ' using function name as the groupBy name is more intuitive.
#' 
#' @param except A vector of string values. Objects to \emph{NOT} include in the collection
#'               Generally, the user will not want to pass objets created inside the function and hence will pass to except
#'               _NOTE_ pass the quoted string-name of the object, not the object itself.
#' @param incl.dots A single logical value. Should the \code{...} be collected as well?  Default is \code{TRUE}.
#'                  \emph{NOTE: Has no effect in functions without dots argument}
#' @param all.names A single logical value. Passed to \code{ls()}. When \code{FALSE}, then objects whose name begins with a '.' are omitted from the collection
#' @param envir     An \code{environment} object. Passed to \code{ls()}. The environment from which to collect the objects. Defaults to \code{parent.frame}
#'
#' @return 
#' for \code{collectArgs}: A list of all of the objects in \code{envir} (less any objects excluded via the parameters). The names of the list are the names of object in \code{envir}.
#' 
#' for \code{iterateWithArgs}: A list of the return values of \code{FUNC}, the length of \code{arg_to_iterate_over}. Naming of the list will be handled by \code{\link[base]{do.call}}
#' 
#' @name collectArgs-and-iterateWithArgs
#' @examples
#' sample_function <- function(x, base, thresh=500, verbose=TRUE) {
#' 
#'   some_object    <- is.na(x) ## an example of an object that we will exclude
#'   another_object <- 1:10     ## an example of an object that we will exclude
#' 
#'   if (length(x) > 1) {
#'     return(iterateWithArgs(x, FUNC=sample_function, except=c("some_object", "another_object")))
#'   }
#' 
#'   ret <- (base ^ x)
#' 
#'   if (verbose)
#'     cat(base, "^", x, " is ", ifelse(ret > thresh, "", "NOT "), "larger than ", thresh, "\n")
#' 
#'   return(ret)
#' }
#' 
#' sample_function(5, base=2)
#' sample_function(5:10, base=2)
#' 
#' 
#'  some_function <- function(x, param1, param2, etc, ...) {
#' 
#'    ARGS <- collectArgs(except="x")
#'    return(
#'            lapply(x, function(x_i) 
#'               do.call(some_function, c(ARGS, x=x_i))
#'            )
#'          )
#'  }
NULL
