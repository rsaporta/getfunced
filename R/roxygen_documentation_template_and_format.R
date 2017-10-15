# ========================================================== #
#    roxygen documentation template and format.R             #
# ========================================================== #
#
if (FALSE)
devtools::document("/Users/rsaporta/Development/rpkgs/getfunced/")

#' Roxygen Documentation Template and Format
#'
#' ONE-LINER WHAT DO THESE GROUP OF FUNCS DO? (or the name of the main function)
#'
#' DETAILED DESCRIPTION of what these functions do
#'
#' @name roxygen_documentation_template_and_format
#'
#X## -------------------------------  PARAMS  ------------------------------- ##
#' @param x <DESCRIBE ME>
#'          <WHAT IS IT?>
#'
#' @param tick <DESCRIBE ME>
#'             <WHAT IS IT?>
#'
#'             Defaults to: "#' "
#'
#' @param clear_multiple_lines <DESCRIBE ME>
#'                             <WHAT IS IT?>
#'
#'                             Defaults to: FALSE
#'
#' @param at_least_reps <DESCRIBE ME>
#'                      <WHAT IS IT?>
#'
#'                      Defaults to: 2L
#'
#' @param ... <DESCRIBE ME>
#'            <WHAT IS IT?>
#'
#' @param sep <DESCRIBE ME>
#'            <WHAT IS IT?>
#'
#'            Defaults to: "\n"
#'
#' @param file_full_path <DESCRIBE ME>
#'                       <WHAT IS IT?>
#'
#' @param one_liner <DESCRIBE ME>
#'                  <WHAT IS IT?>
#'
#'                  Defaults to: "ONE-LINER WHAT DO THESE GROUP OF FUNCS DO? (or the name of the main function)"
#'
#' @param detailed_desc <DESCRIBE ME>
#'                      <WHAT IS IT?>
#'
#'                      Defaults to: "DETAILED DESCRIPTION of what these functions do"
#'
#' @param other_sections <DESCRIBE ME>
#'                       <WHAT IS IT?>
#'
#'                       Defaults to: list(section_name = \"text paragraph\")
#'
#' @param aliases <DESCRIBE ME>
#'                <WHAT IS IT?>
#'
#'                Defaults to: c()
#'
#' @param export_func <DESCRIBE ME>
#'                    <WHAT IS IT?>
#'
#'                    Defaults to: "..auto.."
#'
#' @param return <DESCRIBE ME>
#'               <WHAT IS IT?>
#'
#'               Defaults to: "WHAT IS RETURNED??"
#'
#' @param example_text <DESCRIBE ME>
#'                     <WHAT IS IT?>
#'
#'                     Defaults to: "\n"
#'
#' @param filename <DESCRIBE ME>
#'                 <WHAT IS IT?>
#'
#'                 Defaults to: basename(file_full_path)
#'
#' @param title <DESCRIBE ME>
#'              <WHAT IS IT?>
#'
#'              Defaults to: filename
#'
#' @param header <DESCRIBE ME>
#'               <WHAT IS IT?>
#'
#'               Defaults to: filename
#'
#' @param okay_to_source.safety_flag <DESCRIBE ME>
#'                                   <WHAT IS IT?>
#'
#'                                   Defaults to: FALSE
#'
#' @param min_width <DESCRIBE ME>
#'                  <WHAT IS IT?>
#'
#'                  Defaults to: 62L
#'
#' @param left_pad <DESCRIBE ME>
#'                 <WHAT IS IT?>
#'
#'                 Defaults to: 3L
#'
#' @param collapse <DESCRIBE ME>
#'                 <WHAT IS IT?>
#'
#'                 Defaults to: "\n"
#X## ------------------------------------------------------------------------ ##
#'
#' @return
#' WHAT IS RETURNED??
#'
#' @examples
#'
#'
NULL


# framework borrowed from: 
# https://gist.github.com/jefferys/b79fe87314b0dc72fec9

# ### PARAM FORMAT

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ### WITH DEFAULT VALUE
# #' @param param_name param_description.  
# #'        WHAT IS IT?
# #' 
# #'        Default is \code{as.character(substitute(arg_to_iterate_over))}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ### WITH NO DEFAULT
# #' @param param_name param_description.  
# #'        WHAT IS IT?
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# #==========================================================
# # $FILENAME$
# #==========================================================

# #' TITLE
# #' 
# #' ONE-LINER WHAT IT DOES
# #' 
# #' DETAILED DESCRIPTION
# #'
# #' @SECTION-NAME text
# #' 
# #' @name 
# #' @aliases 
# #' 
# #' @param one  ## GRAB THE SHARED PARAMS HERE
# #' @param two 
# #'
# #' @return 
# #'
# #' @examples
# #' 
# NULL

# 
# ## TEMPLATE FOR FUNCTIONS
# #' @rdname NAME
# #' @importFrom magrittr %>%
# #' @export
# 

# if (FALSE) {
#   file_full_path <- "/Users/rsaporta/Development/rpkgs/getfunced/roxygen documentation template and format.R"
#   filename <- basename(file_full_path)

#   title=basename(file_full_path)
#   left_pad = 3

#   example_text <- "/Users/rsaporta/Development/rpkgs/getfunced/example_file.R"
# }

# # 


#' @rdname roxygen_documentation_template_and_format
#' @importFrom magrittr %>%
#X## @export
catn <- function(..., sep="\n")
cat(..., sep="\n")

# DEVING <- TRUE

#' @rdname roxygen_documentation_template_and_format
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
make_group_documentation <- function(
    file_full_path
  , one_liner       = "ONE-LINER WHAT DO THESE GROUP OF FUNCS DO? (or the name of the main function)"
  , detailed_desc   = "DETAILED DESCRIPTION of what these functions do"
  , other_sections  = list(section_name = "text paragraph")
  , aliases         = c()
  , export_func     = "..auto.."
  , return          = "WHAT IS RETURNED??"
  , example_text    = "\n"
  , filename        = basename(file_full_path)
  , title           = filename
  , header          = filename
  , okay_to_source.safety_flag = FALSE
) {
  if (length(file_full_path) != 1 || !is.character(file_full_path) || !nzchar(file_full_path))
    stop("invalid input for file_full_path --  It should be a character string of length 1")
  if (!file.exists(file_full_path))
    stop("file '", file_full_path, "' does not exist")

  .confirm_is_string_of_length1(header)
  .confirm_is_string_of_length1(title)
  .confirm_is_string_of_length1(filename)
  .confirm_is_string_of_length1(one_liner)
  .confirm_is_string_of_length1(detailed_desc)

  if (!is.list(other_sections) ||  is.null(names(other_sections)) || !nzchar(names(other_sections)))
    stop("other_sections should be a named list of strings")

  header %<>% neat_box %>% paste0("\n# ")

  if (missing(title)) {
    title %<>% gsub(pattern="\\.[R|r]$", replace="") %>% title_case_ap_style()
  }
  # title %<>% gsub(pattern=" ", "_")

  name <- tolower(title) %>% gsub("\\s", "_", x=.) %>% paste("@name", .)
  rdname <- sub("^@name", "@rdname", x=name)

  if (file.exists(example_text))
    example_text <- readLines(example_text)

  section_text <- ""
  if (!identical(other_sections, list(section_name = "text paragraph"))) {
      section_text <- sprintf("@%s\n          %s", names(other_sections), as.character(unlist(other_sections, use.names = FALSE))) %>% 
                        paste(collapse="\n\n")
  }

  PARAMS_DOC_GENERAL <- make_param_docs_from_file(file_full_path, okay_to_source.safety_flag=okay_to_source.safety_flag)

  MAIN <- paste(sep="\n"
      , title
      , ""
      , one_liner
      , ""
      , detailed_desc
      , ""
      , section_text
      , ""
      , name
      , ""
      , "## -------------------------------  PARAMS  ------------------------------- ##"
      , PARAMS_DOC_GENERAL
      , "## ------------------------------------------------------------------------ ##"
      , ""
      , "@return"
      , return
      , ""
      , "@examples"
      , example_text
      , ""
    ) %>% 
    add_roxygen_ticks(clear_multiple_lines=TRUE) %>% 
    paste(header, ., "NULL", sep="\n")

  catn(MAIN)

  FUNCS <- get_functions_and_formals(file_full_path, okay_to_source.safety_flag=okay_to_source.safety_flag) %>% 
              names()
  
  if (identical(export_func, "..auto.."))
    export_func <- !grepl("^\\.", FUNCS)
  else if (!is.logical(export_func))
    stop("Further export_func implementation remains as a TODO -- for now options are simply TRUE/FALSE or \"..auto..\"")
  if (any(is.na(export_func)))
    stop("export_func cannot be nor contain NAs")


# &&&&&&&&&&&&&&&&&&& LEFT OFF HERE
#   ## TODO:  use 
#   #    formals_as_list <- get_functions_and_formals(file_full_path, TRUE)
#   # and read the function name to determine whether to export or not.
#   # if not export, preface with a '#'?
# &&& ???  export_func &&&&&&&&
  FUNC_PART <- paste(sep="\n"
      , rdname
      , "@importFrom magrittr %>%"
      , paste0(ifelse(export_func, "", "# "), "@export")
    ) %>% 
    add_roxygen_ticks(clear_multiple_lines=TRUE) %>% 

  catn("\n##   ~~~~~~~~~~~~~~~~~~~\n")

  for (i in seq_along(FUNC_PART)) {
    catn("\n---------------------------------")
    catn("            -= ", FUNCS[[i]], " =- ")
    catn(FUNC_PART[[i]])
    catn("---------------------------------")
  }
  # catn(FUNC_PART)

  return(list(MAIN, FUNC_PART))
}
if (FALSE) {

  # make_group_documentation(file_full_path, okay_to_source.safety_flag=TRUE)
  "~/Development/rpkgs/getfunced/R/roxygen documentation template and format.R" %>% 
  make_group_documentation(okay_to_source.safety_flag=TRUE)
}

#' @rdname roxygen_documentation_template_and_format
#' @importFrom magrittr %>%
#' @export
add_roxygen_ticks <- function(x, tick="#' ", clear_multiple_lines=FALSE, at_least_reps=2L, comment_space="X") {
  pat.comment  <- paste0("^", tick, "\\s*#")
  repl.comment <- paste0("#", comment_space, "#")

  # ret <- strsplit(x, "\\n") %>%
  #         vapply(function(x_i) paste0(tick, x_i, collapse="\n"), character(1L))
  ret <- strsplit(x, "\\n") %>%
          vapply(function(x_i) {
              paste0(tick, x_i) %>% 
              gsub(pattern=pat.comment, replace=repl.comment) %>%
              paste0(collapse="\n")
          }, FUN.VALUE=character(1L))

  if (clear_multiple_lines) {
    pat <- tick %>% rep(at_least_reps) %>% paste0("\n", collapse = "")
    rep <- tick %>% paste0("\n", collapse = "")
    safety.no_infinite_loop <- 100
    while(grepl(pat, ret) && safety.no_infinite_loop) {
      ret %<>% gsub(pattern=pat, replace=rep)
      safety.no_infinite_loop %<>% "-"(1)
    } ## // while-loop
  } ## // if clause



  return(ret)
} 

#' @rdname roxygen_documentation_template_and_format
#' @importFrom magrittr %>%
#X## @export
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

