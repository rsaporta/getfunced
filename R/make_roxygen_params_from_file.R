#' helper function to indent secondary lines of text, based on nchar of param
#' 
#' @param param the name of a function parameter whose description text will be indented in the documentation
#' 
#' (not exported)
indent_from_param <- function(param) {
  if (length(param) > 1)
    return(vapply(param, indent_from_param, FUN.VALUE = character(1L)))
  (nchar(param) + 8) %>% rep(x=" ") %>% paste0(collapse="")
}

#' Takes a file and creates a single, multi-paragraph string of all the params in an appropriate syntax
#' 
#' @param file_full_path The file which will be documented
#' @param okay_to_source.safety_flag A TRUE/FALSE flag. Setting to FALSE will cause function to terminate in error without reading in the file.
#' 
#' (not exported)
make_param_docs_from_file <- function(file_full_path, okay_to_source.safety_flag=FALSE) {
  formals_as_list <- get_functions_and_formals(file_full_path, okay_to_source.safety_flag=okay_to_source.safety_flag)
  parameters_with_defaults <- lapply(formals_as_list, capture_output_of_formals_of_one_function)

  FUNCTIONS  <- names(formals_as_list)
  PARAMETERS <- lapply(formals_as_list, names) %>% unlist(use.names = FALSE)
  DEFAULTS   <- unlist(parameters_with_defaults, use.names = FALSE) %>% 
                  gsub(pattern="\\\\", replacement="\\\\\\\\")  ## escape the double escape

  function_param_lookup <- lapply(names(formals_as_list), function(nm) rep(nm, length(formals_as_list[[nm]]))) %>% 
                            unlist(use.names=FALSE) %>% 
                            sprintf("    [in \\code{%s}]", .)


  defaults_to <- "Defaults to: "                          
  ## IDENTIFY DUPLICATE JUST IN THE PARAMETERS NAME, BUT DIFFERENT DEFAULT VALUE
  dup_param_name <- duplicated(PARAMETERS)
  to_drop <- c()
  # not_using <- "<!!! NOT USING !@#@#@#@ ~~~ !!!>"
  for (param in unique(PARAMETERS[dup_param_name])) {
    inds <- which(PARAMETERS == param)

    dup_defaults <- duplicated(DEFAULTS[inds])
    to_drop %<>% c(inds[dup_defaults])

    inds <- inds[!dup_defaults]

    if (length(inds) > 1) {
      indent <- indent_from_param(param)
      main <- inds[[1L]]
      other <- inds[-1L]
      DEFAULTS[main] %<>% paste0(function_param_lookup[main])
      DEFAULTS[other] %<>% paste0(indent, defaults_to, .)
      DEFAULTS[other] %<>% paste0(function_param_lookup[other])
      DEFAULTS[main]  <- paste(DEFAULTS[inds], collapse="\n\n")
      to_drop %<>% c(other)
    }
  }

  if (length(to_drop)) {
    DEFAULTS <- DEFAULTS[-to_drop]
    PARAMETERS <- PARAMETERS[-to_drop]  
    rm(to_drop)  ## for when running interactively, no accidents
  }

  indents_all <- indent_from_param(PARAMETERS)
  indented_defaults_to <- paste0("\n", indents_all, defaults_to)

  ## RETURN
  paste0(
    "@param ", PARAMETERS, " <DESCRIBE ME>", "\n",
              indents_all,  "<WHAT IS IT?>", "\n",
              ifelse(nzchar(DEFAULTS), indented_defaults_to, ""), DEFAULTS
  , collapse="\n\n")
  # %>% add_roxygen_ticks(clear_multiple_lines=TRUE)
}

