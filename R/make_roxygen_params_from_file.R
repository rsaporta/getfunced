file_full_path <- "~/Development/rpkgs/getfunced/test_file_with_matching_funcs.R"
make_param_docs_from_file <- function(file_full_path) {
  formals_as_list <- get_functions_and_formals(file_full_path, TRUE)
  parameters_with_defaults <- lapply(formals_as_list, capture_output_of_formals_of_one_function)


  FUNCTIONS  <- names(formals_as_list)
  PARAMETERS <- lapply(formals_as_list, names) %>% unlist(use.names = FALSE)
  DEFAULTS   <- unlist(parameters_with_defaults, use.names = FALSE)

  function_param_lookup <- lapply(names(formals_as_list), function(nm) rep(nm, length(formals_as_list[[nm]]))) %>% 
                            unlist(use.names=FALSE) %>% 
                            sprintf("    [in \\code{%s}]", .)


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
      main <- inds[[1L]]
      other <- inds[-1L]
      indent <- rep(" ", nchar(param)) %>% paste0(collapse="")
      DEFAULTS[main] %<>% paste0(function_param_lookup[main])
      DEFAULTS[other] %<>% paste0(indent, .)
      DEFAULTS[main]  <- paste(DEFAULTS[inds], collapse="\n\n")
      # DEFAULTS[other]  <- not_using
    }
  }


&&&&&&&&&&&&&&&&&&&&&&&&&&& THIS ISNT WORKING --- function tags at end --- why not?  
look at wrongly shared  ---- 

  if (length(to_drop)) {
    DEFAULTS <- DEFAULTS[-to_drop]
    PARAMETERS <- PARAMETERS[-to_drop]  
    rm(to_drop)  ## for when running interactively, no accidents
  }

  catn(paste(PARAMETERS, " : ", DEFAULTS))

  ## REMOVE DUPLICATE PARAMS / DEFAULTS
  {
    dups <- duplicated(PARAMETERS) & duplicated(DEFAULTS)
    PARAMETERS %<>% {.[!dups]}
    DEFAULTS   %<>% {.[!dups]}
    function_param_lookup   %<>% {.[!dups]}
    rm(dups) ## for when running interactively, no accidents
  }

  ## IDENTIFY DUPLICATE JUST IN THE PARAMETERS NAME, BUT DIFFERENT DEFAULT VALUE
  still_dup <- duplicated(PARAMETERS)
  for (param in PARAMETERS[still_dup]) {
    inds <- which(PARAMETERS == param)
    main <- inds[[1L]]
    other <- inds[-1L]
    indent <- rep(" ", nchar(param)) %>% c("\n", .) %>% paste0(collapse="")
    DEFAULTS[other] %>% paste0(indent, .)

  }

  paremeters <- names(parameters_with_defaults)
  unlist(parameters, use.names = FALSE)
}
