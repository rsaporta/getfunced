if (FALSE) {
  library(magrittr)
  library(colorout)
  options(width = 101)
  file <- "/Users/rsaporta/Development/rpkgs/rcreds/R/helper_functions.R"
  ret <- get_funced(file)
  for (r in ret) {
    cat("\n")
    cat("|-------------------------------------------------------------|\n")
    cat(r, "\n")
    cat("|-------------------------------------------------------------|\n\n")
  }
}
  

#' @importFrom tools toTitleCase
#' @importFrom rsugeneral topropper
get_funced <- function(file) {

  ret <- mapply(format_function, names(FRMS), FRMS)

  rm(e)
  return(ret)
}


get_functions_and_formals <- function(file) { 
  e <- new.env()
  source(file=file, local=e)
  
  objects <- ls(envir=e, all=TRUE)
  names(objects) <- objects

  functions <- lapply(objects, function(x) if (is.function(get(x, envir=e))) get(x, envir=e))

  formals_as_list <- lapply(functions, formals)

  return(formals_as_list)
}

#' @importFrom magrittr %<>%
#' @importFrom magrittr %>%
#' @importFrom stats setNames
format_function <- function(func_nm, formals_as_list, max_wdith=88, multi_line="auto") {

  if (length(formals_as_list) && !is.list(formals_as_list))
    stop("formals_as_list should be a list --- ## TODO:  Can we simply coerce to list?")

  ## ------------------------------ ##
  ## validate multi_line
  ## ------------------------------ ##
  if (!length(multi_line) == 1)
    stop("'multi_line' should be either \"auto\" or a TRUE/FALSE flag of length 1.  It has length ", length(multi_line), " and is of class '", class(multi_line)[[1L]], "'")
  if (is.na(multi_line))
    multi_line <- "auto"
  if (!(multi_line %in% c(TRUE, FALSE)) &&  !(multi_line %in% "auto"))
    stop("'multi_line' should be either \"auto\" or a TRUE/FALSE flag of length 1.  It has length ", length(multi_line), " and is of class '", class(multi_line)[[1L]], "'")
  ## ------------------------------ ##

  line1 <- func_nm %>% sprintf(fmt="%s <- function(")
  closer <- ")"

  which_integers <- vapply(formals_as_list, function(x) is.integer(x) && length(x) == 1 && !is.na(x), FUN.VALUE = logical(1L))

  if (length(formals_as_list))
    params <- formals_as_list %>% {paste0(names(.), ifelse(nzchar(.), " = ", ""), ., ifelse(which_integers, "L", ""))}
  else
    params <- ""

  if (!(multi_line %in% TRUE))
    single_line_params <- paste(params, collapse=", ") %>% paste0(line1, ., closer)

  ## determine whether to use multi_line or not, by how long a single line would be
  if (multi_line %in% "auto")
    multi_line <- (length(params) > 1)  &&  (nchar(single_line_params) > max_wdith)

  if (multi_line %in% TRUE) {
    ret <- format_multiline(line1=line1, formals_as_list=formals_as_list, right_align_param_names=TRUE, closer=closer)
  } else {
    ret <- single_line_params
  }

  return(ret)
}


format_multiline <- function(line1, formals_as_list, left_pad=2, comma_front=TRUE, right_align_param_names=TRUE, closer=")", pad_closer=FALSE) {
  if (length(line1) != 1  ||  !is.character(line1))
    stop("line1 should be a string of length 1.  It has length ", length(line1), " and is of class '", class(line1)[[1L]], "'")

  lft_side <- names(formals_as_list)
  rgt_side <- capture_output_of_formals(formals_as_list)

  max_l <- lft_side %>% nchar() %>% max(na.rm=TRUE)
  max_r <- rgt_side %>% nchar() %>% max(na.rm=TRUE)

  fmts <- ifelse(nzchar(rgt_side)
            , yes = paste0("%", ifelse(right_align_param_names, "-", ""), max_l + 1, "s", " = ", "%-", max_r + 1, "s")
            ,  no = paste0("%", ifelse(right_align_param_names, "-", ""), max_l + 1, "s", "%s")
          )

  params <- sprintf(fmts, lft_side, rgt_side)

  if (comma_front) {
    commas <- (length(params) - 1) %>% rep(x=", ") %>% c("  ", .)
    params %<>% paste0(commas, .)
  } else {
    commas <- (length(params) - 1) %>% rep(x=",") %>% c(., "")
    params %<>% paste0(commas)
  }

  spacers <- ""
  if (is.numeric(left_pad) && left_pad > 0) {
    spacers <- rep(" ", times=left_pad) %>% paste0(collapse = "")
    if (pad_closer)
      closer  <- rep(" ", times=left_pad - 1) %>% c(closer) %>% paste0(collapse = "")
  }

  ret <- params %>% paste0(spacers, ., collapse="\n") %>% paste(line1, ., closer, sep="\n")

  return(ret)
}


capture_output_of_formals <- function(formals_as_list) {
  ## Would prefer to use capture.output as that is more ine line with the user's original writing
  ##   and better captures values such as `seq(from=1L, to=5L)`  or  `12345L`
  ## However, sometimes, the capture.output introduces artifacts. Especially with non-atmoic vectors
  out_via_co <- vapply(formals_as_list, capture.output, character(1L))
  out_via_ac <- as.character(formals_as_list)

  ## params with no defaults, and params whose default is the empty string look the same
  which_empty_string <- (out_via_co == "[1] \"\"")
  out_via_co[which_empty_string] <- "\"\""

  ## params with no defaults, and params whose default is the empty string look the same
  which_integers <- vapply(formals_as_list, function(x) is.integer(x) && length(x) == 1 && !is.na(x), FUN.VALUE = logical(1L))
  out_via_ac[which_integers] %<>% vapply(paste0, "L", FUN.VALUE = character(1L))

  ret <- ifelse(grepl("^\\s*\\[\\s*\\d\\s*\\]", out_via_co), out_via_ac, out_via_co)

  ## in case curious, when debugging
  if (FALSE) {
    diff_from_ac <- rgt_side != out_via_ac & !which_empty_string & !which_integers
    out_via_co[diff_from_ac]
    out_via_ac[diff_from_ac]

    diff_from_co <- rgt_side != out_via_co & !which_empty_string & !which_integers
    out_via_co[diff_from_co]
    out_via_ac[diff_from_co]
  }

  return(ret)
}

