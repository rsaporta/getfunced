title_case_ap_style <- function(x) {
  
  x <- gsub( "[\\s_]+", " ", x, perl=TRUE )  # whitespace to single space
 
  words_not_to_cap <- c("A", "An", "And", "At", "But", "By", "For", "In", "Nor", "Of", "On", "Or", "So", "The", "To", "Up")

  ## we will err on the side of YES capitalizing these. 
  tricky_words <- c("Yet")  
  ## yet *should* be capitalized when used as an adverb. 
  ##    eg:   "We Haven't Found Them Yet"
  ## yet *should not* be capitalized when used as a conjunction
  ##    eg:   "We Tried and yet We Haven't Found Them Yet"

  regex_no_cap <- words_not_to_cap %>% paste(collapse="|") %>% paste0("( (", ., ")\\b)")

  x <- gsub( "\\b([a-z])([a-z]+)", "\\U\\1\\L\\2" ,x, perl=TRUE ) %>% 
        gsub(pat=regex_no_cap, replace="\\L\\1", perl=TRUE)

  return(x)
}


.confirm_is_string_of_length1 <- function(string, nm_for_err_msg, empty_string_ok=FALSE) {
  if (missing(nm_for_err_msg)) {
    nm_for_err_msg <- as.character(substitute(string))
  }

  if (length(string) != 1)
    stop("Invalid input for '", nm_for_err_msg, "'.  It should be a non-empty string of length 1 -- it has length ", length(string))
  if (!is.character(string))
    stop("Invalid input for '", nm_for_err_msg, "'.  It should be a non-empty string of length 1 -- it is an object of class \"", class(string)[[1L]], "\"")
  if (!empty_string_ok && !nzchar(string))
    stop("Invalid input for '", nm_for_err_msg, "'.  It should be a non-empty string of length 1 -- it is an empty string.")

  return(invisible(TRUE))
}
