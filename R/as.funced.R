print.funced <- function(x, ...) {
  # cat(format_function(x, ...), sep="\n")
  
  lapply(x, cat, "", sep="\n")
  return(invisible())
}

"[.funced" <- function(x, i, ..., na_name=TRUE) {
  ret <- as.funced(base::"["(unclass(x), i, ...))
  return(as.funced(ret))
}


is.funced <- function(x) {
  inherits(x, "funced")
}

as.funced <- function(x) {
  if (is.null(names(x)))
    stop("x must be named")
  if (!is.character(unlist(x, use.names=FALSE, recursive = FALSE)))
    stop("x must be a vector or list of strings")

  class(x) <- c("funced", class(x))

  return(x)
}
