#' Print variable without inconsiderable attributes.
#'
#' @param x printed variable.
#' @param keeps names of the considerable attributes.
#' @param ... arbitrary argments for print.
#'
#' @export
#'
print.noattr <- function(x, keeps = c("names", "row.names", "class", "dim", "dimnames"), ...) {
  attributes(x) <- attributes(x)[keeps]
  print(x, ...)
}

#' Prints variables or expressions  for debug.
#'
#' @param ... printed objects.
#'
#' @export
#' @examplesã€\u0080
#' f <- function(x,y){
#'   debugText(x^y,y)
#'   return(x^y+y)
#' }
#' f(4,6)
#'
debugText <- function(...) {
  key <- as.list(substitute(list(...)))[-1L]
  val <- list(...)
  mapply(function(k, v) {
    if (typeof(k) == "language") {
      cat(as.character(k), "(", class(k)[1], ") = ", sep = "")
    } else cat(k, "(", class(k)[1], ") = ", sep = "")
    
    if (!is.matrix(v) && (is.logical(v) || is.numeric(v) || is.complex(v) || is.character(v))) 
      cat(v, "\n") else {
      cat("\n")
      print(v)
      cat("\n")
    }
  }, key, val)
  cat("\n")
}
