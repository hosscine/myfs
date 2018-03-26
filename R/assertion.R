#' Asserts \code{x} whether probabic number or not.
#'
#' @param x object to test.
#'
#' @importFrom assertthat on_failure is.number
#'
#' @export
#'
is.prob <- function(x) assertthat::is.number(x) && x >= 0 && x <= 1
assertthat::on_failure(is.prob) <- function(call, env){
  paste0(deparse(call$x), " is not a probabic number (a length one numeric vector within [0, 1]).")
}


#' Asserts \code{x} whether numeric range or not.
#'
#' @param x object to test.
#'
#' @export
#'
is.range <- function(x){
  if(!is.numeric(x) || !length(x) == 2) return(FALSE)
  x[1] < x[2]
}
assertthat::on_failure(is.range) <- function(call, env){
  paste0(deparse(call$x), " is not a range vector (a length two numeric vector satisfing x[1] < x[2]).")
}
