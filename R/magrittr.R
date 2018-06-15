#' assign function for pipe stream
#'
#' @param value a value to be assigned to \code{x}.
#' @param x a variable name.
#' @param function_list a list of functions.
#' @param envir the environment to use.
#'
#' @importFrom magrittr freduce
#'
#' @return \code{value} with applied function_list.
#' @export
#'
#' @examples
#' x <- 1:4
#' x.sum <- x %>% passign(x.sqrt, function_list = list(sqrt)) %>% sum
#'
#' print(x.sum)
#' print(x.sqrt)
#'
passign <- function(value, x, function_list = list(), envir = globalenv()){
  if (length(function_list) == 0)
    assign(x = as.character(substitute(x)), value = value, envir = envir)
  else {
    new.value <- freduce(value, function_list)
    assign(x = as.character(substitute(x)), value = new.value, envir = envir)
  }
  return(value)
}

#' magrittr operator with giving nothing to first argment
#'
#' @param lhs a value or the magrittr placeholder.
#' @param rhs a function call using the magrittr semantics.
#'
#' @export
#'
#' @examples
#' # this is equivalent to `2 %>% {rep(5, .)} %>% sqrt`
#' 2 %.% rep(5, .) %>% sqrt
#'
'%.%' <- function(lhs, rhs){
  . <- lhs
  key <- as.list(substitute(list(rhs)))[-1]
  return(eval(parse(text = key)))
}
