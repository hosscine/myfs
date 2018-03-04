#' Overwrite some desirable arguments to the valiability arguments \code{...}.
#'
#' When ellipsis arguments is given from the parents function call as \code{...},
#' frequently we want to overwrite some arguments as fixed desirable arguments.
#' This function settles the situation simply.
#'
#' @param ... fixed and given arguments.
#' @param warn if \code{T}, warnings when fixed and given argments conflict.
#'
#' @importFrom assertthat assert_that
#'
#' @return overwrited ellipsis.
#' @export
#'
#' @examples
#' plot2pi <- function(x, ...){
#'   ellipsis <- overwriteEllipsis(..., x = x, xlim = c(0, 2*pi), col = "red")
#'   do.call(plot, ellipsis)
#' }
#'
#' plot2pi(sin)
#'
#' # this example is warned
#' plot2pi(sin, main = "sin curve", xlim = c(-pi, pi))
#'
overwriteEllipsis <- function(..., warn = T){
  assert_that(is.logical(warn))

  dots <- list(...)
  dots.names <- names(dots)
  unique.names <- unique(dots.names)

  # flags of conflicting with the fixed argments
  conflict.flags <- logical(length(unique.names))
  # index of ellipsis that is used for the returned args
  dots.use.index <- numeric(length(unique.names))

  for (i in 1:length(unique.names)) {

    hit.index <- which(dots.names == unique.names[i])

    if(length(hit.index) > 1) conflict.flags[i] <- TRUE

    dots.use.index[i] <- max(hit.index)

  }

  if(warn && T %in% conflict.flags)
    warning("arguments ", paste(unique.names[conflict.flags],"",collapse = "and "), "is fixed")

  return(dots[dots.use.index])
}
