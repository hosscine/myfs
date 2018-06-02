writeEllipsis <- function(..., append = NULL, warn = T, soft = F){
  assert_that(is.logical(warn))
  if(is.logical(append)) stop("argment append requires over/softwrited ellipsis")

  dots <- list(...)
  if(!is.null(append)) dots <- c(dots, append)
  dots.names <- names(dots)
  unique.names <- unique(dots.names)

  # flags of conflicting with the fixed argments
  conflict.flags <- logical(length(unique.names))
  # index of ellipsis that is used for the returned args
  dots.use.index <- numeric(length(unique.names))

  for (i in 1:length(unique.names)) {

    hit.index <- which(dots.names == unique.names[i])

    if(length(hit.index) > 1) conflict.flags[i] <- TRUE

    if(soft) dots.use.index[i] <- min(hit.index)
    else dots.use.index[i] <- max(hit.index)

  }

  if(warn && T %in% conflict.flags)
    if(soft) warning("arguments ", paste(unique.names[conflict.flags],"",collapse = "and "),
                     "conflicted with recommeded argments")
    else warning("arguments ", paste(unique.names[conflict.flags],"",collapse = "and "),
                 "is fixed")

  return(dots[dots.use.index])
}

#' Overwrite some fixed arguments to the arguments \code{...} given by user call.
#'
#' When ellipsis arguments so called \code{...} is given by user call,
#' frequently we want to overwrite some arguments as fixed desirable arguments.
#' This function helps to settles the situation simply.
#'
#' @param ... fixed and given arguments.
#' @param append argments list to be appended.
#' @param warn if \code{T}, warnings when fixed and argments given to user call conflict.
#'
#' @importFrom assertthat assert_that
#'
#' @return overwrited ellipsis.
#' @seealso \code{\link{softwriteEllipsis}}
#' @export
#'
#' @examples
#' # plot function with xlim = c(0, 2 * pi) and col = "red"
#' plot2pired <- function(x, ...){
#'   elp <- overwriteEllipsis(..., x = x, xlim = c(0, 2 * pi), col = "red")
#'   do.call(plot, elp)
#' }
#'
#' plot2pired(sin)
#'
#' # this example is warned due to conflict argments
#' plot2pired(sin, main = "sin curve", xlim = c(-pi, pi))
#'
overwriteEllipsis <- function(..., append = NULL, warn = T)
  writeEllipsis(..., warn = warn, soft = F, append = append)

#' Overwrite some desirable arguments to \code{...} softly.
#'
#' This function is minor modificated version of \code{\link{overwriteEllipsis}}.
#' The only difference is that don't overwrite conflict argments.
#'
#' @param ... fixed and given arguments.
#' @param append argments list to be appended.
#' @param warn if \code{T}, warnings when fixed and given argments conflict.
#'
#' @return overwrited ellipsis.
#' @seealso \code{\link{overwriteEllipsis}}
#' @export
#'
#' @examples
#' # plot function with xlim = c(0, 2 * pi) and col = "red" defaultly
#' plot2pi <- function(x, ...){
#'   elp <- overwriteEllipsis(..., x = x, xlim = c(0, 2 * pi))
#'   elp <- softwriteEllipsis(..., append = elp, col = "red")
#'   do.call(plot, elp)
#' }
#'
#' # this example is evaluated as col = "red"
#' plot2pi(sin)
#'
#' # this example is evaluated as col = "blue"
#' plot2pi(sin, col = "blue")
#'
softwriteEllipsis <- function(..., append = NULL, warn = F)
  writeEllipsis(..., warn = warn, soft = T, append = append)
