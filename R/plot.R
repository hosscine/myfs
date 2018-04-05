
#' Changes margins for plot
#'
#' @param top.4 maring of top. default is 4.
#' @param left.4 margin of left. defualt is 4.
#' @param bottom.5 margin of bottom. default is 5.
#' @param right.2 margin of right. default is 2.
#'
#' @export
#'
setMargin <- function(top.4 = 4, left.4 = 4, bottom.5 = 5, right.2 = 2)
  graphics::par(mar=c(bottom.5, left.4, top.4, right.2))

#' Reset the graphics parameter mfrow as defalt.
#'
#' @importFrom graphics par
#'
#' @export
#'
resetMfrow <- function() par(mfrow = c(1, 1))
