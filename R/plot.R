
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
  graphics::par(mar = c(bottom.5, left.4, top.4, right.2))

#' Changes margins to zeros
#'
#' @export
#'
setMarginZero <- function()
  graphics::par(mar = c(0, 0, 0, 0))

#' Reset the graphics parameter mfrow as defalt.
#'
#' @importFrom graphics par
#'
#' @export
#'
resetMfrow <- function() par(mfrow = c(1, 1))

#' Improved \code{image} function for matrix visualization
#'
#' This function show \code{matrix} with the direction likes \code{print}.
#' In addition, adds the color bar and labels of axis from dimnames.
#'
#' @param X target matrix.
#' @param trans if \code{T}, plots with transformation.
#' @param col.rainbow if \code{T}, uses rainbow color.
#' @param col.gray0 if \code{T}, the color of cells where values are 0 is set to gray.
#' @param bar.mar margin of color bar.
#'
#' @importFrom colorRamps matlab.like
#' @importFrom colorRamps blue2red
#' @importFrom fields image.plot
#' @importFrom grDevices gray.colors
#'
#' @export
#'
image2 <- function(X, trans = F, col.rainbow = T, col.gray0 = F, bar.mar = 2){
  if(is.null(rownames(X))) rn <- 1:nrow(X)
  else rn <- rownames(X)
  if(is.null(colnames(X))) cn <- 1:ncol(X)
  else cn <- colnames(X)

  if(col.rainbow) clr <- unique(X) %>% length %>% colorRamps::matlab.like
  else clr <- unique(X) %>% length %>% colorRamps::blue2red
  if (col.gray0) clr[1] <- grDevices::gray.colors(20)[2]

  if (trans) {
    graphics::image(1:ncol(X), 1:nrow(X), t(apply(t(X), 1, rev)),
                    xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = clr)
    graphics::grid(ncol(X), nrow(X), lty = 1)
    graphics::axis(side = 1, at = 1:ncol(X), labels = cn, cex.axis = 0.8)
    graphics::axis(side = 2, at = 1:nrow(X), labels = rev(rn), las = T, cex.axis = 0.8)
  }
  else {
    graphics::image(1:nrow(X), 1:ncol(X), t(apply(X, 1, rev)),
                    xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = clr)
    graphics::grid(nrow(X), ncol(X), lty = 1)
    graphics::axis(side = 1, at = 1:nrow(X), labels = rn, cex.axis = 0.8)
    graphics::axis(side = 2, at = 1:ncol(X), labels = rev(cn), las = T, cex.axis = 0.8)
  }
  image.plot(legend.only = T, zlim = c(round(max(X), 2), round(min(X), 2)), col = clr,
             horizontal = F, verbose = F, legend.mar = bar.mar)
}
