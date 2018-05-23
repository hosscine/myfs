
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

#' Plot databars for matrix visualization
#'
#' @param X target matrix.
#' @param grid if \code{T}, drawing grid lines that separete each cells of matrix.
#'
#' @export
#'
#' @examples
#' databar(matrix(1:9, 3, 3))
#'
databar <- function(X, grid = T){
  X.normalize <- round(X / max(X), 2) * 100

  bar.width.margin <- 0.1 / ncol(X)
  bar.height.margin <- 0.1 / nrow(X)

  plot(X, xlim = c(0.5, ncol(X) + 0.5), ylim = c(nrow(X) + 0.5, 0.5),
       type = "n", axes = F, ann = F)
  axis(side = 1, at = 1:ncol(X), labels = colnames(X))
  axis(side = 2, at = 1:nrow(X), labels = rownames(X), las=T)

  if (grid) {
    sapply(0:ncol(X), function(v) abline(v = v + 0.5))
    sapply(0:nrow(X), function(h) abline(h = h + 0.5))
  }

  # drawing bars
  for (x in 1:ncol(X)) {
    for (y in 1:nrow(X)) {
      if (X.normalize[y, x] != 0)
        plotrix::gradient.rect(xleft = x - 0.5 + bar.width.margin,
                               ybottom = y - 0.5 + bar.height.margin,
                               xright = x - 0.5 + (1 - bar.width.margin) * X.normalize[y, x] / 100,
                               ytop = y + 0.5 - bar.height.margin,
                               col = grDevices::cm.colors(100)[1:X.normalize[y, x]])
      graphics::text(x, y, labels = round(X[y, x], 2))
    }
  }
}
