
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
#' @param col.rainbow if \code{T}, uses rainbow color.
#' @param col.gray0 if \code{T}, the color of cells where values are 0 is set to gray.
#'
#' @importFrom colorRamps matlab.like
#' @importFrom colorRamps blue2red
#' @importFrom fields image.plot
#' @importFrom grDevices gray.colors
#'
#' @export
#'
image2 <- function(X, col.rainbow = F, col.gray0 = F){
  if(is.null(rownames(X))) rn <- 1:nrow(X)
  else rn <- rownames(X)
  if(is.null(colnames(X))) cn <- 1:ncol(X)
  else cn <- colnames(X)

  if (col.rainbow) clr <- unique(X) %>% length %>% matlab.like
  else clr <- unique(X) %>% length %>% blue2red
  if (col.gray0) clr[1] <- grDevices::gray.colors(20)[2]

  fields::image.plot(1:ncol(X), 1:nrow(X), t(apply(t(X),1,rev)),
                     axes = F, ann = F, col = clr)
  graphics::grid(ncol(X), nrow(X), lty = 1)
  graphics::axis(side = 3, at = 1:ncol(X), labels = cn, cex.axis = 0.8)
  graphics::axis(side = 2, at = 1:nrow(X), labels = rev(rn), las = T, cex.axis = 0.8)
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

  X.normalize <- if (min(X) < 0){
    X.normalize <- X - min(X)
    round(X.normalize / max(X.normalize), 2) * 100
  }
  else round(X / max(X), 2) * 100

  bar.width.margin <- 0.05
  bar.height.margin <- 0.05

  graphics::plot(X, xlim = c(0.5, ncol(X) + 0.5), ylim = c(nrow(X) + 0.5, 0.5),
                 type = "n", axes = F, ann = F)
  graphics::axis(side = 1, at = 1:ncol(X), labels = colnames(X))
  graphics::axis(side = 2, at = 1:nrow(X), labels = rownames(X), las=T)

  if (grid) {
    sapply(0:ncol(X), function(v) graphics::abline(v = v + 0.5))
    sapply(0:nrow(X), function(h) graphics::abline(h = h + 0.5))
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

#' Plot matlix as a set of lines
#'
#' @param X target matrix.
#' @param legend.locale location of legend, for example, "topleft" or "bottomright"
#'
#' @export
#'
#' @examples
#' x <- seq(0, 2*pi, length.out = 100)
#' X <- rbind(sin(x), cos(x), sin(x) + cos(x))
#' rownames(X) <- c("sin(x)", "cos(x)", "sin(x) + cos(x)")
#'
#' plotLines(X, "topright")
#'
plotLines <- function(X, legend.locale="topleft"){
  ylim <- range(X)
  graphics::plot(0, xlim = c(1, ncol(X)), ylim = range(X), type = "n", ann = F)
  for(n in 1:nrow(X)) graphics::lines(X[n,], col = n)
  if(!is.null(rownames(X)))
    graphics::legend(legend.locale, legend = rownames(X), col=1:nrow(X), pch = 3)
}
