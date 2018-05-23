#dimnamesを表示するimageです
#color.nonlinerは0の値だけを灰色に表示します
#' Improved \code{image} function
#'
#' This function show \code{matrix} with the direction likes \code{print}.
#' In addition, adds the color bar and labels of axis from dimnames.
#'
#' @param X target matrix.
#' @param trans if \code{T}, plots with transformation.
#' @param col.rainbow if \code{T}, uses rainbow color.
#' @param col.gray0 if \cdoe{T}, the color of cells that values are 0 is set to gray.
#' @param bar.mar margin of color bar.
#'
#' @importFrom colorRamps matlab.like
#' @importFrom colorRamps blue2red
#' @importFrom fields image.plot
#' @importFrom grDevices gray.colors
#'
#' @export
#'
image2 <- function(X, trans = F, col.rainbow = T, col.gray0 = F,bar.mar = 2){
  require(fields,warn.conflicts = F)
  require(colorRamps,warn.conflicts = F)
  sp <- length(unique(X))
  rng <- round(range(X),2)
  if(is.null(rownames(X))) rn <- 1:nrow(X)
  else rn <- rownames(X)
  if(is.null(colnames(X))) cn <- 1:ncol(X)
  else cn <- colnames(X)

  if(col.rainbow) clr <- matlab.like(sp)
  else clr <- blue2red(sp)
  if (col.gray0) clr[1] <- gray.colors(20)[2]

  if (trans) {
    image(1:ncol(X),1:nrow(X),t(apply(t(X),1,rev)),xaxt="n",yaxt="n",xlab = "",ylab = "",col = clr)
    grid(ncol(X),nrow(X),lty = 1)
    axis(side = 1,at = 1:ncol(X),labels = cn,cex.axis=0.8)
    axis(side = 2,at = 1:nrow(X),labels = rev(rn),las=T,cex.axis=0.8)
  }
  else{
    image(1:nrow(X),1:ncol(X),t(apply(X,1,rev)),xaxt="n",yaxt="n",xlab = "",ylab = "",col = clr)
    grid(nrow(X),ncol(X),lty = 1)
    axis(side = 1,at = 1:nrow(X),labels = rn,cex.axis=0.8)
    axis(side = 2,at = 1:ncol(X),labels = rev(cn),las=T,cex.axis=0.8)
  }
  image.plot(legend.only=T,zlim=c(rng[1],rng[2]),col=clr,horizontal = F,verbose = F,legend.mar = bar.mar)
  # SetMargin()
}

# 行列の実測値を見やすいデータバーを表示します
#' Title
#'
#' @param X
#' @param grid
#'
#' @return
#' @export
#'
#' @examples
databar <- function(X, grid = T){
  require(plotrix)

  m <- max(X)
  normX <- round(X / m,2)*100

  x <- 1:ncol(X)
  xl <- length(x)
  y <- 1:nrow(X)
  yl <- length(y)
  xmar <- 0.1 / xl
  ymar <- 0.1 / yl

  plot(X, xlim=c(0.5,x[xl]+0.5), ylim=c(y[yl]+0.5,0.5), type = "n", axes = F, ann = F)
  axis(side = 1, at = x, labels = colnames(X))
  axis(side = 2, at = y, labels = rownames(X), las=T)
  if (grid) {
    sapply(c(0,x), function(v)abline(v=v+0.5))
    sapply(c(0,y), function(h)abline(h=h+0.5))
  }

  for (x in 1:xl) {
    for (y in 1:yl) {
      if (normX[y,x]!=0) gradient.rect(xleft = x-0.5+xmar, ybottom = y-0.5+ymar,
                                       xright = x-0.5+(1-xmar)*normX[y,x]/100, ytop = y+0.5-ymar,
                                       col = cm.colors(100)[1:normX[y,x]])
      text(x,y,labels = round(X[y,x],2))
    }
  }
}

#行列の各行を一つの系列と見なしてline plotします
plot.lines <- function(X,legend.locale="topleft"){
  ylim <- range(X)
  plot(1,xlim = c(1,ncol(X)),ylim = ylim,type = "n")
  for (n in 1:nrow(X)) lines(X[n,],col=n)
  if(!is.null(rownames(X)))legend(legend.locale,legend = rownames(X),col=1:nrow(X),pch = 3)
}

# 軸やラベルを消去したプロット3dです
planePlot3d <- function(X,...){
  require(rgl)
  plot3d(X,xlab = "",ylab = "",zlab = "",axes = F,...)
}

