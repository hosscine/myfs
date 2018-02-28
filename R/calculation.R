#' Generates random natural numbers.
#'
#' @param n number of the random numbers.
#' @param min min of random numbers.
#' @param max max of random numbers.
#'
#' @importFrom stats runif
#'
#' @return random natural numbers.
#' @export
runifN <- function(n,min=1,max=10){
  if(min > max) stop("argment max is not larger than min.")
  if(!min-round(min)==0 || !max-round(max)==0) stop("argment max and min must be integer.")
  return(ceiling(runif(n,min=min-1,max = max)))
}

# rchoose2 <- function(n,r=2,min=1,max=10){
#   chose <- matrix(runifN(n,min = min,max = max),n,r)
#   chose[,2] <- chose[,2] + runifN(n,min = 1,max = max-1)
#   chose[,2] <- ifelse(chose[,2]>max,chose[,2]-max,chose[,2])
#   chose
# }

#' Calculates norm of \code{x}.
#'
#' @param x input numeric vector.
#'
#' @return norm of \code{x}.
#' @export
vnorm <- function(x) sqrt(sum(x^2))


#' Calculates norm for each row of \code{X}.
#'
#' @param X input numeric matrix.
#'
#' @return norm for each row of \code{X}.
#' @export
rowNorm <- function(X) sqrt(rowSums(X^2))

#' Calculates \code{X - a} with row first.
#'
#' R's default calculation of \code{X - a} is with col first likes \code{X[,1] - a}, \code{X[,2] - a}, ....
#' This function implements calclation with row first likes \code{X[1,] - a}, \code{X[1,] - a}, ....
#'
#' @param X Minused matrix.
#' @param a Minuses vector.
#'
#' @return \code{X - a} in row first.
#' @export
rowMinus <- function(X,a) t(t(X)-a)

#' Calculates \code{X \* a} with row first.
#'
#' R's default calculation of \code{X \* a} is with col first likes \code{X[,1] \* a}, \code{X[,2] \* a}, ....
#' This function implements calclation with row first likes \code{X[1,] \* a}, \code{X[1,] \* a}, ....
#'
#' @param X Timed matrix.
#' @param a Times vector.
#'
#' @return \code{X \* a} with row first.
rowTimes <- function(X,a) t(t(X)*a)

#' Replaces NA from matrix with maintaining the dimension.
#'
#' @param X input matrix.
#' @param replace value to be replaced for NA.
#'
#' @return NA removed matrix.
#' @export
replaceNA <- function(mat, replace = 0) return(ifelse(is.na(mat),replace,mat))

#' \code{order} function regards a duplication value as the same ranking.
#'
#' @param x input vector.
#' @param decreasing if \code{TRUE}, proceses in decreasing.
#'
#' @return order of \code{x}.
#' @export
order2 <- function(x, decreasing = F){
  v <- sort(unique(x))
  if(decreasing) v <- rev(v)
  o <- numeric(length(x))
  for (val in 1:length(v)) {
    o[which(x==v[val])] <- val
  }
  return(o)
}

#行列に２次元の窓状にアクセスします
window2d <- function(center,mat,win,radius=(nrow(win)-1)/2){
  if(missing(win)&&missing(radius)) stop("win or radius is needed")
  tempind <- cbind(center-radius,center+radius)
  ind <- tempind
  winind <- matrix(c(1,1,radius*2+1,radius*2+1),2,2)
  ind[,1][tempind[,1]<1] <- 1
  if(tempind[1,2] > nrow(mat)) ind[1,2] <- nrow(mat)
  if(tempind[2,2] > ncol(mat)) ind[2,2] <- ncol(mat)
  winind <- winind + ind - tempind
  if(missing(win))return(list(ind,winind))
  return(list(mat[seq(ind[1,1],ind[1,2]),seq(ind[2,1],ind[2,2])],
              win[seq(winind[1,1],winind[1,2]),seq(winind[2,1],winind[2,2])]))
}

#重複を処理した行列の順位を返します
MatRank <- function(mat){
  ranks <- (floor(rank(mat))+1)/2
  return(matrix(ranks,nrow(mat),ncol(mat),byrow = T))
}

#which.max,minのarr.ind=T版です
which.min.matrix <- function(X,decreasing = F){
  r <- MatRank(X)
  s <- unique(sort(r,decreasing = decreasing))
  olst <- lapply(s, function(s,r){which(r==s,arr.ind = T)},r)
  return(matlist(olst)[,2:1])
}

#行列の各行の昇順の順位を返します nrow >> ncolの時に早いです
order.matrix <- function(X,ignore0 = F){
  if(nrow(X)<=ncol(X)) warning("when nrow <= ncol, using t(apply(X,1,order)) is faster")
  odr <- X
  work.X <- X
  acs <- cbind(1:nrow(X),0)
  if(ignore0){
    if(max(abs(X))==Inf) stop("ignore0 is invalid when X include Inf")
    else work.X[which(X==0)] <- max(X)+1
  }
  for(i in 1:ncol(X)){
    acs[,2] <- max.col(-work.X)
    odr[,i] <- acs[,2]
    work.X[acs] <- Inf
  }
  return(odr)
}

#行列の各行を昇順にソートします nrow >> ncolの時に早いです
sort.matrix <- function(X,ignore0 = F){
  if(nrow(X)<=ncol(X)) warning("when nrow <= ncol, using t(apply(X,1,order)) is faster")
  srt <- X
  work.X <- X
  acs <- cbind(1:nrow(X),0)
  if(ignore0){
    if(max(abs(X))==Inf) stop("ignore0 is invalid when X include Inf")
    else work.X[which(X==0)] <- max(X)+1
  }
  for(i in 1:ncol(X)){
    acs[,2] <- max.col(-work.X)
    srt[,i] <- X[acs]
    work.X[acs] <- Inf
  }
  return(srt)
}

#リストでネストされたリスト/ベクトルを行列に変換します
matlist <- function(lst,nrow=length(lst),ncol=length(lst[[nrow]]),rname=names(lst),cname=names(lst[[nrow]])){
  if (length(lst)==0) stop("input list is empty")
  lst <- lapply(lst,function(l){length(l)<-ncol;l})
  mat <- matrix(unlist(lst), nrow, ncol,byrow = T)
  rownames(mat) <- rname
  colnames(mat) <- cname
  return(mat)
}

#2つのベクトルのユークリッド距離を返します
euclidean <- function(x,y){
  sqrt(sum((x-y)^2))
}

#行列やベクトルが指定された要素を持つかテストします
have <- function(x,element){
  return(ifelse(length(which(x==element)) > 0,yes = T,no = F))
}

#離散データの積分値を区分求積法によって求めます
quadrature <- function(x,y){
  if (class(x)=="density") {
    y <- x$y
    x <- x$x
  }
  sum(diff(x)*y[2:length(y)])
}
