#' Generates random natural numbers.
#'
#' @param n number of the random numbers.
#' @param min min of the random numbers.
#' @param max max of the random numbers.
#'
#' @importFrom stats runif
#'
#' @return random natural numbers.
#' @export
#'
runifN <- function(n,min=1,max=10){
  if(min > max) stop("argment max is not larger than min.")
  if(!min-round(min)==0 || !max-round(max)==0) stop("argment max and min must be integer.")
  return(ceiling(runif(n,min=min-1,max = max)))
}

#' Calculates norm of \code{x}.
#'
#' @param x target vector.
#'
#' @return norm of \code{x}.
#' @export
#'
vnorm <- function(x) sqrt(sum(x^2))


#' Calculates norm for each row of \code{X}.
#'
#' @param X target matrix.
#'
#' @return norm for each row of \code{X}.
#' @export
#'
rowNorm <- function(X) sqrt(rowSums(X^2))

#' Calculates \code{X - a} with row first.
#'
#' R's default calculation of \code{X - a} is with col first likes \code{X[,1] - a}, \code{X[,2] - a}, ....
#' This function implements calclation with row first likes \code{X[1,] - a}, \code{X[1,] - a}, ....
#'
#' @param X minused matrix.
#' @param a minuses vector.
#'
#' @return \code{X - a} in row first.
#' @export
#'
rowMinus <- function(X,a) t(t(X)-a)

#' Calculates \code{X \* a} with row first.
#'
#' R's default calculation of \code{X \* a} is with col first likes \code{X[,1] \* a}, \code{X[,2] \* a}, ....
#' This function implements calclation with row first likes \code{X[1,] \* a}, \code{X[1,] \* a}, ....
#'
#' @param X timed matrix.
#' @param a times vector.
#'
#' @return \code{X \* a} with row first.
#'
rowTimes <- function(X,a) t(t(X)*a)

#' Replaces NA from matrix with maintaining the dimension.
#'
#' @param X target matrix.
#' @param replace value to be replaced for NA.
#'
#' @return NA removed matrix.
#' @export
#'
replaceNA <- function(X, replace = 0) return(ifelse(is.na(X),replace,X))

#' \code{order} function regards a duplication value as the same ranking.
#'
#' @param x target vector.
#' @param decreasing if \code{TRUE}, proceses in decreasing.
#'
#' @return order of \code{x}.
#' @export
#'
order2 <- function(x, decreasing = F){
  v <- sort(unique(x))
  if(decreasing) v <- rev(v)
  o <- numeric(length(x))
  for (val in 1:length(v)) {
    o[which(x==v[val])] <- val
  }
  return(o)
}

#' Calculates rank of each elements of matrix.
#'
#' @param X target matrix.
#'
#' @return matrix presents rank.
#' @export
#'
rank.matrix <- function(X){
  ranks <- (floor(rank(X))+1)/2
  return(matrix(ranks, nrow(X), ncol(X), byrow = T))
}

#' Searches index of minimum element from matrix.
#'
#' @param X target matrix.
#'
#' @return index of minimum element.
#' @export
#'
which.min.ind <- function(X){
  r <- myfs::rank.matrix(X)
  s <- unique(sort(r))
  olst <- lapply(s, function(s,r) which(r==s,arr.ind = T), r)
  return(myfs::matlist(olst)[,2:1])
}

#' Searches index of maximum element from matrix.
#'
#' @param X target matrix.
#'
#' @return index of maximum element.
#' @export
#'
which.max.ind <- function(X){
  r <- myfs::rank.matrix(X)
  s <- unique(sort(r,decreasing = T))
  olst <- lapply(s, function(s,r) which(r==s,arr.ind = T), r)
  return(myfs::matlist(olst)[,2:1])
}

#' Orders matrix in each rows.
#'
#' @param X target matrix.
#' @param ignore0 special option when \code{X} is distance matrix.
#' @param decreasing if \code{T}, orders in decreasing.
#'
#' @return matrix with orderd each rows.
#' @export
#'
order.col <- function(X, decreasing = F, ignore0 = F){
  if(nrow(X)<=ncol(X)) warning("when nrow <= ncol, using t(apply(X,1,order)) is faster")
  odr <- X
  if(decreasing) work.X <- -X
  else work.X <- X
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

#' Sorts matrix in each rows.
#'
#' @param X target matrix.
#' @param ignore0 special option when \code{X} is distance matrix.
#' @param decreasing if \code{T}, sorts in decreasing.
#'
#' @return matrix with sorted each rows.
#' @export
#'
sort_col <- function(X, decreasing = F, ignore0 = F){
  if(nrow(X)<=ncol(X)) warning("when nrow <= ncol, using t(apply(X,1,sort)) is faster")
  srt <- X
  if(decreasing) work.X <- -X
  else work.X <- X
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

#' Converts lists or vectors that nested by list to matrix.
#'
#' @param lst lists or vectors nested by list.
#' @param nrow number of rows.
#' @param ncol number of cols.
#' @param rname rownames of matrix after converted.
#' @param cname colnames of matrix after converted.
#'
#' @return converted matrix.
#' @export
#'
#' @examples
#' lst <- lapply(1:5,rep,3)
#' matlist(lst)
#'
matlist <- function(lst, nrow=length(lst), ncol=length(lst[[nrow]]), rname=names(lst), cname=names(lst[[nrow]])){
  if (length(lst)==0) stop("input list is empty")
  lst <- lapply(lst,function(l){length(l)<-ncol;l})
  mat <- matrix(unlist(lst), nrow, ncol,byrow = T)
  rownames(mat) <- rname
  colnames(mat) <- cname
  return(mat)
}

#' Calculates euclidean distance of vectors.
#'
#' @param x a vector.
#' @param y a vector.
#'
#' @return euclidean distance between \code{x} and \code{y}.
#' @export
#'
euclidean <- function(x,y) sqrt(sum((x-y)^2))

#' Integrates discrete vector by quadrature.
#'
#' @param x target density object calculated by function \code{density()}.
#'
#' @return integration of \code{x}.
#' @export
#'
#' @examples
#' n <- rnorm(100)
#' dens <- density(n)
#' quadrature(dens)
#'
quadrature <- function(x){
  assert_that(class(x) == "density")
  y <- x$y
  x <- x$x
  sum(diff(x)*y[2:length(y)])
}

#' Calculates angle of 2-dimensional vectors.
#'
#' This function calculates the angle of x to y in anticlockwise.
#'
#' @param x vector 1.
#' @param y vector 2.
#'
#' @return angle in radian.
#' @export
#'
#' @examples
#' # a example of orthogonal vectors
#'
#' angle(c(1, 0), c(0, 1))
#' # = 1 / 2 * pi
#'
#' angle(c(0, 1), c(1, 0))
#' # = 3 / 2 * pi
#'
angle <- function(x, y){
  assert_that(length(x) == 2)
  assert_that(length(y) == 2)

  xrad <- atan2(x[2], x[1])
  if(xrad < 0) xrad <- xrad + 2*pi
  yrad <- atan2(y[2], y[1])
  if(yrad < 0) yrad <- yrad + 2*pi

  angle <- yrad - xrad
  if(angle < 0) return(angle + 2*pi)
  else return(angle)
}

#' Padding (expanding rows and cols) for \code{matrix}.
#'
#' @param X target matrix.
#' @param size expanding size.
#' @param replace values of expanded elements. if NULL, copies border values of the matrix.
#'
#' @importFrom assertthat is.number
#'
#' @return padding matrix.
#' @export
#'
padding.matrix <- function(X, size = 1, replace = NULL){
  # check args
  assert_that(nrow(X) >= 2 && ncol(X) >= 2)
  if(is.number(replace)) data <- replace
  else data <- 0

  # simple padding
  padded <- matrix(data, nrow(X) + size * 2, ncol(X) + size * 2)
  padded[(1 + size):(nrow(padded) - size), (1 + size):(ncol(padded) - size)] <- X

  # padding with border values of matrix.
  if(is.null(replace)){
    padded[expand.grid(1:(1 + size), 1:(1 + size)) %>% as.matrix] <-
      X[1, 1] # top left
    padded[expand.grid((2 + size):(size + nrow(X) - 1), 1:(1 + size)) %>% as.matrix] <-
      X[2:(nrow(X) - 1), 1] # middle left
    padded[expand.grid((size + nrow(X)):nrow(padded), 1:(1 + size)) %>% as.matrix] <-
      X[nrow(X), 1] # bottom left

    if(ncol(X) > 2){
      ind.top.center <- expand.grid(1:(1 + size), (2 + size):(size + ncol(X) - 1)) %>% as.matrix
      ind.top.center <- ind.top.center[sapply(1:(size + 1), seq, to = nrow(ind.top.center), by = size + 1) %>% as.numeric,]
      padded[ind.top.center] <- X[1, 2:(ncol(X) - 1)] # top center

      ind.bottom.center <- expand.grid((size + nrow(X)):nrow(padded), (2 + size):(size + ncol(X) - 1)) %>% as.matrix
      ind.bottom.center <- ind.bottom.center[sapply(1:(size + 1), seq, to = nrow(ind.bottom.center), by = size + 1) %>% as.numeric,]
      padded[ind.bottom.center] <- X[nrow(X), 2:(ncol(X) - 1)] # bottom center
    }

    padded[expand.grid(1:(1 + size), (size + ncol(X)):ncol(padded)) %>% as.matrix] <-
      X[1, ncol(X)] # top right
    padded[expand.grid((2 + size):(size + nrow(X) - 1), (size + ncol(X)):ncol(padded)) %>% as.matrix] <-
      X[2:(nrow(X) - 1), ncol(X)] # middle right
    padded[expand.grid((size + nrow(X)):nrow(padded), (size + ncol(X)):ncol(padded)) %>% as.matrix] <-
      X[nrow(X), ncol(X)] # bottom right
  }

  return(padded)
}
