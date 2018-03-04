#' Measure the elapsed time of \code{eval} and returns the result.
#'
#' @param eval evalated contents.
#' @param keep if \code{T}, returns evalated object with elapsed time.
#' @param return if \code{T}, returns evalated object.
#'
#' @return evalated object.
#' @export
#' @examples runif.sum <- evalTimer(sum(runif(5000)))
#'
evalTimer <- function(eval,keep=F,return=T){
  cat("Timer start:",as.character(Sys.time()),"\n")
  t <- proc.time()
  ret <- eval
  cat("Timer end  :",as.character(Sys.time()),"\n\n")
  cat("elapsed time is:",(time <- proc.time()-t)[3])
  # print(time <- proc.time()-t)
  if(keep) attr(ret,"time") <- time
  if(return) return(ret)
}


#' Waits until enter key is pressd.
#'
#' @export
#' @examples　
#' f <- function(n){
#'   mat <- matrix(runif(n**2,n,n))
#'   plot(mat); keywait()
#'   image(mat)
#' }
#' f(3)
#'
keywait <- function() invisible(readline(prompt="Press [enter] to continue"))


#' Save global environment.
#'
#' Save global environment as non named file ".RData" without closing RStudio.
#'
#' @export
#'
saveEnvironment <- function(){
  if(is.todayDirectory()) stop("Current directory is not project directory.")
  . <- save.image(".RData")
}
