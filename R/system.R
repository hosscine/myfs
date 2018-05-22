#' Measure the elapsed time of \code{eval} and returns the result.
#'
#' @param eval evalated code.
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
#' Directory to save is set project directory.
#'
#' @export
#'
saveEnvironment <- function(){
  setwdProject()
  . <- save.image(".RData")
}

#' Sets current directory to project directory.
#'
#' @importFrom rstudioapi getActiveProject
#'
#' @export
setwdProject <- function() setwd(rstudioapi::getActiveProject())

#' Checks and creates a unknown directory when it is not exist.
#'
#' @param pass target directory pass.
#'
#' @importFrom assertthat is.dir
#'
#' @export
#'
dir.create.deep <- function(pass){
  sppass <- strsplit(pass,split = "/")
  sppass <- as.list(sppass[[1]])
  for (i in 1:length(sppass)) {
    sppass.tmp <- sppass[1:i]
    sppass.tmp$sep <- "/"
    pass.tmp <- do.call(paste,sppass.tmp)
    if(pass.tmp == "" || nchar(pass.tmp) == 2) next

    dirok <- try(is.dir(pass.tmp), silent = T)
    if(dirok != T) dir.create(pass.tmp)
  }
  if(!is.dir(pass)) stop(paste("creation of directory",pass,"is failed."))
}
