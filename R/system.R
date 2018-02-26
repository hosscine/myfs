#' Measure the elapsed time of \code{eval} and returns the result.
#'
#' @param eval evalated contents.
#' @param keep if \code{T}, returns evalated object with elapsed time.
#'
#' @return evalated object.
#' @export
#'
#' @examples runif.sum <- evalTimer(sum(runif(5000)))
evalTimer <- function(eval,keep=F){
  print(Sys.time())
  t <- proc.time()
  ret <- eval
  print(Sys.time())
  print(time <- proc.time()-t)
  if(keep) attr(ret,"time") <- time
  ret
}

#' Save global environment.
#'
#' Save global environment as non named file ".RData" without closing RStudio.
#'
#' @export
saveEnvironment <- function(){
  if(is.todayDirectory()) stop("Current directory is not project directory.")
  . <- save.image(".RData")
}

#' Prints variables or expressions  for debug.
#'
#' @param ... printed objects.
#'
#' @export
#'
#' @examples　
#' f <- function(x,y){
#'   debugText(x^y,y)
#'   return(x^y+y)
#' }
#' f(4,6)
debugText <- function(...){
  key <- as.list(substitute(list(...)))[-1L]
  val <- list(...)
  mapply(
    function(k, v){
      if (typeof(k)=="language") {cat(as.character(k),"(",class(k)[1],") = ",sep = "")}
      else cat(k,"(",class(k)[1],") = ",sep = "")

      if(!is.matrix(v) && (is.logical(v) || is.numeric(v) || is.complex(v) || is.character(v))){ cat(v, "\n") }
      else{ cat("\n"); print(v); cat("\n") }
    },
    key, val)
  cat("\n")
}

#' Waits until enter key is pressd.
#'
#' @export
#'
#' @examples　
#' f <- function(n){
#'   mat <- matrix(runif(n**2,n,n))
#'   plot(mat); keywait()
#'   image(mat)
#' }
#' f(3)
keywait <- function() invisible(readline(prompt="Press [enter] to continue"))

#' Sets working directory to today directory.
#'
#' @export
todaywd <- function(){
  today <- format(Sys.time(),"%Y_%m_%d")
  setwd(savedDir())
  if (length(which((list.files()==today)==T))==0) dir.create(today)
  setwd(paste0(savedDir(),"/",today))
}

#' Creates sub directory for saving a number of images.
#'
#' @param name name of the sub directory.
#' @param current force create the sub directory even if current directory is not today directory.
#'
#' @export
subwd <- function(name, current=F){
  if(is.todayDirectory() || current){
    dir.create(name)
    setwd(paste0(savedDir(),"/",today <- format(Sys.time(),"%Y_%m_%d"),"/",name))
  }
  else {
    stop("working directory is not in today folder.")
  }
}

#' Checks if current directory is today directory or not.
is.todayDirectory <- function(){
  today <- format(Sys.time(),"%Y_%m_%d")
  return(getwd()==paste0(Sys.getenv("HOME"),substr(savedDir(),2,10000),"/",today))
}

#' Saves plot with title.
#'
#' @param name title of the plot.
#' @param scale size of saved image.
#'
#' @export
savepngTitle <- function(name,scale=c(450,500)){
  graphics::title(name)
  todaywd()
  savepng(name,scale=scale)
}

#' Saves plot to today directory.
#'
#' @param name names of the saved image file without ".png".
#' @param current force create the sub directory even if current directory is not today directory.
#' @param scale size of saved image.
#'
#' @importFrom grDevices dev.copy dev.off png
#'
#' @export
savepng <- function(name, current = F, scale = c(450,500)){
  if(is.todayDirectory()==F&&current==F)stop("working directory is not today folder.
                                             use function 'todaywd()' or 'savepng' with option current=T.")
  if(length(scale) == 1)
    grDevices::dev.copy(grDevices::png,file=paste(name,".png",sep=""),width=scale,height=scale)
  else if(length(scale) ==2)
    grDevices::dev.copy(grDevices::png,file=paste(name,".png",sep=""),width=scale[1],height=scale[2])
  else stop("length of scale must be lower than 2.")
  grDevices::dev.off()
}

#' Retruns or Sets default save directory.
#'
#' @param pass save directory pass if you want to change this.
#'
#' @return save directory
#' @export
savedDir <- function(pass){
  if(missing(pass)){
    if(is.null(getOption("FTG.funcs.savedirectory"))){
      lpass <- system.file("config", package = "FTG.funcs")
      # lpass <- paste0(getwd(),"/config")
      if(length(dir(lpass))==0){
        dirpass <- paste0("~/R/","\u5b9f\u9a13\u30c7\u30fc\u30bf")
        save(dirpass,file = paste0(lpass,"/dir"))
        options("FTG.funcs.savedirectory"=dirpass)
      }
      else{
        load(paste0(lpass,"/dir"))
        options("FTG.funcs.savedirectory"=dirpass)
      }
    }
  }
  else{
    save(pass,file = paste0(pass,"/dir"))
    options("FTG.funcs.savedirectory"=pass)
  }
  return(getOption("FTG.funcs.savedirectory"))
}
