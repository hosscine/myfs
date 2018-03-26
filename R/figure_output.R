#' Sets working directory to today directory.
#'
#' @export
#'
todaywd <- function(){
  today <- format(Sys.time(),"%Y_%m_%d")
  setwd(savedDir())
  if(length(which((list.files()==today)==T))==0) dir.create(today)
  setwd(paste0(savedDir(),"/",today))
}

#' Creates sub directory for saving a number of images.
#'
#' @param name name of the sub directory.
#' @param current force create the sub directory even if current directory is not today directory.
#'
#' @export
#'
subwd <- function(name, current = F){
  if(is.todayDirectory() || current){
    dir.create(name)
    setwd(paste0(savedDir(),"/",today <- format(Sys.time(),"%Y_%m_%d"),"/",name))
  }
  else {
    stop("working directory is not in today folder.")
  }
}

#' Retruns or Sets default save directory.
#'
#' When \code{saveDir} called without \code{pass}, returns default save directory which is
#' contained package directory.
#' When with \code{pass}, sets default save directory as pass.
#'
#' @param pass save directory pass if you want to change this.
#'
#' @return save directory
#' @export
#'
savedDir <- function(pass){
  if(missing(pass)){
    if(is.null(getOption("myfs.savedirectory"))){
      # try to road package config file
      lpass <- system.file("config", package = "myfs")
      # whether package config is null or not
      if(length(dir(lpass))==0){
        dirpass <- paste0("~/R/","\u5b9f\u9a13\u30c7\u30fc\u30bf")
        save(dirpass,file = paste0(lpass,"/dir"))
        options("myfs.savedirectory"=dirpass)
      }
      else{
        load(paste0(lpass,"/dir"))
        options("myfs.savedirectory"=dirpass)
      }
    }
  }
  else{
    save(pass,file = paste0(pass,"/dir"))
    options("myfs.savedirectory"=pass)
  }
  return(getOption("myfs.savedirectory"))
}

#' Checks if current directory is today directory or not.
is.todayDirectory <- function(){
  today <- format(Sys.time(),"%Y_%m_%d")
  return(getwd()==paste0(Sys.getenv("HOME"),substr(savedDir(),2,10000),"/",today))
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
#' @examples
#' plot(sin, xlim = c(0,2*pi))
#' #todaywd()
#' #savepng("sin curve")
#'
savepng <- function(name, current = F, scale = c(450, 500)){
  if(is.todayDirectory()==F&&current==F)
    stop("working directory is not today folder.
          \nuse function 'todaywd()' or call 'savepng()' with args 'current = T'.")

  if(length(scale) == 1)
    dev.copy(grDevices::png,file=paste(name,".png",sep=""),width=scale,height=scale)
  else if(length(scale) ==2)
    dev.copy(grDevices::png,file=paste(name,".png",sep=""),width=scale[1],height=scale[2])
  else stop("length of scale must be lower than 2.")
  dev.off()
}

#' Saves plot with adding title on the figure.
#'
#' @param name title of the plot.
#' @param current force create the sub directory even if current directory is not today directory.
#' @param scale size of saved image.
#'
#' @export
#'
savepngTitle <- function(name, current = F, scale=c(450, 500)){
  graphics::title(name)
  todaywd()
  savepng(name, current = current, scale = scale)
}
