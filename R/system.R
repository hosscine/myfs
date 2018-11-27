#' Measure the elapsed time of \code{eval} and returns the result.
#'
#' @param eval evalated code.
#' @param index.msg integer or character to be used as message.
#' @param keep if \code{T}, returns evalated object with elapsed time.
#' @param return if \code{T}, returns evalated object.
#'
#' @return evalated object.
#' @export
#' @examples runif.sum <- evalTimer(sum(runif(5000)))
#'
eval_timer <- function(eval, index.msg = NULL, keep = F, return = T) {
  if (!is.null(index.msg)) assert_that(length(index.msg) == 1)

  if (is.null(index.msg))
    msg <- crayon::white("timer")
  else if (assertthat::is.number(index.msg))
    msg <- crayon::bold(paste0("step", index.msg, ":"))
  else if (is.character(index.msg))
    msg <- crayon::bold(paste0(index.msg, ":"))
  else stop("index.msg must be a integer or a character")

  cat(msg, crayon::white("start", as.character(Sys.time()), "\n"))
  t <- proc.time()["elapsed"]
  ret <- eval
  cat(msg, crayon::white("end  ", as.character(Sys.time()), "\n"))
  time <- round(proc.time()["elapsed"] - t, 2)
  cat(crayon::cyan("elapsed time:", time, "\n"))
  # print(time <- proc.time()-t)
  if (keep)
    attr(ret, "elapsed") <- time
  if (return)
    return(ret)
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
keywait <- function() invisible(readline(prompt = "Press [enter] to continue"))


#' Save global environment.
#'
#' Save global environment as non named file '.RData' without closing RStudio.
#' Directory to save is set project directory.
#'
#' @export
#'
saveEnvironment <- function() {
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
dir.create.deep <- function(pass) {
  sppass <- strsplit(pass, split = "/")
  sppass <- as.list(sppass[[1]])
  for (i in 1:length(sppass)) {
    sppass.tmp <- sppass[1:i]
    sppass.tmp$sep <- "/"
    pass.tmp <- do.call(paste, sppass.tmp)
    if (pass.tmp == "" || nchar(pass.tmp) == 2)
      next

    dirok <- try(is.dir(pass.tmp), silent = T)
    if (dirok != T)
      dir.create(pass.tmp)
  }
  if (!is.dir(pass))
    stop(paste("creation of directory", pass, "is failed."))
}
