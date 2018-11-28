#' Measure the elapsed time of \code{eval} and returns the result.
#'
#' @param eval evalated code.
#' @param index.msg integer or character to be used as message.
#' @param keep if \code{T}, returns evalated object with elapsed time.
#' @param return if \code{T}, returns evalated object.
#'
#' @return evalated object.
#' @export
#' @examples
#' ex1 <- eval_timer(runif(50000))
#'
#' ex2 <- eval_timer(sum(1:5000), index.msg = "large sum", keep = T)
#' print(attr(ex2, "elapsed))
#'
#' ex3 <- lapply(1:3, function(n) eval_timer(runifN(n * 50000), index.msg = n))
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

#' Summarize object size in environment.
#'
#' @param accurate if `TRUE`, summarize with using [pryr::object_size()]
#' to summarize `environment` or `function` object more accuratery.
#' @param env target environment.
#' However, that is very slower when `env` contains large objects.
#' @return a tibble that contains summarizing result.
#' @export
#' @seealso [pryr::object_size()]
#' @references this function was supported by `pryr` package
#' authored by Hadly Wickman.
memory_summary <- function(accurate = F, env = .GlobalEnv) {
  env <- env %>% as.list
  if (accurate)
    size <- purrr::map_dbl(env, pryr::object_size)
  else
    size <- purrr::map_dbl(env, utils::object.size)
  power <- purrr::map_dbl(size, ~ min(floor(log(abs(.), 1000)), 4))
  logged <- (size / (1000^power)) %>% round
  name <- names(size)
  cls <- purrr::map_chr(env, ~ class(.)[1])
  len <- purrr::map_int(env, length)
  unit <- c("B", "KB", "MB", "GB", "TB")

  cat("total memory used: ")
  size %>% sum %>% print.bytes()

  tibble::tibble(name = name, logged = logged, unit = unit[power + 1],
                 class = cls, length = len, size = as.numeric(size)) %>%
    dplyr::arrange(size %>% dplyr::desc())
}

print.bytes <- function (x, digits = 3, ...) {
  # pryr:::print.bytes()
  power <- min(floor(log(abs(x), 1000)), 4)
  if (power < 1) {
    unit <- "B"
  }
  else {
    unit <- c("kB", "MB", "GB", "TB")[[power]]
    x <- x/(1000^power)
  }
  formatted <- format(signif(x, digits = digits), big.mark = ",",
                      scientific = FALSE)
  cat(formatted, " ", unit, "\n", sep = "")
}
