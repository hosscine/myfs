
#' Prepare the process that enables to print out your R-notebook document more beautifully
#'
#' enable to background color of code chunks and remove 'hide' button.
#'
#' @export
#'
preparePrintify <- function() {
  utils::download.file("https://gist.githubusercontent.com/penpenpng/b6f0e157d09ccce9ac4f6b0cdfe1a2f9/raw/printify.html",
    destfile = "printify.html")
  cat("add YAML header of your R-notebook belows:
*************
output:
  html_notebook:
  includes:
    after_body: printify.html
*************\n\n")

  cat("printify.html is authored by penpenpng (https://github.com/penpenpng).\n")
}
