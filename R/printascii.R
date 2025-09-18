#' Print ASCII
#'
#' More Detailed Description
#'
#' @param x String containing the filename
#' If valid file, is printed
#' File to be printed must be in working directory
#' @examples
#' print_ascii("light_ascii.txt") (Provided)
#'@export
print_ascii <- function(x) {
  art <- readLines(x, warn = FALSE)
  cat(paste(art, collapse = "\n"), "\n")
}


