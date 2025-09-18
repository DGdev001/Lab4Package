#' Print ASCII
#'
#' More Detailed Description
#'
#' @param x String containing the filename
#' If valid file, is printed
#' File to be printed must be in working directory
#' Examples
#' print_ascii("light_ascii.txt") (Provided)
#'


print_ascii <- function(file = x) {
  art <- readLines(file, warn = FALSE)
  cat(paste(art, collapse = "\n"), "\n")
}

print_ascii("light_ascii.txt")
