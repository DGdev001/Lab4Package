usethis::use_vignette("Birthday function-David Norvell")
#' Birthday Problem Probability
#'
#' Calculates the probability that at least two people in a group of `n`
#' share the same birthday, assuming 365 days in a year.
#'
#' @param n Integer or vector of integers. The number of people in the group(s).
#' @return A numeric vector of probabilities for each value of `n`.
#' @examples
#' birthday(n = 20:25)
#' @export
birthday <- function(n) {
  sapply(n, function(k) {
    if (k > 365) return(1.0)
    if (k <= 1) return(0.0)

    prob_unique <- 1
    for (i in 0:(k - 1)) {
      prob_unique <- prob_unique * ((365 - i) / 365)
    }
    prob_shared <- 1 - prob_unique
    return(prob_shared)
  })
}

usethis::use_vignette("Birthday function-David Norvell")
