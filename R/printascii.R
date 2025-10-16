#' Print ASCII
#'
#' More Detailed Description
#'
#' @param x String containing the filename
#' If valid file, is printed
#' File to be printed must be in working directory
#' @examples
#' print_ascii("light_ascii.txt") (Provided)
#' @export
print_ascii <- function(x) {
  art <- readLines(x, warn = FALSE)
  cat(paste(art, collapse = "\n"), "\n")
}

#' Birthday Problem Probability
#'
#' Calculates the probability that at least two people in a group of `n`
#' share the same birthday, assuming 365 days in a year.
#'
#' @param n Integer or vector of integers. The number of people in the group(s).
#' @return A numeric vector of probabilities for each value of `n`.
#' @examples
#' birthdayDGN(n = 20:25)
#' @export
birthdayDGN <- function(n) {
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
install.packages("usethis",repos = "http://cran.us.r-project.org")
install.packages("rmarkdown, repos=","http://cran.us.r-project.org")
usethis::use_vignette("BirthdayFunctionDGN")

#' MyHyper from Lab5
#' Simulates hypergeometric distribution via repeated sampling
#' @param x Integer, determines the number of iterations
#' @param n Integer, sets the population size (# of items)
#' @param r Integer, number of "Successes"
#' @param n Integer, sample size drawn without replacement
#' @examples
#' myhyper(iter=100,N=20,r=12,n=5)
#' @export
myhyper=function(iter=100,N=20,r=12,n=5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  #Make a vector to hold the number of successes over the trials
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(rep(c(1,0),c(r,N-r)),n,replace=FALSE)
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="HYPERGEOMETRIC simulation", xlab="Number of successes")
  succ.tab/iter
}

#'Modified function for lab 7
#'Displays the curve, shaded area between it and x axis from
#'neg inf to x=a, then calculates the area
#'@param mu, Mean of the given normal distribution
#'@param sigma, Standard Deviation of given normal distribution
#'@param a, upper bound for the shaded area
#'@export
myncurve <- function(mu, sigma, a) {
  # Plot the normal curve
  curve(dnorm(x, mean = mu, sd = sigma),
        xlim = c(mu - 4*sigma, mu + 4*sigma),
        ylab = "Density",
        main = paste("Normal Curve: mu =", mu, ", sigma =", sigma))

  # Shade area from -Inf to a
  x_vals <- seq(mu - 4*sigma, a, length = 1000)
  y_vals <- dnorm(x_vals, mean = mu, sd = sigma)
  polygon(c(mu - 4*sigma, x_vals, a), c(0, y_vals, 0), col = "lightblue")

  # Calculate probability P(X <= a)
  prob <- round(pnorm(a, mean = mu, sd = sigma), 4)

  # Display probability on the plot
  text(a, max(y_vals)/2, paste("P(X ≤", a, ") =", prob))

  # Return results as a list
  return(list(mu = mu, sigma = sigma, a = a, probability = prob))
}
