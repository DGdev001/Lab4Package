#' Print ASCII
#'
#' More Detailed Description
#'
#' @param x String containing the filename
#' If valid file, is printed
#' File to be printed must be in working directory
#' @examples
#' print_ascii("inst/extdata/light_ascii.txt") (Provided)
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
install.packages("rmarkdown", repos ="http://cran.us.r-project.org")
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

#'Modified function for lab 6
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
#FIRE Dataset for Lab 7
#' FIRE data set.
#'
#' A data set from MS
#'
#' @format A data frame with 15 rows and 2 variables:
#' \describe{
#'   \item{DISTANCE}{length in cm}
#'   \item{DAMAGE}{Dollars damage}
#'   ...
#' }
#' @source \url{https://www.crcpress.com/Statistics-for-Engineering-and-the-Sciences/Mendenhall-Sincich/p/book/9781498728850}
"fire"

#' Central Limit Theorem simulation for the Poisson Distribution
#' Used to demonstrate that it'll work with continuous or discrete distributions
#' @param n Integer. Sample size for each iteration. Default is 10.
#' @param iter Integer. Number of iterations (simulated samples). Default is 10000.
#' @param lambda Numeric. The rate parameter of the Poisson distribution. Default is 10.
#' @param For options to modify the graphical output of \code{hist()}.
#'
#' @return This function produces three plots:
#' \itemize{
#'   \item A histogram of the sample means with a theoretical normal curve.
#'   \item A barplot of the simulated Poisson samples.
#'   \item A plot of the theoretical Poisson probability mass function.
#' }
#'
#' @details
#' Used in Lab8 MATH-4753
#'
#'
#'
#' @examples
#' \dontrun{
#'   mycltp(n = 10, iter = 10000, lambda = 10)
#' }
#'
#' @export
#'
mycltp <- function(n = 10, iter = 10000, lambda = 10, ...) {
  ## Generate Poisson samples
  y <- stats::rpois(n * iter, lambda = lambda)

  ## Arrange into matrix (rows = sample size, columns = iterations)
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)

  ## Compute sample means
  w <- apply(data, 2, mean)

  ## Prepare histogram data (without plotting yet)
  param <- graphics::hist(w, plot = FALSE)
  ymax <- 1.1 * max(param$density)

  ## Set up layout: histogram on top, barplot + PMF below
  graphics::layout(matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE))

  ## Plot histogram of sample means
  graphics::hist(
    w,
    freq = FALSE,
    ylim = c(0, ymax),
    col = grDevices::rainbow(max(w)),
    main = paste(
      "Histogram of sample mean\n",
      "sample size = ", n,
      " iter = ", iter,
      " lambda = ", lambda,
      sep = ""
    ),
    xlab = "Sample mean",
    ...
  )

  ## Overlay theoretical normal approximation
  graphics::curve(
    stats::dnorm(x, mean = lambda, sd = sqrt(lambda / n)),
    add = TRUE,
    col = "red",
    lty = 2,
    lwd = 3
  )

  ## Barplot of simulated raw Poisson samples
  graphics::barplot(
    table(y) / (n * iter),
    col = grDevices::rainbow(max(y)),
    main = "Barplot of sampled y",
    ylab = "Relative Frequency",
    xlab = "y"
  )

  ## Theoretical Poisson probability function
  x <- 0:max(y)
  graphics::plot(
    x,
    stats::dpois(x, lambda = lambda),
    type = "h",
    lwd = 5,
    col = grDevices::rainbow(max(y)),
    main = "Probability function for Poisson",
    ylab = "Probability",
    xlab = "y"
  )

  invisible(NULL)
}
#' Bootstrap estimator with histogram and confidence interval
#'
#' @param iter Number of bootstrap iterations.
#' @param x Numeric vector of data.
#' @param fun Function (or function name as a string) to compute the statistic.
#' @param alpha Significance level for the confidence interval.
#' @param cx Character expansion factor for printed text.
#' @param ... Additional graphical arguments passed to hist().
#'
#' @details
#' Used in Lab 9 for MATH-4753.
#'
#' @examples
#' \dontrun{
#'   myboot2(10000, x = rnorm(20), fun = "mean", alpha = 0.05, cx = 1.5)
#' }
#'
#' @export
myboot2 <- function(iter = 10000, x, fun = mean, alpha = 0.05, cx = 1.5, ...) {

  # Allow fun to be character string
  fun <- match.fun(fun)

  n <- length(x)

  # Bootstrap resampling
  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nr = n, nc = iter, byrow = TRUE)

  # Bootstrap statistics
  xstat <- apply(rs.mat, 2, fun)

  # Confidence interval
  ci <- quantile(xstat, c(alpha/2, 1 - alpha/2))

  # Histogram
  para <- hist(xstat, freq = FALSE, las = 1,
               main = paste("Histogram of Bootstrap Sample Statistics\n",
                            "alpha = ", alpha, "  iter = ", iter, sep=""),
               ...)

  # Point estimate from original data
  pte <- fun(x)

  abline(v = pte, lwd = 3, col = "black")

  # CI segment
  segments(ci[1], 0, ci[2], 0, lwd = 4)
  text(ci[1], 0, paste("(", round(ci[1], 2), sep=""), col="red", cex=cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep=""), col="red", cex=cx)

  # Annotate point estimate
  text(pte, max(para$density)/2, round(pte, 2), cex = cx)

  invisible(list(ci=ci, fun=fun, x=x, pte=pte))
}


#' Maximum Likelihood Plotter for Repeated Samples
#'
#' Computes and plots the log-likelihood across a sequence of parameter values
#' for repeated sampling from the same distribution. The function identifies
#' the maximizing parameter value, marks it on the plot, and returns useful
#' diagnostic information including slope changes around the maximum.
#'
#' @param lfun A log-likelihood function of the form \code{function(x, param)}. (no default)
#' @param x A numeric vector of observed data values. Default: \code{c(1,2,3)}.
#' @param param A numeric vector of parameter values to search over.
#'   Default: \code{seq(0, 1, length.out = 100)}.
#' @param ... Additional graphical parameters passed to \code{plot()}.
#'
#' @details
#' The function evaluates the log-likelihood over all provided parameter values
#' using \code{outer()} and sums likelihood contributions column-wise. It then
#' produces a plot of the likelihood function and marks the estimated maximizing
#' value using a vertical line and plotted point.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{i} – Index location of the maximum parameter value.
#'   \item \code{parami} – Parameter value that maximizes the log-likelihood.
#'   \item \code{yi} – Log-likelihood value at the maximum.
#'   \item \code{slope} – Vector of slopes for local diagnostic checking.
#' }
#'
#' @export
#'
#' @examples
#' # Example 1 — Binomial log likelihood
#' logbin <- function(x, p) sum(dbinom(x, size = 1, prob = p, log = TRUE))
#' mymaxlik(lfun = logbin,
#'          x = c(9,9,1,9,9,9),
#'          param = seq(0, 1, length.out = 100),
#'          xlab = expression(pi), main = "Binomial Example")
#'
#' # Example 2 — Poisson log likelihood
#' logpoiss <- function(x, lambda) sum(dpois(x, lambda = lambda, log = TRUE))
#' mymaxlik(lfun = logpoiss,
#'          x = c(3,4,3,5),
#'          param = seq(0, 20, length.out = 100),
#'          xlab = expression(lambda), main = "Poisson Example")
mymaxlik <- function(lfun,
                     x = c(1, 2, 3),
                     param = seq(0, 1, length.out = 100),
                     ...) {

  if (!is.function(lfun)) stop("lfun must be a function.")
  if (!is.numeric(x)) stop("x must be numeric.")
  if (!is.numeric(param)) stop("param must be numeric.")

  np <- length(param)

  # Create a vectorized version of the likelihood function
  vectorized_lfun <- function(param_value) {
    sum(sapply(x, function(xi) lfun(xi, param_value)))
  }

  # Calculate likelihood for each parameter value
  y <- sapply(param, vectorized_lfun)

  # Plot likelihood
  plot(param, y, col = "blue", type = "l", lwd = 2,
       main = "Maximum Likelihood Plot",
       xlab = "Parameter",
       ylab = "Log-Likelihood",
       ...)

  # Identify maximizing index
  i <- which.max(y)

  # Add graphical markers
  abline(v = param[i], lwd = 2, col = "red")
  points(param[i], y[i], pch = 19, cex = 1.5, col = "black")
  axis(3, param[i], round(param[i], 2))

  # Local slope diagnostic
  if (i - 3 >= 1 && i + 2 <= np) {
    slope <- (y[(i - 2):(i + 2)] - y[(i - 3):(i + 1)]) /
      (param[(i - 2):(i + 2)] - param[(i - 3):(i + 1)])
  } else {
    slope <- NA
  }

  return(list(i = i, parami = param[i], yi = y[i], slope = slope))
}

