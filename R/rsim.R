#' Simulating Correlated Scores
#'
#' Simulates vectors of scores with specified correlation, means, and
#' standard deviations.
#'
#' @param n number of observations. If \code{length(n) > 1}, the length
#' is taken to be the number required.
#' @param rho correlation in the population.
#' @param x vector of scores to be correlated with. If \code{x} is supplied,
#' \code{n} is overridden to be \code{length(x)}, meanx and sdx are set to
#' \code{mean(x)} and \code{sd(x)}, and y is created to
#' correlate at \code{rho} with \code{x}.
#' @param meanx,sdx the desired mean and standard deviation of x.
#' @param meany,sdy the desired mean and standard deviation of y.
#' @param to.data.frame should the results be converted to a \code{data.frame}?
#' Defaults to \code{FALSE}.
#' @return When \code{to.data.frame} is \code{TRUE}, returns a data frame of
#' scores on x and y. Otherwise, a list containing:
#' \describe{
#' \item{x}{\code{x} when supplied, or the simulated vector of scores}
#' \item{y}{the vector y sampled from a population correlating with
#' \code{x} at \code{rho}}
#' \item{rho}{the supplied correlation \code{rho}}
#' \item{r}{the estimated correlation between x and y}
#' \item{meanx, sdx, meany, sdy}{the means and standard deviations of x and y}
#' }
#' @export
rsim <- function(n, rho, x, meanx = 0, sdx = 1, meany = 0, sdy = 1,
  to.data.frame = FALSE) {

  if(missing(x))
    x <- rnorm(n, meanx, sdx)
  else {
    n <- length(x)
    meanx <- mean(x, na.rm = TRUE)
    sdx <- sd(x, na.rm = TRUE)
  }

  y <- (x - meanx) * rho +
  	rnorm(n, 0, sdx) * sqrt(1 - rho^2)
  y <- y * sdy / sdx + meany

  if(to.data.frame)
    out <- data.frame(x = x, y = y)
  else
    out <- list(x = x, y = y, rho = rho, r = cor(x, y),
      meanx = meanx, sdx = sdx, meany = meany, sdy = sdy)

  return(out)
}
