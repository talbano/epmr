#' Reliability Study
#'
#' Functions for estimating internal consistency reliability, adjusting
#' reliability, and estimating the standard error of measurement.
#'
#' @param x a matrix of scored item responses, or a variance/covariance matrix
#' for a set of scored item responses when \code{sigma = TRUE}.
#' @param sigma boolean with default \code{FALSE} indicating whether or not
#' \code{x} is a variance/covariance matrix.
#' @param r the reliability.
#' @param k factor by which test length changes, used to predict a
#' corresponding change in reliability.
#' @param sd the standard deviation of total observed scores.
#' @return Returns a data frame of estimated reliability coefficients,
#' currently including alpha and omega, with corresponding standard errors of
#' measurement. Reliability coefficients can also be obtained individually
#' with \code{alpha} and \code{omega}. \code{sbr} returns a new
#' reliability coefficient predicted using the Spearman-Brown formula.
#' \code{sem} returns the standard error of measurement.
#' @export
rstudy <- function(x, sigma = FALSE) {

  x <- as.matrix(x)
  ni <- ncol(x)

  if(any(!dim(x)))
    return(NA)
  if(!sigma)
    x <- var(x[complete.cases(x), ])

  sjj <- sum(diag(x))
  s2 <- sum(x)
  lambda <- factanal(covmat = x,
    factors = 1)$loadings[1:ni] * sqrt(diag(x))

  r <- c(alpha = (1 - sjj / s2) * ni / (ni - 1),
    omega = sum(lambda)^2 / sum(x))

  return(data.frame(r = r, sem = sem(r, sqrt(s2))))
}

#' @rdname rstudy
#' @export
alpha <- function(x, sigma = FALSE) {

  x <- as.matrix(x)
  ni <- ncol(x)

  if(any(!dim(x)))
    return(NA)
  if(!sigma)
    x <- var(x[complete.cases(x), ])

  sjj <- sum(diag(x))
  s2 <- sum(x)

  return((1 - sjj/s2) * ni/(ni - 1))
}

#' @rdname rstudy
#' @export
omega <- function(x, sigma = FALSE) {

  x <- as.matrix(x)
  ni <- ncol(x)

  if(any(!dim(x)))
    return(NA)
  if(!sigma)
    x <- var(x[complete.cases(x), ])

  lambda <- factanal(covmat = x,
    factors = 1)$loadings[1:ni] * sqrt(diag(x))

  return(sum(lambda)^2/sum(x))
}

#' @rdname rstudy
#' @export
sbr <- function(r, k) {

  return((k * r)/((k - 1) * r + 1))
}

#' @rdname rstudy
#' @export
sem <- function(r, sd) {

  return(sd * sqrt(1 - r))
}
