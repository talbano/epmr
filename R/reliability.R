#' Internal Consistency Reliability
#'
#' Functions for estimating internal consistency reliability for a matrix
#' of scored item responses or a variance/covariance matrix.
#'
#' @param x a matrix of scored item responses, or a variance/covariance matrix
#' for a set of scored item responses when \code{sigma = TRUE}.
#' @param sigma boolean with default \code{FALSE} indicating whether or not
#' \code{x} is a variance/covariance matrix.
#' @return Returns the estimated reliability coefficient, alpha for \code{alpha}
#' and omega for \code{omega}.
alpha <- function(x, sigma = FALSE) {

  x <- as.matrix(x)
  ni <- ncol(x)

  if(!sigma)
    x <- var(x[complete.cases(x), ])

  sjj <- sum(diag(x))
  s2 <- sum(x)

  return((1 - sjj/s2) * ni/(ni - 1))
}

#' @rdname alpha
omega <- function(x, sigma = FALSE) {

  x <- as.matrix(x)
  ni <- ncol(x)
  x <- x[complete.cases(x), ]

  if(any(!dim(x)))
    return(NA)

  if(!sigma)
    x <- var(x[complete.cases(x), ])

  lambda <- factanal(covmat = x,
    factors = 1)$loadings[1:ni] * sqrt(diag(x))

  return(sum(lambda)^2/sum(x))
}
