#' Factor Analysis Study
#'
#' Functions for examining the dimensionality of a data set via
#' exploratory factor analysis. Currently a simple wrapper for factanal.
#'
#' @param x matrix or data.frame of scored item responses, one row per person,
#' one column per item.
#' @param factors the number of factors to extract.
#' @param covmat optional covariance matrix. Not used if \code{x} is supplied.
#' @param complete logical with default \code{FALSE} indicating whether or not
#' \code{x} should be reduced to rows with complete data across all columns.
#' @param ... further arguments passed to or from other functions.
#' @param ylim vector of limits for y-axis in the scree plot, passed to \code{par}.
#' @param h y-axis value specifying a horizonal line the scree plot, passed to
#' \code{abline}.
#' @return \code{fastudy} runs an exploratory factor analysis using the
#' \code{factanal} function, and returns a matrix of factor loadings. The
#' print method displays eigenvalues and proportion of variance explained
#' per factor.
#'
#' @export
fastudy <- function(x, factors, covmat, complete = TRUE, ...) {
  if (!missing(x)) {
    if (complete) x <- x[complete.cases(x), ]
    out <- factanal(x, factors = factors, ...)
  } else if (!missing(covmat)) {
    out <- factanal(covmat = covmat, factors = factors, ...)
  } else stop("either 'x' or 'covmat' must be supplied")
  class(out) <- c("fastudy", "factanal")
  return(out)
}

#' @rdname fastudy
#' @export
plot.fastudy <- function(x, ylim, h = 1, ...) {
  nf <- ncol(x$loadings)
  ei <- colSums(x$loadings^2)
  if (missing(ylim)) ylim <- c(0, max(ei))
  plot(c(1 - nf * .1, nf + nf * .1), ylim, type = "n", xaxt = "n",
    xlab = "Factor", ylab = "Eigenvalue")
  axis(1, at = 1:nf, labels = 1:nf)
  points(1:nf, ei)
  lines(1:nf, ei)
  if (!is.null(h)) abline(h = h)
}

#' @export
print.fastudy <- function(x, digits = 3, cutoff = 0.1, ...) {
  cat("\nExploratory Factor Analysis\n")
  print(x$loadings, digits = digits, cutoff = cutoff, ...)
  cat("\n")
}
