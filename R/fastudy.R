#' Factor Analysis Study
#'
#' Functions for examining the dimensionality of a data set via
#' exploratory factor analysis.
#'
#' @param x matrix or data.frame of scored item responses, one row per person,
#' one column per item.
#' @param complete logical with default \code{FALSE} indicating whether or not
#' \code{x} should be reduced to rows with complete data across all columns.
#' @param ... further arguments passed to or from other functions.
#' @export
fastudy <- function(x, factors, complete = TRUE, ...) {
  if (complete) x <- x[complete.cases(x), ]
  out <- factanal(x, factors = factors, ...)
  class(out) <- c("fastudy", "factanal")
  return(out)
}

#' @rdname fastudy
#' @export
plot.fastudy <- function(x, ylim = c(1, nrow(x$loadings)), ...) {
  nf <- ncol(x$loadings)
  #ni <- nrow(x$loadings)
  ei <- apply(x$loadings, 2, function(y) sum(y^2))
  plot(c(1, nf), ylim, type = "n", xaxt = "n", xlab = "Factor",
    ylab = "Eigenvalue", xlim = c(1 - nf * .1, nf + nf * .1))
  axis(1, at = 1:nf, labels = 1:nf)
  points(1:nf, ei)
  lines(1:nf, ei)
}

#' @export
print.fastudy <- function(x, digits = 3, cutoff = 0.1, ...) {
  cat("\nExploratory Factor Analysis\n")
  print(x$loadings, digits = digits, cutoff = cutoff, ...)
  cat("\n")
}
