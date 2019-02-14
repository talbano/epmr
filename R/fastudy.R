#' Factor Analysis Study
#'
#' Functions for examining the dimensionality of a data set via
#' exploratory factor analysis. \code{fastudy} is currently a simple
#' wrapper for factanal. \code{plot.fastudy} generates a scree plot
#' using \code{fastudy} output.
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
#' @examples
#'
#' # Exploratory factor analysis of the PISA approaches to learning scale
#' # Create vector of item names
#' items <- c("st27q01", "st27q03", "st27q05", "st27q07", "st27q04",
#'   "st27q08", "st27q10", "st27q12", "st27q02", "st27q06",
#'   "st27q09", "st27q11", "st27q13")
#'
#' # Reduce PISA09 to complete data for Great Britain
#' pisa_gbr <- na.omit(PISA09[PISA09$cnt == "GBR", items])
#'
#' # Fit EFA with six factors
#' fa_al <- fastudy(pisa_gbr, factors = 6)
#'
#' # Scree plot
#' plot(fa_al, ylim = c(0, 2))
#'
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
  Eigenvalue <- colSums(x$loadings^2)
  Factor <- 1:nf
  eidf <- dplyr::tibble(Eigenvalue = Eigenvalue, Factor = Factor)
  out <- ggplot2::ggplot(eidf, ggplot2::aes(x = Factor, y = Eigenvalue)) +
    ggplot2::geom_point() + ggplot2::geom_line() +
    ggplot2::scale_x_discrete(limits = 1:nf, labels = 1:nf)
  if (!missing(ylim))
    out <- out + ggplot2::ylim(ylim)
  if (!is.null(h))
    out <- out + ggplot2::geom_abline(slope = 0, intercept = h)
  return(out)
}

#' @export
print.fastudy <- function(x, digits = 3, cutoff = 0.1, ...) {
  cat("\nExploratory Factor Analysis\n")
  print(x$loadings, digits = digits, cutoff = cutoff, ...)
  cat("\n")
}
