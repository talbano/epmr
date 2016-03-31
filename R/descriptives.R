#' Descriptive Analyses
#'
#' Functions for obtaining descriptive statistics for a vector or matrix
#' of quantitative variables.
#'
#' @param x vector, matrix, or data.frame of scores with one column
#' per variable.
#' @param complete logical with default \code{TRUE} indicating whether or not
#' \code{x} should be reduced to rows with complete data across all columns.
#' @param na.rm logical with default \code{FALSE} specifying whether missings
#' should be removed before calculating individual descriptives.
#' @return Returns a matrix of descriptive statistics, including the mean,
#' median, standard deviation, skewness, kurtosis, minimum, maximum,
#' and number of complete cases (in columns) by variable (in rows).
#' @export
desanaly <- function(x, complete = TRUE, na.rm = FALSE) {

  x <- cbind(x)
  if(complete)
    x <- x[complete.cases(x), ]
  out <- t(apply(x, 2, function(y) c(mean(y, na.rm = na.rm),
    median(y, na.rm = na.rm),
    sd(y, na.rm = na.rm),
    skew(y, na.rm = na.rm),
    kurt(y, na.rm = na.rm),
    min(y, na.rm = na.rm),
    max(y, na.rm = na.rm),
    sum(complete.cases(y)))))

  rownames(out) <- colnames(x)
  colnames(out) <-
    c("mean", "median", "sd", "skew", "kurt", "min", "max", "n")

  return(out)
}

kurt <- function (x, na.rm = FALSE) {

  mx <- mean(x, na.rm = na.rm)
  sx <- sd(x, na.rm = na.rm)
  return(sum((x - mx)^4)/(length(x) * sx^4))
}

skew <- function(x, na.rm = FALSE) {

  mx <- mean(x, na.rm = na.rm)
  sx <- sd(x, na.rm = na.rm)
  return(sum((x - mx)^3)/(length(x) * sx^3))
}
