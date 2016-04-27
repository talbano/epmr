#' Descriptive Study
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
#' @return \code{dstudy} returns a data frame of descriptive statistics, including
#' the mean, median, standard deviation, skewness, kurtosis, minimum, maximum,
#' number of complete cases, and number of NAs (in columns), by variable
#' (in rows). \code{kurt} and \code{skew} return the kurtosis and skewness.
#' \code{summiss} and \code{sumcomp} count missings and complete cases.
#'
#' @export
dstudy <- function(x, complete = TRUE, na.rm = FALSE) {

  x <- cbind(x)
  if(complete)
    x <- cbind(x[complete.cases(x), ])
  out <- t(apply(x, 2, function(y) c(mean(y, na.rm = na.rm),
    median(y, na.rm = na.rm),
    sd(y, na.rm = na.rm),
    skew(y, na.rm = na.rm),
    kurt(y, na.rm = na.rm),
    min(y, na.rm = na.rm),
    max(y, na.rm = na.rm),
    sumcomp(y),
    summiss(y))))

  rownames(out) <- colnames(x)
  colnames(out) <-
    c("mean", "median", "sd", "skew", "kurt", "min", "max", "n", "na")
  out <- data.frame(out)
  class(out) <- c("dstudy", "data.frame")
  return(out)
}

#' @export
print.dstudy <- function(x, digits = 3, ...) {
  print.data.frame(x, digits = digits, ...)
}

#' @rdname dstudy
#' @export
kurt <- function (x, na.rm = FALSE) {

  mx <- mean(x, na.rm = na.rm)
  sx <- sd(x, na.rm = na.rm)
  return(sum((x - mx)^4)/(length(x) * sx^4))
}

#' @rdname dstudy
#' @export
skew <- function(x, na.rm = FALSE) {

  mx <- mean(x, na.rm = na.rm)
  sx <- sd(x, na.rm = na.rm)
  return(sum((x - mx)^3)/(length(x) * sx^3))
}

#' @rdname dstudy
#' @export
summiss <- function(x) sum(is.na(x))

#' @rdname dstudy
#' @export
sumcomp <- function(x) sum(complete.cases(x))
