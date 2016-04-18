#' Rescaling Scores
#'
#' \code{setrange} truncates a quantitative variable to have a set minimum
#' and maximum. \code{setmin} truncates only to the minimum, and \code{setmax}
#' truncates only to the maximum. \code{setmean} and \code{setsd} set the
#' mean and standard deviation.
#'
#' @param x vector of scores to be modified.
#' @param y optional vector of scores from which new parameters will be taken.
#' @param to numeric value(s) that parameters in \code{x} will be set to,
#' defaulting to the \code{min}, \code{max}, \code{range}, \code{mean}, or
#' \code{sd} of \code{y}.
#' @name rescaling
#' @return Returns a vector of \code{length(x)} scores, with new parameters.
NULL

#' @rdname rescaling
#' @export
setrange <- function(x, y, to = range(y)) {

  x[x <= to[1]] <- to[1]
  x[x >= to[2]] <- to[2]
  return(x)
}

#' @rdname rescaling
#' @export
setmin <- function(x, y, to = min(y)) {

  x[x <= to[1]] <- to[1]
  return(x)
}

#' @rdname rescaling
#' @export
setmax <- function(x, y, to = max(y)) {

  x[x >= to[1]] <- to[1]
  return(x)
}

#' @rdname rescaling
#' @export
setmean <- function(x, y, to = mean(y)) {

  x <- x - mean(x) + to
  return(x)
}

#' @rdname rescaling
#' @export
setsd <- function(x, y, to = sd(y)) {

  x <- x / sd(x) * to
  return(x)
}
