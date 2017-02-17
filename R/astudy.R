#' Interrater Agreement Study
#'
#' Functions for estimating agreement in scores from a pair of raters.
#'
#' @param x,y vectors of scores from two different raters for the same units
#' of observation. Alternatively, if \code{y} is missing, \code{x} can be a
#' matrix of scores with two columns, one per rater, or a crosstab of rater
#' scores.
#' @param w matrix of weights applied to adjust the severity of disagreements
#' in calculating weighted kappa. Be default, cells 1 off from the diagonal
#' are weighted 1, 2 off are weighted 2, etc.
#' @param lev vector of all unique possible scores.
#' @return Returns a list of agreement indices.
#' @export
astudy <- function(x, y, w, lev) {

  if (!missing(y))
    x <- data.frame(x, y)
  if (dim(x)[1] != dim(x)[2]) {
    if (ncol(x) > 2)
      stop("too many variables provided in 'x'")
    if (missing(lev))
      lev <- sort(unique(c(unlist(x))))
    x[, 1] <- factor(x[, 1], levels = lev)
    x[, 2] <- factor(x[, 2], levels = lev)
    xtab <- table(x[, 1], x[, 2])
  }
  else xtab <- x
  if(any(is.na(xtab)))
      stop("crosstab contains NAs")

  n <- sum(xtab)
  ftab <- table(x[, 1], x[, 2])
  rt <- rowSums(ftab)
  ct <- colSums(ftab)
  ptab <- (rt %*% t(ct)) / n
	po <- sum(diag(ftab))
	pc <- sum(diag(ptab))
  if(missing(w)) {
    ns <- nrow(xtab) - 1
    w <- matrix(sapply(0:ns, function(i) abs(0:ns - i)),
      nrow = ns + 1)
  }

  out <- c(agree = po / n,
    kappa = (po - pc)/(n - pc),
    wkappa = 1 - sum(ftab * w)/sum(ptab * w))

	return(out)
}
