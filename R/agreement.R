#' Interrater Agreement
#'
#' Functions for estimating agreement in scores from a pair of raters.
#'
#' @param x,y vectors of scores from two different raters for the same units
#' of observation. Alternatively, if \code{y} is missing, \code{x} can be a
#' matrix of scores with two columns, one per rater.
#' @param lev vector of all unique possible scores.
#' @return Returns a list of agreement indices.
agree <- function(x, y, lev = unique(unlist(x))) {

  if(!missing(y))
    x <- as.matrix(data.frame(x, y))
  if(ncol(x) > 2)
    stop("too many variables provided in 'x'")

  x[, 1] <- factor(x[, 1], levels = lev)
	x[, 2] <- factor(x[, 2], levels = lev)
	ptab <- table(x[, 1], x[, 2])

  n <- sum(ptab)
	po <- sum(diag(ptab))/n
	pc <- sum((rowSums(ptab)/n) * (colSums(ptab)/n))
	out <- list(pctagree = po, kappa = (po - pc)/(1 - pc))

	return(out)
}
