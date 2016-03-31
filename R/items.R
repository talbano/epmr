#' Classical Item Analyses
#'
#' Functions for estimating classical item analysis indices, including
#' item difficulty (means and standard deviations by item),
#' item discrimination (item-total and corrected item-total correlations),
#' and alpha if item deleted. Descriptive statistics and reliability
#' estimates are also returned at the scale level.
#'
#' @param x matrix or data.frame of scored item responses.
#' @param subset optional vector for selecting a subset of columns from \code{x}.
#' @param scores optional vector of construct scores used to calculate
#' item discrimination.
#' @return Returns a list with three elements: \code{item} contianing a
#' data.frame of item analysis output, \code{scale} containing descriptives
#' for the full scale, and \code{reliability} containing a vector of
#' internal consistency reliability estimates.
#' @export
itemanaly <- function(x, subset = 1:ncol(x), scores) {

  x <- as.matrix(x[, subset])
  p <- apply(x, 2, mean, na.rm = TRUE)
  np <- apply(x, 2, function(x) sum(!is.na(x)))
  nna <- apply(x, 2, function(x) sum(is.na(x)))
	s <- apply(x, 2, sd, na.rm = TRUE)
  total <- apply(x, 1, sum, na.rm = TRUE)
  pb <- apply(x, 2, function(x)
  	cor(x, total, use = "c"))
  cpb <- apply(x, 2, function(x)
  	cor(x, total - x, use = "c"))
  aid <- sapply(1:ncol(x), function(i)
		tryCatch(alpha(x[, -i]), error = function(y) y))

	out <- list(items = data.frame(m = p, sd = s, n = np,
  	na = nna, pb, cpb, aid),
  	scale = danl(total, na.rm = TRUE),
  	reliability = tryCatch(ranl(x), error = function(x) x))
  if(!missing(scores))
    out$items$pb2 <- apply(x, 2, function(x)
      cor(x, scores, use = "c"))

  return(out)
}
