#' Scored Item Study
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
#' @return Returns a list with three elements: \code{item} containing a
#' data.frame of item analysis output, \code{scale} containing descriptives
#' for the full scale, and \code{reliability} containing a vector of
#' internal consistency reliability estimates.
#' @export
istudy <- function(x, subset = 1:ncol(x), scores) {

  x <- as.matrix(x[, subset])
  items <- data.frame(m = colMeans(x, na.rm = TRUE),
    sd = apply(x, 2, sd, na.rm = TRUE),
    n = apply(x, 2, sumcomp),
    na = apply(x, 2, summiss))
  total <- rowSums(x, na.rm = TRUE)
  items$rit <- apply(x, 2, function(y)
  	cor(y, total, use = "c"))
  items$ritc <- apply(x, 2, function(y)
  	cor(y, total - y, use = "c"))
  if(!missing(scores))
    items$rit2 <- apply(x, 2, function(y)
      cor(y, scores, use = "c"))
  items$aid <- sapply(1:ncol(x), function(i)
		tryCatch(alpha(x[, -i]), error = function(y) y))

	out <- list(items = items, alpha = tryCatch(alpha(x),
	  error = function(e) e))
	class(out) <- c("istudy", "list")

  return(out)
}

#' @export
print.istudy <- function(x, digits = 3, ...) {
  cat("\nScored Item Study\n\n")
  cat("Alpha:", round(x$alpha, 4), "\n\n")
  cat("Item statistics:\n")
  print.data.frame(x$items, digits = digits, ...)
}
