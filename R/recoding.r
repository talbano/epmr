#' Recoding Data
#'
#' Recodes or reverse codes a vector of scores
#'
#' @param x vector of scores to be recoded.
#' @param from list of old values in \code{x} which will be recoded according to
#' the name of the list element, unless \code{to} is specified.
#' @param to vector of new values to be assigned to corresponding value(s)
#' in \code{from}, assuming \code{from} and \code{to} are the same length.
#' @param reverse logical, with default \code{TRUE} when \code{from} is missing.
#' If TRUE, old and new are ignored and \code{x} is simply reverse coded.
#' Assumes all possible values are at least ordinal scale quantitative and
#' observed.
#' @return Returns a vector of recoded scores.
#' @export
recode <- function(x, from, to = names(from), reverse = missing(from)) {

	if(reverse) {
		from <- sort(unique(x))
		to <- from[order(from, decreasing = TRUE)]
	}

	index <- lapply(from, function(y) x %in% y)

	for(i in 1:length(index))
		x[index[[i]]] <- to[i]

	return(x)
}
