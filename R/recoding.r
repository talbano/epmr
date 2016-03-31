#' Recoding Data
#'
#' Recodes or reverse codes a vector of scores
#'
#' @param x vector of scores to be recoded
#' @param old list of old values in \code{x} which will be recoded according to
#' the name of the list element, unless new is specified.
#' @param new vector of new values to be assigned to corresponding value(s)
#' in \code{old}, assuming \code{old} and \code{new} are the same length.
#' @param rev logical, with default \code{FALSE}. If TRUE, old and new are
#' ignored and \code{x} is simply reverse coded. Assumes all possible values
#' are at least ordinal scale quantitative and observed.
#' @return Returns a vector of recoded scores.
#' @export
recode <- function(x, old, new = names(old), rev = TRUE) {

	if(rev) {
		old <- sort(unique(x))
		new <- old[order(old, decreasing = TRUE)]
	}

	index <- lapply(old, function(y) x %in% y)

	for(i in 1:length(index))
		x[index[[i]]] <- new[i]

	return(x)
}
