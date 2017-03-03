#' Item Option Study
#'
#' Functions for analyzing distributions of unscored item responses by
#' ability, also known as distractor analysis.
#'
#' @param x matrix or data.frame of unscored item responses.
#' @param groups optional grouping variable as a vector with length
#' \code{nrow(x)}. When \code{cuts} are given, groups are created by comparing
#' \code{scores} to \code{cuts}.
#' @param scores optional vector of scores defining the ability or trait
#' levels of respondents. When \code{key} is provided, scores are calculated
#' by first scoring item responses in \code{x}.
#' @param cuts vector of cut scores used to define groups.
#' @param key vector of keyed responses, one per column in \code{x}.
#' @param labels character vector for labeling groups in output, with length
#' \code{length(groups)}.
#' @param itemid vector of labels for items, defaulting to column names in
#' \code{x}, if present, otherwise "Item1", "Item2", etc.
#' @param filename optional path to file where output will be written.
#' @param sep character string used to separate output in written file.
#' @return Returns three lists of distractor analysis tables, one per item,
#' all stored within a larger list. These lists are \code{counts} for raw
#' frequencies, \code{rowpct} for percentages by row, and \code{colpct} for
#' percentages by column. Each table within each of these lists contains the
#' distribution of unscored item responses by group.
#' @export
ostudy <- function(x, groups, scores, cuts = c(0, 1/3, 2/3, 1), key,
  labels = c("low", "mid", "high"),
  itemid = colnames(x), filename, sep = ",") {

  x <- cbind(x)
  ni <- ncol(x)

  if (is.null(itemid))
    itemid <- paste("Item", 1:ni, sep = "")

  if (missing(groups)) {
    if (missing(scores)) {
      if (missing(key))
        stop("Either 'groups', 'scores', or 'key' must be provided")
      else {
        x <- x[complete.cases(x), ]
        xs <- matrix(ifelse(unlist(x) ==
          rep(key, each = nrow(x)), 1, 0), ncol = ni)
        scores <- apply(xs, 1, sum)
      }
    }
    cc <- complete.cases(cbind(x, scores))
    if(!all(cc)) {
      warning(sum(!cc), " cases with missing data removed.")
      x <- cbind(x[cc, ])
      scores <- scores[cc]
    }
    cuts <- quantile(scores, cuts)
    groups <- cut(scores, cuts, include.lowest = TRUE,
      labels = labels)
  }

  out <- vector("list", length = ni)
  for (i in 1:ni) {
    out[[i]] <- table(x[, i], groups)
    if (!missing(key))
      rownames(out[[i]])[rownames(out[[i]]) == key[i]] <-
        paste(key[i], "*", sep = "")
  }
  names(out) <- itemid

  if (!missing(filename)) {
    tempcon <- file(filename, "a")
    for (i in itemid) {
      temptab <- cbind(i = rownames(out[[i]]),
        matrix(c(out[[i]]), ncol = ncol(out[[i]])))
        temptab <- rbind(c(i, colnames(out[[i]])), temptab)
        temptab <- unlist(apply(temptab, 1, paste,
          collapse = sep))
      writeLines(temptab, tempcon)
    }
    close(tempcon)
  }

	rowpct <- lapply(out, function(x)
		t(round(apply(x, 1, function(y) y / sum(y)), 2) * 100))
	colpct <- lapply(out, function(x)
		round(apply(x, 2, function(y) y / sum(y)), 2) * 100)

	out <- list(counts = out, rowpct = rowpct,
		colpct = colpct)

  return(out)
}
