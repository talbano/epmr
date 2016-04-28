#' Measurement Scale Study
#'
#' Functions for running multiple studies on a set of scored item responses,
#' including a descriptive study of the total scale scores, and item,
#' internal consistency reliability, and item response theory studies.
#'
#' @param x matrix or data.frame of scored item responses.
#' @param subset optional vector for selecting a subset of columns from \code{x}.
#' @param scores optional vector of construct scores.
#' @param complete logical with default \code{TRUE} indicating whether or not
#' \code{x} should be reduced to rows with complete data across all columns.
#' @param na.rm logical with default \code{FALSE} specifying whether missings
#' should be removed before calculating individual descriptives.
#' @return Returns a list with elements pertaining to each study, including:
#' \code{descriptives}, \code{items}, \code{reliability}, and \code{irt}.
#' @export
mstudy <- function(x, subset = 1:ncol(x), scores, complete = TRUE,
  na.rm = FALSE) {

  itm <- istudy(x, subset = subset, scores = scores)
  des <- dstudy(rowSums(x, na.rm = TRUE), complete = complete,
    na.rm = na.rm)
  rel <- rstudy(x)
}
