#' Differential Item Functioning Study
#'
#' Functions for estimating differential item functioning with a set of
#' scored item responses.
#'
#' @param x matrix or data.frame of scored item responses.
#' @param subset optional vector for selecting a subset of columns from \code{x}.
#' @param scores optional vector of construct scores.
#' @param complete logical with default \code{TRUE} indicating whether or not
#' \code{x} should be reduced to rows with complete data across all columns.
#' @param na.rm logical with default \code{FALSE} specifying whether missings
#' should be removed before calculating individual descriptives.
#' @export
difstudy <- function(x, subset = 1:ncol(x), scores, complete = TRUE,
  na.rm = FALSE) {

}
