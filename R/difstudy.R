#' Differential Item Functioning Study
#'
#' Functions for estimating differential item functioning with a set of
#' scored item responses.
#'
#' @param x matrix or data.frame of scored item responses.
#' @param subset optional vector for selecting a subset of columns from \code{x}.
#' @param scores optional vector of construct scores.
#' @export
difstudy <- function(x, subset = 1:ncol(x), scores, complete = TRUE,
  na.rm = FALSE) {

}
