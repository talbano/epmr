#' Differential Item Functioning Study
#'
#' Functions for estimating differential item functioning with a set of
#' scored item responses.
#'
#' @param x matrix or data.frame of scored item responses.
#' @param groups vector defining dichotomous grouping variable.
#' @param focal string identifying label used in \code{groups}
#' to represent the focal group.
#' @param scores optional vector of construct scores, defaulting to row sums
#' over \code{x} when missing.
#' @param subset optional vector for selecting a subset of columns from
#' \code{x} that contain scored item responses.
#' @param complete logical with default \code{TRUE} indicating whether or not
#' \code{x} should be reduced to rows with complete data across all columns.
#' @param na.rm logical with default \code{FALSE} specifying whether missings
#' should be removed before calculating individual descriptives.
#'
#' @examples
#' # Calculate total reading scores, as in Chapter 2
#' ritems <- c("r414q02", "r414q11", "r414q06", "r414q09",
#'   "r452q03", "r452q04", "r452q06", "r452q07", "r458q01",
#'   "r458q07", "r458q04")
#' rsitems <- paste0(ritems, "s")
#' x_items <- PISA09[PISA09$cnt == "BEL", rsitems]
#' x_items <- na.omit(x_items)
#' groups <- sample(c("m", "f"), nrow(x_items), replace = TRUE)
#' x_totals <- rowSums(x_items, na.rm = TRUE)
#' difstudy(x = x_items, groups = groups, focal = "f", scores = x_totals)
#'
#' @export
difstudy <- function(x, groups, focal, scores, subset = 1:ncol(x),
  complete = TRUE, na.rm = FALSE) {
  if (complete) xc <- complete.cases(x)
  else xc <- 1:nrow(x)
  x <- x[xc, subset]
  if (missing(scores))
    scores <- rowSums(x, na.rm = na.rm)
  else scores <- scores[xc]
  groups <- as.character(groups[xc])
  if (length(unique(groups)) != 2)
    stop("only two levels supported in 'groups'")
  if (!focal %in% groups)
    stop("'groups' must contain one or more values coded as 'focal'")
  out <- mhd(x, groups, focal, scores)
  class(out) <- c("difstudy", "data.frame")
  return(out)
}

# Dichotomous Mantel-Haenszel DIF
#' @rdname difstudy
mhd <- function(x, groups, focal, scores) {
  ni <- ncol(x)
  x_scale <- sort(unique(scores))
  ns <- length(x_scale)
  out <- data.frame(mh = numeric(ni), chisq = numeric(ni))
  for (i in 1:ni) {
    y <- table(unlist(x[, i]), groups == focal, scores)
    n_a <- y[2, 1, ] # correct, reference
    n_b <- y[1, 1, ] # incorrect, reference
    n_c <- y[2, 2, ] # correct, focal
    n_d <- y[1, 2, ] # incorrect, focal
    n_cor <- n_a + n_c
    n_inc <- n_b + n_d
    n_ref <- n_a + n_b
    n_foc <- n_c + n_d
    n_t <- apply(y, 3, sum)
    e_a <- n_ref * n_cor / n_t
    v_a <- (n_ref * n_cor * n_foc * n_inc) / (n_t^2 * (n_t - 1))
    out$mh[i] <- sum(n_a * n_d / n_t) / sum(n_b * n_c / n_t)
    out$chisq[i] <- (abs(sum(n_a) - sum(e_a)) - .5)^2 / sum(v_a)
  }
  out$delta <- log(out$mh) * -2.35
  out$delta_abs <- abs(out$delta)
  out$chisq_p <- pchisq(out$chisq, 1, lower.tail = FALSE)
  out$ets_level <- ""
  out$ets_level[out$delta_abs < 1 | out$chisq_p >= .05] <- "a"
  out$ets_level[out$delta_abs > 1 & out$delta_abs < 1.5 &
    out$chisq_p < .05] <- "b"
  out$ets_level[out$delta_abs > 1.5 & out$chisq_p < .05] <- "c"
  out <- out[, c("mh", "delta", "delta_abs", "chisq", "chisq_p", "ets_level")]
  return(out)
}

#' @export
print.difstudy <- function(x, digits = 3, ...) {
  cat("\nDifferential Item Functioning Study\n\n")
  print.data.frame(x, digits = digits, ...)
  cat("\n")
}
