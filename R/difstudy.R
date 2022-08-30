#' Differential Item Functioning Study
#'
#' Functions for estimating differential item functioning with a set of
#' scored item responses.
#'
#' @param x matrix or data.frame of scored item responses.
#' @param groups vector defining dichotomous grouping variable.
#' @param focal string identifying label used in \code{groups}
#' to represent the focal group, with no default.
#' @param scores optional vector of construct scores, defaulting to row sums
#' over all columns in \code{x} when \code{NULL} and \code{anchor_items} is
#' not specified.
#' @param anchor_items vector of item names or column numbers for
#' identifying anchoring items in \code{x}. These will be used when finding
#' construct scores if \code{scores} is missing, defaulting to all columns in
#' \code{x}.
#' @param dif_items vector of item names or column numbers identifying what
#' items in \code{x} should be analyzed for DIF, defaulting
#' to all columns in \code{x}.
#' @param complete logical with default \code{TRUE} indicating whether or not
#' \code{x}, \code{groups}, and \code{scores} should be reduced to cases
#' with complete data.
#' @param na.rm logical with default \code{FALSE} specifying whether missings
#' should be removed before calculating individual descriptives.
#' @param p_cut numeric cutoff for evaluating the p-value for the
#' Mantel-Haenszel chi-square, with default 0.05.
#' @param sdp_w string for choosing the weight when finding the standardized
#' difference statistic, defaulting to the number of test takers in the
#' focal group.
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
difstudy <- function(x, groups, focal, scores = NULL,
  anchor_items = 1:ncol(x), dif_items = 1:ncol(x), complete = TRUE,
  na.rm = FALSE, p_cut = 0.05, sdp_w = c("focal", "reference", "total")) {
  if (!all(unlist(x) %in% c(0, 1, NA)))
    stop("'x' can only contain score values 0, 1, and NA.")
  if (complete)
    xc <- complete.cases(x, groups, scores)
  else xc <- 1:nrow(x)
  x <- x[xc, ]
  if (is.null(scores))
    scores <- rowSums(x[, anchor_items], na.rm = na.rm)
  else scores <- scores[xc]
  groups <- as.character(groups[xc])
  if (length(unique(groups)) != 2)
    stop("only two levels supported in 'groups'")
  if (!focal %in% groups)
    stop("'groups' must contain one or more values coded as 'focal'")
  sdp_w <- match.arg(sdp_w)
  out <- dif_prop(x[, dif_items], groups, focal, scores, p_cut, sdp_w)
  out <- data.frame(item = paste(dif_items), out)
  class(out) <- c("difstudy", "data.frame")
  return(out)
}

# Dichotomous Mantel-Haenszel and Standardization DIF
#' @rdname difstudy
dif_prop <- function(x, groups, focal, scores, p_cut, sdp_w) {
  ni <- ncol(x)
  x_scale <- sort(unique(scores))
  ns <- length(x_scale)
  out <- data.frame(rn = numeric(ni), fn = numeric(ni), r1 = numeric(ni),
    f1 = numeric(ni), r0 = numeric(ni), f0 = numeric(ni), mh = numeric(ni),
    chisq = numeric(ni), sdp = numeric(ni))
  for (i in 1:ni) {
    xi <- unlist(x[, i])
    y <- table(xi, groups == focal, scores)
    out$rn[i] <- sum(y[, 1, ])
    out$fn[i] <- sum(y[, 2, ])
    out$r1[i] <- sum(y[2, 1, ])
    out$r0[i] <- sum(y[1, 1, ])
    out$f1[i] <- sum(y[2, 2, ])
    out$f0[i] <- sum(y[1, 2, ])
    if (sd(xi, na.rm = TRUE) == 0)
      out$mh[i] <- out$chisq[i] <- NA
    else {
      n_a <- y[2, 1, ] # correct, reference
      n_b <- y[1, 1, ] # incorrect, reference
      n_c <- y[2, 2, ] # correct, focal
      n_d <- y[1, 2, ] # incorrect, focal
      n_cor <- as.numeric(n_a + n_c)
      n_inc <- as.numeric(n_b + n_d)
      n_ref <- as.numeric(n_a + n_b)
      n_foc <- as.numeric(n_c + n_d)
      n_t <- apply(y, 3, sum)
      e_a <- n_ref * n_cor / n_t
      v_a <- (n_ref * n_cor * n_foc * n_inc) / (n_t^2 * (n_t - 1))
      out$mh[i] <- sum(n_a * n_d / n_t, na.rm = TRUE) /
        sum(n_b * n_c / n_t, na.rm = TRUE)
      out$chisq[i] <- (abs(sum(n_a, na.rm = TRUE) - sum(e_a, na.rm = TRUE)) -
          .5)^2 / sum(v_a, na.rm = TRUE)
      k <- switch(sdp_w, focal = n_foc, reference = n_ref, total = n_t)
      out$sdp[i] <- sum(k * (n_c / n_foc - n_a / n_ref), na.rm = TRUE) /
        sum(k, na.rm = TRUE)
    }
  }
  out$delta <- log(out$mh) * -2.35
  out$delta_abs <- abs(out$delta)
  out$chisq_p <- pchisq(out$chisq, 1, lower.tail = FALSE)
  out$ets_level <- out$sdp_level <- ""
  out$ets_level[out$delta_abs < 1 | out$chisq_p >= p_cut] <- "a"
  out$ets_level[out$delta_abs >= 1 & out$delta_abs < 1.5 &
    out$chisq_p < p_cut] <- "b"
  out$ets_level[out$delta_abs >= 1.5 & out$chisq_p < p_cut] <- "c"
  out$sdp_level[abs(out$sdp) < .05] <- "a"
  out$sdp_level[abs(out$sdp) >= .05 & abs(out$sdp) < .10] <- "b"
  out$sdp_level[abs(out$sdp) >= .10] <- "c"
  out <- out[, c("rn", "fn", "r1", "f1", "r0", "f0", "mh", "delta",
    "delta_abs", "chisq", "chisq_p", "ets_level", "sdp", "sdp_level")]
  return(out)
}

#' @export
print.difstudy <- function(x, digits = 2, ...) {
  cat("\nDifferential Item Functioning Study\n\n")
  print.data.frame(x, digits = digits, ...)
  cat("\n")
}
