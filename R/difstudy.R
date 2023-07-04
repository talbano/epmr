#' Differential Item Functioning Study
#'
#' Functions for estimating differential item functioning (DIF) with a set of
#' scored item responses.
#'
#' @param x matrix or data.frame of scored item responses.
#' @param groups vector defining dichotomous grouping variable.
#' @param ref string identifying label used in \code{groups}
#' to represent the reference group, with no default.
#' @param scores optional vector of construct scores, defaulting to row sums
#' over all columns in \code{x} when \code{NULL} and \code{anchor_items} is
#' not specified.
#' @param method what DIF method to use, with \code{"mh"} for Mantel-Haenszel
#' and \code{"lr"} for logistic regression.
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
#' # Vector of item names
#' ritems <- c("r414q02", "r414q11", "r414q06", "r414q09",
#'   "r452q03", "r452q04", "r452q06", "r452q07", "r458q01",
#'   "r458q07", "r458q04")
#' rsitems <- paste0(ritems, "s")
#' # Subset PISA09 to Belgium and Germany
#' pisa <- subset(PISA09, cnt %in% c("BEL", "DEU"))
#' x_totals <- rowSums(pisa[, rsitems], na.rm = TRUE)
#' # Uniform DIF with Mantel-Haenszel
#' difstudy(x = pisa[, rsitems], groups = pisa$cnt, ref = "BEL",
#'   scores = x_totals, method = "mh")
#' # Uniform and nonuniform DIF with logistic regression
#' difstudy(x = pisa[, rsitems], groups = pisa$cnt, ref = "BEL",
#'   method = "lr")
#'
#' @export
difstudy <- function(x, groups, ref, scores = NULL, method = c("mh", "lr"),
  anchor_items = 1:ncol(x), dif_items = 1:ncol(x), complete = TRUE,
  na.rm = FALSE, p_cut = 0.05, sdp_w = c("focal", "reference", "total")) {
  x <- as.data.frame(x)
  if (!all(unlist(x) %in% c(0, 1, NA)))
    stop("'x' can only contain score values 0, 1, and NA.")
  if (complete)
    xc <- complete.cases(x, groups, scores)
  else xc <- 1:nrow(x)
  x <- x[xc, ]
  if (is.null(scores))
    scores <- rowSums(x[, anchor_items], na.rm = na.rm)
  else scores <- scores[xc]
  groups <- data.frame(groups)[xc, , drop = FALSE]
  for(i in seq_along(ref))
    groups[, i] <- relevel(factor(groups[, i]), ref = ref[i])
  method <- match.arg(method)
  sdp_w <- match.arg(sdp_w)
  if (method == "mh") {
    dif_out <- dif_cat(x[, dif_items], groups, ref, scores, p_cut, sdp_w)
    out <- list(method = method, uniform = dif_out)
  }
  else if (method == "lr") {
    dif_out <- dif_lr(x[, dif_items], groups, scores, p_cut)
    out <- c(method = method, dif_out)
  }
  class(out) <- c("difstudy", "list")
  return(out)
}

# Dichotomous Mantel-Haenszel and Standardization DIF
dif_cat <- function(x, groups, ref, scores, p_cut, sdp_w) {
  ni <- ncol(x)
  x_scale <- sort(unique(scores))
  ns <- length(x_scale)
  out <- data.frame(mh = numeric(ni), chisq = numeric(ni), sdp = numeric(ni))
  for (i in 1:ni) {
    xi <- unlist(x[, i])
    y <- table(xi, groups[, 1] != ref[1], scores)
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
  out$ets_lev <- out$sdp_lev <- ""
  out$ets_lev[out$delta_abs < 1 | out$chisq_p >= p_cut] <- "a"
  out$ets_lev[out$delta_abs >= 1 & out$delta_abs < 1.5 &
    out$chisq_p < p_cut] <- "b"
  out$ets_lev[out$delta_abs >= 1.5 & out$chisq_p < p_cut] <- "c"
  out$sdp_lev[abs(out$sdp) < .05] <- "a"
  out$sdp_lev[abs(out$sdp) >= .05 & abs(out$sdp) < .10] <- "b"
  out$sdp_lev[abs(out$sdp) >= .10] <- "c"
  out <- out[, c("mh", "delta", "chisq", "chisq_p", "ets_lev", "sdp",
    "sdp_lev")]
  rownames(out) <- colnames(x)
  return(out)
}

# Dichotomous Logistic Regression DIF
dif_lr <- function(x, groups, scores, p_cut) {
  ni <- ncol(x)
  glm_n <- glm_u <- glm_x <- vector("list", ni)
  for (i in 1:ni) {
    temp_df <- data.frame(y = x[[i]], t = scores, groups)
    glm_n[[i]] <- glm(y ~ t, family = "binomial", data = temp_df)
    glm_u[[i]] <- glm(y ~ t + ., family = "binomial", data = temp_df)
    glm_x[[i]] <- glm(y ~ t * ., family = "binomial", data = temp_df)
  }
  dev <- cbind(sapply(glm_n, deviance),
    sapply(glm_u, deviance), sapply(glm_x, deviance))
  r2 <- cbind(sapply(glm_n, r2_nag),
    sapply(glm_u, r2_nag), sapply(glm_x, r2_nag))
  deg <- cbind(sapply(glm_n, df.residual),
    sapply(glm_u, df.residual), sapply(glm_x, df.residual))
  chi <- cbind(dev[, 1] - dev[, 2:3], dev[, 2] - dev[, 3])
  chidf <- cbind(deg[, 1] - deg[, 2:3], deg[, 2] - deg[, 3])
  r2d <- cbind(r2[, 2:3] - r2[, 1], r2[, 3] - r2[, 2])
  chip <- pchisq(chi, chidf, lower.tail = FALSE)
  dif <- lapply(1:3, function(i)
    data.frame(chisq = chi[, i], chisq_df = chidf[, i],
      chisq_p = chip[, i], r2d = r2d[, i],
      zum_lev = rd_zum(r2d[, i], chip[, i], p_cut),
      jod_lev = rd_jod(r2d[, i], chip[, i], p_cut)))
  rownames(dif[[1]]) <- rownames(dif[[2]]) <-
    rownames(dif[[3]]) <- colnames(x)
  out <- list(uniform = dif[[1]], nonuniform = dif[[3]], both = dif[[2]],
    deviance = dev, r2 = r2, glm_n = glm_n,
    glm_u = glm_u, glm_x = glm_x)
  return(out)
}

#' @export
print.difstudy <- function(x, digits = 2, ...) {
  cat("\nDifferential Item Functioning Study\n\n")
  cat(switch(x$method, mh = "Mantel-Haenszel", lr = "Logistic regression"),
    "method\n\n")
  cat("Uniform DIF\n\n")
  print.data.frame(x$uniform, digits = digits, ...)
  cat("\n")
  if (x$method == "lr") {
    cat("Nonuniform DIF\n\n")
    print.data.frame(x$nonuniform, digits = digits, ...)
    cat("\nUniform and nonuniform DIF\n\n")
    print.data.frame(x$both, digits = digits, ...)
    cat("\n")
  }
}

# Pseudo R2, Cox and Snell
r2_cox <- function(object, n = length(object$y)) {
  1 - exp((object$deviance - object$null.deviance) / n)
}

# Pseudo R2, Nagelkerke
r2_nag <- function(object, n = length(object$y)) {
  r2_cox(object, n) / (1 - exp(-object$null.deviance / n))
}

# R2 difference levels, Zumbo
rd_zum <- function(x, p = rep(0, length(x)), p_cut = 0.05) {
  out <- character(length(x))
  out[p >= p_cut | x < 0.13] <- "a"
  out[p < p_cut & x >= 0.13 & x < 0.26] <- "b"
  out[p < p_cut & x >= 0.26] <- "c"
  return(out)
}

# R2 difference levels, Jodoin
rd_jod <- function(x, p = rep(0, length(x)), p_cut = 0.05) {
  out <- character(length(x))
  out[p >= p_cut | x < 0.035] <- "a"
  out[p < p_cut & x >= 0.035 & x < 0.07] <- "b"
  out[p < p_cut & x >= 0.07] <- "c"
  return(out)
}

