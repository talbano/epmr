#' Reliability Study
#'
#' Functions for estimating internal consistency reliability, adjusting
#' reliability, and estimating the standard error of measurement.
#'
#' @param x a matrix of scored item responses, or a variance/covariance matrix
#' for a set of scored item responses when \code{sigma = TRUE}.
#' @param n number of participants, defaulting to number of rows in
#' \code{x} when \code{sigma = FALSE}.
#' @param sigma logical with default \code{FALSE} indicating whether or not
#' \code{x} is a variance/covariance matrix.
#' @param use string for specifying how to handle incomplete cases
#' @param se logical with default \code{FALSE} for finding analytical
#' standard errors and confidence intervals for alpha. Requires that
#' \code{x} be a matrix of scored item responses, or \code{n} be provided.
#' @param r the reliability.
#' @param k factor by which test length changes, used to predict a
#' corresponding change in reliability.
#' @param sd the standard deviation of total observed scores.
#' @return Returns a data frame of estimated reliability coefficients,
#' currently including alpha and omega, with corresponding standard errors of
#' measurement. Reliability coefficients can also be obtained individually
#' with \code{coef_alpha} and \code{coef_omega}. \code{sb_r} returns a new
#' reliability coefficient predicted using the Spearman-Brown formula.
#' \code{sem} returns the standard error of measurement.
#'
#' @examples
#' ritems <- c("r414q02", "r414q11", "r414q06", "r414q09", "r452q03",
#'   "r452q04", "r452q06", "r452q07", "r458q01", "r458q07", "r458q04")
#' rsitems <- paste0(ritems, "s")
#' rstudy(PISA09[PISA09$cnt == "JPN", rsitems], se = TRUE)
#'
#' @export
rstudy <- function(x, n, sigma = FALSE, use = "everything", se = FALSE) {

  alpha <- coef_alpha(x, n = n, sigma = sigma, use = use, se = se)
  omega <- coef_omega(x, sigma = sigma)
  out <- list(alpha = alpha$alpha, omega = omega, ni = alpha$ni,
    sigma = alpha$sigma, sd = sqrt(sum(alpha$sigma)), n = alpha$n,
    q = alpha$q, alpha_se = alpha$se, alpha_ci = alpha$ci)
  class(out) <- "rstudy"

  return(out)
}

#' @export
print.rstudy <- function(x, digits = 3, ...) {
  cat("\nReliability Study\n\n")
  cat("Number of items:", x$ni, "\n\n")
  cat("Number of cases:", x$n, "\n\n")
  coef <- c(alpha = x$alpha, omega = x$omega)
  out <- data.frame(coef = coef, sem = sem(coef, x$sd))
  cat("Estimates:\n")
  print.data.frame(out, digits = digits, ...)
  cat("\n")
  if (!is.null(x$alpha_se)) {
    cat("Alpha se:", round(x$alpha_se, 2), "\n\n")
    cat("Alpha 95% CI:", paste(round(x$alpha_ci, 2), collapse = ", "),
      "\n\n")
  }
}

#' @rdname rstudy
#' @export
coef_alpha <- function(x, n = NULL, sigma = FALSE, use = "everything",
  se = FALSE) {

  x <- as.matrix(x)
  use <- match.arg(use, c("all.obs", "complete.obs", "pairwise.complete.obs",
    "everything", "na.or.complete"))
  if (any(!dim(x)))
    stop("'x' must be a covariance matrix, or data.frame or matrix of ",
      "item responses")
  if (!sigma) {
    if (use == "complete.obs") n <- sum(complete.cases(x))
    else n <- dim(x)[1]
    x <- var(x, use = use)
  }
  ni <- dim(x)[2]
  s2 <- sum(x)
  trx <- sum(diag(x))
  alpha <- (1 - trx/s2) * ni/(ni - 1)
  if (se) {
    if (is.null(n)) {
      stop("when 'se = TRUE', x must be a matrix of scored item responses, ",
        "or 'n' must be provided")
    }
    else {
      q <- (2 * ni^2) / ((ni - 1)^2 * s2^3) *
        (s2 * (sum(diag(x * x)) + trx^2) - 2 * trx * sum(x * x))
      se <- sqrt(q / n)
      ci <- c(max(0, alpha - 1.96 * se), min(alpha + 1.96 * se, 1))
    }
  }
  else {
    q <- se <- ci <- NULL
  }
  return(list(alpha = alpha, q = q, se = se, ci = ci, sigma = x,
    n = n, ni = ni))
}

#' @rdname rstudy
#' @export
coef_omega <- function(x, sigma = FALSE, use = "everything") {

  x <- as.matrix(x)
  ni <- ncol(x)

  if (any(!dim(x)))
    return(NA)
  if (!sigma) {
    use <- match.arg(use, c("all.obs", "complete.obs", "pairwise.complete.obs",
      "everything", "na.or.complete"))
    x <- var(x, use = use)
  }

  lambda <- factanal(covmat = x, factors = 1)$loadings[1:ni] * sqrt(diag(x))

  return(sum(lambda)^2 / sum(x))
}

#' @rdname rstudy
#' @export
sb_r <- function(r, k) {

  return((k * r)/((k - 1) * r + 1))
}

#' @rdname rstudy
#' @export
sem <- function(r, sd) {

  return(sd * sqrt(1 - r))
}
