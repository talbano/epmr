#' Criterion Validity Study
#'
#' Functions for estimating validity coefficients
#'
#' @param x,y vectors of scores of the same length from a test
#' \code{x} and the criterion it is intended to predict \code{y}.
#' @param covariates \code{data.frame} of covariates to control for when
#' finding the consequential validity ratio.
#' @param ... further arguments passed to or from other functions.
#' @param rx,ry reliabilities of \code{x} and \code{y} used to correct for
#' attenuation in the validity coefficient.
#' @return \code{critstudy} returns a list of results.
#'
#' @examples
#'
#' ritems <- c("r414q02", "r414q11", "r414q06", "r414q09",
#'   "r452q03", "r452q04", "r452q06", "r452q07", "r458q01",
#'   "r458q07", "r458q04") |> paste0("s")
#' sitems <- c("s428q01", "s131q02d", "s131q04d", "s514q02", "s514q04",
#'   "s428q05", "s438q01t", "s514q03", "s465q01", "s465q02", "s438q02",
#'   "s465q04", "s438q03d", "s428q03", "s415q02", "s415q08t",
#'   "s415q07t")|> paste0("s")
#' pisa <- subset(PISA09, bookid == 3)
#' critstudy(x = rowSums(pisa[, ritems], na.rm = TRUE),
#'    y = rowSums(pisa[, sitems], na.rm = TRUE),
#'    covariates = pisa[, c("escs", "sex", "homelang", "pared")])
#'
#' @export
critstudy <- function(x, y, covariates, rx = 1, ry = 1, ...) {

  df <- data.frame(y = y, x = x, covariates)
  m_base <- lm(y ~ x, data = df, ...) |> summary()
  r2_base <- c(m_base$r.squared, m_base$adj.r.squared)
  m_main <- lm(y ~ ., data = df, ...) |> summary()
  m_int <- paste("y ~ x + (", paste(colnames(covariates), collapse = " + "),
    ")^", ncol(covariates)) |> as.formula() |> lm(data = df, ...) |>
    summary()
  r2_main <- c(m_main$r.squared, m_main$adj.r.squared)
  r2_int <- c(m_int$r.squared, m_int$adj.r.squared)
  names(r2_base) <- names(r2_main) <- names(r2_int) <- c("raw", "adjusted")
  r <- crit_r(x, y, rx, ry)

  out <- list(r = r, r2_base = r2_base, r2_main = r2_main, r2_int = r2_int,
    cvr_main = r2_base / r2_main, cvr_int = r2_base / r2_int, rx = rx, ry = ry)
  class(out) <- "critstudy"

  return(out)
}

#' @export
print.critstudy <- function(x, digits = 3, ...) {
  cat("\nCriterion Validity Study\n\n")
  cat("R with rx ", round(x$rx, digits), " and ry ", round(x$ry, digits), ": ",
    round(x$r, digits), "\n\n", sep = "")
  rbind("R-squared" = x$r2_base, "R-squared main" = x$r2_main,
    "R-squared interaction" = x$r2_int, "CVR main" = x$cvr_main,
    "CVR interaction" = x$cvr_int) |>
    print(digits = digits)
}

#' @rdname critstudy
#' @export
crit_r <- function(x, y = NULL, rx, ry) {
  cor(x, y) / sqrt(rx * ry)
}
