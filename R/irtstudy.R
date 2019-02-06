#' Item Response Theory Study
#'
#' Functions for estimating item response theory item and person parameters.
#' Currently, only the 1 parameter Rasch model is supported.
#'
#' @param x matrix or data.frame of scored item responses, one row per person,
#' one column per item.
#' @param complete logical with default \code{FALSE} indicating whether or not
#' \code{x} should be reduced to rows with complete data across all columns.
#' @param ip vector or matrix of item parameters, with a in the first column, b
#' in the second, and c in the third.
#' @param theta vector of thetas over which to estimate the corresponding function.
#' @param ... further arguments passed to or from other functions, including
#' \code{\link{glmer}}
#' @examples
#'
#' ritems <- c("r414q02", "r414q11", "r414q06", "r414q09",
#'   "r452q03", "r452q04", "r452q06", "r452q07", "r458q01",
#'   "r458q07", "r458q04")
#' rsitems <- paste0(ritems, "s")
#' pisagbr <- PISA09[PISA09$cnt == "GBR", rsitems]
#' irtgbr <- irtstudy(pisagbr)
#'
#' @export
irtstudy <- function(x, complete = FALSE, ...) {

  if (!all(unlist(x) %in% c(0, 1, NA)))
    stop("'x' can only contain score values 0, 1, and NA.")
  ni <- ncol(x)
  if (complete)
    x <- x[complete.cases(x), ]
  else {
    allna <- rowSums(is.na(x)) == ni
    if (any(allna)) {
      x <- x[!allna, ]
      warning(sum(allna), " cases with missing data on all items removed.")
    }
  }

  np <- nrow(x)
  inames <- colnames(x)
  if (is.null(inames))
    inames <- paste0("item", 1:ni)
  xl <- data.frame(score = c(unlist(x)), person = rep(1:np, ni),
    item = rep(1:ni, each = np), row.names = NULL)

  m <- lme4::glmer(score ~ -1 + (1 | item) + (1 | person),
    data = xl, family = "binomial")
  dat <- data.frame(x, theta = unlist(lme4::ranef(m)$person))
  ip <- data.frame(a = 1, b = -unlist(lme4::ranef(m)$item), c = 0,
    row.names = inames)
  dat$se <- rtef(ip, dat$theta)$se

  out <- list(data = dat, ip = ip, np = np, ni = ni,
    vc = unlist(lme4::VarCorr(m)),
    fit = c(AIC = AIC(m), BIC = BIC(m), logLik = logLik(m),
      deviance = deviance(m), df.residual = df.residual(m)))
  class(out) <- c("irtstudy", "list")

  return(out)
}

#' @export
print.irtstudy <- function(x, ...) {
  cat("\nItem Response Theory Study\n\n")
  cat(x$np, "people,", x$ni, "items\n\n")
  cat("Model fit with lme4::glmer\n")
  print(x$fit)
  cat("\nRandom effects\n")
  print(data.frame(Std.Dev = sqrt(x$vc), Var = x$vc,
    row.names = c("person", "item")))
}

# Item response function
#' @rdname irtstudy
#' @export
rirf <- function(ip, theta = seq(-4, 4, length = 100)){

  if (NCOL(ip) == 1)
    ip <- cbind(1, ip, 0)
  else if (NCOL(ip) == 2)
    ip <- cbind(ip, 0)
  if (NCOL(ip) != 3)
    stop("'ip' can only contain up to three parameters per item.")
  ni <- NROW(ip)
  out <- rbind(sapply(1:ni, function(i) ip[i, 3] + (1 - ip[i, 3]) /
      (1 + exp(ip[i, 1] * (-theta + ip[i, 2])))))
  colnames(out) <- rownames(ip)
  out <- data.frame(theta = theta, out)
  return(out)
}

# Item information function
#' @rdname irtstudy
#' @export
riif <- function(ip, theta = seq(-4, 4, length = 100)){

  out <- rirf(ip, theta)
  out <- data.frame(theta = theta, out[, -1] * (1 - out[, -1]))
  colnames(out)[-1] <- rownames(ip)
  return(out)
}

# Item error function
#' @rdname irtstudy
#' @export
rief <- function(ip, theta = seq(-4, 4, length = 100)){

  out <- data.frame(theta = theta, 1 / sqrt(riif(ip, theta)[, -1]))
  colnames(out)[-1] <- rownames(ip)
  return(out)
}

# Test response function
#' @rdname irtstudy
#' @export
rtrf <- function(ip, theta = seq(-4, 4, length = 100)){

  out <- data.frame(theta = theta,
    p = apply(rirf(ip, theta)[, -1], 1, sum))
  return(out)
}

# Test information function
#' @rdname irtstudy
#' @export
rtif <- function(ip, theta = seq(-4, 4, length = 100)){

  out <- data.frame(theta = theta,
    i = apply(riif(ip, theta)[, -1], 1, sum))
  return(out)
}

# Test error function
#' @rdname irtstudy
#' @export
rtef <- function(ip, theta = seq(-4, 4, length = 100)){

  out <- data.frame(theta = theta,
    se = 1 / sqrt(rtif(ip, theta)$i))
  return(out)
}
