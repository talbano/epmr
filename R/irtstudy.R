#' Item Response Theory Study
#'
#' Functions for estimating item response theory item and person parameters.
#' Currently, only the 1 parameter Rasch model is supported.
#'
#' @param x matrix or data.frame of scored item responses, one row per person,
#' one column per item.
#' @param complete logical with default \code{FALSE} indicating whether or not
#' \code{x} should be reduced to rows with complete data across all columns.
#' @param ... further arguments passed to or from other functions, including
#' \code{\link{lme4::glmer}}
#' @export
irtstudy <- function(x, complete = FALSE, ...) {

  if(!all(unlist(x) %in% c(0, 1, NA)))
    stop("'x' can only contain score values 0, 1, and NA.")
  ni <- ncol(x)
  if(complete)
    x <- x[complete.cases, ]
  else {
    allna <- rowSums(is.na(x)) == ni
    if(any(allna)) {
      x <- x[!allna, ]
      warning(sum(allna), " cases with missing data on all items removed.")
    }
  }

  np <- nrow(x)
  inames <- colnames(x)
  if(is.null(inames))
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
rirf <- function(x, theta = seq(-4, 4, length = 100)){

  if(NCOL(x) == 1)
    x <- cbind(1, x, 0)
  else if(NCOL(x) == 2)
    x <- cbind(x, 0)
  if(NCOL(x) != 3)
    stop("'x' can only contain up to three parameters per item.")
  ni <- NROW(x)
  out <- sapply(1:ni, function(i) x[i, 3] + (1 - x[i, 3]) /
      (1 + exp(x[i, 1] * (-theta + x[i, 2]))))
  colnames(out) <- rownames(x)
  out <- data.frame(theta = theta, out)
  return(out)
}

# Item information function
#' @rdname irtstudy
#' @export
riif <- function(x, theta = seq(-4, 4, length = 100)){

  out <- rirf(x, theta)
  out <- data.frame(theta = theta, out[, -1] * (1 - out[, -1]))
  colnames(out)[-1] <- rownames(x)
  return(out)
}

# Item error function
#' @rdname irtstudy
#' @export
rief <- function(x, theta = seq(-4, 4, length = 100)){

  out <- riif(x, theta)
  out$se <- 1 / sqrt(out$pq)
  return(out)
}

# Test response function
#' @rdname irtstudy
#' @export
rtrf <- function(x, theta = seq(-4, 4, length = 100)){

  out <- data.frame(theta = theta)
  out$p <- apply(rbind(rirf(x, theta)$p), 1, sum)
  return(out)
}

# Test information function
#' @rdname irtstudy
#' @export
rtif <- function(x, theta = seq(-4, 4, length = 100)){

  out <- data.frame(theta = theta,
    i = apply(riif(x, theta)[, -1], 1, sum))
  return(out)
}

# Test error function
#' @rdname irtstudy
#' @export
rtef <- function(x, theta = seq(-4, 4, length = 100)){

  out <- data.frame(theta = theta,
    se = 1 / sqrt(rtif(x, theta)$i))
  return(out)
}
