#' Generalizability and Decision Studies
#'
#' Functions for running generalizability (G) and decision (D) studies
#' for estimating reliability in multi-faceted designs.
#'
#' @param x for the default method, a \code{formula} object sent to the
#' \code{\link{lme4}} package. Otherwise, an object of class \code{\link{merMod}},
#' output by the \code{\link{lmer}} function. \code{x} can also be a data frame,
#' where a model with a single facet is assumed, with people in rows and the
#' facet in columns.
#' @param data a data frame containing the variables named in formula. Required
#' when x is not a data frame.
#' @param n vector of counts used in adjusting the g coefficient. Not currently
#' used.
#' @param id character string naming the id variable for the unit of observation,
#' defaulting to \code{"person"}.
#' @param random logical indicating whether the facet given in columns when
#' \code{x} is data frame should be treated as a random (default) or fixed
#' effect.
#' @param column_label string for labeling the column facet when \code{x} is
#' data frame.
#' @param ... further arguments passed to or from other methods.
#' @return Returns a list containing the original call, model formula,
#' reliabilities, variance components, and n counts by facet.
#'
#' @examples
#' # Reading items
#' ritems <- c("r414q02", "r414q11", "r414q06", "r414q09",
#'   "r452q03", "r452q04", "r452q06", "r452q07", "r458q01",
#'   "r458q07", "r458q04")
#' rsitems <- paste0(ritems, "s")
#'
#' # Subset of PISA09 data for Belgium
#' pisa <- subset(PISA09, cnt == "BEL")[, rsitems]
#'
#' # Convert to long format
#' np <- nrow(pisa)
#' ni <- ncol(pisa)
#' pisal <- data.frame(person = factor(rep(1:np, ni)),
#'   item = rep(rsitems, each = np),
#'   score = unlist(pisa), row.names = 1:(np * ni))
#'
#' # Compare coefficient alpha using rstudy and gstudy
#' rout <- rstudy(pisa, use = "complete.obs")
#' gout <- gstudy(pisa, column_label = "item")
#' rout$alpha
#' gout$g
#'
#' # Alternatively, find g using long data and formula method
#' # Results differ slightly because missing data are excluded with the
#' # data frame method but included by default with the formula method
#' goutl <- gstudy(score ~ (1 | person) + (1 | item), data = pisal)
#' goutl$g
#'
#' @export
gstudy <- function(x, ...) UseMethod("gstudy")

#' @describeIn gstudy Method defaults to formula interface.
#' @export
gstudy.default <- function(x, data = NULL, ...) {

  gstudy.formula(x, data, ...)
}

#' @describeIn gstudy Method for formula objects.
#' @export
gstudy.formula <- function(x, data = NULL, id = "person", ...) {

  mer <- lme4::lmer(x, data = data, ...)
  gstudy.merMod(mer, id = id, ...)
}

#' @describeIn gstudy Method for wide data frames with people as rows and a
#' single facet as columns
#' @export
gstudy.data.frame <- function(x, random = TRUE, column_label = "rater",
  id = "person", ...) {

  x <- x[complete.cases(x), ]
  nc <- ncol(x)
  nr <- nrow(x)
  xl <- data.frame(unlist(x), factor(rep(1:nr, nc)),
    factor(rep(1:nc, each = nr)))
  colnames(xl) <- c("score", id, column_label)

  if (random)
    f <- formula(sprintf("score ~ 1 + (1 | %s) + (1 | %s)",
      id, column_label))
  else f <- formula(sprintf("score ~ 1 + (1 | %s) + %s", id, column_label))
  gstudy.formula(f, data = xl, id = id, ...)
}

#' @describeIn gstudy Method for objects of class \code{\link{merMod}}.
#' @export
gstudy.merMod <- function(x, n, id = "person", ...) {

  cl <- match.call()
  vc <- unlist(lme4::VarCorr(x))
  s2 <- attr(lme4::VarCorr(x), "sc")^2
  nf <- length(vc)
  n1 <- n2 <- summary(x)$ngrps
  fnames <- names(n1)
  if(missing(n)) {
  	n <- n1[!grepl(":", fnames)]
  	nnames <- names(n)
  }
  else {
    nnames <- names(n)
	  for(i in nnames)
      for(j in nnames[nnames != i])
        n2[paste(i, j, sep = ":")] <- n[i] * n[j]
    n2[nnames] <- n
    n2 <- n2[fnames]
  }

  # g is relative and includes only terms interacting with id
  # d is absolute and includes all terms
  # One doesn't adjust by n, another does
  # Separate out SEM for each
  n2r <- n2
  n2r[grepl(id, fnames)] <- n2r[grepl(id, fnames)] / n2r[id]
  #gr <- vc[id] / sum(vc[id], vc[fnames != id &
  #  grepl(id, names(vc))], s2)

  # Error terms
  gfe <- sum((vc / n2r)[fnames != id &
    grepl(id, names(vc))], s2 / prod(n[nnames != id]))
  gne <- sum(vc[fnames != id & grepl(id, names(vc))], s2)
  dfe <- sum((vc / n2r)[fnames != id], s2 / prod(n[nnames != id]))
  dne <- sum(vc[fnames != id], s2)

  # Reliabilities
  gf <- vc[id] / sum(vc[id], gfe)
  gn <- vc[id] / sum(vc[id], gne)
  df <- vc[id] / sum(vc[id], dfe)
  dn <- vc[id] / sum(vc[id], dne)
  g <- data.frame(g = c(gf, gn, df, dn), sem = sqrt(c(gfe, gne, dfe, dne)),
    row.names = c("Relative Average", "Relative Single", "Absolute Average",
      "Absolute Single"))

  vcout <- data.frame("variance" = c(vc, s2))
  rownames(vcout)[nrow(vcout)] <- "residual"

  out <- list(call = cl, lmercall = x@call, g = g, vc = vcout,
    n = n, n1 = n1, n2 = n2r)
  class(out) <- "gstudy"

  return(out)
}

#' @export
print.gstudy <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {

  cat("\nGeneralizability Study\n\n")
  cat("Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
    "\n", sep = "")
  cat("\nModel Formula:\n", paste(deparse(formula(x$lmercall)),
    sep = "\n", collapse = "\n"), "\n", sep = "")
  cat("\nReliability:\n")
  print(format(x$g, digits = digits))

  out <- data.frame(variance = round(x$vc, digits),
    n1 = NA, n2 = NA)
  out[names(x$n1), "n1"] <- x$n1
  out[names(x$n2), "n2"] <- x$n2
  cat("\nVariance components:\n")
  print(out)

  cat("\nDecision n:\n")
  print(x$n)
}
