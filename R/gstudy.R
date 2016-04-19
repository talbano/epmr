#' Generalizability and Decision Studies
#'
#' Functions for running generalizability (G) and decision (D) studies
#' for estimating reliability in multi-faceted designs.
#'
#' @param x for the default method, a \code{formula} object sent to the
#' \code{\link{lme4}} package. Otherwise, an object of class \code{\link{merMod}},
#' output by the \code{\link{lmer}} function. \code{x} can also be a data frame,
#' where a one facet model is assumed with people in rows.
#' @param data a required data frame containing the variables named in formula.
#' @param n vector of counts used in adjusting the g coefficient. Not currently
#' used.
#' @param id character string naming the id variable for the unit of observation,
#' defaulting to \code{"person"}.
#' @param ... further arguments passed to or from other methods.
#' @return Returns a list containing the g and d coefficients, the variance
#' components, and n counts by facet.
#' @export
gstudy <- function(x, ...) UseMethod("gstudy")

#' @describeIn gstudy Method defaults to formula interface.
gstudy.default <- function(x, data = NULL, ...) {

  gstudy.formula(x, data, ...)
}

#' @describeIn gstudy Method for formula objects.
gstudy.formula <- function(x, data = NULL, ...) {

  mer <- lme4::lmer(x, data = data, ...)
  return(gstudy.merMod(mer))
}

#' @describeIn gstudy Method for wide data frames with people as rows and a
#' single facet as columns
gstudy.data.frame <- function(x, ...) {

  x <- x[complete.cases(x), ]
  nc <- ncol(x)
  nr <- nrow(x)
  xl <- data.frame(score = unlist(x), person = rep(1:nr, nc),
    rep(1:nc, each = nr))
  colnames(xl)[3] <- "rater" # Add support for different name

  return(gstudy.formula(score ~ 1 + (1 | person) + (1 | rater),
    data = xl, ...))

}

#' @describeIn gstudy Method for objects of class \code{\link{merMod}}.
gstudy.merMod <- function(x, n, id = "person") {

  vc <- unlist(lme4::VarCorr(x))
  s2 <- attr(lme4::VarCorr(x), "sc")^2
  nf <- length(vc)
  ns <- summary(x)$ngrps
  fnames <- names(ns)
  #if(missing(n)) {
  	n <- ns[!grepl(":", fnames)]
  	nnames <- names(n)
  #}
  #else {
  #  nnames <- names(n)
	#  for(i in nnames)
  #    for(j in nnames[nnames != i])
  #      ns[paste(i, j, sep = ":")] <- n[i]*n[j]
  #  ns[nnames] <- n
  #  ns <- ns[fnames]
  #}

  g <- vc[id] / sum(vc[id], (vc / ns)[fnames != id &
    grepl(id, names(vc))], s2 / prod(n[nnames != id]))
  d <- vc[id] / sum(vc[id], (vc / ns)[fnames != id],
    s2 / prod(n[nnames != id]))
  names(g) <- names(d) <- NULL


  vcout <- data.frame("variance" = c(vc, s2))
  rownames(vcout)[nrow(vcout)] <- "residual"

  return(list(g = g, d = d, vc = vcout, ns = ns))
}
