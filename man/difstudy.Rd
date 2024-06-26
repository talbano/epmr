% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/difstudy.R
\name{difstudy}
\alias{difstudy}
\title{Differential Item Functioning Study}
\usage{
difstudy(
  x,
  groups,
  ref,
  scores = NULL,
  method = c("mh", "lr"),
  anchor_items = 1:ncol(x),
  dif_items = 1:ncol(x),
  complete = TRUE,
  na.rm = FALSE,
  p_cut = 0.05,
  sdp_w = c("focal", "reference", "total"),
  contrast_type = "contr.treatment"
)
}
\arguments{
\item{x}{matrix or data.frame of scored item responses.}

\item{groups}{vector defining categorical grouping variable.}

\item{ref}{string identifying label used in \code{groups}
to represent the reference group, with no default.}

\item{scores}{optional vector of construct scores, defaulting to row sums
over all columns in \code{x} when \code{NULL} and \code{anchor_items} is
not specified.}

\item{method}{what DIF method to use, with \code{"mh"} for Mantel-Haenszel
and \code{"lr"} for logistic regression.}

\item{anchor_items}{vector of item names or column numbers for
identifying anchoring items in \code{x}. These will be used when finding
construct scores if \code{scores} is missing, defaulting to all columns in
\code{x}.}

\item{dif_items}{vector of item names or column numbers identifying what
items in \code{x} should be analyzed for DIF, defaulting
to all columns in \code{x}.}

\item{complete}{logical with default \code{TRUE} indicating whether or not
\code{x}, \code{groups}, and \code{scores} should be reduced to cases
with complete data.}

\item{na.rm}{logical with default \code{FALSE} specifying whether missings
should be removed before calculating individual descriptives.}

\item{p_cut}{numeric cutoff for evaluating the p-value for the
Mantel-Haenszel chi-square, with default 0.05.}

\item{sdp_w}{string for choosing the weight when finding the standardized
difference statistic, defaulting to the number of test takers in the
focal group.}

\item{contrast_type}{string for the type of contrasts to use in logistic
regression, defaulting to \code{"contr.treatment"}.}
}
\description{
Functions for estimating differential item functioning (DIF) with a set of
scored item responses.
}
\examples{
# Vector of item names
ritems <- c("r414q02", "r414q11", "r414q06", "r414q09",
  "r452q03", "r452q04", "r452q06", "r452q07", "r458q01",
  "r458q07", "r458q04")
rsitems <- paste0(ritems, "s")
# Subset PISA09 to Belgium and Germany
pisa <- subset(PISA09, cnt \%in\% c("BEL", "DEU"))
x_totals <- rowSums(pisa[, rsitems], na.rm = TRUE)
# Uniform DIF with Mantel-Haenszel
difstudy(x = pisa[, rsitems], groups = pisa$cnt, ref = "BEL",
  scores = x_totals, method = "mh")
# Uniform and nonuniform DIF with logistic regression
difstudy(x = pisa[, rsitems], groups = pisa$cnt, ref = "BEL",
  method = "lr")

}
