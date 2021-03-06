% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/irtplot.R
\name{ip_plot}
\alias{ip_plot}
\alias{irf_plot}
\alias{tef_plot}
\title{irtplot}
\usage{
ip_plot(
  x,
  theta,
  b,
  groups = rep(1, length(theta)),
  tests = rep(1, length(b)),
  type = "density"
)

irf_plot(x, ip = x$ip, theta = seq(-4, 4, length = 100))

tef_plot(x, ip = x$ip, theta = seq(-4, 4, length = 100))
}
\arguments{
\item{x}{irtstudy output.}

\item{theta}{vector of theta values, taken by default from \code{x} in
\code{ip_plot} and in other functions defaulting to a sequence of values.}

\item{b}{vector of item location parameters, taken by default from
\code{x}.}

\item{groups}{vector defining person groups, defaulting to one group.}

\item{tests}{vector defining tests, i.e., item groups, defaulting to one.}

\item{type}{type of plot generated by \code{ip_plot}, whether "density",
the default, or "frequency", i.e., histogram.}

\item{ip}{data frame of item parameters, taken from \code{x}.}
}
\description{
Item Response Theory Plotting
}
\details{
Functions for plotting results from item response theory models.
}
\examples{

ritems <- c("r414q02", "r414q11", "r414q06", "r414q09",
  "r452q03", "r452q04", "r452q06", "r452q07", "r458q01",
  "r458q07", "r458q04")
rsitems <- paste0(ritems, "s")
pisagbr <- PISA09[PISA09$cnt == "GBR", rsitems]
irtgbr <- irtstudy(pisagbr)

# Item person map
print(ip_plot(irtgbr))

# Item response functions
print(irf_plot(irtgbr))

# Test response functions
print(tef_plot(irtgbr))

}
