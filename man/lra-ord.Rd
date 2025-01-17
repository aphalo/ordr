% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun-lra.r
\name{lra-ord}
\alias{lra-ord}
\alias{lra}
\title{Log-ratio analysis}
\usage{
lra(x, compositional = FALSE, weighted = TRUE)
}
\arguments{
\item{x}{A numeric matrix or rectangular data set.}

\item{compositional}{Logical; whether to normalize rows of \code{x} to sum to 1.}

\item{weighted}{Logical; whether to weight rows and columns by their sums.}
}
\value{
Given an \eqn{n * p} data matrix and setting \eqn{r=min(n,p)},
\code{lra()} returns a list of class \code{"lra"} containing three elements:
\itemize{
\item{sv}{The \eqn{r-1} singular values}
\item{row.coords}{The \eqn{n * (r-1)} matrix
of row standard coordinates.}
\item{column.coords}{The \eqn{p * (r-1)} matrix
of column standard coordinates.}
}
}
\description{
Represent log-ratios between variables based on their values on
a population of cases.
}
\details{
Log-ratio analysis (LRA) is based on a double-centering of log-transformed
data, usually weighted by row and column totals. The technique is suitable
for positive-valued variables on a common scale (e.g. percentages). The
distances between variables' coordinates (in the full-dimensional space) are
their pairwise log-ratios. The distances between cases' coordinates are
called their \emph{log-ratio distances}, and the total variance is the weighted
sum of their squares.

LRA is not implemented in standard R distributions but is a useful member of
the ordination toolkit. This is a minimal implementation following
Greenacre's (2010) exposition in Chapter 7.
}
\references{
Greenacre MJ (2010) \emph{Biplots in Practice}. Fundacion BBVA, ISBN:
978-84-923846.
\url{https://www.fbbva.es/microsite/multivariate-statistics/biplots.html}
}
