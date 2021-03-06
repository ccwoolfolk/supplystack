% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{supplystack}
\alias{supplystack}
\title{supplystack Constructor}
\usage{
supplystack(p, q, nms = NULL)
}
\arguments{
\item{p}{Numeric vector. Represents cost or price for each producer. Alternatively, a matrix in which column values belong to a single producer; rows represent different cost components.}

\item{q}{Numeric vector. Represents quantity for each producer.}

\item{nms}{Character vector (optional). Producer names for visualization.}
}
\description{
Create a supplystack object
}
\examples{
supplystack(p=c(100, 50, 75), q=rep(50, 3), nms=c('A','B','C'))
supplystack(p=matrix(c(5, 10, 15, 20), 2, 2, dimnames=list(c("Cost1", "Cost2"))), q=c(10, 15))
}

