% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mts_filterMeta.R
\name{mts_filterMeta}
\alias{mts_filterMeta}
\title{General purpose metadata filtering for \emph{mts} time series objects}
\usage{
mts_filterMeta(mts, ...)
}
\arguments{
\item{mts}{\emph{mts} object.}

\item{...}{Logical predicates defined in terms of the variables in
\code{mts$meta}.}
}
\value{
A subset of the incoming \emph{mts} time series object.
(A list with \code{meta} and \code{data} dataframes.)
}
\description{
A generalized metadata filter for \emph{mts} objects to
choose rows/cases where conditions are true.  Multiple conditions are
combined with \code{&} or separated by a comma. Only rows where the condition
evaluates to TRUE are kept. Rows where the condition evaluates to FALSE or
\code{NA} are dropped.

If an empty \emph{mts} object is passed in, it is immediately returned,
allowing for multiple filtering steps to be piped together and only checking
for an empty \emph{mts} object at the end of the pipeline.
}
\note{
Filtering is done on variables in \code{mts$meta}.
}
\examples{
library(MazamaTimeSeries)

# Filter for all labels with "SCSH"
scap <-
  example_mts \%>\%
  mts_filterMeta(communityRegion == "El Monte")

dplyr::select(scap$meta, ID, label, longitude, latitude, communityRegion)

head(scap$data)

}
\seealso{
\link{mts_filterData}
}
