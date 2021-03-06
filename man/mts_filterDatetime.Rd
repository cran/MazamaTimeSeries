% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mts_filterDatetime.R
\name{mts_filterDatetime}
\alias{mts_filterDatetime}
\title{Datetime filtering for \emph{mts} time series objects}
\usage{
mts_filterDatetime(
  mts = NULL,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  unit = "sec",
  ceilingStart = FALSE,
  ceilingEnd = FALSE
)
}
\arguments{
\item{mts}{\emph{mts} object.}

\item{startdate}{Desired start datetime (ISO 8601).}

\item{enddate}{Desired end datetime (ISO 8601).}

\item{timezone}{Olson timezone used to interpret dates.}

\item{unit}{Units used to determine time at end-of-day.}

\item{ceilingStart}{Logical instruction to apply
\code{\link[lubridate]{ceiling_date}} to the \code{startdate} rather than
\code{\link[lubridate]{floor_date}}.}

\item{ceilingEnd}{Logical instruction to apply
\code{\link[lubridate]{ceiling_date}} to the \code{enddate} rather than
\code{\link[lubridate]{floor_date}}.}
}
\value{
A subset of the incoming \emph{mts} time series object.
(A list with \code{meta} and \code{data} dataframes.)
}
\description{
Subsets an \code{mts} object by datetime. This function
allows for sub-day filtering as opposed to \code{mts_filterDate()} which
always filters to day-boundaries.

Datetimes can be anything that is understood by
\code{MazamaCoreUtils::parseDatetime()}. For non-\code{POSIXct} values,
the recommended format is \code{"YYYY-mm-dd HH:MM:SS"}.

Timezone determination precedence assumes that if you are passing in
\code{POSIXct} values then you know what you are doing:

\enumerate{
\item{get timezone from \code{startdate} if it is \code{POSIXct}}
\item{use passed in \code{timezone}}
\item{get timezone from \code{mts}}
}
}
\examples{
library(MazamaTimeSeries)

example_mts \%>\%
  mts_filterDatetime(
    startdate = "2019-07-03 06:00:00",
    enddate = "2019-07-06 18:00:00"
  ) \%>\%
  mts_extractData() \%>\%
  dplyr::pull(datetime) \%>\%
  range()

}
\seealso{
\link{mts_filterData}

\link{mts_filterDate}

\link{mts_filterMeta}
}
