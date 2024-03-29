% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sts_filterDatetime.R
\name{sts_filterDatetime}
\alias{sts_filterDatetime}
\title{Datetime filtering for \emph{sts} time series objects}
\usage{
sts_filterDatetime(
  sts = NULL,
  startdate = NULL,
  enddate = NULL,
  timezone = NULL,
  unit = "sec",
  ceilingStart = FALSE,
  ceilingEnd = FALSE,
  includeEnd = FALSE
)
}
\arguments{
\item{sts}{MazamaSingleTimeseries \emph{sts} object.}

\item{startdate}{Desired start datetime (ISO 8601).}

\item{enddate}{Desired end datetime (ISO 8601).}

\item{timezone}{Olson timezone used to interpret dates.}

\item{unit}{Units used to determine time at end-of-day.}

\item{ceilingStart}{Logical instruction to apply
\code{\link[lubridate]{ceiling_date}} to the \code{startdate} rather than
\code{\link[lubridate]{floor_date}}}

\item{ceilingEnd}{Logical instruction to apply
\code{\link[lubridate]{ceiling_date}} to the \code{enddate} rather than
\code{\link[lubridate]{floor_date}}}

\item{includeEnd}{Logical specifying that records associated with \code{enddate}
should be included.}
}
\value{
A subset of the incoming \emph{sts} time series object.
(A list with \code{meta} and \code{data} dataframes.)
}
\description{
Subsets a MazamaSingleTimeseries object by datetime. This function
allows for sub-day filtering as opposed to \code{sts_filterDate()} which
always filters to day-boundaries.

Datetimes can be anything that is understood by
\code{MazamaCoreUtils::parseDatetime()}. For non-\code{POSIXct} values,
the recommended format is \code{"YYYY-mm-dd HH:MM:SS"}.

Timezone determination precedence assumes that if you are passing in
\code{POSIXct} values then you know what you are doing.

\enumerate{
\item{get timezone from \code{startdate} if it is \code{POSIXct}}
\item{use passed in \code{timezone}}
\item{get timezone from \code{sts}}
}
}
\note{
The returned \code{sts} object will contain data running from the
beginning of \code{startdate} until
the \strong{beginning} of \code{enddate} -- \emph{i.e.} no values associated
with \code{enddate} will be returned. To include \code{enddate} you can
specify \code{includeEnd = TRUE}.
}
\examples{
library(MazamaTimeSeries)

example_sts \%>\%
  sts_filterDatetime(
    startdate = "2018-08-08 06:00:00",
    enddate = "2018-08-14 18:00:00"
  ) \%>\%
  sts_extractData() \%>\%
  head()

}
\seealso{
\link{sts_filter}

\link{sts_filterDate}
}
