#' @encoding UTF-8
#' @title Example \emph{sts} dataset
#' @format An \emph{sts} object composed of "meta" and "data" dataframes.
#' @description The \code{example_sts} dataset provides a quickly loadable
#' version of an \emph{sts} object for practicing and code examples.
#'
#' This dataset was was generated on 2021-01-08 by running:
#'
#' \preformatted{
#' library(AirSensor)
#'
#' example_sts <- example_pat
#' example_sts$meta$elevation <- as.numeric(NA)
#' example_sts$meta$locationName <- example_sts$meta$label
#'
#' save(example_sts, file = "data/example_sts.rda")
#' }
"example_sts"


#' @encoding UTF-8
#' @title Example \emph{mts} dataset
#' @format An \emph{mts} object composed of "meta" and "data" dataframes.
#' @description The \code{example_mts} dataset provides a quickly loadable
#' version of an \emph{mts} object for practicing and code examples.
#'
#' This dataset was was generated on 2021-10-07 by running:
#'
#' \preformatted{
#' library(AirSensor)
#'
#' communities <- c("Alhambra/Monterey Park", "El Monte")
#'
#' example_mts <-
#'   example_sensor_scaqmd \%>\%
#'   sensor_filterMeta(communityRegion \%in\% communities)
#'
#' # Add required "locationName"
#' example_mts$meta$locationName <- example_mts$meta$siteName
#'
#' save(example_mts, file = "data/example_mts.rda")
#' }
"example_mts"


#' @encoding UTF-8
#' @title Example RAWS dataset
#' @format An \emph{sts} object composed of "meta" and "data" dataframes.
#' @description The \code{example_raws} dataset provides a quickly loadable
#' example of the data generated by the **RAWSmet** package. This data is a
#' \code{sts} object containing hourly measurements from a RAWS weather station
#' in Saddle Mountain, WA, between July 2002 and December 2017.
#'
#' This dataset was was generated on 2022-02-17 by running:
#'
#' \preformatted{
#' library(RAWSmet)
#'
#' setRawsDataDir("~/Data/RAWS")
#'
#' example_raws <-
#'   cefa_load(nwsID = "452701") \%>\%
#'   raws_filterDate(20160701, 20161001)
#'
#' save(example_raws, file = "data/example_raws.rda")
#' }
"example_raws"


#' @encoding UTF-8
#' @title Carmel Valley example dataset
#' @format An \emph{mts} object with 600 rows and 2 columns of data.
#' @description The \code{Carmel_Valley} dataset provides a quickly loadable
#' version of a single-sensor \emph{mts_monitor} object for practicing and code
#' examples.
#'
#' @details
#' In August of 2016, the Soberanes fire in California burned along the Big Sur
#' coast. It was at the time the most expensive wildfire in US history. This
#' dataset contains PM2.5 monitoring data for the monitor in Carmel Valley which
#' shows heavy smoke as well as strong diurnal cycles associated with sea
#' breezes. Data are stored as an \emph{mts} object and are used in some
#' examples in the package documentation.
#'
#' This dataset was generated on 2022-10-12 by running:
#'
#' \preformatted{
#' library(AirMonitor)
#'
#' Carmel_Valley <-
#'   airnow_loadAnnual(2016) \%>\%
#'   monitor_filterMeta(deviceDeploymentID == "a9572a904a4ed46d_840060530002") \%>\%
#'   monitor_filterDate(20160722, 20160815)
#'
#' save(Carmel_Valley, file = "data/Carmel_Valley.rda")
#' }
#'
"Carmel_Valley"


#' @encoding UTF-8
#' @title Camp Fire example dataset
#' @format A \emph{mts} object with 360 rows and 134 columns of data.
#' @description The \code{Camp_Fire} dataset provides a quickly loadable
#' version of a \emph{mts_monitor} object for practicing and code examples.
#'
#' @details
#' The 2018 Camp Fire was the deadliest and most destructive wildfire in California's
#' history, and the most expensive natural disaster in the world in 2018 in
#' terms of insured losses. The fire caused at least 85 civilian fatalities and
#' injured 12 civilians and five firefighters. It covered an area of 153,336
#' acres and destroyed more than 18,000 structures, most with the first 4 hours.
#' Smoke from the fire resulted in the worst air pollution ever for the
#' San Francisco Bay Area and Sacramento Valley.
#'
#' This dataset was was generated on 2022-10-12 by running:
#'
#' \preformatted{
#' library(AirMonitor)
#'
#' Camp_Fire <-
#'   monitor_loadAnnual(2018) \%>\%
#'   monitor_filter(stateCode == 'CA') \%>\%
#'   monitor_filterDate(
#'     startdate = 20181108,
#'     enddate = 20181123,
#'     timezone = "America/Los_Angeles"
#'  ) \%>\%
#'  monitor_dropEmpty()
#'
#' save(Camp_Fire, file = "data/Camp_Fire.rda")
#' }
"Camp_Fire"

