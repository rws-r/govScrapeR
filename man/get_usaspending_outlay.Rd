% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{get_usaspending_outlay}
\alias{get_usaspending_outlay}
\title{get_usaspending_outlay}
\usage{
get_usaspending_outlay(
  id = NULL,
  returnTable = FALSE,
  returnRaw = FALSE,
  verbose = FALSE
)
}
\arguments{
\item{id}{A unique id created via clean_and_match / generate_unique_key}

\item{returnTable}{Logical, whether to return aggregated numeric value, or complete table.}

\item{returnRaw}{Logical, whether to return raw data before processing.}

\item{verbose}{Logical, whether to provide feedback.}
}
\value{
Either a numeric value, or a data.frame.
}
\description{
A utility function to get outlay values from usaspending.gov.
}
\examples{
\dontrun{
get_usaspending_outlay(id="XXXXX-XXXXXX",returnTable=F)
}
}
