% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{generate_unique_key}
\alias{generate_unique_key}
\title{generate_unique_key}
\usage{
generate_unique_key(
  unique_key = NULL,
  PIID = NULL,
  toptier_code = NULL,
  award_type = NULL,
  idvPIID = NULL,
  idvAgency = NULL
)
}
\arguments{
\item{unique_key}{A supplied unique key}

\item{toptier_code}{An agency code}

\item{award_type}{Either "contract","grant","IDV"}

\item{award_id}{A PIID number}
}
\value{
A character string
}
\description{
A utility function to generate a unique key as used at usaspending.gov.
}
\examples{
\dontrun{
generate_unique_key(PIID="XXX999333",toptier_code="0900",award_type="grant")
}
}
