% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc.R
\name{scripts_to_JSON}
\alias{scripts_to_JSON}
\title{scripts_to_JSON}
\usage{
scripts_to_JSON(script, verbose = FALSE, return = "normal")
}
\arguments{
\item{script}{Script scraped.}

\item{verbose}{Whether to provide feedback}
}
\value{
Data.frame from JSON
}
\description{
Utility function to clean scripts snagged and conver to JSON.
}
\examples{
\dontrun{
scripts_to_JSON(scripts)
}
}
