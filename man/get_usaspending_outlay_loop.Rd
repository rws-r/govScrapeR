% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{get_usaspending_outlay_loop}
\alias{get_usaspending_outlay_loop}
\title{get_usaspending_outlay_loop}
\usage{
get_usaspending_outlay_loop(
  merged_data = NULL,
  progress_style = 2,
  returnTable = FALSE
)
}
\arguments{
\item{merged_data}{Data created by clean_and_match(). Cycles through unique_ids in data.}

\item{progress_style}{A numeric value of 1 or 2, to denote progressbar style. Set ot 0 for no response.}

\item{returnTable}{Logical, whether to return a table, or simple value.}
}
\value{
A data.frame
}
\description{
A utility function to run get_usaspending_outlay on a loop of provided unique_ids.
}
\examples{
\dontrun{
get_usaspending_outlay_loop(M)
}

}
