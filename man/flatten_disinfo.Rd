% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/disinfo_functions.R
\name{flatten_disinfo}
\alias{flatten_disinfo}
\title{Flatten disinfo object}
\usage{
flatten_disinfo(disinfo)
}
\arguments{
\item{disinfo}{A disinfo object.}
}
\description{
This attempts to merge as much as possible and always
returns a single data frame. It either returns a
data frame where either each row is a creative work
or where each row is a claim (if the disinfo object contains no creative works).
}
\examples{
d <- disinfo()
\dontrun{
d \%>\%
  add_all("all") \%>\%
  flatten_disinfo()
  }
}
