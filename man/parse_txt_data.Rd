% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_get-helpers.R
\name{parse_txt_data}
\alias{parse_txt_data}
\title{Parse raw text data for all tables, except items}
\usage{
parse_txt_data(txt)
}
\arguments{
\item{txt}{The raw text data returned from \code{\link{get_raw_data}}}
}
\value{
data.frame - conversion of raw text data from API
}
\description{
Parse raw text data for all tables, except items
}
\keyword{internal}
