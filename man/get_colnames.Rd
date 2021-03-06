% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_get-helpers.R
\name{get_colnames}
\alias{get_colnames}
\title{Returns the column names for a given table}
\usage{
get_colnames(table, raw, db)
}
\arguments{
\item{table}{A string specifying the specific table from the Oregon Extended
live database. Should be one of "Accommodations", "Answers", "Districts",
"Exams", "Items", "Preferences", "Schools", "Students", "Students_old",
"Submissions", "SupplementalDistricts", "SupplementalSchools", "Tasks",
"User", "UserStudents", or "UserStudents_old".}

\item{raw}{Logical, defaults to \code{FALSE}. Should the original tables
from the database be returned? If \code{FALSE}, cleaned up names are
returned and, for the \code{"Items"} table, item difficulties are
returned instead of the full item attributes.}

\item{db}{A string specifying the database to query. Defaults to
\code{NULL}, in which case the most recent database is queried. These
names should be specified either as a four digit year (e.g., 1718) or as
in the database, e.g., \code{"ORExt1718"}
would query the 1718 database.}
}
\value{
Character vector - formatted column names in table
}
\description{
This is a weird function and one that is likely to need updating over time.
As you can see below, the names depend on the year, because the tables don't
always have the same columns in each year. I had already written the main
function when I realized this so the rest of it is kind of hacked together
from that. You might want to consider redoing this so it calls separate
functions depending on the year, but there is a lot of overlap. Also new
tables are likely to be added to the database that might be relevant.
}
\keyword{internal}
