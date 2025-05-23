% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster_functions.R
\name{get_baseline_dates}
\alias{get_baseline_dates}
\title{Generate baseline dates vector}
\usage{
get_baseline_dates(end_date, test_length, baseline_length, guard = 0)
}
\arguments{
\item{end_date}{End date of the test interval}

\item{test_length}{(integer) length of the test interval in days}

\item{baseline_length}{(integer) length of baseline period in days}

\item{guard}{(integer) default = 0; buffer between end of baseline and start
of test interval}
}
\value{
vector of dates
}
\description{
Function to generate baseline dates given an end date and test length, plus
optional guard, and length of baseline
}
