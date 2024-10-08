% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orca_functions.r
\name{get_orca_ibq}
\alias{get_orca_ibq}
\title{Process IBQ Data for Visit 2}
\usage{
get_orca_ibq(token, timestamp = T)
}
\arguments{
\item{token}{Unique REDCap token ID}

\item{timestamp}{Boolean for whether to include survey timestamp}
}
\value{
A data frame for the completed surveys
}
\description{
This function will download and return the mean scores for the IBQ subscales (surgency, negative affect, effortful control).
}
