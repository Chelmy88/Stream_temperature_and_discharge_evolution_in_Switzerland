% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/STL_analysis.R
\name{get_remainder_analysis}
\alias{get_remainder_analysis}
\title{Perform analysis of the remainder of the STL}
\usage{
get_remainder_analysis(STL_output, s_windows, version)
}
\arguments{
\item{STL_output}{List of output from the STL analysis, where keys are
\code{s.window} values.}

\item{s_windows}{Vector of \code{s.window} values for the stl analysis
(see \code{link[stats]{stl}})}

\item{version}{"raw" or "post_seasons_loess" depending if a "post_seasons_loess"
has be applied (whihc is the case when \code{\link{get_STL_analysis}}) is run
at daily time scale.}
}
\value{
aa
}
\description{
Get the ACF and PACF for the remainder of the STL analysis. For meterological
stations, the CCF between the hydrological variable and the meteorological
variable is also computed.
}
