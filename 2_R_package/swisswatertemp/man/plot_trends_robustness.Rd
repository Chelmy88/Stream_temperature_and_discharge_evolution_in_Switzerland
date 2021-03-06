% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Test_trends_robustness.R
\name{plot_trends_robustness}
\alias{plot_trends_robustness}
\title{Produce tend robustness plots for T, Q, TA and P.}
\usage{
plot_trends_robustness(rivers_data)
}
\arguments{
\item{rivers_data}{The dataset of rivers data}
}
\description{
This function produces the trend robustness plots by using the robust liner model
function (rlm) from the MASS package (Figures S9 and S10 in supplementary) and
by using trends when removing one year at the begining or end of the time series
(Figures S11 and S12 in supplementary).
Figures are saved in plots/trend_robustness.pdf
}
