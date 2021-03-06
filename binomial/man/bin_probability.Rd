% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bin_probability.R
\name{bin_probability}
\alias{bin_probability}
\title{bin_probability}
\usage{
bin_probability(success, trials, prob)
}
\arguments{
\item{success:}{total number of successes}

\item{trials:}{total number of trials}

\item{prob:}{probability of success}
}
\value{
binomial probability or raise an error if parameters will not work
}
\description{
calculates number of combinations after checking if it is a valid calculation
}
\examples{
bin_probability(success = 2, trials = 5, prob = 0.5)
bin_probability(success = 0:2, trials = 5, prob = 0.5)
bin_probability(success = 55, trials = 100, prob = 0.45)
}
