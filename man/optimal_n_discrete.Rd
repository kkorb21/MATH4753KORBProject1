% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimal_n_discrete.R
\name{optimal_n_discrete}
\alias{optimal_n_discrete}
\title{Determine the Optimal Number of Tickets Sold Using Discrete Binomial Approximation}
\usage{
optimal_n_discrete(N, p, gamma, n_range)
}
\arguments{
\item{N}{Integer. Capacity constraint (maximum number of attendees allowed).}

\item{p}{Numeric. Probability that a single ticket holder shows up (between 0 and 1).}

\item{gamma}{Numeric. Tolerable risk level for exceeding capacity (between 0 and 1).}

\item{n_range}{Integer vector. Range of ticket counts to evaluate (e.g., \code{200:220}).}
}
\value{
Integer. The value of \code{n} within the specified range that minimizes
the absolute difference between the target and actual cumulative probability.
A base R plot is also produced to visualize the objective function.
}
\description{
This function computes the optimal number of tickets to sell such that the probability
of having more attendees than capacity \code{N} stays below a threshold \code{gamma},
assuming each attendee independently shows up with probability \code{p}.
It evaluates the discrepancy between the target and actual binomial cumulative probability
across a range of ticket counts and selects the value of \code{n} that minimizes this difference.
}
\examples{
optimal_n_discrete(N = 200, p = 0.95, gamma = 0.2, n_range = 200:220)
}
