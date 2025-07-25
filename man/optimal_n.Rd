% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optimal_n.R
\name{optimal_n}
\alias{optimal_n}
\title{Compute Optimal Ticket Sales Using Discrete and Continuous Methods}
\usage{
optimal_n(N = 200, p = 0.95, gamma = 0.2, n_range = c(190, 220), step = 1)
}
\arguments{
\item{N}{Integer. Capacity limit of the venue (e.g., number of seats).}

\item{p}{Numeric. Probability a ticket holder shows up (between 0 and 1).}

\item{gamma}{Numeric. Tolerable risk level for exceeding capacity (between 0 and 1).}

\item{n_range}{Numeric vector of length 2. Interval of candidate values for n (e.g., \code{c(190, 220)}).}

\item{step}{Numeric. Step size for evaluating the discrete grid (default is 1).}
}
\value{
A named list containing:
\item{nd}{Optimal discrete number of tickets based on binomial tail minimization.}
\item{nc}{Optimal continuous ticket value using normal approximation (rounded).}
\item{root}{Root of the continuous approximation where tail probability equals threshold.}
\item{N, p, gamma}{Parameters used in computation.}

Two plots are produced using base graphics:
\itemize{
  \item A line plot of the discrete objective function with optimal n highlighted.
  \item A curve for the continuous approximation with root solution annotated.
}
}
\description{
This function determines the optimal number of tickets to sell to minimize the
probability of exceeding a venue's capacity, based on binomial and normal approximations.
It provides both discrete (exact) and continuous (normal approximation) estimates of
the optimal ticket count and produces visualizations to compare the two methods.
}
\examples{
optimal_n(N = 200, p = 0.95, gamma = 0.2, n_range = c(190, 220), step = 1)
}
