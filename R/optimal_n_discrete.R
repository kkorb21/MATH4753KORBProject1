#' Determine the Optimal Number of Tickets Sold Using Discrete Binomial Approximation
#'
#' This function computes the optimal number of tickets to sell such that the probability
#' of having more attendees than capacity \code{N} stays below a threshold \code{gamma},
#' assuming each attendee independently shows up with probability \code{p}.
#' It evaluates the discrepancy between the target and actual binomial cumulative probability
#' across a range of ticket counts and selects the value of \code{n} that minimizes this difference.
#'
#' @name optimal_n_discrete
#' @param N Integer. Capacity constraint (maximum number of attendees allowed).
#' @param p Numeric. Probability that a single ticket holder shows up (between 0 and 1).
#' @param gamma Numeric. Tolerable risk level for exceeding capacity (between 0 and 1).
#' @param n_range Integer vector. Range of ticket counts to evaluate (e.g., \code{200:220}).
#'
#' @return Integer. The value of \code{n} within the specified range that minimizes
#' the absolute difference between the target and actual cumulative probability.
#' A base R plot is also produced to visualize the objective function.
#'
#' @importFrom stats pbinom
#' @importFrom graphics plot points abline
#'
#' @export
#'
#' @examples
#' optimal_n_discrete(N = 200, p = 0.95, gamma = 0.2, n_range = 200:220)

optimal_n_discrete <- function(N, p, gamma, n_range) {
  # Objective function: difference from 1 - gamma
  f_objective <- function(n) {
    (1 - gamma) - pbinom(N, size = n, prob = p)
  }

  # Evaluate objective function and absolute values
  f_n <- sapply(n_range, f_objective)
  abs_fn <- abs(f_n)

  # Find n that minimizes |f(n)|
  min_idx <- which.min(abs_fn)
  optimal_n <- n_range[min_idx]

  # Plot the results
  plot(n_range, abs_fn, type = "b", col = "red", pch = 19,
       ylim = range(c(f_n, abs_fn)), ylab = "Objective", xlab = "n",
       main = paste("Objective vs n to find optimal tickets sold\n(",
                    optimal_n, ") gamma =", gamma, "N =", N))
  points(n_range, f_n, col = "blue", type = "b", pch = 19)

  # Highlight optimal point and reference lines
  abline(h = 0, col = "red", lty = 2)
  abline(v = optimal_n, col = "red", lty = 2)

  return(optimal_n)
}
