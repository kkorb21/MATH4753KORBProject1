#' Determine the Optimal Number of Tickets Sold Using Normal Approximation
#'
#' This function finds the optimal (possibly fractional) number of tickets to sell
#' such that the probability of exceeding capacity \code{N} is below a specified risk threshold \code{gamma}.
#' It uses the normal approximation to the binomial distribution with a 0.5 continuity correction,
#' evaluating the deviation between the desired confidence level and the approximated cumulative probability.
#'
#' @name optimal_n_continuous
#' @param N Integer. Capacity of the venue (maximum allowed attendees).
#' @param p Numeric. Probability a single ticket holder shows up (0 to 1).
#' @param gamma Numeric. Acceptable probability of exceeding capacity (e.g., 0.2).
#' @param n_range Numeric vector. Range of ticket values to evaluate (can be fractional, e.g., \code{seq(200, 220, 0.1)}).
#'
#' @return Numeric. The value of \code{n} within \code{n_range} that minimizes the absolute difference
#' from the target tail probability. A base R plot is also produced to visualize the results.
#'
#' @importFrom stats pnorm
#' @importFrom graphics plot abline
#' @export
#'
#' @examples
#' optimal_n_continuous(N = 200, p = 0.95, gamma = 0.2, n_range = seq(200, 220, by = 0.1))
optimal_n_continuous <- function(N, p, gamma, n_range) {
  # Objective function using normal approximation with endpoint correction
  f_objective_normal <- function(n) {
    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))
    (1 - gamma) - pnorm(N + 0.5, mean = mu, sd = sigma)
  }

  # Evaluate objective function and absolute deviation
  f_n <- sapply(n_range, f_objective_normal)
  abs_fn <- abs(f_n)

  # Identify optimal n
  min_idx <- which.min(abs_fn)
  optimal_n <- n_range[min_idx]

  # Plot the results
  plot(n_range, f_n, type = "l", col = "darkgreen", lwd = 2,
       ylim = range(c(f_n, abs_fn)), xlab = "n", ylab = "Objective",
       main = paste("Objective vs n to find optimal tickets sold\n(",
                    round(optimal_n, 7), ") gamma =", gamma,
                    "N =", N, "continuous"))
  abline(h = 0, col = "blue")
  abline(v = optimal_n, col = "blue")

  return(optimal_n)
}
