#' Compute Optimal Ticket Sales Using Discrete and Continuous Methods
#'
#' This function determines the optimal number of tickets to sell to minimize the
#' probability of exceeding a venue's capacity, based on binomial and normal approximations.
#' It provides both discrete (exact) and continuous (normal approximation) estimates of
#' the optimal ticket count and produces visualizations to compare the two methods.
#'
#' @name optimal_n
#' @param N Integer. Capacity limit of the venue (e.g., number of seats).
#' @param p Numeric. Probability a ticket holder shows up (between 0 and 1).
#' @param gamma Numeric. Tolerable risk level for exceeding capacity (between 0 and 1).
#' @param n_range Numeric vector of length 2. Interval of candidate values for n (e.g., \code{c(190, 220)}).
#' @param step Numeric. Step size for evaluating the discrete grid (default is 1).
#'
#' @return A named list containing:
#' \item{nd}{Optimal discrete number of tickets based on binomial tail minimization.}
#' \item{nc}{Optimal continuous ticket value using normal approximation (rounded).}
#' \item{root}{Root of the continuous approximation where tail probability equals threshold.}
#' \item{N, p, gamma}{Parameters used in computation.}
#'
#' Two plots are produced using base graphics:
#' \itemize{
#'   \item A line plot of the discrete objective function with optimal n highlighted.
#'   \item A curve for the continuous approximation with root solution annotated.
#' }
#'
#' @importFrom stats pbinom pnorm optimise
#' @importFrom graphics plot curve abline
#' @importFrom rootSolve uniroot.all
#' @export
#'
#' @examples
#' optimal_n(N = 200, p = 0.95, gamma = 0.2, n_range = c(190, 220), step = 1)
optimal_n <- function(N = 200, p = 0.95, gamma = 0.20,
                           n_range = c(190, 220), step = 1) {

  # Discrete objective function
  f_discrete <- function(n) {
    (1 - gamma) - pbinom(N, size = n, prob = p)
  }

  # Continuous approximation
  f_continuous <- function(n) {
    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))
    (1 - gamma) - pnorm(N + 0.5, mean = mu, sd = sigma)
  }

  # Sequence for discrete evaluation
  n_seq <- seq(n_range[1], n_range[2], by = step)
  f_disc_vals <- sapply(n_seq, f_discrete)
  f_cont_vals <- sapply(seq(n_range[1], n_range[2], by = 0.1), f_continuous)

  # Discrete optimum
  nd <- n_seq[which.min(abs(f_disc_vals))]

  # Continuous root (equality solution)
  root <- uniroot.all(f_continuous, interval = n_range)[1]

  # Continuous minimizer (approximation)
  nc <- optimise(function(n) abs(f_continuous(n)), interval = n_range)$minimum

  # Discrete plot
  plot(n_seq, f_disc_vals, type = "b", pch = 19, col = "blue",
       main = paste("Objective vs n (Discrete Binomial)\nOptimal:", nd,
                    "| gamma =", gamma, "N =", N),
       xlab = "n", ylab = "Objective")
  abline(h = 0, col = "red", lty = 2)
  abline(v = nd, col = "red", lty = 2)

  # Continuous curve plot
  curve(f_continuous, from = n_range[1], to = n_range[2], col = "darkgreen", lwd = 2,
        main = paste("Objective vs n (Normal Approximation)\nOptimal:", round(nc, 4),
                     "| gamma =", gamma, "N =", N),
        xlab = "n", ylab = "Objective")
  abline(h = 0, col = "blue", lty = 2)
  abline(v = nc, col = "blue", lty = 2)

  return(list(
    nd = nd,
    nc = round(nc, 4),
    root = round(root, 4),
    N = N,
    p = p,
    gamma = gamma
  ))
}
