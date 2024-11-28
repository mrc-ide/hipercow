mysim <- function(mu, sd = 1, n = 1000) {
  r <- rnorm(n, mu, sd)
  c(mean(r), var(r))
}
