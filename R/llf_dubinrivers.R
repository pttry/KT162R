# Log-likelihood function of binary outcome sample selection model proposed by Dubin and Rivers (1989)

  # Note the sampleSelection package does this as well, and this is slow.
  # Uses mvtnorm package


llf_dubinrivers <- function(parameters) {
  selection_param <- parameters[1]
  outcome_param <- parameters[2]
  rho <- parameters[3]
  P0 <- sum(log(sapply(-selection_param * xs[!selected], pnorm)))
  P1 <- sum(log(sapply(as.list(as.data.frame(matrix(c(selection_param * xs[selected & yo],outcome_param * xo[selected & yo]), nrow = 2, byrow = TRUE))),
                       pmvnorm, lower = -Inf, mean = c(0,0), sigma = matrix(c(1,rho,rho,1), ncol = 2))))
  P2 <- sum(log(sapply(as.list(as.data.frame(matrix(c(selection_param * xs[selected & !yo], -outcome_param * xo[selected & !yo]), nrow = 2, byrow = TRUE))),
                       pmvnorm, lower = -Inf, mean = c(0,0), sigma = matrix(c(1,-rho,-rho,1), ncol = 2))))
  ll <- P0 + P1 + P2
  ll
}
