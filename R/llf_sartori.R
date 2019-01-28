# Log-likelihood function of the binary outcome sample selection model proposed by Sartori (2003)

  # "x" is currently the single explanatory variable. In Sartori's estimator the explanatory variables are the same for
  # the selection and outcome equations.

  # "Parameters" is a vector, where the first element is the coefficient in the selection equation and the second
  # element is the coefficient in the outcome equation.

  # NOTE: rho does not enter the estimator since the estimator assumes it to be 1

llf_sartori <- function(parameters) {

  selection_param <- parameters[1]
  outcome_param <- parameters[2]

  P0 <- sum(log(pnorm(-selection_param * x))[!ys])
  P1 <- sum(log((pnorm(-outcome_param * x) - pnorm(-selection_param * x))[ys & !yo][(selection_param - outcome_param)*x[ys & !yo] > 0]))
  P2 <- sum(log(sapply(lapply(lapply(1:sum(ys & yo), function(i) matrix(c(selection_param * x[ys & yo], outcome_param * x[ys & yo]), nrow = 2, byrow = TRUE)[,i]),
                              pnorm), min)))
  ll <- P0 + P1 + P2
  ll
}
