# Log-likelihood function of the binary outcome sample selection model proposed by Sartori (2003)

  # "x" is currently the single explanatory variable. In Sartori's estimator the explanatory variables are the same for
  # the selection and outcome equations.

  # "Parameters" is a vector, where the first k elements are the coefficients in the selection equation and the rest are
  # the coefficients of the outcome equation

  # NOTE: rho does not enter the estimator since the estimator assumes it to be 1

llf_sartori <- function(parameters) {

  selection_param <- as.matrix(parameters[1:k])
  outcome_param <- as.matrix(parameters[(k+1):length(parameters)])

  m <- matrix(c(X[ys & yo,] %*% selection_param, X[ys & yo,] %*% outcome_param), nrow = 2, byrow = TRUE)

  P0 <- pnorm(X %*% -selection_param)[!ys]
  P1 <- (pnorm(X %*% -outcome_param) - pnorm(X %*% -selection_param))[ys & !yo][(X[ys & !yo,] %*% (selection_param - outcome_param)) > 0]
  P2 <- sapply(
          lapply(
            lapply(1:sum(ys & yo), function(i) m[,i]), pnorm),
          min)
  ll <- sum(log(P0)) + sum(log(P1)) + sum(log(P2))
  ll
}


llf_sartori(c(0,1.25,-0.7, 1.5))
