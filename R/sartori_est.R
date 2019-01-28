# This function estimates the sartori binary outcome sample selection model.

   # X is the n x k matrix containing all the regressors.
   # ys is the selection vector
   # yo is the outcome vector

# Maybe a good idea to use probit model estimates as starting values

sartori_est <- function(ys, yo, X, start_values = c(2,4,-3,6)) {

  X <- cbind(1,X)
  k <- dim(X)[2]

  maxLik(llf_sartori, start = start_values)

}
