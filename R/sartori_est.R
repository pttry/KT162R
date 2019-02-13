# This function estimates the sartori binary outcome sample selection model.

   # X is the n x k matrix containing all the regressors.
   # ys is the selection vector
   # yo is the outcome vector

   # Something weird going on. Added k = k, X = X to the maxLik function call and to the arguments of the llf_sartori
   # function since, llf_sartori seems to look for variables in global environment or smth.

# Maybe a good idea to use probit model estimates as starting values

sartori_est <- function(ys, yo, X, start_values = "probit") {

  if(start_values == "probit") {
       start_values2 <- coef(glm(yo ~ X, family = binomial(link = "probit")))
       start_values1 <- coef(glm(ys ~ X, family = binomial(link = "probit")))
  }

  X <- cbind(1,X)
  k <- dim(X)[2]

  maxLik(llf_sartori, start = c(start_values1, start_values2) , k = k, X = X)

}
