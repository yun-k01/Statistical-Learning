library(readr)
library(dplyr)
dat_Q1A2 <- read_csv("Desktop/STAT/STAT 457/dat_Q1A2.csv")


# creating a function to calculate the gradient
grad_fxn <- function(X, Y, delta = 0.01, epsilon = 1e-8, maxitr = 100000){
  # initialize beta value
  iter_beta = rep(0, ncol(X))
  n = 300
  p = 3
  M = c(0.1, 0.3, 0.5, 0.9)
  
  # iterative update
  for (k in 1:maxitr) {
    for (j in 1:length(M)) {  # looking at each M
      if (Y - (t(iter_beta) %*% X)  < -M[i]) {
        temp_grad = -(1/n) * 2 * M %*% X
      }
      else if (-M <= Y - (t(iter_beta) %*% X)  && Y - (t(iter_beta) %*% X)  <= M) {
        temp_grad = 2 %*% (Y - (t(iter_beta) * X))
      }
      else {
        temp_grad = 2 * M
      }
      
      #if the gradient is small enough, stop
      if (max(abs(tmp_grad)) < epsilon){
        break
      }
      
      #gradient descent
      iter_beta = iter_beta - delta*tmp_grad
    }
  }
  if (k == maxitr){cat("maximum iteration reached\n")}
  else{ cat("Iteration #: ", k)}  
  
  return(iter_beta)
  
}
X = as.matrix(dat_Q1A2[,2:4])
Y = as.vector(dat_Q1A2$Y)
grad_fxn(X, Y, 0.01, 1e-8, 10^4)
