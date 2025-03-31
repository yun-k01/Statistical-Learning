library(dplyr)
library(readr)
library(glmnet)
# library(doParallel)

A4Q1_X <- read_csv("Desktop/STAT/STAT 457/A4Q1_X.csv")
View(A4Q1_X)
A4Q1_Y <- read_csv("Desktop/STAT/STAT 457/A4Q1_Y.csv")
View(A4Q1_Y)

### QUESTION 1
## A)
# performing PCA on X
tr_pca = prcomp(A4Q1_X)

# plotting explained variance ration
par(mfrow=c(1,2))
ex_var = tr_pca$sdev^2/sum(tr_pca$sdev^2)
plot(ex_var, ylab = 'Explained percentage')
plot(cumsum(ex_var), ylab = 'Cumlative')

# finding smallest nos of components
cumsum(ex_var)
ex_var

# considering first 2 principal components and plotting obs as circle in 2D plane
# red circles for "benign", blue circels for "malignant"
Y = A4Q1_Y
par(mfrow=c(1,1))
plot(tr_pca$x[Y == "benign", 1], tr_pca$x[Y == "benign", 2],
     col = 'red', xlim = c(-20, 7), ylim = c(-10, 15),
     xlab ='PC 1', ylab = 'PC 2')
points(tr_pca$x[Y == "malignant", 1], tr_pca$x[Y == "malignant", 2],col = 'blue')

### Q4
library(readr)
A4Q4_X <- read_csv("Desktop/STAT/STAT 457/A4Q4_X.csv")
A4Q4_Y <- read_csv("Desktop/STAT/STAT 457/A4Q4_Y.csv")

# using glm function
glmData = cbind.data.frame(A4Q4_X, A4Q4_Y)

beta_glm = glm(Y~., data = glmData, family=binomial)

# using newton-raphson
n = 600
X = as.matrix(A4Q4_X)
Y = as.matrix(A4Q4_Y)

# X = as.matrix(cbind(X0 = rep(1,n), A4Q4_X)

NewtonRaphson <- function(X, Y, maxIter, epsilon, lambda = 1){
  n = 600
  #initialization for beta and W used in computing grad and Hessain
  beta_nr = rep(0, ncol(X))
  W_old = matrix(0, n, n)
  for(id in 1:maxIter){#run at most 10000 steps
    beta_old = beta_nr
    
    p_old = exp(X %*% beta_old)/(1+exp(X %*% beta_old))
    diag(W_old) = p_old * (1 - p_old) #change the diag
    
    #compute the grad and Hessian
    tmp_grad = (-1/n) * (t(X) %*% (Y - p_old)) + (lambda * beta_old)
    tmp_Hes = (-1/n) * (t(X) %*% W_old %*% X) + lambda
    beta_nr = beta_old + solve(tmp_Hes) %*% tmp_grad
    
    if(mean(sqrt((beta_nr-beta_old)^2)) <= epsilon){
      break
    }
  }
  return(beta_nr)
}

beta_nr = NewtonRaphson(X,Y,1e-8, 10^4)

# looking at the accuracy
rmse = 1/15 * sum((beta_nr - beta_glm$coefficients[2:16])^2)
rmse




