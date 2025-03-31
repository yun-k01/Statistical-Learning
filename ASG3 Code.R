library(readr)
library(glmnet)
Q1A3_X <- read_csv("Desktop/STAT/STAT 457/Q1A3_X.csv")
Q1A3_Y <- read_csv("Desktop/STAT/STAT 457/Q1A3_Y.csv")

## Q1b
soft_threshod <- function(u, lambda){
  #Input: scalar u, positive scalar lambda
  #Output: if |u| <= lambda, output 0; if u > lambda, output u-lambda; 
  #if u < -lambda, output u + lambda
  
  return(sign(u) * max(0, abs(u) - lambda))
}

myElasticNet <- function(X, y, lambda = 0.1, alpha = 0.6, tol = 1e-16, maxitr = 1000){
  n = 400
  p = 30
  beta = rep(0, p) #initialization
  
  for (k in 1:maxitr) # the grand loop 
  {
    beta_old = beta #record the solution from previous iteration
    
    for (j in 1:ncol(X)) # update each beta_j in a loop 
    {
      r = y - X[, -j] %*% beta[-j]
      
      beta[j] = beta[j] = soft_threshod(t(r) %*% X[, j] / (sum(X[, j]^2) + n*lambda*(alpha)), n*lambda*alpha / (sum(X[, j]^2) + (n*lambda*(1-alpha))))
    }
    
    # check if beta changed more than the tolerance level in this iteration
    if (sum(abs(beta - beta_old)) < tol) break;
  }
  return(list("itr" = k, "beta" = beta))
}
myfit = myElasticNet(as.matrix(Q1A3_X), Q1A3_Y)
elasticfit = glmnet(as.matrix(Q1A3_X), as.matrix(Q1A3_Y), alpha = 0.6, lambda = 0.1, intercept = FALSE) 
sum((myfit$beta - elasticfit$beta)^2)




library(readr)
library(splines)
library(caret)
Q4A3_train <- read_csv("Desktop/STAT/STAT 457/Q4A3_train.csv")
Q4A3_test <- read_csv("Desktop/STAT/STAT 457/Q4A3_test.csv")
attach(Q4A3_test)

# making design matrix
intercept = rep(1, 500)
design_matrix = data.frame(intercept, Q4A3_train$x)

myfit1 = lm(Q4A3_train$y~Q4A3_train$x)

# linear regression procedure
# 1. find beta hat
n = 500
alpha = summary(myfit1)$coef[1]
beta = summary(myfit1)$coef[2]

## 3b
# dense grid
g_x = function(x, alpha = summary(myfit1)$coef[1], beta = summary(myfit1)$coef[2], d) {
  n = 500
  g_hat = rep(0, d)
  for (i in 1:n) {
    for (j in 1:d) {
      g_hat[j] = alpha * sin(x[j]) + beta * cos(x[j])
    }
  }
  return(g_hat)
}
g_ab1 = g_x(x = Q4A3_train$x, d = 8)

my.grid = seq(-1,1,length.out=500)
preds = predict(myfit1, newdata = data.frame(g_ab1))
plot(Q4A3_train$x, Q4A3_train$y, pch = 19, ylab = "y", xlab = "x", col = "gray")
lines(my.grid, preds,lwd=2)


#3c cross validaton
# we many consider a set of possible d values

#ctrl = trainControl(method = "cv", number = 10)

#model = train(y ~ x, data = Q4A3_train, method = "lm", trControl = ctrl)
alld = c(1:20)
nfold = 10
errorMatrix = matrix(NA, length(alld), nfold)

for (l in 1:nfold)
{
  for (k in 1:length(alld))
  {
    pred_x = g_x(x = Q4A3_train$x, d = k)
    errorMatrix[k, l] = mean((pred_x - Q4A3_train$x)^2)
  }
}

alld[which.min(apply(errorMatrix, 1, mean))]

#3d
myfit2 = lm(test_y ~ test_x)
g_hat = function(x, alpha = summary(myfit2)$coef[1], beta = summary(myfit2)$coef[2], d) {
  m = 400
  g_hat = rep(0, d)
  for (k in 1:n) {
    for (j in 1:d) {
      g_hat[j] = alpha * sin(x[j]) + beta * cos(x[j])
    }
  }
  return(g_hat)
}
g_ab2 = g_hat(x = test_x, d = 20)

mse = rep(0, 20)
for (j in 1:20) {
  mse[j] = mean(sum(g_ab2[j] - test_x)^2) 
}
min(mse)
