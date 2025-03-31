library(readr)
library(dplyr)
library(randomForest)
library(glmnet)
library(plotmo)
test_data <- read_csv("Desktop/STAT/STAT 457/STAT 457 Proj 1/test_data.csv")
train_data <- read_csv("Desktop/STAT/STAT 457/STAT 457 Proj 1/train_data.csv")

train_data$pickup_date = as.numeric(train_data$pickup_date)
train_data$pickup_time = as.numeric(train_data$pickup_time)
test_data$pickup_date = as.numeric(test_data$pickup_date)
test_data$pickup_time = as.numeric(test_data$pickup_time)

# look at rmse of train pred to true train y

## multiple linear regression
myfit1 = lm(fare_amount ~ ., data = train_data)
summary(myfit1)
rmse1 = sqrt(mean(myfit1$residuals^2))

# perform stepwise regression
full = myfit1
null = lm(fare_amount ~ 1, data = train_data)
sfit = step(null, scope = list(lower = null, upper = full), direction = 'both')
summary(sfit)
sfit_rmse = sqrt(mean(sfit$residuals^2))


## performing lasso
x_var = as.matrix(train_data[-2])
y_var = as.vector(train_data$fare_amount)
lassofit = glmnet(x = x_var, y = train_data$fare_amount, alpha = 1)
plot_glmnet(lassofit, label = TRUE, xvar= "lambda")

# selecting lambda using cross validation
lasso.cv.out = cv.glmnet(x = x_var, y = train_data$fare_amount, alpha = 1) 
plot(lasso.cv.out)

lasso.min <- lasso.cv.out$lambda.min
mse2 = min(lasso.cv.out$cvm)
rmse2 = sqrt(mse2)

# thus we find the optimal value of lambda is 0.0547, and the mean squared error is 9.758


##performing ridge regression
myfit4 = glmnet(x = x_var, y = y_var, alpha = 0)
plot_glmnet(myfit4, label = TRUE, xvar = "lambda")
# use cv to select lambda
ridge.cv.out = cv.glmnet(x_var, y_var, alpha = 0) 
plot(ridge.cv.out)
ridge.min <- ridge.cv.out$lambda.min
mse3 = min(ridge.cv.out$cvm)
rmse3 = sqrt(mse3)
## looking at the rmse of the 3 regressions to see which performed best
regr1_rmse = data.frame(Regression_Model = c('Linear', 'Lasso', 'Ridge'), RMSE = c(rmse1, rmse2, rmse3))


## random forest regression
rfModel = randomForest(fare_amount ~ ., data = train_data, importance = TRUE, ntree= 1000)
# pred_test2 = predict(rfModel, new_data = test_data)

# testing accuracy
rf_mse = rfModel$mse
rf_rmse = sqrt(min(rf_mse))

## boosting trees
library(gbm)
boost_data = as.data.frame(train_data[c(1:7)])
test_data
boostModel = gbm(fare_amount ~ ., data = boost_data,
                 distribution = "gaussian",
                 n.trees = 300,
                 shrinkage = 0.5, 
                 bag.fraction = 0.8)

# use cv to choose nos of iterations
cv.num = gbm.perf(boostModel)
yhat.boost.cv = predict(boostModel, newdata = boost_data[, -2], n.trees = cv.num)
boost_rmse = sqrt(mean((yhat.boost.cv - unlist(boost_data[, 2]))^2))


# using xgBoost
library(xgboost)
x_train_data = as.matrix(boost_data[, -c(2, 8)])
xgb_train = xgb.DMatrix(data = x_train_data, label = unlist(boost_data[, 2]))
xgb_test = xgb.DMatrix(data = test_boost_data)

# creating the xgboost model
# model = xgb.train(data = xgb_train, max.depth = 3, nrounds = 100)
# summary(model)
xgboostModel = xgboost(data = xgb_train, max.depth = 3, nrounds = 12, verbose = 0)
# prediction accuaracy - RMSE
xgboost_rmse = min(xgboostModel$evaluation_log[,2])


# Looking at all RMSEs
overall_rmse = data.frame(Regression_Model = c('Linear', 'Lasso', 'Ridge', 'RandomForest', 'Boosting', 'xgBoost'), RMSE = c(rmse1, rmse2, rmse3, rf_rmse, boost_rmse, xgboost_rmse))
overall_rmse
# Thus, we will use the linear regression to perform the predictions
pred_test1 = predict(myfit1, new_data = test_data)
test_predictions1 = data.frame(uid = test_data$uid, fare_amount = pred_test1)
write.csv(test_predictions1, "W23P1_predictions1.csv", row.names = F)

pred_test2 = predict(rfModel, new_data = test_data)
test_predictions2 = data.frame(uid = test_data$uid, fare_amount = pred_test2)
write.csv(test_predictions2, "W23P1_predictions2.csv", row.names = F)

pred_test3 = predict(boostModel, newdata = test_data, n.trees = cv.num)
test_predictions3 = data.frame(uid = test_data$uid, fare_amount = pred_test3)
write.csv(test_predictions3, "W23P1_predictions3.csv", row.names = F)


