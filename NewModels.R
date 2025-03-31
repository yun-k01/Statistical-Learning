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

set.seed(10)
random_index <- sample(nrow(train_data), 28000) # the training set is .8 of the partition
df_train = train_data[random_index, ]
df_valid = train_data[-random_index, ]

df_train = df_train[-8]
df_valid = df_valid[-8]
train_data = train_data[-8]

## linear regression
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
x_var = as.matrix(df_train[-2])
y_var = as.vector(df_train$fare_amount)
lassofit = glmnet(x = x_var, y = df_train$fare_amount, alpha = 1)
plot_glmnet(lassofit, label = TRUE, xvar= "lambda")

# selecting lambda using cross validation
lasso.cv.out = cv.glmnet(x = x_var, y = df_train$fare_amount, alpha = 1) 
# plot(lasso.cv.out)

# one-standard deviation rule
Ytest.1se = predict(lasso.cv.out, s = lasso.cv.out$lambda.1se, newx = as.matrix(df_valid[-2]))
lasso1se = sqrt(mean((Ytest.1se - df_valid$fare_amount)^2))

# smallest cross validation error
Ytest.min = predict(lasso.cv.out, s = lasso.cv.out$lambda.min, newx=as.matrix(df_valid[-2]))
lassomin = sqrt(mean((Ytest.min - df_valid$fare_amount)^2))

coef(lasso.cv.out, s = lasso.cv.out$lambda.min)
df_valid[2]
##performing ridge regression
ridgefit = glmnet(x = x_var, y = df_train$fare_amount, alpha = 0)
plot_glmnet(myfit4, label = TRUE, xvar = "lambda")
# use cv to select lambda
ridge.cv.out = cv.glmnet(x_var, df_train$fare_amount, alpha = 0) 
plot(ridge.cv.out)

lam.seq = exp(seq(-5, 3, length=100))
ridge.cv.out = cv.glmnet(x = x_var, y = df_train$fare_amount, alpha = 0, lambda = lam.seq) 
plot(ridge.cv.out)

ridge.cv.out$lambda.min
ridge.cv.out$lambda.1se

Ytest.1se = predict(ridge.cv.out, s = ridge.cv.out$lambda.1se, newx=as.matrix(df_valid[-2]))
ridge1se = sqrt(mean((Ytest.1se - df_valid$fare_amount)^2))

Ytest.min = predict(ridge.cv.out, s = ridge.cv.out$lambda.min, newx=as.matrix(df_valid[,-2]))
ridgemin = sqrt(mean((Ytest.min - df_valid$fare_amount)^2))

## looking at the rmse of the 3 regressions to see which performed best
regr1_rmse = data.frame(Regression_Model = c('Linear', 'Stepwise', 'Lasso_1se', 'Lasso_min', 'Ridge_1se', 'Ridge_min'), RMSE = c(rmse1, sfit_rmse, lasso1se, lassomin, ridge1se, ridgemin))
regr1_rmse



## random forest
rfModel = randomForest(fare_amount ~ ., data = train_data[-c(1, 8)], importance = TRUE, ntree= 1000)
# testing accuracy
rf_rmse = sqrt(min(rfModel$mse))
train_data




## boosting 
library(gbm)
boost_data = as.data.frame(df_train)
boost_valid = as.data.frame(df_valid)

set.seed(10)
gbm_model = gbm(fare_amount ~ ., data = boost_data,
                distribution = "gaussian",
                n.trees = 200,  # set n.trees = 200
                interaction.depth = 5,   # set interaction depth = 5
                cv.folds = 5)

cv.num1 = gbm.perf(gbm_model)
# find n.trees = 78 under cross validation
yhat.boost.cv1 = predict(gbm_model, newdata = boost_valid[-2], n.trees = cv.num1)
gbm_rmse1 = sqrt(mean((yhat.boost.cv1 - unlist(boost_valid[2]))^2))
# gbm_rmse1 = 4.32606

set.seed(10)
gbm_model1 = gbm(fare_amount ~ ., data = boost_data,
                 distribution = "gaussian",
                 n.trees = 78,
                 interaction.depth = 5, 
                 cv.folds = 5)

cv.num2 = gbm.perf(gbm_model) # confirms cv.num = 78
yhat.boost.cv2 = predict(gbm_model1, newdata = boost_valid[-2], n.trees = cv.num2)
gbm_rmse2 = sqrt(mean((yhat.boost.cv2 - unlist(boost_valid[2]))^2))
# gbm_rmse2 = 4.32606


# use cv to choose nos of iterations
cv.num = gbm.perf(boostModel)
yhat.boost.cv = predict(boostModel, newdata = boost_valid[-2], n.trees = cv.num)
boostTree_rmse = sqrt(mean((yhat.boost.cv - unlist(boost_valid[2]))^2))

boostModel = gbm(fare_amount ~ ., data = boost_data,
                 distribution = "gaussian",
                 n.trees = 99,
                 interaction.depth = 3, 
                 cv.folds = 5)
cv.num = gbm.perf(boostModel)
yhat.boost.cv = predict(boostModel, newdata = boost_valid[-1], n.trees = cv.num)
boostTree_rmse = sqrt(mean((yhat.boost.cv - unlist(boost_valid[1]))^2))

pred_test4 = predict(boostModel, newdata = test_data)
test_predictions4 = data.frame(uid = test_data$uid, fare_amount = pred_test4)
# write.csv(test_predictions4, "boost_tree_pred.csv", row.names = F)

boom_test = predict(gbm_model1, newdata = test_data)
boom_pred = data.frame(uid = test_data$uid, fare_amount = boom_test)
write.csv(boom_pred, "W23P1_predictions4.csv", row.names = F)

library(caret)
ctrl = trainControl(method = "repeatedcv",
                    number = 10,
                    repeats = 5)
gbm.Grid = expand.grid(interaction.depth = c(2,3,4,5), 
                       n.trees = (1:5)*200, 
                       shrinkage = c(0.1, 0.05),
                       n.minobsinnode = 10) 

gbm.cv.model <- train(fare_amount ~ ., data = df_train,
                      method = "gbm",
                      trControl = ctrl,
                      tuneGrid = gbm.Grid,
                      verbose = FALSE)
boost.pred = predict(gbm.cv.model, newdata = df_valid[,-2] )
boost.predrmse = sqrt(mean((boost.pred - df_valid$fare_amount)^2))

boost_rmse = min(gbm.cv.model$results[5])  # find the minimized RMSE exists when shrinkage = 0.05, interaction.depth = 5, n.trees = 200

pred_test5 = predict(gbm.cv.model, newdata = test_data)
test_predictions5 = data.frame(uid = test_data$uid, fare_amount = pred_test5)
# write.csv(test_predictions5, "W23P1_predictions5.csv", row.names = F)
gbm.cv.model$results


boost.pred = predict(gbm.cv.model, newdata = df_valid[-1])
boost.predrmse = sqrt(mean((boost.pred - df_valid$fare_amount)^2))



## xgboost
library(xgboost)
boost_data = as.data.frame(df_train)
boost_valid = as.data.frame(df_valid)
boost_test = as.data.frame(test_data)
xgb_train = xgb.DMatrix(data = as.matrix(boost_data[-2]), label = unlist(boost_data[2]))
xgb_valid = xgb.DMatrix(data = as.matrix(boost_valid[-2]), label = unlist(boost_valid[2]))
xgb_test = xgb.DMatrix(data = as.matrix(boost_test))

xgboostModel = xgboost(data = xgb_train, shrinkage = 0.05, interaction.depth = 5, nrounds = 200)
xgboost_rmse = min(xgboostModel$evaluation_log[,2])

pred_test6 = predict(xgboostModel, xgb_test)
test_predictions6 = data.frame(uid = test_data$uid, fare_amount = pred_test6)
write.csv(test_predictions6, "W23P1_predictions6.csv", row.names = F)

# Comparing the RMSE of all models
overall_rmse = data.frame(Regression_Model = c('Linear', 'Stepwise', 'RandomForest', 'Boosting', 'xgBoost'), RMSE = c(rmse1, sfit_rmse, rf_rmse, boost_rmse, xgboost_rmse))

## Creating Predictions
# linear regression
linear_test = predict(myfit1, new_data = test_data)
linear_predictions = data.frame(uid = test_data$uid, fare_amount = linear_test)
write.csv(linear_predictions, "linear_predictions.csv", row.names = F)

# stepwise regression
stepwise_test = predict(sfit, new_data = test_data)
stepwise_predictions = data.frame(uid = test_data$uid, fare_amount = stepwise_test)
write.csv(stepwise_predictions, "stepwise_predictions.csv", row.names = F)

# random forest
rf_test = predict(rfModel, new_data = test_data)
rf_predictions = data.frame(uid = test_data$uid, fare_amount = rf_test)
write.csv(rf_predictions, "rf_predictions.csv", row.names = F)

# trained boosting model
boost_test = predict(gbm.cv.model, new_data = test_data)
boost_predictions = data.frame(uid = test_data$uid, fare_amount = boost_test)
write.csv(boost_predictions, "boost_predictions.csv", row.names = F)

# boosting tree
boost_tree = predict(boostModel, new_data = test_data)
boosttree_predictions = data.frame(uid = test_data$uid, fare_amount = boost_tree)
write.csv(boosttree_predictions, "boosttree_predictions.csv", row.names = F)

# xgboost
xgboost_test = predict(xgboostModel, xgb_test)
xgboost_predictions = data.frame(uid = test_data$uid, fare_amount = xgboost_test)
write.csv(xgboost_predictions, "xgboost_predictions.csv", row.names = F)

