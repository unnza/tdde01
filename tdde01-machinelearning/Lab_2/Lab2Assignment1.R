##################################
#         LAB 2 Assignment 1     #
##################################
library(glmnet)
library(dplyr)
library(tidyr)


#Divide data randomly into train and test (50/50) by using the codes from the lectures
tecator = read.csv("tecator.csv")
n = dim(tecator)[1]
set.seed(12345)
#split data
id = sample (1:n, floor(n*0.5)) #id = 50% 
train = tecator[id,] #train 50% 
test = tecator[-id,] #test 50%

#########
#   1   #
#########
#Assume that fat can be modeled as a linear regression in which absorbance characteristics
#(Channels) are used as features. Report the underlying probabilistic model, fit the linear
#regression to the training data and estimate the training and test errors. 
#Comment on the quality of fit and prediction and therefore on the quality of the model

#train:
train_df = as.data.frame(train)
train_LM = lm(Fat~. -Sample -Protein -Moisture -Fat, data=train_df) 
pred_train=predict(train_LM,train_df) #predicted values from LM
MSE_train = mean((train_df$Fat-pred_train)^2)#mean square error between predicted values and data values
MSE_train

#test:
test_df = as.data.frame(test)
pred_test=predict(train_LM,test_df) #predicted values from LM
MSE_test = mean((test_df$Fat-pred_test)^2)#mean square error between predicted values and data values
MSE_test
#comment: large MSE --> lm is probably not optimal for the data

#########
#   2   #
#########
#Assume now that fat can be modeled as a LASSO regression in which all Channels are used as features. 
#Report the cost function that should be optimized in this scenario

#comment: in report (function from lecture)

#########
#   3   #
#########
#Fit the LASSO regression model to the training data. Present a plot illustrating how the regression
#coefficients depend on the log of penalty factor (log lambda) and interpret this plot. 
#What value of the penalty factor can be chosen if we want to select a model with only three features?
y = train_df$Fat #select values from fat column
x=as.matrix(train_df[c(2:101)]) #features channel 1:100
lasso=glmnet(x, y,family="gaussian",alpha=1)
plot(lasso, xvar = "lambda")
#-0.5<lambda <0

#########
#   4   #
#########
#Repeat step 3 but fit Ridge instead of LASSO regression and compare the plots from steps 3 and 4. 
#Conclusions? 
ridge=glmnet(x, y,family="gaussian",alpha=0)
plot(ridge, xvar = "lambda")
#comment: coef --> 0 simultaneously

#########
#   5   #
#########
#Use cross-validation with default number of folds to compute the optimal LASSO model. 
#Present a plot showing the dependence of the CV score on log lambda and comment how the CV score changes with log lambda. 
#Report the optimal lambda and how many variables were chosen in this model
#Does the information displayed in the plot suggest that the optimal lambda value results in statistically
#significantly better prediction than log lambda = -4?
#Finally, create a scatter plot of the original test versus predicted test values for the model corresponding
#to optimal lambda and comment whether the model predictions are good. 
lasso_crossvalidation=cv.glmnet(x, y,family="gaussian",alpha=1)
plot(lasso_crossvalidation) #plot of CV score
log(lasso_crossvalidation$lambda.min) #optimnal penalty factor
lasso_optimal=glmnet(x, y,family="gaussian",alpha=1, lambda=lasso_crossvalidation$lambda.min) #optimal lasso function
coef(lasso_optimal)#8 coeff
#comment: smaller log lambda --> smaller MSE but difference is not significant (in CI) 
y_test = test_df$Fat #select values from fat column
x_test=as.matrix(test_df[c(2:101)])   #features 
y_test_lasso = predict(lasso_optimal,x_test) #using features from test data to predict y using lasso model

plot(y_test,col = "orange")
points( y_test_lasso, col = "blue")







