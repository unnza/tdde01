##################################
#         LAB 1 Assignment 2     #
##################################

# 1
library(dplyr)
library(tidyr)
parkinsons = read.csv("parkinsons.csv")
#scale data: 
parkinsons_scale = scale(parkinsons) 

n = dim(parkinsons_scale)[1]
set.seed(12345)

#id = 60% of parkinsons
id = sample (1:n, floor(n*0.6))

#train 60% of parkinsons
train = parkinsons_scale[id,] 

#test 40% of parkinsons
test = parkinsons_scale[-id,] 

#########
#   2   #
#########

#linear regression model of motor_UPDRS as a function of voice characteristics
#exclude age, sex, subject, test_time from model (not releated to voice):

#train:
train_df = as.data.frame(train)
parkinsons_train_LM = lm(motor_UPDRS~. -age -sex -subject. -test_time -motor_UPDRS -total_UPDRS -1,data=train_df) 
pred_train=predict(parkinsons_train_LM) #predicted values from LM
MSE_parkinsons_train = mean((train_df$motor_UPDRS-pred_train)^2) #mean square error between predicted values and data values

#test
test_df = as.data.frame(test)
parkinsons_test_LM = lm(motor_UPDRS~. -age -sex -subject. -test_time -motor_UPDRS -total_UPDRS -1,data=test_df) 
pred_test=predict(parkinsons_test_LM)#predicted values from LM
MSE_parkinsons_test = mean((test_df$motor_UPDRS-pred_test)^2)#mean square error between predicted values and data values

#significantly contributing variables
summary(parkinsons_train_LM)

#########
#   3   #
#########

#3a) loglikelihood - calculates the probablity of the parameters fitting the model, higher values indicates a better fit
loglikelihood_function <- function(data_in,theta,sigma) {
  #divide data into X and Y:
  data_X = data_in[,7:22] #column 7 to 22 (columns related to voice)
  data_Y = data_in[,5] #column 5, motor_UPDRS which is a function of voice
  n = nrow(data_X)
  loglikelihood = (-n*1/2*log(2*pi*sigma^2))-(1/(2*sigma^2))*sum((data_Y - ((data_X) %*% theta))^2)
  return(loglikelihood)
}

#3b) ridge function - adds a factor to the loglikelihood_function to penalize complexity 
ridge_function <- function(data_in,theta,sigma,lambda){
  n = nrow(data_in)
  data_X = data_in[,7:22]
  ridge_penalty = lambda * sum(theta^2) #penalizes complexity
  ridge = -loglikelihood_function(data_in,theta,sigma) + ridge_penalty
  return(ridge)
}

#3c) ridgeOpt - optimizes the results from the ridge function
#help function - create an "array" of the parameters theta and sigma that is used in the ridge function
help_ridge_function <- function(data_in,empty_array, lambda) {
  theta = empty_array[1:16]
  sigma = empty_array[17]
  return(ridge_function(data_in,theta, sigma, lambda))
}
#ridgeoptfunction - uses help function 
ridgeopt_function <- function(data_in,lambda){
  optim(rep(1:17), fn=help_ridge_function, lambda=lambda, data_in = data_in, method = "BFGS") #rep 17 times to optimize all parameters
}

#3d) degrees of freedom - calculates the number of independent variables that can vary 
degrees_of_freedom_function <- function(data_in,lambda){
  data_X = data_in[,7:22]
  I = diag(ncol(data_X))
  #use hat matrix = X * inv( X' * X + lambda * I ) * X' to calculate the degrees of freedom
  hat_matrix = (data_X)%*%solve((t(data_X)%*%data_X + lambda*I))%*%t(data_X)
  degrees_of_freedom = (sum(diag(hat_matrix)))
  return (degrees_of_freedom)
}

#########
#   4   #
#########
train_data_X = train[,7:22]
test_data_X = test[, 7:22]

#compute optimal parameters for lambda = 1, 100 and 1000:
ridge_1 = ridgeopt_function(data=train,lambda=1)
ridge_100 = ridgeopt_function(data=train,lambda=100)
ridge_1000 = ridgeopt_function(data=train,lambda=1000)

#compute predicted values of y lambda = 1, 100 and 1000 using train data
#lambda_train = 1
y1_train = (train_data_X %*%(as.matrix(ridge_1$par))[1:16,])
MSE_train1 = mean((train[,5] - y1_train)^2)
MSE_train1
#lambda_train = 100
y100_train = (train_data_X %*%(as.matrix(ridge_100$par))[1:16,])
MSE_train100 = mean((train[,5] - y100_train)^2)
MSE_train100
#lambda_train = 1000
y1000_train = (train_data_X %*%(as.matrix(ridge_1000$par))[1:16,])
MSE_train1000 = mean((train[,5] - y1000_train)^2)
MSE_train1000

#compute predicted values of y lambda = 1, 100 and 1000 using test data
#lambda_test = 1
y1_test = (test_data_X %*%(as.matrix(ridge_1$par))[1:16,])
MSE_test1 = mean((test[,5] - y1_test)^2)
MSE_test1 
#lambda_test = 100
y100_test = (test_data_X %*%(as.matrix(ridge_100$par))[1:16,])
MSE_test100 = mean((test[,5] - y100_test)^2)
MSE_test100 
#lambda_test = 1000
y1000_test = (test_data_X %*%(as.matrix(ridge_1000$par))[1:16,])
MSE_test1000 = mean((test[,5] - y1000_test)^2)
MSE_test1000 

#compute degrees of freedom for lambda = 1, 100 and 1000
df_1 = degrees_of_freedom_function(lambda = 1, data = train)
df_100 = degrees_of_freedom_function(lambda = 100, data = train)
df_1000 = degrees_of_freedom_function(lambda = 1000, data = train)



