##################################
#         LAB 2 Assignment 3     #
##################################

#########
#   1   #
#########
#Scale all variables except of ViolentCrimesPerPop and implement PCA by using function eigen(). 
#Report how many components are needed to obtain at least 95% of variance in the data. 
#What is the proportion of variation explained by each of the first two principal components?

library(dplyr)
library(tidyr)
library(ggplot2)
library(ISLR)
communities_unscaled = read.csv("communities.csv")
#scale data: 
communities = scale(communities_unscaled[1:100])
communities = as.data.frame(communities)
communities = communities %>%
  mutate(ViolentCrimesPerPop = communities_unscaled$ViolentCrimesPerPop) #adding last column unscaled 

#from lecture 2c: covariance matrix S = (1/x)*t(x)*x
S = cov(communities)
var(communities)
eigen_communities = eigen(S)

variance = eigen_communities$values / sum(eigen_communities$values)*100
sprintf("%2.3f", variance)

for (i in 1:length(variance)){
  if (sum(variance[1:i]) > 95)
    print(i)
}

print(cumsum(variance) >= 95)

#########
#   2   #
#########
#Repeat PCA analysis by using princomp() function and make the trace plot of the first principle component. 
#Do many features have a notable contribution to this component? Report which 5 features contribute mostly 
#(by the absolute value) to the first principle component. Comment whether these features have anything in common
#and whether they may have a logical relationship to the crime level. Also provide a plot of the PC scores in the 
#coordinates (PC1, PC2) in which the color of the points is given by ViolentCrimesPerPop. Analyse this plot (hint: use ggplot2 package ).

res<-princomp(communities)

plot(res)
z=res$loadings
pc1=z[,1]
plot(pc1) #shows how much each of the individual features contributes to PC1
which.max(pc1)

abs.pc1=abs(pc1) #we use abs 
sorted.pc1 = sort(abs.pc1, decreasing = TRUE) #sort the features depending on how much they contribute
five_contribute_mostly = sorted.pc1[1:5]
five_contribute_mostly

pc1_scores = res$scores[,1]
pc2_scores = res$scores[,2]

ggplot(data=communities, aes(x=pc1_scores, y=pc2_scores)) + geom_point(aes(color = communities$ViolentCrimesPerPop))

#########
#   3   #
#########
#Split the original data into training and test (50/50) and scale both features and response appropriately, 
#and estimate a linear regression model from training data in which ViolentCrimesPerPop is target and all 
#other data columns are features. Compute training and test errors for these data and comment on the quality of model.

n = dim(communities)[1]
set.seed(12345)
#communities_scaled = scale(communities_unscaled)
#split data
id = sample(1:n, floor(n*0.5)) #id = 50% of communities
train = communities[id,] #train 50% of communities
test = communities[-id,] #test 50% of communities
train_df = as.data.frame(train)
test_df = as.data.frame(test)

communities_lm = lm(ViolentCrimesPerPop~.,-1, data=train_df) # -1 because no intercept
pred_train=predict(communities_lm, train_df)
confusionmatrix_train=table(train_df$ViolentCrimesPerPop, pred_train)
missclassification_error_train=(1-sum(diag(confusionmatrix_train))/nrow(train_df))
confusionmatrix_train
missclassification_error_train
MSE_train = sum((train_df$ViolentCrimesPerPop-pred_train)^2) / length(pred_train)
MSE_train

pred_test=predict(communities_lm, test_df)
confusionmatrix_test=table(test_df$ViolentCrimesPerPop, pred_test)
missclassification_error_test=(1-sum(diag(confusionmatrix_test))/nrow(test_df))
confusionmatrix_test
missclassification_error_test
MSE_test = sum((test_df$ViolentCrimesPerPop-pred_test)^2) / length(pred_test)
MSE_train
MSE_test

R2 = sum((MSE_test-mean(test_df$ViolentCrimesPerPop))^2/ sum(test_df$ViolentCrimesPerPop-mean(test_df$ViolentCrimesPerPop))^2)
MSE_train
MSE_test
R2

#########
#   4   #
#########
#Implement a function that depends on parameter vector 𝜃and represents the cost function for linear regression 
#without intercept on the training data set.Afterwards, use BFGS method (optim() function without gradient specified)
#to optimize this cost with starting point theta(null) = 0 and compute training and test errors for every iteration number. 
#Present a plot showing dependence of both errors on the iteration number and comment which iteration number is optimal according 
#to the early stopping criterion. Compute the training and test error in the optimal model, compare them with results in step 3 and
#make conclusions.
#a. Hint 1: don’t store parameters from each iteration (otherwise it will take a lot of memory), instead compute and store test errors directly.
#b. Hint 2: discard some amount of initial iterations, like 500, in your plot to make the dependencies visible.

testE = list() #test errors that we want to save in a list
trainE = list() #train error list
k=0

MSE_train_function = function(theta){
  MSE_train_temp = mean((train_df$ViolentCrimesPerPop-(as.matrix(train_df[,1:100]))%*%theta)^2)
  print(MSE_train_temp)
  MSE_test_temp = mean((test_df$ViolentCrimesPerPop-(as.matrix(test_df[,1:100]))%*%theta)^2)
  .GlobalEnv$k = .GlobalEnv$k+1
  .GlobalEnv$trainE[[k]]=MSE_train_temp
  .GlobalEnv$testE[[k]]=MSE_test_temp
  return(MSE_train_temp)
  }

res = optim(rep(0,100), fn=MSE_train_function, method="BFGS")
trainE
testE

which.min(trainE) 
which.min(testE) 

#finns 20000 parametrar - vad avgör vilken kombination man ska välja? 

# let's make a plot

plot((19910:19920), trainE[19910:19920], col="blue", ylim = c(0.06686,0.0669))
lines((1000:20000), testE[1000:20000], col="pink")

trainE[19914] # from 0.2591772 in (3) to 0.0668811
testE[2166] # from 1.684078 in (3) to 0.08288133

print(trainE[19914])
print(testE[2166])

# conclusion: the model it better since test error decreased which indicates that the model is not as overfitted as before



