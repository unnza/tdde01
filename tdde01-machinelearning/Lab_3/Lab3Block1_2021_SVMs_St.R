# Lab 3 block 1 of 732A99/TDDE01/732A68 Machine Learning
# Author: jose.m.pena@liu.se
# Made for teaching purposes

install.packages('kernlab')
library(kernlab)
set.seed(1234567890)
 
data(spam)
foo <- sample(nrow(spam))
spam <- spam[foo,]
spam[,-58]<-scale(spam[,-58])
tr <- spam[1:3000, ]
va <- spam[3001:3800, ]
trva <- spam[1:3800, ]
te <- spam[3801:4601, ] 

by <- 0.3
err_va <- NULL
for(i in seq(by,5,by)){
  filter <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=i,scaled=FALSE)
  mailtype <- predict(filter,va[,-58])
  t <- table(mailtype,va[,58])
  err_va <-c(err_va,(t[1,2]+t[2,1])/sum(t))
}

filter0 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter0,va[,-58])
t <- table(mailtype,va[,58])
err0 <- (t[1,2]+t[2,1])/sum(t)
err0

filter1 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter1,te[,-58])
t <- table(mailtype,te[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)
err1

filter2 <- ksvm(type~.,data=trva,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter2,te[,-58])
t <- table(mailtype,te[,58])
err2 <- (t[1,2]+t[2,1])/sum(t)
err2

filter3 <- ksvm(type~.,data=spam,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter3,te[,-58])
t <- table(mailtype,te[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)
err3

# Questions

# 1. Which filter do we return to the user ? filter0, filter1, filter2 or filter3? Why?
# We choose filter 3, because it uses the most data and therefore creates the model with the largest predictive power 

# 2. What is the estimate of the generalization error of the filter returned to the user? err0, err1, err2 or err3? Why?
# When using filter3, we use all data (including test data) as training data, and therefore we will not get an accurate error
# since we have already fitted the model to the test data
# Instead, we use err2, since it is uses the second most data, but we still have test data left for testing the model and calculating the error. 
# With err2 we will get the upper bound of the error, since the error for filter 3 will not be worse than err2. 

# 3. Implementation of SVM predictions.

index_support_vectors<-alphaindex(filter3)[[1]] #rows of index points, points of data that are closest to the plane
coefficients<-coef(filter3)[[1]]
intercept<- -b(filter3)
kernel <- rbfdot(sigma = 0.05) #gaussian kernel where width/radius is sigma=0.05
k<-NULL
for(i in 1:10){ # We produce predictions for just the first 10 points in the dataset.
  point <- spam[i, -58] #rows are i, we want all columns except 58 which is the true classification
  y_hat <- 0
  for(j in 1:length(index_support_vectors)){
    support_vectors = spam[index_support_vectors[j],-58]
    k <- coefficients[j]*kernel(unlist(spam[i,-58]),unlist(support_vectors))
    y_hat <- y_hat + as.numeric(k) 

  }
  
  k_vector[i]<-y_hat + intercept 
}

values <- predict(filter3,spam[1:10,-58], type = "decision")
print(data.frame(k_vector,values))

