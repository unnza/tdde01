##################################
#         LAB 1 Assignment 1     #
##################################
library(kknn)
#########
#   1   #
#########
optdigits = read.csv("optdigits.csv",header = FALSE)
n = dim(optdigits)[1]
set.seed(12345)
#split data
id = sample (1:n, floor(n*0.5)) #id = 50% of optdigits
train = optdigits[id,] #train 50% of optdigits
id2 = setdiff(1:n, id) #difference: n-id (100%-50%)
set.seed(12345)
id3 = sample(id2, floor(n*0.25))
validation = optdigits[id3,]
id4 = setdiff(id2,id3) #difference id2-id3 (50%-25%)
test = optdigits[id4,]

#########
#   2   #
#########
#train-kkn:
kknnTrain=kknn(as.factor(train$V65)~., train, train, k=30,kernel="rectangular")
#compute misclass-error:
predTrain= kknnTrain$fitted.values #predictions
predTrain
confusionMatrixTrain = table(train$V65,predTrain)
confusionMatrixTrain
misclassTrain = (1-sum(diag(confusionMatrixTrain))/length(train$V65))
misclassTrain
#test-kkn:
kknnTest=kknn(as.factor(train$V65)~., train, test, k=30,kernel="rectangular") 
#compute misclass-error:
predTest= kknnTest$fitted.values #predictions
confusionMatrixTest = table(test$V65,predTest) 
misclassTest = (1-sum(diag(confusionMatrixTest))/length(test$V65))

#########
#   3   #
#########
#probabilities for digit 8:
probabilityTrain = kknnTrain$prob
probability_8_Train = probabilityTrain[,9] #probabilities from column "8"
#easiest to classify (2 cases):
high_prob_8 = matrix(0, nrow=2, ncol=2)#matrix with the probability of the 8's with highest prob and its row.
for (i in 1:2){ 
  for(j in 1:length(probability_8_Train)){
    if(train[j,65] == 8 && probability_8_Train[j] > high_prob_8[i,2]){#inspecting the last column - find digit 8
      if (j != high_prob_8[1,1]){
        high_prob_8[i,1] = j
        high_prob_8[i,2] = probability_8_Train[j]
      }
    }
  } 
}
#hardest to classify (3 cases):
low_prob_8 = matrix(1, nrow=3, ncol=2)#matrix with the probability of the 8's with lowest prob and its row.
for (i in 1:3){ 
  for(j in 1:length(probability_8_Train)){
    if(train[j,65] == 8 && probability_8_Train[j] < low_prob_8[i,2]){#inspecting the last column - find digit 8
      if (j != low_prob_8[1,1] && j!= low_prob_8[2,1]){
        low_prob_8[i,1] = j
        low_prob_8[i,2] = probability_8_Train[j]
      }
    }
  } 
}
#heatmaps:
heatmapPlot <- function(row_number){
  plotData = as.numeric(train[row_number,1:64])
  plotMatrix = matrix(plotData, nrow = 8, byrow = TRUE)
  heatmap(plotMatrix, Rowv = NA, Colv = NA, main = row_number)
}
heatmapPlot(195)#easy
heatmapPlot(129)#easy
heatmapPlot(520)#hard
heatmapPlot(1294)#hard
heatmapPlot(431)#hard

#########
#   4   #
#########
#training:
misclass_errorsTrain =c(0,30)
for (i in 1:30){
kknnTrain2=kknn(as.factor(train$V65)~., train = train, test = train, k=i,kernel="rectangular")
predTrain2= kknnTrain2$fitted.values #predictions
confusionMatrixTrain2 = table(train$V65,predTrain2) 
misclassTrain2 = (1-sum(diag(confusionMatrixTrain2))/length(train$V65)) 
misclass_errorsTrain[i] = misclassTrain2
}
#validation: 
misclass_errorsVal = c(0,30)
for (i in 1:30){
  kknnValidation=kknn(as.factor(train$V65)~., train = train, test = validation, k=i,kernel="rectangular")
  predValidation= kknnValidation$fitted.values #predictions
  confusionMatrixValidation = table(validation$V65,predValidation) 
  misclassValidation = (1-sum(diag(confusionMatrixValidation))/length(validation$V65)) 
  misclass_errorsVal[i] = misclassValidation
}
#plot misclassification rate and k:
plot(misclass_errorsTrain, xlab = "K", ylab = "Misclassification Rate", col ="red", ylim = c(0, 0.1))
points(misclass_errorsVal, col = "blue") #when k increases the error increases since the model becomes less complex (less accurate)
which.min(misclass_errorsVal) #receive minimal error for validation data
#test data model for optimal k = 3 
kknnTest2=kknn(as.factor(train$V65)~., train, test, k=3,kernel="rectangular") 
predTest2= kknnTest2$fitted.values #predictions 
confusionMatrixTest2 = table(test$V65,predTest2) 
misclassTest2 = (1-sum(diag(confusionMatrixTest2))/length(test$V65)) 

#########
#   5   #
#########
crossEntropy_k = c()
for (k in 1:30){
  kknnValidation_CE=kknn(as.factor(train$V65)~., train, validation, k = k,kernel="rectangular")
  probabilityValidation_CE = kknnValidation_CE$prob #probabilities for each k
 
  sum_crossEntropy = 0 #for each k - create new variable
  for (i in 1:nrow(validation)){#for each row: check last column- what is the "correct" number?
    for (j in 0:9){
      if (validation[i,65] == j) { #find "correct" number of the row
        sum_crossEntropy <- sum_crossEntropy + (log(as.numeric(probabilityValidation_CE[i, j+1])+ 1e-15))
      }
    }
  }
  crossEntropy_k = c(crossEntropy_k,-sum_crossEntropy)
}
plot(crossEntropy_k,  xlab = "K", ylab = "Cross Entropy",col ="darkgreen")
