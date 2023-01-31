##################################
#         LAB 2 Assignment 2     #
##################################

#########
#   1   #
#########
#Import the data to R, remove variable “duration” and divide into training/validation/test as 
#40/30/30: use data partitioning code specified in Lecture 2a.

bank_full = read.csv2("bank-full.csv", stringsAsFactors = TRUE) #read file with strings
bank_full$duration=c() #removes column duration

n = dim(bank_full)[1]

set.seed(12345)
#split data
id = sample (1:n, floor(n*0.4)) #id = 40% of bank_full
train = bank_full[id,] #train 40% of bank_full
id2 = setdiff(1:n, id) #difference: n-id (100%-40%)
set.seed(12345)
id3 = sample(id2, floor(n*0.3))
validation = bank_full[id3,]
id4 = setdiff(id2,id3) #difference id2-id3 (60%-30%)
test = bank_full[id4,]

#########
#   2   #
#########
#Fit decision trees to the training data so that you change the default settings one by one (i.e. not simultaneously):
library(tree)

#2a) Decision Tree with default settings.
tree_default=tree(as.factor(y)~., data=train)
plot(tree_default)
text(tree_default)
deviance(tree_default)
#2b) Decision Tree with smallest allowed node size equal to 7000. 
tree_minsize=tree(as.factor(y)~., data=train, control = tree.control(nrow(train), minsize = 7000))
plot(tree_minsize)
text(tree_minsize)
#2c) Decision trees minimum deviance to 0.0005.
tree_mindev=tree(as.factor(y)~., data=train, control = tree.control(nrow(train), mindev = 0.0005))
plot(tree_mindev)
text(tree_mindev,pretty = 0)
#Report the misclassification rates for the training and validation data.

#train: 
misclass_default = summary(tree_default)$"misclass"[1]/summary(tree_default)$"misclass"[2]
misclass_minsize = summary(tree_minsize)$"misclass"[1]/summary(tree_minsize)$"misclass"[2]
misclass_mindev = summary(tree_mindev)$"misclass"[1]/summary(tree_mindev)$"misclass"[2]
#results:
misclass_default 
misclass_minsize
misclass_mindev

#validation: 
#default:
predicted_default = predict(tree_default,newdata = validation, type = "class")
table_default = table(validation$y,predicted_default)
misclass_default_validation = (1-sum(diag(table_default))/length(validation$y))
#minsize:
predicted_minsize = predict(tree_minsize,newdata = validation, type = "class")
table_minsize = table(validation$y,predicted_minsize)
misclass_minsize_validation = (1-sum(diag(table_minsize))/length(validation$y))
#mindev:
predicted_mindev = predict(tree_mindev,newdata = validation, type = "class")
table_mindev = table(validation$y,predicted_mindev)
misclass_mindev_validation = (1-sum(diag(table_mindev))/length(validation$y))
#results:
misclass_default_validation
misclass_minsize_validation
misclass_mindev_validation

#########
#   3   #
#########
#Use training and validation sets to choose the optimal tree depth in the model 2c: study the trees up to 50 leaves. 
#from lecture 2b:
mindev_result=cv.tree(tree_mindev)
plot(mindev_result$size, mindev_result$dev, type = "b", col = "purple", xlab="Size", ylab= "Dev")

trainScore = rep(0,50)
validationScore =rep(0,50)
for (i in 2:50){
  pruned_tree_mindev = prune.tree(tree_mindev, best = i)
  pred = predict(pruned_tree_mindev, newdata = validation, type = "tree")
  trainScore[i] = deviance(pruned_tree_mindev)/nrow(train)
  validationScore[i] = deviance(pred)/nrow(validation)
}
best_leaves = which.min(validationScore[2:50])#optimal depth
#results:
best_leaves 

#Present a graph of the dependence of deviances for the training and the validation data on the number of leaves and interpret this graph in terms of bias-variance tradeoff.
plot(2:50, trainScore[2:50], type = "b", col = "pink",xlab = "number of leaves",ylab = "devaiance")
points(2:50, validationScore[2:50],type = "b", col = "purple")
legend(x = "topright", legend = c("Training","Validation"),col = c("pink", "purple"), pch = "--")

#Report the optimal amount of leaves and which variables seem to be most important for decision making in this tree. 
best_leaves #optimal amount 
optimal_tree = prune.tree(tree_mindev, best = best_leaves)
plot(optimal_tree)
text(optimal_tree)
summary(optimal_tree) #variables most important: "poutcome" "month"    "contact"  "pdays"    "age"      "day"      "balance"  "housing" 


#########
#   4   #
#########
#Estimate the confusion matrix, accuracy and F1 score for the test data by using the optimal model from step 3. 
predicted_optimal_test = predict(optimal_tree,test, type = "class")
table_optimal_test = table(test$y,predicted_optimal_test)
#result:
table_optimal_test

#F1:
precision = table_optimal_test[2,2]/(sum(table_optimal_test[,2]))
recall = table_optimal_test[2,2]/(sum(table_optimal_test[2,]))
F1_score = (2*precision*recall)/(precision+recall)
#result:
F1_score
#accuracy:
accuracy = (table_optimal_test[1,1] + table_optimal_test[2,2])/(sum(table_optimal_test[,1])+sum(table_optimal_test[,2]))
#result:
accuracy

#########
#   5   #
#########
#Perform a decision tree classification of the test data with the following loss matrix
#report the confusion matrix for the test data. 
#tree with loss matrix: 
predicted_loss = predict(optimal_tree,test, type="vector")
new_loss = ifelse((predicted_loss[,"no"]/predicted_loss[,"yes"]>5), "no", "yes")
loss_table = table(observed = test$y, predicted = new_loss)
test$y
new_loss
loss_table
#F1:
precision_loss = loss_table[2,2]/(sum(loss_table[,2]))
recall_loss = loss_table[2,2]/(sum(loss_table[2,]))
F1_score_loss = (2*precision_loss*recall_loss)/(precision_loss+recall_loss)
#result:
F1_score_loss
#accuracy:
accuracy_loss = (loss_table[1,1] + loss_table[2,2])/(sum(loss_table[,2])+sum(loss_table[,1]))
#result:
accuracy_loss

#########
#   6   #
#########
#Use the optimal tree and a logistic regression model to classify the test data by using the following principle:
#where𝜋 = 0.05,0.1,0.15, ... 0.9,0.95. 
#Compute the TPR and FPR values for the two models and plot the corresponding ROC curves. 
#Conclusion? Why precision- recall curve could be a better option here?
pi = seq(from = 0.05, to = 0.95, by = 0.05)

#TPR and FPR:
#optimal tree: 
pred_tree = predict(optimal_tree,test, type="vector")
TPR_vector_tree = c()
FPR_vector_tree = c()

#logistic regression: 
regression=glm(y~., data=train, family="binomial")
TPR_vector_regression = c()
FPR_vector_regression = c()

for (i in 1:length(pi))
{
  #optimal tree
  prob_pred_tree = predict(optimal_tree,test, type="vector") #predictions if yes > pi
  pred_tree =  ifelse(prob_pred_tree[,2]>pi[i], "yes", "no")
  matrix_tree = table(prediction=pred_tree, observation=test$y)
  nrow(matrix_tree)
  if(nrow(matrix_tree)<2){
    matrix_tree<- rbind(matrix_tree,"yes" = 0)
  } 
  TPR_vector_tree[i] = matrix_tree[2,2] / sum(matrix_tree[,2])
  FPR_vector_tree[i] = matrix_tree[2,1] / sum(matrix_tree[,1])
  
  #regression
  prob_pred_regression = predict(regression, test, type='response')
  pred_regression = ifelse(prob_pred_regression>pi[i], "yes", "no")
  matrix_regression = table(prediction=pred_regression, observation=test$y)
  TPR_vector_regression[i] = matrix_regression[2,2] / sum(matrix_regression[,2])
  FPR_vector_regression[i] = matrix_regression[2,1] / sum(matrix_regression[,1])
}
#ROC curves:
plot(FPR_vector_tree, TPR_vector_tree, col  = "purple", type = 'l',xlab = "FPR",  ylab = "TPR", main = "ROC-curves")

lines(FPR_vector_regression, TPR_vector_regression, col = "orange")
legend(x = "topleft", legend = c("Regression","Tree"),col = c("orange", "purple"), pch = "--")

