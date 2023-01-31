##################################
#         LAB 1 Assignment 3     #
##################################

diabetes = read.csv("pima-indians-diabetes.csv", header = FALSE)

#1: Make a scatterplot showing a plasma glucose concentration on age where obs. are colored by diabetes levels

#x = age (column 8) and y = plasma (column 2), diabetes boolean (column 9) 

plot(diabetes[,2], diabetes[,8],xlab = "plasma-glucose-concentration", ylab = "age",col = ifelse(diabetes[,9]==1, "pink", "blue"))
title(main = "Scatterplot - Plasma and Age")

#2 Train a logistic regression model with y = diabetes as target, x1 = plasma glucose conc., x2 = age as features.
#Make a prediction for all obs by using r = 0.5 as classification threshold. 

#family bionmial since it is two possible classes
diabetes_glm = glm(diabetes[,9]~ diabetes[,2] + diabetes[,8], family = "binomial")
prob = predict (diabetes_glm, type = "response")
pred = ifelse(prob>0.5, 1, 0) #1 is diabetes and 0 is not 
table(diabetes[,9])
coef(diabetes_glm)#gives coeff to the equation 
confusionMatrix_diabetes = table(diabetes[,9],pred)
missclass_error_diabetes = (1 - sum(diag(confusionMatrix_diabetes))/nrow(diabetes))

missclass_error_diabetes

plot(diabetes[,2], diabetes[,8], xlab = "plasma-glucose-concentration",ylab = "age", col = ifelse(pred==1, "orange", "blue"))
title(main = "Predicted Scatterplot")
summary(diabetes_glm)

#3 Use model in 2 to report the boundary between the two classes
#add a curve showing this boundary to the scatter plots

#We want to calculate a linear boundary y = kx+m 
#We know that theta0 + theta1*plasma + theta2*age  = 0 since t(theta) * X = 0 
#Because of this age = (-theta1*plasma-theta0)/theta2
k_diabetes = -coef(diabetes_glm)[2]/coef(diabetes_glm)[3] #slope
m_diabetes = -coef(diabetes_glm)[1]/coef(diabetes_glm)[3] #intercept 
abline(m_diabetes,k_diabetes)


#4 Make same kind of plots as in step 2 but use r = 0.2 and r = 0.8
#r = 0.2

diabetes_glm = glm(diabetes[,9]~ diabetes[,2] + diabetes[,8], family = "binomial", data = diabetes[,c(2,8,9)])
prob = predict (diabetes_glm, type = "response")
pred = ifelse(prob>0.2, 1, 0) #1 is diabetes and 0 is not 
table(diabetes[,9])
pred
coef(diabetes_glm)#gives coeff to the equation 
confusionMatrix_diabetes = table(diabetes[,9],pred)
missclass_error_diabetes = (1 - sum(diag(confusionMatrix_diabetes))/nrow(diabetes))

missclass_error_diabetes

plot(diabetes[,2], diabetes[,8],xlab = "plasma-glucose-concentration",ylab = "age",  col = ifelse(pred==1, "orange", "blue"))
title(main = "Predicted Scatter Plot r = 0.2")

#r = 0.8
diabetes_glm = glm(diabetes[,9]~ diabetes[,2] + diabetes[,8], family = "binomial", data = diabetes[,c(2,8,9)])
prob = predict (diabetes_glm, type = "response")
pred = ifelse(prob>0.8, 1, 0) #1 is diabetes and 0 is not 
table(diabetes[,9])
pred
coef(diabetes_glm)#gives coeff to the equation 
confusionMatrix_diabetes = table(diabetes[,9],pred)
missclass_error_diabetes = (1 - sum(diag(confusionMatrix_diabetes))/nrow(diabetes))

missclass_error_diabetes

plot(diabetes[,2], diabetes[,8],xlab = "plasma-glucose-concentration",ylab = "age", col = ifelse(pred==1, "orange", "blue"))
title(main = "Predicted Scatter Plot r = 0.8")

#5 perform basis function expansion trick by computing new features 
#z1 = x1^4, z2 = x1^3 *x2, z3 = x1^2 * x2^2, z4 = x1*x2^3, z5 = x2^4

diabetes = diabetes %>%
  mutate(z1 = diabetes$V2^4, z2 = diabetes$V2^3*diabetes$V8, z3 = diabetes$V2^2*diabetes$V8^2, z4 = diabetes$V2*diabetes$V8^3, z5 = diabetes$V8^4) #adding columns to dataframe
diabetes_glm_expansion = glm(diabetes[,9]~ diabetes[,2] + diabetes[,8] + diabetes$z1 + diabetes$z2 + diabetes$z3 + diabetes$z4 + diabetes$z5, family = "binomial")
prob_expansion = predict(diabetes_glm_expansion, type = "response")
pred_expansion = ifelse(prob_expansion>0.5, 1, 0) #1 is diabetes and 0 is not 
table(diabetes[,9])
pred_expansion

confusionMatrix_diabetes_expansion = table(diabetes[,9],pred_expansion)
missclass_error_diabetes_expansion = (1 - sum(diag(confusionMatrix_diabetes_expansion))/nrow(diabetes))

confusionMatrix_diabetes_expansion
missclass_error_diabetes_expansion

plot(diabetes[,2], diabetes[,8], xlab = "plasma-glucose-concentration", ylab = "age", col = ifelse(pred_expansion==1, "orange", "blue"))
title(main = "Predicted Expansion Scatter Plot")

summary(diabetes_glm_expansion)




