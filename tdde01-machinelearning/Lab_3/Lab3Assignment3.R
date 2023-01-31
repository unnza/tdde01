##################################
#         LAB 3 Assignment 3     #
##################################

#########
#   1   #
#########
#Train a neural network to learn the trigonometric sine function. To do so, sample 500 points uniformly at random in the interval [0,10]. 
#Apply the sine function to each point. The resulting value pairs are the data points available to you. 
#Use 25 of the 500 points for training and the rest for test. Use one hidden layer with 10 hidden units. 
#You do not need to apply early stopping. Plot the training and test data, and the predictions of the learned NN on the test data. 
#You should get good results. Comment your results.

install.packages("neuralnet")
library(neuralnet)
set.seed(1234567890)
Var <- runif(500, 0, 10)
data <- data.frame(Var, Sin=sin(Var))
train <- data[1:25,] # Training
test <- data[26:500,] # Test
# random initialization of the weights in the interval [-1, 1]
# one hidden layer with ten hidden units. 
# w1=10, b1=10, w2=10, b2=1 --> Total 31 weights. 
weight_initial <- runif(min = -1, max = 1, n=31) 
nn <- neuralnet(formula = Sin ~ Var, 
                hidden = 10, 
                startweights = weight_initial, 
                data = train)

plot(train, cex=2, col = "black")
points(test, col = "purple", cex=1)
points(test[,1],predict(nn,test), col="deeppink", cex=1)
legend(x="bottomleft", legend = c("Training data", "Test data","Predictions"), col=c("black","purple","deeppink"), pch = "o")

#########
#   2   #
#########    
#In question (1), you used the default logistic (a.k.a. sigmoid) activation function, i.e. act.fct = "logistic". 
#Repeat question (1) with the following custom activation functions: h1 (x) = x, h2 (x) = max{0, x} and h3 (x) = ln(1 + exp x) 
#(a.k.a. linear, ReLU and softplus). See the help file of the neuralnet package to learn how to use custom activation functions. 
#Plot and comment your results.

linear <- function(x) x
ReLU <- function(x) ifelse(x>0,x,0) #return 0 if negative value, else return the value
softplus <- function(x) log(1+exp(x))

#linear
nn_1 <- neuralnet(formula = Sin ~ Var, 
                hidden = 10, 
                startweights = weight_initial, 
                data = train,
                act.fct = linear)
plot(train, cex=2)
points(test, col = "deeppink", cex=1)
points(test[,1],predict(nn_1,test), col="orange", cex=1)
legend(x="bottomleft", legend = c("Training data", "Test data","Predictions"), col=c("black","deeppink","orange"), pch = "o")

#ReLU
nn_2 <- neuralnet(formula = Sin ~ Var, 
                  hidden = 10, 
                  startweights = weight_initial, 
                  data = train,
                  act.fct = ReLU)
plot(train, cex=2)
points(test, col = "deeppink", cex=1)
points(test[,1],predict(nn_2,test), col="orange", cex=1)
legend(x="bottomleft", legend = c("Training data", "Test data","Predictions"), col=c("black","deeppink","orange"), pch = "o")

#softplus
nn_3 <- neuralnet(formula = Sin ~ Var, 
                  hidden = 10, 
                  startweights = weight_initial, 
                  data = train,
                  act.fct = softplus)
plot(train, cex=2)
points(test, col = "deeppink", cex=1)
points(test[,1],predict(nn_3,test), col="orange", cex=1)
legend(x="bottomleft", legend = c("Training data", "Test data","Predictions"), col=c("black","deeppink","orange"), pch = "o")

#########
#   3   #
#########
#Sample 500 points uniformly at random in the interval [0,50], and apply the sine function to each point. 
#Use the NN learned in question (1) to predict the sine function value for these new 500 points. 
#You should get mixed results. Plot and comment your results.

set.seed(1234567890)
Var3 <- runif(500, 0,50) #500 new random variables on interval 0-50
data3 <- data.frame(Var=Var3, Sin=sin(Var3)) #new dataframe
plot(data3[,1],predict(nn,data3), col="deeppink", cex=1, main = "Prediction vs true values", xlab = "x", ylab = "y")
points(data3, col="darkgrey")
legend(x="bottomleft", legend = c("true values", "predicted values"), col=c("darkgrey","deeppink"), pch = "o")

#########
#   4   #
#########
#In question (3), the predictions seem to converge to some value. Explain why this happens. 
#To answer this question, you may need to get access to the weights of the NN learned. 
#You can do it by running nn or nn$weights where nn is the NN learned.

nn$weights[[1]][[1]] #b(1) and w(1)
nn$weights[[1]][[2]] #b(2) and w(2)
#sum of the interesting weights in w(2) and b(2):
nn$weights[[1]][[2]][2]+nn$weights[[1]][[2]][4]+nn$weights[[1]][[2]][7]+ nn$weights[[1]][[2]][1]
#plot of the nn:
plot(nn) 

#########
#   5   #
#########
#Sample 500 points uniformly at random in the interval [0,10], and apply the sine function to each point. 
#Use all these points as training points for learning a NN that tries to predict x from sin(x), 
#i.e. unlike before when the goal was to predict sin(x) from x. Use the learned NN to predict the training data. 
#You should get bad results. Plot and comment your results. 
#Help: Some people get a convergence error in this question. 
#It can be solved by stopping the training before reaching convergence by setting threshold = 0.1.

#new nn with y = var and x = sin:
nn5 <- neuralnet(formula = Var ~ Sin, 
                hidden = 10, 
                startweights = weight_initial, 
                data = train, 
                threshold = 0.1)
set.seed(1234567890)
Var5 <- runif(500, 0, 10) #500 new random variables on interval 0-10
data5 <- data.frame(Var=Var5, Sin=sin(Var5)) #new dataframe
plot(data5[,2],data5[,1], col = "darkgrey", xlab = "Sin(x)", ylab = "x")
points(data5[,2],predict(nn5,data5), col="deeppink", cex=1)
legend(x="bottomleft", legend = c("true values", "predicted values"), col=c("darkgrey","deeppink"), pch = "o")



