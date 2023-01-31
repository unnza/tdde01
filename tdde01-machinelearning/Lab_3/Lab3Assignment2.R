#2. SUPPORT VECTOR MACHINES
#The code in the file Lab3Block1 2021 SVMs St.R performs SVM model selection to clas- sify the spam dataset. 
#To do so, the code uses the function ksvm from the R package kernlab, which also includes the spam dataset. 
#All the SVM models to select from use the radial basis function kernel (also known as Gaussian) with a width of 0.05. 
#"The C param- eter varies between the models. Run the code in the file Lab3Block1 2021 SVMs St.R and answer the following questions.
#(1) Which filter do you return to the user ? filter0, filter1, filter2 or filter3? Why?
#(2) What is the estimate of the generalization error of the filter returned to the user? err0, err1, err2 or err3? Why?
#(3) Once a SVM has been fitted to the training data, a new point is essentially classified according to the sign of a
#linear combination of the kernel function values between the support vectors and the new point. 
#You are asked to implement this linear combination for filter3. You should make use of the functions alphaindex, 
#coef and b that return the indexes of the support vectors, the linear coefficients for the support vectors, 
#and the negative intercept of the linear combination. See the help file of the kernlab package for more information. 
#You can check if your results are correct by comparing them with the output of the function predict where you set type = "decision"
#Do so for the first 10 points in the spam dataset. Feel free to use the template provided in the Lab3Block1 2021 SVMs St.R file.

