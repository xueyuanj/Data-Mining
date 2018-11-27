#load diabetes data
library(lars)
data(diabetes)
attach(diabetes)
#define n and p (consider only main effects)
n=dim(diabetes$x)[1]
p=dim(diabetes$x)[2]
#partition data into two groups:training and test
set.seed(2016)
test=sample(n,round(n/4))
#create training data
y.train=diabetes$y[-test]
X.train=diabetes$x[-test,]
#create test data
y.test=diabetes$y[test]
X.test=diabetes$x[test,]
###############################################################
#AIC (forward selection and backward elimination)
fullmodel=lm(y.train~X.train[,1]+X.train[,2]+X.train[,3]+X.train[,4]+X.train[,5]+X.train[,6]+X.train[,7
]+X.train[,8]+X.train[,9]+X.train[,10])
basemodel=lm(y.train~1)
#forward selection
step(basemodel,scope=list(upper=fullmodel,lower=~1),direction="forward")
#AIC=2632.17, model with 2nd,3rd,4th,7th and 9th predictors
#backward elimination
step(fullmodel,direction="backward")
#AIC=2631.98,model with 2nd,3rd,4th,5th,6th and 9th predictors
#from AIC model selection criterion, we choose the model from backward elimination (smallest
AIC)
###############################################################
#first, we center the y and standardize the X (training data)
y.train.cent=y.train-mean(y.train)
X.train.std=matrix(NA,nrow=nrow(X.train),ncol=ncol(X.train))
for (j in 1:ncol(X.train))
{X.train.std[,j]<-(X.train[,j]-mean(X.train[,j]))/sd(X.train[,j])}
###############################################################
#Ridge Regression
library(MASS)
values.lambda=seq(0,50,by=0.001)
ridge=lm.ridge(y.train.cent~X.train.std,lambda=values.lambda)
#find which lambda minimizes the GCV (generalized cross validation) error
lambda.ridge=values.lambda[which.min(ridge$GCV)]
lambda.ridge
plot(values.lambda,ridge$GCV,col="red",xlab="Values of Lambda", ylab="GCV
error",main="Figure 1")
#store the coefficients for the "best" ridge regression model
coefficients.ridge=coef(ridge)[which.min(ridge$GCV),]
coefficients.ridge
##############################################################
#Lasso Regression
lasso.reg=lars(X.train.std,y.train.cent,type="lasso")
#find which lambda minimizes the Cp criterion
lambda.lasso=lasso.reg$lambda[which.min(lasso.reg$Cp)]
lambda.lasso
#store the coefficients for the "best" Lasso Regression model
coefficients.lasso=coef(lasso.reg)[which.min(lasso.reg$Cp),]
coefficients.lasso
#############################################################
#Compare selected linear model, ridge regression and lasso regression through test MSE
#first, we center the y and standardize the X (test data)
y.test.cent=y.test-mean(y.train)
X.test.std=matrix(NA,nrow=nrow(X.test),ncol=ncol(X.test))
for (j in 1:ncol(X.test))
{X.test.std[,j]<-(X.test[,j]-mean(X.train[,j]))/sd(X.train[,j])}
#backward elimination model from "step" function
y.newAIC.test=152.6-198.7*X.test[,2]+440.7*X.test[,3]+367.4*X.test[,4]-984.8*X.test[,5]+783.3*
X.test[,6]+900.6*X.test[,9]
y.newAIC.train=152.6-198.7*X.train[,2]+440.7*X.train[,3]+367.4*X.train[,4]-984.8*X.train[,5]+783.3*X.train[,6]+900.6*X.train[,9]
#test MSE for the "best" linear model
mse.step.test=mean((y.newAIC.test-y.test)^2)
mse.step.test
#training MSE for the "best" linear model
mse.step.training=mean((y.newAIC.train-y.train)^2)
mse.step.training
#Ridge Regression
y.new.ridge.test=coefficients.ridge[1]+X.test.std%*%coefficients.ridge[-1]
y.new.ridge.training=coefficients.ridge[1]+X.train.std%*%coefficients.ridge[-1]
#test MSE for the "best" Ridge Regression model
mse.ridge.test=mean((y.test.cent-y.new.ridge.test)^2)
mse.ridge.test
#training MSE for the "best" Ridge Regression model
mse.ridge.training=mean((y.train.cent-y.new.ridge.training)^2)
Mse.ridge.training
#Lasso Regression
y.new.lasso.test=X.test.std%*%coefficients.lasso
y.new.lasso.training=X.train.std%*%coefficients.lasso
#test MSE for the "best" Ridge Regression model
mse.lasso.test=mean((y.new.lasso.test-y.test.cent)^2)
mse.lasso.test
#training MSE for the "best" Ridge Regression model
mse.lasso.training=mean((y.new.lasso.training-y.train.cent)^2)
mse.lasso.training
#Conclusion: We choose the selected linear model from AIC backward elimination (smallest test
MSE)
#we use the full data set to estimate the coefficients to obtain more accurate estimates
#we use the same predictors as in backward elimination
final.model=lm(y~x[,2]+x[,3]+x[,4]+x[,5]+x[,6]+x[,9],data=diabetes)
summary(final.model)