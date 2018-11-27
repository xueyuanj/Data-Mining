library(MASS)
library(klaR)
library(glmnet)
library(ggplot2)
library(reshape)
### Load data, male = 1 and female = 0 are factors ###
voice <- read.csv("~/Box Sync/Fall 2016/STAT 557/Project2/voice.csv",
header = TRUE, stringsAsFactors = FALSE)
voice$label[voice$label == "male"] <- 1
voice$label[voice$label == "female"] <- 0
voice$label <- as.numeric(voice$label)
### There is a high collinearity in the dataset ###
par(mfrow = c(1,2))
plot(voice$dfrange, voice$maxdom)
plot(voice$centroid, voice$meanfreq)
par(mfrow = c(1,1))
#cor(voice[,1:20])
#cor(voice$skew, voice$kurt^(1/2))
nvoice <- voice[,-c(6,12,19)]
#lda(label ~ ., data = nvoice)
### Split data into training and testing dataset ###
set.seed(2016)
n <- nrow(nvoice)
nvar <- ncol(nvoice)
test.idx <- sample(n, size = round(n/4))
voice.train <- nvoice[-test.idx, ]
voice.test <- nvoice[ test.idx, ]
ntrain <- n - length(test.idx)
ntest <- length(test.idx)
### Method 1: linear regression ###
fit.lm <- lm(label ~., data = voice.train)
pre.lm.train <- ifelse( predict(fit.lm, voice.train) > 0.5, 1, 0 )
pre.lm.test <- ifelse( predict(fit.lm, voice.test) > 0.5, 1, 0 )

cer.lm.train <- sum(abs(voice.train$label- pre.lm.train)) / ntrain
cer.lm.test <- sum(abs(voice.test$label - pre.lm.test)) / ntest
### Method 2: logistic regression ###
fit.logit <- glm(label ~., family = binomial, data = voice.train)
pre.logit.train <- ifelse( predict(fit.logit, voice.train, type = "response") > 0.5, 1, 0 )
pre.logit.test <- ifelse( predict(fit.logit, voice.test, type = "response") > 0.5, 1, 0 )
cer.logit.train <- sum(abs(voice.train$label - pre.logit.train)) / ntrain
cer.logit.test <- sum(abs(voice.test$label - pre.logit.test)) / ntest
coef(fit.logit)
table(voice.train$label, pre.logit.train)
table(voice.test$label, pre.logit.test)
### Method 3: LDA ###
fit.lda <- lda(label ~ ., data = voice.train)
pre.lda.train <- as.numeric(as.character(predict(fit.lda, voice.train)$class))
pre.lda.test <- as.numeric(as.character(predict(fit.lda, voice.test)$class))
cer.lda.train <- sum(abs(voice.train$label - pre.lda.train)) / ntrain
cer.lda.test <- sum(abs(voice.test$label - pre.lda.test)) / ntest
### Method 4: QDA ###
fit.qda <- qda(label ~ ., data = voice.train)
pre.qda.train <- as.numeric(as.character(predict(fit.qda, voice.train)$class))
pre.qda.test <- as.numeric(as.character(predict(fit.qda, voice.test)$class))
cer.qda.train <- sum(abs(voice.train$label - pre.qda.train)) / ntrain
cer.qda.test <- sum(abs(voice.test$label - pre.qda.test)) / ntest
### Method 5: RDA ###
set.seed(2016)
fit.rda <- rda(label ~ ., data = voice.train, gamma = 0)
pre.rda.train <- as.numeric(as.character(predict(fit.rda, voice.train)$class))
pre.rda.test <- as.numeric(as.character(predict(fit.rda, voice.test)$class))
cer.rda.train <- sum(abs(voice.train$label - pre.rda.train)) / ntrain
cer.rda.test <- sum(abs(voice.test$label - pre.rda.test)) / ntest
fit.rda$regularization # optimal lambda
### Method 6: ridge linear regression ###
fit.lm.ridge <- cv.glmnet(y = voice.train$label, x = data.matrix(voice.train[,-nvar]), alpha = 0)
pre.lm.ridge.train <- ifelse( predict(fit.lm.ridge, newx = data.matrix(voice.train[,-nvar] )) > .5, 1, 0
)
pre.lm.ridge.test <- ifelse( predict(fit.lm.ridge, newx = data.matrix(voice.test[,-nvar] )) > .5, 1, 0 )
cer.lm.ridge.train <- sum(abs(voice.train$label - pre.lm.ridge.train)) / ntrain
cer.lm.ridge.test <- sum(abs(voice.test$label - pre.lm.ridge.test)) / ntest
### Method 7: lasso linear regression ###
fit.lm.lasso <- cv.glmnet( y = voice.train$label, x = data.matrix(voice.train[,-nvar]), alpha = 1)
pre.lm.lasso.train <- ifelse( predict(fit.lm.lasso, newx = data.matrix(voice.train[,-nvar] )) > .5, 1, 0
)
pre.lm.lasso.test <- ifelse( predict(fit.lm.lasso, newx = data.matrix(voice.test[,-nvar] )) > .5, 1, 0 )
cer.lm.lasso.train <- sum(abs(voice.train$label - pre.lm.lasso.train)) / ntrain
cer.lm.lasso.test <- sum(abs(voice.test$label - pre.lm.lasso.test)) / ntest
### Method 8: ridge logistic regression ###
fit.logit.ridge <- cv.glmnet( y = voice.train$label, x = data.matrix(voice.train[,-nvar]), alpha = 0,
family = "binomial")
pre.logit.ridge.train <- ifelse( predict(fit.logit.ridge, data.matrix(voice.train[,-nvar] ), type =
"response") >.5, 1, 0 )
pre.logit.ridge.test <- ifelse( predict(fit.logit.ridge, data.matrix(voice.test[,-nvar] ), type =
"response") > .5, 1, 0 )
cer.logit.ridge.train <- sum(abs(voice.train$label - pre.logit.ridge.train)) / ntrain
cer.logit.ridge.test <- sum(abs(voice.test$label - pre.logit.ridge.test)) / ntest
coef(fit.logit.ridge)
table(voice.train$label, pre.logit.ridge.train)
table(voice.test$label, pre.logit.ridge.test)
### Method 9: lasso logistic regression ###
fit.logit.lasso <- cv.glmnet( y = voice.train$label, x = data.matrix(voice.train[,-nvar]), alpha = 1,
family = "binomial")
pre.logit.lasso.train <- ifelse( predict(fit.logit.lasso, data.matrix(voice.train[,-nvar] ), type =
"response") >.5, 1, 0 )
pre.logit.lasso.test <- ifelse( predict(fit.logit.lasso, data.matrix(voice.test[,-nvar] ), type =
"response") > .5, 1, 0 )
cer.logit.lasso.train <- sum(abs(voice.train$label - pre.logit.lasso.train)) / ntrain
cer.logit.lasso.test <- sum(abs(voice.test$label - pre.logit.lasso.test)) / ntest
coef(fit.logit.lasso)
table(voice.train$label, pre.logit.lasso.train)
table(voice.test$label, pre.logit.lasso.test)
### PCA ###
center.train <- colMeans(voice.train[,-nvar])
scale.train <- apply(voice.train[,-nvar], 2, sd)
pc.train <- prcomp(voice.train[,-nvar], scale. = TRUE)
rotation <- pc.train$rotation
train.rotation <- as.matrix(scale(voice.train[,-nvar], center.train, scale.train)) %*%
as.matrix(rotation)
train.rotation <- as.data.frame(pc.train$x)
test.rotation <- as.data.frame(as.matrix(scale(voice.test[,-nvar], center.train, scale.train)) %*%
rotation)
train.rotation$label <- voice.train$label
test.rotation$label <- voice.test$label
#plot(pc$sdev^2/sum(pc$sdev^2), type = 'b', col=3, lwd=2)
#plot(pc$sdev, type = 'b', col=3, lwd=2)
#k = 3 # Only the first k componnts will be used
cer.pc.lda <- matrix(NA, 3, nvar-1)
cer.pc.qda <- matrix(NA, 3, nvar-1)
cer.pc.rda <- matrix(NA, 3, nvar-1)
cer.pc.lda[1,] <- cer.pc.qda[1,] <- cer.pc.rda[1,] <- 1:(nvar-1)
rownames(cer.pc.lda) <- rownames(cer.pc.qda) <- rownames(cer.pc.rda) <- c("npc","train",
"test")
for (k in 1:(nvar-1)){
pc.voice.train <- train.rotation[,c(1:k,nvar)]
pc.voice.test <- test.rotation[,c(1:k,nvar)]
pc.lda <- lda(label ~ ., data = pc.voice.train)
pre.pc.lda.train <- as.numeric(as.character(predict(pc.lda, pc.voice.train)$class))
pre.pc.lda.test <- as.numeric(as.character(predict(pc.lda, pc.voice.test)$class))
cer.pc.lda[2,k] <- sum(voice.train$label != pre.pc.lda.train) / ntrain
cer.pc.lda[3,k] <- sum(voice.test$label != pre.pc.lda.test) / ntest
pc.qda <- qda(label ~ ., data = pc.voice.train)
pre.pc.qda.train <- as.numeric(as.character(predict(pc.qda, pc.voice.train)$class))
pre.pc.qda.test <- as.numeric(as.character(predict(pc.qda, pc.voice.test)$class))
cer.pc.qda[2,k] <- sum(voice.train$label != pre.pc.qda.train) / ntrain
cer.pc.qda[3,k] <- sum(voice.test$label != pre.pc.qda.test) / ntest

pc.rda <- rda(label ~ ., data = pc.voice.train, gamma = 0)
pre.pc.rda.train <- as.numeric(as.character(predict(pc.rda, pc.voice.train)$class))
pre.pc.rda.test <- as.numeric(as.character(predict(pc.rda, pc.voice.test)$class))
cer.pc.rda[2,k] <- sum(voice.train$label != pre.pc.rda.train) / ntrain
cer.pc.rda[3,k] <- sum(voice.test$label != pre.pc.rda.test) / ntest
}
#Forward selection and Backward elimination in Linear Regression
fullmodel=lm(gender~.,data=voice.training)
basemodel=lm(gender~1,data=voice.training)
#forward selection
step(basemodel,scope=list(upper=fullmodel,lower=~1),direction="forward")
#backward elimination
step(fullmodel,direction="backward")
#Linear Regression using the model from backward elimination
linear.model3=lm(gender~sd + median + Q25+ Q75 + skew + kurt + sp.ent + sfm + mode +
meanfun + minfun + maxfun + meandom,data=voice.training)
summary(linear.model3)
linear.predict.test3=predict(linear.model3,voice.test)
linear.predict.training3=predict(linear.model3,voice.training)
#prediction of the class for the test data
linear.predict.test3[linear.predict.test3>=0.5]=1
linear.predict.test3[linear.predict.test3<0.5]=0
#prediction of the class for the training data
linear.predict.training3[linear.predict.training3>=0.5]=1
linear.predict.training3[linear.predict.training3<0.5]=0
#confusion matrices
table(linear.predict.test3,voice.test$gender)
table(linear.predict.training3,voice.training$gender)
#classification error rate on test data
linear.test.error.rate3=mean(linear.predict.test3!=voice.test$gender)
linear.test.error.rate3
#classification error rate on training data
linear.training.error.rate3=mean(linear.predict.training3!=voice.training$gender)
linear.training.error.rate3
###############################################################
#Forward selection and Backward elimination in Logistic Regression
fullmodel.logistic=glm(gender~.,data=voice.training,family="binomial")
basemodel.logistic=glm(gender~1,data=voice.training,family="binomial")
#forward selection
step(basemodel.logistic,scope=list(upper=fullmodel,lower=~1),direction="forward")
#backward elimination
step(fullmodel.logistic,direction="backward")
#Logistic Regression using the model from backward elimination
glm.fit3=glm(gender~Q25 +Q75 + kurt + sp.ent + sfm + meanfun + minfun +
modindx,data=voice.training,family="binomial")
summary(glm.fit3)
#predict probabilities using the test data
glm.probs.test3=predict(glm.fit3,voice.test,type="response")
#classify as "female" if the correspoding probability is greater than 0.5 (test data)
glm.pred.test3=rep(0, dim(voice.test) [1])
glm.pred.test3[glm.probs.test3>0.5]=1
#confusion matrix
table(glm.pred.test3,voice.test$gender)
#classification error rate on test data
glm.test.error.rate3=mean(glm.pred.test3!=voice.test$gender)
glm.test.error.rate3
#predict probabilities using the training data
glm.probs.training3=predict(glm.fit3,voice.training,type="response")
#classify as "female" if the correspoding probability is greater than 0.5 (test data)
glm.pred.training3=rep(0, dim(voice.training) [1])
glm.pred.training3[glm.probs.training3>0.5]=1
#confusion matrix
table(glm.pred.training3,voice.training$gender)
#classification error rate on training data
glm.training.error.rate3=mean(glm.pred.training3!=voice.training$gender)
glm.training.error.rate3
#final model (GLM from backward elimination)
glm.finalmodel=glm(gender~ Q25 +Q75 + kurt + sp.ent + sfm + meanfun + minfun +
modindx,data=voice,family="binomial")
summary(glm.finalmodel)
### Logistic regression with PCA ###
glm_pca_er=c()
best_glm_pca=1
best_glm_pca_er=1
train_df_pc_changed=train_df_pc
rank_logit_pca=0
for (i in seq(1,dim(train_df_pc)[2]-1)){
print(i)
### update used PCs
if (i==1){
train_df_pc_changed=train_df_pc_changed
}else{
train_df_pc_changed=train_df_pc_changed[,-dim(train_df_pc_changed)[2]]
}
fit.glm.pca <- glm(as.factor(y)~.,data=train_df_pc_changed, family='binomial')
y.predict.glm.pca.train = predict(fit.glm.pca,train_df_pc, type='response')
y.predict.glm.out.pca.train=rep('female',n*3/4)
y.predict.glm.out.pca.train[y.predict.glm.pca.train>0.5]='male'
current_er=mean(y.predict.glm.out.pca.train!= y_training)
glm_pca_er=append(glm_pca_er,current_er)
if (current_er<=best_glm_pca_er){
best_glm_pca=fit.glm.pca
best_glm_pca_er=current_er
rank_logit_pca=i
}
}
plot(seq(1,17),rev(glm_pca_er),'b.',xlab = 'Number of principal component', ylab =
'Misclassification Rate', main='Logistic regression misclassification rate on training data')
summary(best_glm_pca)
y.predict.glm.pca.train = predict(best_glm_pca,train_df_pc[,-18], type='response')
y.predict.glm.out.pca.train=rep('female',n*3/4)
y.predict.glm.out.pca.train[y.predict.glm.pca.train>0.5]='male'
mean(y.predict.glm.out.pca.train!= y_training)
table(y.predict.glm.out.pca.train, y_training)
y.predict.glm.pca = predict(best_glm_pca,test_df_pc, type='response')
y.predict.glm.out.pca=rep('female',n/4)
y.predict.glm.out.pca[y.predict.glm.pca>0.5]='male'
mean(y.predict.glm.out.pca!= y_testing)
table(y.predict.glm.out.pca, y_testing)
### Make some plots ###
# LDA: misclassfication with k components
ggplot(as.data.frame(t(cer.pc.lda))) +
geom_point(aes(x=npc, y=train, colour="train")) +
geom_line(aes(x=npc, y=train, colour="train")) +
geom_point(aes(x=npc, y=test, colour="test")) +
geom_line(aes(x=npc, y=test, colour="test")) +
xlab('Number of components') + ylab('Classification error rate') +
scale_colour_manual("",breaks = c("train", "test"),values = c("train"="blue","test"="green")) +
labs(title="LDA with principal components")
# QDA: misclassfication with k components
ggplot(as.data.frame(t(cer.pc.qda))) +
geom_point(aes(x=npc, y=train, colour="train")) +
geom_line(aes(x=npc, y=train, colour="train")) +
geom_point(aes(x=npc, y=test, colour="test")) +
geom_line(aes(x=npc, y=test, colour="test")) +
xlab('Number of components') + ylab('Classification error rate') +
scale_colour_manual("",breaks = c("train", "test"),values = c("train"="blue","test"="green")) +
labs(title="QDA with principal components")
# RDA: misclassfication with k components
ggplot(as.data.frame(t(cer.pc.rda))) +
geom_point(aes(x=npc, y=train, colour="train")) +
geom_line(aes(x=npc, y=train, colour="train")) +
geom_point(aes(x=npc, y=test, colour="test")) +
geom_line(aes(x=npc, y=test, colour="test")) +
xlab('Number of components') + ylab('Classification error rate') +
scale_colour_manual("",breaks = c("train", "test"),values = c("train"="blue","test"="green")) +

labs(title="RDA with principal components")
# Comparsion among methods without dimension reduction
method1 <- c("lm","logit","LDA","QDA","RDA")
train <- c(cer.lm.train, cer.logit.train, cer.lda.train, cer.qda.train, cer.rda.train)
test <- c(cer.lm.test, cer.logit.test, cer.lda.test, cer.rda.test, cer.rda.test)
error1 <- as.data.frame(t(rbind(train, test)))
rownames(error1) <- method1
error1$method <- c("lm","logistic","LDA","QDA","RDA")
re.error1 <- melt(error1)
ggplot(re.error1) +
geom_bar(aes(x = (method), y = value, fill = factor(variable)), stat = "identity", position =
"dodge") +
xlab("Classifier") + ylab("Misclassification rate")
