#Speech Recognition
#read the file
voice=read.csv("Project3_dataset.txt",header=FALSE,stringsAsFactors=FALSE)
voice<-voice[-1,]
voice<-voice[,2:258]
dim(voice)
names=as.character(c(1:256,"g"))
for(i in 1:256){
coleach=paste("x.",(as.character(i)), sep = "")
names[i]=coleach
}
colnames(voice)=names
#code the response variable
voice$g[voice$g == "aa"]<- 1
voice$g[voice$g == "ao"]<- 2
voice$g[voice$g == "dcl"]<- 3
voice$g[voice$g == "iy"]<- 4
voice$g[voice$g == "sh"]<- 5
voice$g<-as.factor(voice$g)
#define n and p
n=dim(voice)[1]
p=dim(voice)[2]-1
for (i in 1:p)
{voice[,i]<-as.numeric(voice[,i])}
#partition data into two groups: training and test
set.seed(2016)
test=sample(n,round(n/4))
#create training data
voice.training=voice[-test,]
#create test data
voice.test=voice[test,]
#LDA
fit.lda=lda(g~., data = voice.train)
pre.lda.train=as.numeric(as.character(predict(fit.lda, voice.train)$class))
pre.lda.test=as.numeric(as.character(predict(fit.lda, voice.test)$class))
lda.table.train=table( Actual=(actual<-voice.train[,257]), Classified=(classified<-pre.lda.train) )
lda.err.train=(train.n-sum(diag(lda.table.train)))/train.n
lda.err.train
#[1] 0.05913661
lda.table.test=table( Actual=(actual<-voice.test[,257]), Classified=(classified<-pre.lda.test) )
lda.err.test=(test.n-sum(diag(lda.table.test)))/test.n
lda.err.test
#[1] 0.07630878
###Confusion table###
lda.table.train
lda.table.test
###Time###
lda.time=system.time(lda(g~., data = voice.train))[3]
lda.time
#elapsed
# 1.238
#LDA on PCA
center.train <- colMeans(voice.train[,-(p+1)])
scale.train <- apply(voice.train[,-(p+1)], 2, sd)
pc.train <- prcomp(voice.train[,-(p+1)], scale. = TRUE)
pc.train
rotation <- pc.train$rotation
train.rotation <- as.matrix(scale(voice.train[,-(p+1)], center.train, scale.train)) %*%
as.matrix(rotation)
train.rotation <- as.data.frame(pc.train$x)
test.rotation <- as.data.frame(as.matrix(scale(voice.test[,-(p+1)], center.train, scale.train)) %*%
rotation)
train.rotation$g <- voice.train$g
test.rotation$g <- voice.test$g
#Plot the PVE and cumulative PVE
par(mfrow=c(1,1))
#plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained ",
ylim=c(0,1),type='b')

#plot(cumsum(pve), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type='b')
cer.pc.lda <- matrix(NA, 3, p)
cer.pc.lda[1,] <- 1:(p)
rownames(cer.pc.lda) <- c("npc","train", "test")
for (k in 1:p){
pc.voice.train <- train.rotation[,c(1:k,p+1)]
pc.voice.test <- test.rotation[,c(1:k,p+1)]
pc.lda <- lda(g ~ ., data = pc.voice.train)
pre.pc.lda.train <- as.numeric(as.character(predict(pc.lda, pc.voice.train)$class))
pre.pc.lda.test <- as.numeric(as.character(predict(pc.lda, pc.voice.test)$class))
cer.pc.lda[2,k] <- sum(voice.train$g != pre.pc.lda.train) / train.n
cer.pc.lda[3,k] <- sum(voice.test$g != pre.pc.lda.test) / test.n
}
cer.pc.lda
#Plot with k components
ggplot(as.data.frame(t(cer.pc.lda))) +
geom_point(aes(x=npc, y=train, colour="train")) +
geom_line(aes(x=npc, y=train, colour="train")) +
geom_point(aes(x=npc, y=test, colour="test")) +
geom_line(aes(x=npc, y=test, colour="test")) +
xlab('Number of components') + ylab('Classification error rate') +
scale_colour_manual("",breaks = c("train", "test"),values = c("train"="blue","test"="green")) +
labs(title="LDA with principal components")
which.min(cer.pc.lda[2,1:10])
cer.pc.lda[2,9]
cer.pc.lda[3,9]
#QDA
library(ROCR)
roccurve=function(pred,truth,...)
{pr=prediction(pred,truth)
perf=performance(pr,'tpr','fpr')
plot(perf,...)}
time.rf.qda=system.time(fit.qda <- qda(y~.,data=train_df))[3]

time.rf.qda
y.predict_train.qda = predict(fit.qda,train_df)$class
y.predict_train.qda_percent=predict(fit.qda,train_df)$posterior
y.predict.qda = predict(fit.qda,test_df)$class
y.predict.qda_percent=predict(fit.qda,test_df)$posterior
table(y.predict.qda, y_testing)
mean(y.predict.qda!=y_testing)
table(y.predict_train.qda, y_training)
mean(y.predict_train.qda!=y_training)
type=c('aa','ao','dcl','iy','sh')
par(mfrow=c(2,3))
for (i in seq(1,5)){
print(i)
y_testing_aa=y_testing
y_testing_aa[y_testing_aa==type[i]]=1
y_testing_aa[y_testing_aa!='1']=0
y_testing_aa=as.numeric(y_testing_aa)
y_predict_aa=y.predict.qda_percent[,i]
y_training_aa=y_training
y_training_aa[y_training_aa==type[i]]=1
y_training_aa[y_training_aa!='1']=0
y_training_aa=as.numeric(y_training_aa)
y_predict_train_aa=y.predict_train.qda_percent[,i]
roccurve(y_predict_train_aa,y_training_aa,main=paste("ROC curve for class ",type[i]),col=4)
roccurve(y_predict_aa,y_testing_aa,add=T,col=2)
legend(0.5,0.5,c("Training data", "Test
data"),col=c(4,2),cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
}
par(mfrow=c(1,2))
type=c('aa','ao','dcl','iy','sh')
for (i in seq(1,5)){
print(i)
y_testing_aa=y_testing
y_testing_aa[y_testing_aa==type[i]]=1
y_testing_aa[y_testing_aa!='1']=0
y_testing_aa=as.numeric(y_testing_aa)
y_predict_aa=y.predict.qda_percent[,i]

y_training_aa=y_training
y_training_aa[y_training_aa==type[i]]=1
y_training_aa[y_training_aa!='1']=0
y_training_aa=as.numeric(y_training_aa)
y_predict_train_aa=y.predict_train.qda_percent[,i]
if (i!=1){
roccurve(y_predict_train_aa,y_training_aa,main="Training data",col=i,add=T)
}else{
roccurve(y_predict_train_aa,y_training_aa,main="Training data",col=i)
}
}
legend(0.5,0.5,c("Class 1", "Class 2","Class 3","Class 4", "Class
5"),col=1:5,cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
for (i in seq(1,5)){
print(i)
y_testing_aa=y_testing
y_testing_aa[y_testing_aa==type[i]]=1
y_testing_aa[y_testing_aa!='1']=0
y_testing_aa=as.numeric(y_testing_aa)
y_predict_aa=y.predict.qda_percent[,i]
y_training_aa=y_training
y_training_aa[y_training_aa==type[i]]=1
y_training_aa[y_training_aa!='1']=0
y_training_aa=as.numeric(y_training_aa)
y_predict_train_aa=y.predict_train.qda_percent[,i]
if (i!=1){
roccurve(y_predict_aa,y_testing_aa,main="Testing data",col=i,add=T)
}else{
roccurve(y_predict_aa,y_testing_aa,main="Testing data",col=i)
}
}
legend(0.5,0.5,c("Class 1", "Class 2","Class 3","Class 4", "Class
5"),col=1:5,cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
best_test_mr=1
best_test_qda=c()
test_mr_all=c()
train_mr_all=c()
X_training_pc_tmp=X_training_pc
X_testing_pc_tmp=X_testing_pc
for (i in seq(1,dim(X_training_pc)[2])){
print('start')
X_training_pc_tmp=X_training_pc[,seq(1,i)]
X_testing_pc_tmp=X_testing_pc[,seq(1,i)]
train_df_pc=data.frame(y=y_training,X=X_training_pc_tmp)
test_df_pc=data.frame(y=y_testing,X=X_testing_pc_tmp)
print(paste('fit model',i))
time.rf.qda=system.time(fit.qda <- qda(y~.,data=train_df_pc))[3]
time.rf.qda
print('test model')
y.predict_train.qda = predict(fit.qda,train_df_pc)$class
y.predict_train.qda_percent=predict(fit.qda,train_df_pc)$posterior
y.predict.qda = predict(fit.qda,test_df_pc)$class
y.predict.qda_percent=predict(fit.qda,test_df_pc)$posterior
table(y.predict.qda, y_testing)
test_mr=mean(y.predict.qda!=y_testing)
table(y.predict_train.qda, y_training)
train_mr=mean(y.predict_train.qda!=y_training)
test_mr_all=append(test_mr_all,test_mr)
train_mr_all=append(train_mr_all,train_mr)
if (test_mr < best_test_mr){
best_test_mr=test_mr
best_test_qda=fit.qda
print(paste(best_test_mr,i))
}
}
par(mfrow=c(1,1))
plot(x_index,train_mr_all,type="l",col="blue",ylim=c(0,0.45),ylab='',xlab='',main='Misclassification
Rate of QDA with Dimension Reduction')
par(new=T)
plot(x_index,test_mr_all,type="l",col="red",ylim=c(0,0.45),ylab='Misclassification
Rate',xlab='Number of PC')
legend(200,0.4,c("Testing data", "Training
data"),col=c('red','blue'),cex=0.8,lty=c(1,1),lwd=c(1.5,1.5))
X_training_pc_tmp=X_training_pc[,seq(1,8)]
X_testing_pc_tmp=X_testing_pc[,seq(1,8)]
train_df_pc=data.frame(y=y_training,X=X_training_pc_tmp)
test_df_pc=data.frame(y=y_testing,X=X_testing_pc_tmp)
dim(train_df_pc)
time.rf.qda=system.time(fit.qda <- qda(y~.,data=train_df_pc))[3]
time.rf.qda
y.predict_train.qda = predict(fit.qda,train_df_pc)$class
y.predict_train.qda_percent=predict(fit.qda,train_df_pc)$posterior
y.predict.qda = predict(fit.qda,test_df_pc)$class
y.predict.qda_percent=predict(fit.qda,test_df_pc)$posterior
attributes(predict(fit.qda,test_df_pc))
table(y.predict.qda, y_testing)
mean(y.predict.qda!=y_testing)
table(y.predict_train.qda, y_training)
mean(y.predict_train.qda!=y_training)
library(ROCR)
roccurve=function(pred,truth,...)
{pr=prediction(pred,truth)
perf=performance(pr,'tpr','fpr')
plot(perf,...)}
type=c('aa','ao','dcl','iy','sh')
par(mfrow=c(2,3))
for (i in seq(1,5)){
print(i)
y_testing_aa=y_testing
y_testing_aa[y_testing_aa==type[i]]=1
y_testing_aa[y_testing_aa!='1']=0
y_testing_aa=as.numeric(y_testing_aa)
y_predict_aa=y.predict.qda_percent[,i]
y_training_aa=y_training
y_training_aa[y_training_aa==type[i]]=1
y_training_aa[y_training_aa!='1']=0
y_training_aa=as.numeric(y_training_aa)
y_predict_train_aa=y.predict_train.qda_percent[,i]
roccurve(y_predict_train_aa,y_training_aa,main=paste("ROC curve for class ",type[i]),col=4)
roccurve(y_predict_aa,y_testing_aa,add=T,col=2)
legend(0.5,0.5,c("Training data", "Test
data"),col=c(4,2),cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))

}
par(mfrow=c(1,2))
type=c('aa','ao','dcl','iy','sh')
for (i in seq(1,5)){
print(i)
y_testing_aa=y_testing
y_testing_aa[y_testing_aa==type[i]]=1
y_testing_aa[y_testing_aa!='1']=0
y_testing_aa=as.numeric(y_testing_aa)
y_predict_aa=y.predict.qda_percent[,i]
y_training_aa=y_training
y_training_aa[y_training_aa==type[i]]=1
y_training_aa[y_training_aa!='1']=0
y_training_aa=as.numeric(y_training_aa)
y_predict_train_aa=y.predict_train.qda_percent[,i]
if (i!=1){
roccurve(y_predict_train_aa,y_training_aa,main="Training data",col=i,add=T)
}else{
roccurve(y_predict_train_aa,y_training_aa,main="Training data",col=i)
}
}
legend(0.5,0.5,c("Class 1", "Class 2","Class 3","Class 4", "Class
5"),col=1:5,cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
for (i in seq(1,5)){
print(i)
y_testing_aa=y_testing
y_testing_aa[y_testing_aa==type[i]]=1
y_testing_aa[y_testing_aa!='1']=0
y_testing_aa=as.numeric(y_testing_aa)
y_predict_aa=y.predict.qda_percent[,i]
y_training_aa=y_training
y_training_aa[y_training_aa==type[i]]=1
y_training_aa[y_training_aa!='1']=0
y_training_aa=as.numeric(y_training_aa)
y_predict_train_aa=y.predict_train.qda_percent[,i]
if (i!=1){
roccurve(y_predict_aa,y_testing_aa,main="Testing data",col=i,add=T)
}else{
roccurve(y_predict_aa,y_testing_aa,main="Testing data",col=i)
}

}
legend(0.5,0.5,c("Class 1", "Class 2","Class 3","Class 4", "Class
5"),col=1:5,cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
#RDA
time.rf.rda=system.time(fit.rda <- rda(y~.,data=train_df, gamma = 0))[3]
time.rf.rda
y.predict_train.rda = predict(fit.rda,train_df)$class
y.predict_train.rda_percent=predict(fit.rda,train_df)$posterior
y.predict.rda = predict(fit.rda,test_df)$class
y.predict.rda_percent=predict(fit.rda,test_df)$posterior
table(y.predict.rda, y_testing)
mean(y.predict.rda!=y_testing)
table(y.predict_train.rda, y_training)
mean(y.predict_train.rda!=y_training)
type=c('aa','ao','dcl','iy','sh')
par(mfrow=c(2,3))
for (i in seq(1,5)){
print(i)
y_testing_aa=y_testing
y_testing_aa[y_testing_aa==type[i]]=1
y_testing_aa[y_testing_aa!='1']=0
y_testing_aa=as.numeric(y_testing_aa)
y_predict_aa=y.predict.rda_percent[,i]
y_training_aa=y_training
y_training_aa[y_training_aa==type[i]]=1
y_training_aa[y_training_aa!='1']=0
y_training_aa=as.numeric(y_training_aa)
y_predict_train_aa=y.predict_train.rda_percent[,i]
roccurve(y_predict_train_aa,y_training_aa,main=paste("ROC curve for class ",type[i]),col=4)
roccurve(y_predict_aa,y_testing_aa,add=T,col=2)
legend(0.5,0.5,c("Training data", "Test
data"),col=c(4,2),cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))

}
par(mfrow=c(1,2))
type=c('aa','ao','dcl','iy','sh')
for (i in seq(1,5)){
print(i)
y_testing_aa=y_testing
y_testing_aa[y_testing_aa==type[i]]=1
y_testing_aa[y_testing_aa!='1']=0
y_testing_aa=as.numeric(y_testing_aa)
y_predict_aa=y.predict.rda_percent[,i]
y_training_aa=y_training
y_training_aa[y_training_aa==type[i]]=1
y_training_aa[y_training_aa!='1']=0
y_training_aa=as.numeric(y_training_aa)
y_predict_train_aa=y.predict_train.rda_percent[,i]
if (i!=1){
roccurve(y_predict_train_aa,y_training_aa,main="Training data",col=i,add=T)
}else{
roccurve(y_predict_train_aa,y_training_aa,main="Training data",col=i)
}
}
legend(0.5,0.5,c("Class 1", "Class 2","Class 3","Class 4", "Class
5"),col=1:5,cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
for (i in seq(1,5)){
print(i)
y_testing_aa=y_testing
y_testing_aa[y_testing_aa==type[i]]=1
y_testing_aa[y_testing_aa!='1']=0
y_testing_aa=as.numeric(y_testing_aa)
y_predict_aa=y.predict.rda_percent[,i]
y_training_aa=y_training
y_training_aa[y_training_aa==type[i]]=1
y_training_aa[y_training_aa!='1']=0
y_training_aa=as.numeric(y_training_aa)
y_predict_train_aa=y.predict_train.rda_percent[,i]
if (i!=1){
roccurve(y_predict_aa,y_testing_aa,main="Testing data",col=i,add=T)
}else{
roccurve(y_predict_aa,y_testing_aa,main="Testing data",col=i)
}

}
legend(0.5,0.5,c("Class 1", "Class 2","Class 3","Class 4", "Class
5"),col=1:5,cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
############# RDA PCA
best_test_mr_rda=1
best_test_rda=c()
test_mr_all_rda=c()
train_mr_all_rda=c()
X_training_pc_tmp=X_training_pc
X_testing_pc_tmp=X_testing_pc
for (i in seq(1,20)){
print('start')
X_training_pc_tmp=X_training_pc[,seq(1,i)]
X_testing_pc_tmp=X_testing_pc[,seq(1,i)]
train_df_pc=data.frame(y=y_training,X=X_training_pc_tmp)
test_df_pc=data.frame(y=y_testing,X=X_testing_pc_tmp)
print(paste('fit model',i))
time.rf.rda=system.time(fit.rda_pc <- rda(y~.,data=train_df_pc, gamma = 0))[3]
time.rf.rda
print('test model')
y.predict_train.rda = predict(fit.rda_pc,train_df_pc)$class
y.predict_train.rda_percent=predict(fit.rda_pc,train_df_pc)$posterior
y.predict.rda = predict(fit.rda_pc,test_df_pc)$class
y.predict.rda_percent=predict(fit.rda_pc,test_df_pc)$posterior
table(y.predict.rda, y_testing)
test_mr=mean(y.predict.rda!=y_testing)
table(y.predict_train.qda, y_training)
train_mr=mean(y.predict_train.rda!=y_training)
test_mr_all_rda=append(test_mr_all_rda,test_mr)
train_mr_all_rda=append(train_mr_all_rda,train_mr)
if (test_mr < best_test_mr_rda){
best_test_mr_rda=test_mr
best_test_rda=fit.rda_pc
print(paste(best_test_mr_rda,i))
best_test_rda_pc_time=time.rf.rda
}
}
par(mfrow=c(1,1))

plot(seq(1,20),train_mr_all_rda,type="p",col="blue",ylim=c(0,0.45),ylab='',xlab='',main='Misclassi
fication Rate of RDA with dimension reduction')
lines(seq(1,20),train_mr_all_rda,col="blue")
par(new=T)
plot(seq(1,20),test_mr_all_rda,type="p",col="red",ylim=c(0,0.45),ylab='Misclassification
Rate',xlab='Number of PC')
lines(seq(1,20),test_mr_all_rda,col="red")
legend(15,0.4,c("Testing data", "Training
data"),col=c('red','blue'),cex=0.8,lty=c(1,1),lwd=c(1.5,1.5))
X_training_pc_tmp=X_training_pc[,seq(1,16)]
X_testing_pc_tmp=X_testing_pc[,seq(1,16)]
train_df_pc=data.frame(y=y_training,X=X_training_pc_tmp)
test_df_pc=data.frame(y=y_testing,X=X_testing_pc_tmp)
time.rf.rda=system.time(fit.rda_pc <- rda(y~.,data=train_df_pc, gamma = 0))[3]
time.rf.rda
y.predict_train.rda = predict(best_test_rda,train_df_pc)$class
y.predict_train.rda_percent=predict(best_test_rda,train_df_pc)$posterior
y.predict.rda = predict(best_test_rda,test_df_pc)$class
y.predict.rda_percent=predict(best_test_rda,test_df_pc)$posterior
attributes(predict(best_test_rda,test_df_pc))
table(y.predict.rda, y_testing)
mean(y.predict.rda!=y_testing)
table(y.predict_train.rda, y_training)
mean(y.predict_train.rda!=y_training)
type=c('aa','ao','dcl','iy','sh')
par(mfrow=c(2,3))
for (i in seq(1,5)){
print(i)
y_testing_aa=y_testing
y_testing_aa[y_testing_aa==type[i]]=1
y_testing_aa[y_testing_aa!='1']=0
y_testing_aa=as.numeric(y_testing_aa)
y_predict_aa=y.predict.rda_percent[,i]
y_training_aa=y_training
y_training_aa[y_training_aa==type[i]]=1
y_training_aa[y_training_aa!='1']=0

y_training_aa=as.numeric(y_training_aa)
y_predict_train_aa=y.predict_train.rda_percent[,i]
roccurve(y_predict_train_aa,y_training_aa,main=paste("ROC curve for class ",type[i]),col=4)
roccurve(y_predict_aa,y_testing_aa,add=T,col=2)
legend(0.5,0.5,c("Training data", "Test
data"),col=c(4,2),cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
}
par(mfrow=c(1,2))
type=c('aa','ao','dcl','iy','sh')
for (i in seq(1,5)){
print(i)
y_testing_aa=y_testing
y_testing_aa[y_testing_aa==type[i]]=1
y_testing_aa[y_testing_aa!='1']=0
y_testing_aa=as.numeric(y_testing_aa)
y_predict_aa=y.predict.rda_percent[,i]
y_training_aa=y_training
y_training_aa[y_training_aa==type[i]]=1
y_training_aa[y_training_aa!='1']=0
y_training_aa=as.numeric(y_training_aa)
y_predict_train_aa=y.predict_train.rda_percent[,i]
if (i!=1){
roccurve(y_predict_train_aa,y_training_aa,main="Training data",col=i,add=T)
}else{
roccurve(y_predict_train_aa,y_training_aa,main="Training data",col=i)
}
}
legend(0.5,0.5,c("Class 1", "Class 2","Class 3","Class 4", "Class
5"),col=1:5,cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
for (i in seq(1,5)){
print(i)
y_testing_aa=y_testing
y_testing_aa[y_testing_aa==type[i]]=1
y_testing_aa[y_testing_aa!='1']=0
y_testing_aa=as.numeric(y_testing_aa)
y_predict_aa=y.predict.rda_percent[,i]
y_training_aa=y_training

y_training_aa[y_training_aa==type[i]]=1
y_training_aa[y_training_aa!='1']=0
y_training_aa=as.numeric(y_training_aa)
y_predict_train_aa=y.predict_train.rda_percent[,i]
if (i!=1){
roccurve(y_predict_aa,y_testing_aa,main="Testing data",col=i,add=T)
}else{
roccurve(y_predict_aa,y_testing_aa,main="Testing data",col=i)
}
}
legend(0.5,0.5,c("Class 1", "Class 2","Class 3","Class 4", "Class
5"),col=1:5,cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
##############
#CART
library(tree)
voice.cart=tree(g~.,data=voice.training)
summary(voice.cart)
#computing time
time.cart=system.time(tree(g~.,data=voice.training))[3]
time.cart
#plot tree structure
plot(voice.cart)
#node labels
text(voice.cart,pretty=0)
#prediction on test data
voice.cart.pred.test=predict(voice.cart,voice.test,type="class")
#prediction on training data
voice.cart.pred.training=predict(voice.cart,voice.training,type="class")
#confusion matrix on test data
table(voice.cart.pred.test,voice.test$g)
#confusion matrix on training data
table(voice.cart.pred.training,voice.training$g)
#classification error rate on test data
cart.test.error.rate=mean(voice.cart.pred.test!=voice.test$g)
cart.test.error.rate

#classification error rate on training data
cart.training.error.rate=mean(voice.cart.pred.training!=voice.training$g)
cart.training.error.rate
#pruning
set.seed(2016)
cv.voice=cv.tree(voice.cart,FUN=prune.misclass)
cv.voice
#computing time
time.prune=system.time(cv.tree(voice.cart,FUN=prune.misclass))[3]
time.prune
#plot of error rate vs size and k
par(mfrow=c(1,2))
plot(cv.voice$size,cv.voice$dev,type="b")
plot(cv.voice$k,cv.voice$dev,type="b")
#take the tree with 10 nodes (prune the tree)
par(mfrow=c(1,1))
prune.voice=prune.misclass(voice.cart,best=10)
system.time(prune.misclass(voice.cart,best=10))[3]
#plot tree structure
plot(prune.voice)
#node labels
text(prune.voice,pretty=0)
prune.voice
summary(prune.voice)
#prediction on test data
voice.prune.pred.test=predict(prune.voice,voice.test,type="class")
#prediction on training data
voice.prune.pred.training=predict(prune.voice,voice.training,type="class")
#confusion matrix on test data
table(voice.prune.pred.test,voice.test$g)
#confusion matrix on training data
table(voice.prune.pred.training,voice.training$g)

#classification error rate on test data
prune.test.error.rate=mean(voice.prune.pred.test!=voice.test$g)
prune.test.error.rate
#classification error rate on training data
prune.training.error.rate=mean(voice.prune.pred.training!=voice.training$g)
prune.training.error.rate
#Random Forest
library(randomForest)
set.seed(2016)
#m=sqrt(p)=16
voice.rf=randomForest(g~.,data=voice.training,mtry=sqrt(p),importance=TRUE)
voice.rf
#computing time
time.rf=system.time(randomForest(g~.,data=voice.training,mtry=sqrt(p),importance=TRUE))[3]
time.rf
#prediction on test data
voice.rf.pred.test=predict(voice.rf,voice.test,type="class")
#prediction on training data
voice.rf.pred.training=voice.rf$predicted
#confusion matrix on test data
table(voice.rf.pred.test,voice.test$g)
#confusion matrix on training data
table(voice.rf.pred.training,voice.training$g)
#classification error rate on test data
rf.test.error.rate=mean(voice.rf.pred.test!=voice.test$g)
rf.test.error.rate
#classification error rate on training data
rf.training.error.rate=mean(voice.rf.pred.training!=voice.training$g)
rf.training.error.rate
#see the importance of each variable
importance(voice.rf)
#plots
varImpPlot(voice.rf)

#Conclusion: Pruning the tree does not improve misclassification error rate
#Random Forest is the best
#ROC curves for CART
library(ROCR)
roccurve=function(pred,truth,...)
{pr=prediction(pred,truth)
perf=performance(pr,'tpr','fpr')
plot(perf,...)}
#recode the response
#class 1
#training data
voice.training.class1=rep(1,length(voice.training$g))
voice.training.class1[voice.training$g != 1]<- 0
voice.training.class1<-as.factor(voice.training.class1)
#test data
voice.test.class1=rep(1,length(voice.test$g))
voice.test.class1[voice.test$g != 1]<- 0
voice.test.class1<-as.factor(voice.test.class1)
#class 2
#training data
voice.training.class2=rep(2,length(voice.training$g))
voice.training.class2[voice.training$g != 2]<- 0
voice.training.class2<-as.factor(voice.training.class2)
#test data
voice.test.class2=rep(2,length(voice.test$g))
voice.test.class2[voice.test$g != 2]<- 0
voice.test.class2<-as.factor(voice.test.class2)
#class 3
#training data
voice.training.class3=rep(3,length(voice.training$g))
voice.training.class3[voice.training$g != 3]<- 0
voice.training.class3<-as.factor(voice.training.class3)
#test data
voice.test.class3=rep(3,length(voice.test$g))


voice.test.class3[voice.test$g != 3]<- 0
voice.test.class3<-as.factor(voice.test.class3)
#class 4
#training data
voice.training.class4=rep(4,length(voice.training$g))
voice.training.class4[voice.training$g != 4]<- 0
voice.training.class4<-as.factor(voice.training.class4)
#test data
voice.test.class4=rep(4,length(voice.test$g))
voice.test.class4[voice.test$g != 4]<- 0
voice.test.class4<-as.factor(voice.test.class4)
#class 5
#training data
voice.training.class5=rep(5,length(voice.training$g))
voice.training.class5[voice.training$g != 5]<- 0
voice.training.class5<-as.factor(voice.training.class5)
#test data
voice.test.class5=rep(5,length(voice.test$g))
voice.test.class5[voice.test$g != 5]<- 0
voice.test.class5<-as.factor(voice.test.class5)
#ROC plots
par(mfrow=c(2,3))
roccurve(as.numeric(voice.cart.pred.training.prob[,1]),as.numeric(voice.training.class1),main="R
OC curve for class 1",col=4)
roccurve(as.numeric(voice.cart.pred.test.prob[,1]),as.numeric(voice.test.class1),add=T,col=2)
legend(0.5,0.5,c("Training data", "Test
data"),col=c(4,2),cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
roccurve(as.numeric(voice.cart.pred.training.prob[,2]),as.numeric(voice.training.class2),main="R
OC curve for class 2",col=4)
roccurve(as.numeric(voice.cart.pred.test.prob[,2]),as.numeric(voice.test.class2),add=T,col=2)
legend(0.5,0.5,c("Training data", "Test data"),col =
c(4,2),cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
roccurve(as.numeric(voice.cart.pred.training.prob[,3]),as.numeric(voice.training.class3),main="R
OC curve for class 3",col=4)

roccurve(as.numeric(voice.cart.pred.test.prob[,3]),as.numeric(voice.test.class3),add=T,col=2)
legend(0.5,0.5,c("Training data", "Test data"),col =
c(4,2),cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
roccurve(as.numeric(voice.cart.pred.training.prob[,4]),as.numeric(voice.training.class4),main="R
OC curve for class 4",col=4)
roccurve(as.numeric(voice.cart.pred.test.prob[,4]),as.numeric(voice.test.class4),add=T,col=2)
legend(0.5,0.5,c("Training data", "Test data"),col =
c(4,2),cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
roccurve(as.numeric(voice.cart.pred.training.prob[,5]),as.numeric(voice.training.class5),main="R
OC curve for class 5",col=4)
roccurve(as.numeric(voice.cart.pred.test.prob[,5]),as.numeric(voice.test.class5),add=T,col=2)
legend(0.5,0.5,c("Training data", "Test data"),col =
c(4,2),cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
par(mfrow=c(1,2))
roccurve(as.numeric(voice.cart.pred.training.prob[,1]),as.numeric(voice.training.class1),main="T
raining data",col=1)
roccurve(as.numeric(voice.cart.pred.training.prob[,2]),as.numeric(voice.training.class2),add=T,c
ol=2)
roccurve(as.numeric(voice.cart.pred.training.prob[,3]),as.numeric(voice.training.class3),add=T,c
ol=3)
roccurve(as.numeric(voice.cart.pred.training.prob[,4]),as.numeric(voice.training.class4),add=T,c
ol=4)
roccurve(as.numeric(voice.cart.pred.training.prob[,5]),as.numeric(voice.training.class5),add=T,c
ol=5)
legend(0.5,0.5,c("Class 1", "Class 2","Class 3","Class 4", "Class
5"),col=1:5,cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
roccurve(as.numeric(voice.cart.pred.test.prob[,1]),as.numeric(voice.test.class1),main="Test
data",col=1)
roccurve(as.numeric(voice.cart.pred.test.prob[,2]),as.numeric(voice.test.class2),add=T,col=2)
roccurve(as.numeric(voice.cart.pred.test.prob[,3]),as.numeric(voice.test.class3),add=T,col=3)
roccurve(as.numeric(voice.cart.pred.test.prob[,4]),as.numeric(voice.test.class4),add=T,col=4)
roccurve(as.numeric(voice.cart.pred.test.prob[,5]),as.numeric(voice.test.class5),add=T,col=5)
legend(0.5,0.5,c("Class 1", "Class 2","Class 3","Class 4", "Class
5"),col=1:5,cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
#ROC curves for Random Forest
par(mfrow=c(2,3))

roccurve(as.numeric(voice.rf.pred.training.prob[,1]),as.numeric(voice.training.class1),main="RO
C curve for class 1",col=4)
roccurve(as.numeric(voice.rf.pred.test.prob[,1]),as.numeric(voice.test.class1),add=T,col=2)
legend(0.5,0.5,c("Training data", "Test
data"),col=c(4,2),cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
roccurve(as.numeric(voice.rf.pred.training.prob[,2]),as.numeric(voice.training.class2),main="RO
C curve for class 2",col=4)
roccurve(as.numeric(voice.rf.pred.test.prob[,2]),as.numeric(voice.test.class2),add=T,col=2)
legend(0.5,0.5,c("Training data", "Test data"),col =
c(4,2),cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
roccurve(as.numeric(voice.rf.pred.training.prob[,3]),as.numeric(voice.training.class3),main="RO
C curve for class 3",col=4)
roccurve(as.numeric(voice.rf.pred.test.prob[,3]),as.numeric(voice.test.class3),add=T,col=2)
legend(0.5,0.5,c("Training data", "Test data"),col =
c(4,2),cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
roccurve(as.numeric(voice.rf.pred.training.prob[,4]),as.numeric(voice.training.class4),main="RO
C curve for class 4",col=4)
roccurve(as.numeric(voice.rf.pred.test.prob[,4]),as.numeric(voice.test.class4),add=T,col=2)
legend(0.5,0.5,c("Training data", "Test data"),col =
c(4,2),cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
roccurve(as.numeric(voice.rf.pred.training.prob[,5]),as.numeric(voice.training.class5),main="RO
C curve for class 5",col=4)
roccurve(as.numeric(voice.rf.pred.test.prob[,5]),as.numeric(voice.test.class5),add=T,col=2)
legend(0.5,0.5,c("Training data", "Test data"),col =
c(4,2),cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
par(mfrow=c(1,2))
roccurve(as.numeric(voice.rf.pred.training.prob[,1]),as.numeric(voice.training.class1),main="Trai
ning data",col=1)
roccurve(as.numeric(voice.rf.pred.training.prob[,2]),as.numeric(voice.training.class2),add=T,col=
2)
roccurve(as.numeric(voice.rf.pred.training.prob[,3]),as.numeric(voice.training.class3),add=T,col=
3)
roccurve(as.numeric(voice.rf.pred.training.prob[,4]),as.numeric(voice.training.class4),add=T,col=
4)
roccurve(as.numeric(voice.rf.pred.training.prob[,5]),as.numeric(voice.training.class5),add=T,col=
5)
legend(0.5,0.5,c("Class 1", "Class 2","Class 3","Class 4", "Class
5"),col=1:5,cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))

roccurve(as.numeric(voice.rf.pred.test.prob[,1]),as.numeric(voice.test.class1),main="Test
data",col=1)
roccurve(as.numeric(voice.rf.pred.test.prob[,2]),as.numeric(voice.test.class2),add=T,col=2)
roccurve(as.numeric(voice.rf.pred.test.prob[,3]),as.numeric(voice.test.class3),add=T,col=3)
roccurve(as.numeric(voice.rf.pred.test.prob[,4]),as.numeric(voice.test.class4),add=T,col=4)
roccurve(as.numeric(voice.rf.pred.test.prob[,5]),as.numeric(voice.test.class5),add=T,col=5)
legend(0.5,0.5,c("Class 1", "Class 2","Class 3","Class 4", "Class
5"),col=1:5,cex=0.8,text.width=0.2,lty=c(1,1),lwd=c(1.5,1.5))
