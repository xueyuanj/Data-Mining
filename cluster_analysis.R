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
# K-Means with different nstart
set.seed(2016)
km.out.1=kmeans(voice[,1:256], 5, nstart = 1)
km.out.2=kmeans(voice[,1:256], 5, nstart = 5)
km.out.3=kmeans(voice[,1:256], 5, nstart = 20)
km.out.4=kmeans(voice[,1:256], 5, nstart = 50)
### Evaluate with adjusted rand index #####
adjustedRandIndex(km.out.1$cluster, voice$g)
#[1] 0.6830596
adjustedRandIndex(km.out.2$cluster, voice$g)
#[1] 0.6830596
adjustedRandIndex(km.out.3$cluster, voice$g)
#[1] 0.6830596
adjustedRandIndex(km.out.4$cluster, voice$g)
#[1] 0.6830596
### K-Means with different Ks ####
# K=1
set.seed(2016)
k1.out.1=kmeans(voice[,1:256], 1, nstart = 1)
k1.out.2=kmeans(voice[,1:256], 1, nstart = 5)
k1.out.3=kmeans(voice[,1:256], 1, nstart = 20)
k1.out.4=kmeans(voice[,1:256], 1, nstart = 50)
adjustedRandIndex(k1.out.1$cluster, voice$g)
#[1] 0
adjustedRandIndex(k1.out.2$cluster, voice$g)
#[1] 0
adjustedRandIndex(k1.out.3$cluster, voice$g)
#[1] 0
adjustedRandIndex(k1.out.4$cluster, voice$g)
#[1] 0
# K=2
set.seed(2016)
k2.out.1=kmeans(voice[,1:256], 2, nstart = 1)
k2.out.2=kmeans(voice[,1:256], 2, nstart = 5)
k2.out.3=kmeans(voice[,1:256], 2, nstart = 20)
k2.out.4=kmeans(voice[,1:256], 2, nstart = 50)
adjustedRandIndex(k2.out.1$cluster, voice$g)
#[1] 0.1638288
adjustedRandIndex(k2.out.2$cluster, voice$g)
#[1] 0.1638288
adjustedRandIndex(k2.out.3$cluster, voice$g)
#[1] 0.1638288
adjustedRandIndex(k2.out.4$cluster, voice$g)
#[1] 0.1638288

# K=3
set.seed(2016)
k3.out.1=kmeans(voice[,1:256], 3, nstart = 1)
k3.out.2=kmeans(voice[,1:256], 3, nstart = 5)
k3.out.3=kmeans(voice[,1:256], 3, nstart = 20)
k3.out.4=kmeans(voice[,1:256], 3, nstart = 50)
adjustedRandIndex(k3.out.1$cluster, voice$g)
#[1] 0.4038819
adjustedRandIndex(k3.out.2$cluster, voice$g)
#[1] 0.4038819
adjustedRandIndex(k3.out.3$cluster, voice$g)
#[1] 0.4038819
adjustedRandIndex(k3.out.4$cluster, voice$g)
#[1] 0.4038819
# K=4
set.seed(2016)
k4.out.1=kmeans(voice[,1:256], 4, nstart = 1)
k4.out.2=kmeans(voice[,1:256], 4, nstart = 5)
k4.out.3=kmeans(voice[,1:256], 4, nstart = 20)
k4.out.4=kmeans(voice[,1:256], 4, nstart = 50)
adjustedRandIndex(k4.out.1$cluster, voice$g)
#[1] 0.7471005
adjustedRandIndex(k4.out.2$cluster, voice$g)
#[1] 0.7471005
adjustedRandIndex(k4.out.3$cluster, voice$g)
#[1] 0.7471005
adjustedRandIndex(k4.out.4$cluster, voice$g)
#[1] 0.7471005
# K=6
set.seed(2016)
k6.out.1=kmeans(voice[,1:256], 6, nstart = 1)
k6.out.2=kmeans(voice[,1:256], 6, nstart = 5)
k6.out.3=kmeans(voice[,1:256], 6, nstart = 20)
k6.out.4=kmeans(voice[,1:256], 6, nstart = 50)
adjustedRandIndex(k6.out.1$cluster, voice$g)

#[1] 0.6426412
adjustedRandIndex(k6.out.2$cluster, voice$g)
#[1] 0.6411766
adjustedRandIndex(k6.out.3$cluster, voice$g)
#[1] 0.6406434
adjustedRandIndex(k6.out.4$cluster, voice$g)
#[1] 0.6411766
# Rand index changes with different number of clusters K
vK <- 1:10
M <- 1000
adjrand <- rep(NA, length(vK))
for (K in 1:10){
temp <- rep(NA, M)
for (i in 1:M){
out <- kmeans(phoneme[,1:nvar], centers = K, nstart = 1, iter.max = 20)
temp[i] <- adjustedRandIndex(label, out$cluster)
}
adjrand[K] <- mean(temp)
}
adjrand
plot(1:10,adjrand, ylim = c(0,0.7), xlab = 'Number of clusters', ylab = 'Adjusted Rand Index', type =
'o', col = 3, pch = 20, lwd = 1.5)
# Gaussian Mixture model
### lasso variable selection
fit.lm.lasso=cv.glmnet(X,y,family="multinomial",type.measure="mse")
plot(fit.lm.lasso)
Best_lasso_model=coef(fit.lm.lasso)
all_lasso_coef=cbind(Best_lasso_model$aa,Best_lasso_model$ao,Best_lasso_model$dcl,Best_lass
o_model$iy,Best_lasso_model$sh)
all_lasso_coef_1=rowSums((as.matrix(all_lasso_coef)[-1,]!=0))>=2
X_lasso_select=X[,all_lasso_coef_1]
dim(X_lasso_select)
### GMM fitting & ARI
for (i in (2:10)){
gmm_fit=Mclust(X_lasso_select,G=i)
gmm_fit_class=gmm_fit$classification
ard=adjustedRandIndex(gmm_fit_class,y)
print(ard)
}

#PCA
pr.out=prcomp(voice[,1:256],scale=T)
#proportion of total variance explained by each component
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
pve
cumsum(pve)
#plots
plot(pve,type='o',ylab="PVE",xlab="Principal Component",col='blue')
plot(cumsum(pve),type='o',ylab="Cumulative PVE",xlab="Principal Component",col='brown3')
#data
voice.pca=as.data.frame(pr.out$x[,1:2])
names(voice)=c("PC1","PC2")
colnames(voice.pca)=names(voice.pca)
###########################################################################
#K-MEANS
set.seed(2016)
par(mfrow=c(2,3))
#Kmeans clustering with K=2
km.out2=kmeans(voice.pca,2,nstart=20)
#cluster assignments
km.out2$cluster
#plot clusters
plot(voice.pca,col=(km.out2$cluster+1),main="K-means with K=2",xlab="",ylab="",pch=20,cex=2)
set.seed(2016)
#Kmeans clustering with K=3
km.out3=kmeans(voice.pca,3,nstart=20)
#cluster assignments
km.out3$cluster
#plot clusters
plot(voice.pca,col=(km.out3$cluster+1),main="K-means with K=3",xlab="",ylab="",pch=20,cex=2)

set.seed(2016)
#Kmeans clustering with K=4
km.out4=kmeans(voice.pca,4,nstart=20)
#cluster assignments
km.out4$cluster
#plot clusters
plot(voice.pca,col=(km.out4$cluster+1),main="K-means with K=4",xlab="",ylab="",pch=20,cex=2)
set.seed(2016)
#Kmeans clustering with K=5
km.out5=kmeans(voice.pca,5,nstart=20)
#cluster assignments
km.out5$cluster
#plot clusters
plot(voice.pca,col=(km.out5$cluster+1),main="K-means with K=5",xlab="",ylab="",pch=20,cex=2)
set.seed(2016)
#Kmeans clustering with K=6
km.out6=kmeans(voice.pca,6,nstart=20)
#cluster assignments
km.out6$cluster
#plot clusters
plot(voice.pca,col=(km.out6$cluster+1),main="K-means with K=6",xlab="",ylab="",pch=20,cex=2)
##################################################################
#GAUSSIAN MIXTURE
library(mclust)
set.seed(2016)
par(mfrow=c(2,3))
#Gaussian Mixture with K=2
gaussian2=Mclust(voice.pca,G=2)
#cluster assignements
gaussian2$classification

#plot clusters
plot(voice.pca,col=(gaussian2$classification+1),main="Gaussian Mixture with
K=2",xlab="",ylab="",pch=20,cex=2)
#Gaussian Mixture with K=3
gaussian3=Mclust(voice.pca,G=3)
#cluster assignements
gaussian3$classification
#plot clusters
plot(voice.pca,col=(gaussian3$classification+1),main="Gaussian Mixture with
K=3",xlab="",ylab="",pch=20,cex=2)
#Gaussian Mixture with K=4
gaussian4=Mclust(voice.pca,G=4)
#cluster assignements
gaussian4$classification
#plot clusters
plot(voice.pca,col=(gaussian4$classification+1),main="Gaussian Mixture with
K=4",xlab="",ylab="",pch=20,cex=2)
#Gaussian Mixture with K=5
gaussian5=Mclust(voice.pca,G=5)
#cluster assignements
gaussian5$classification
#plot clusters
plot(voice.pca,col=(gaussian5$classification+1),main="Gaussian Mixture with
K=5",xlab="",ylab="",pch=20,cex=2)
#Gaussian Mixture with K=6
gaussian6=Mclust(voice.pca,G=6)
#cluster assignements
gaussian6$classification
#plot clusters
plot(voice.pca,col=(gaussian6$classification+1),main="Gaussian Mixture with
K=6",xlab="",ylab="",pch=20,cex=2)

par(mfrow=c(1,1))
#True Clusters
plot(voice.pca,col=(as.numeric(voice$g)+1),main="True Clusters on the First 2 Principal
Components",xlab="",ylab="",pch=20,cex=2)
