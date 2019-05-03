
image1=read.csv(file='image1.txt', sep="", header=FALSE)
image2=read.csv(file='image2.txt', sep="", header=FALSE)
image3=read.csv(file='image3.txt', sep="", header=FALSE)
library(ggplot2)
library(dplyr)

##EDA
prop.table(table(image1$V3))
prop.table(table(image2$V3))
prop.table(table(image3$V3))
##Plotting the original image
ggplot(data=image1, aes(x=V1,y=V2,color=V3))+geom_point()+xlab('x axis')+ylab('y axis')
ggplot(data=image2, aes(x=V1,y=V2,color=V3))+geom_point()+xlab('x axis')+ylab('y axis')
ggplot(data=image3, aes(x=V1,y=V2,color=V3))+geom_point()+xlab('x axis')+ylab('y axis')
##plotting the correlation plot of all variables
library(corrplot)
corrplot(cor(image1), method = 'circle')
corrplot(cor(image2), method = 'circle')
corrplot(cor(image3), method = 'circle')

image11=subset.data.frame(image1, image1$label!=0)
image22=subset.data.frame(image2,image2$label!=0)
image33=subset.data.frame(image3, image3$label!=0)
colnames(image11)=c('x','y','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
colnames(image22)=c('x','y','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
colnames(image33)=c('x','y','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
##boxplot images
image11=(image11[c(3:6)])
image22=(image22[c(3:6)])
image33=(image33[c(3:6)])
image_all=rbind(image11, image22, image33)
image_all_1=image_all[image_all$label==1, ]
boxplot(image_all_1$NDAI, main = 'NDAI when label=1', na.rm=TRUE)
boxplot(image_all_1$SD,main = 'SD when label=1', na.rm=TRUE)
boxplot(image_all_1$CORR, main = 'CORR when label=1', na.rm=TRUE)
image_all_2=image_all[image_all$label==-1, ]
boxplot(image_all_2$NDAI,  main = 'NDAI when label=-1', na.rm=TRUE)
boxplot(image_all_2$SD,  main = 'SD when label=1', na.rm=TRUE)
boxplot(image_all_2$CORR,  main = 'CORR when label=-1', na.rm=TRUE)
summary(image_all_1)
summary(image_all_2)

##subsetting images to modulos of 3
image1_1=image1[image1$V1 %% 3==0 & image1$V2 %% 3 ==0, ]
image1_2=image1[image1$V1 %% 3==1 & image1$V2 %% 3 ==0, ]
image1_3=image1[image1$V1 %% 3==2 & image1$V2 %% 3 ==0, ]
image1_4=image1[image1$V1 %% 3==0 & image1$V2 %% 3 ==1, ]
image1_5=image1[image1$V1 %% 3==0 & image1$V2 %% 3 ==2, ]
image1_6=image1[image1$V1 %% 3==1 & image1$V2 %% 3 ==1, ]
image1_7=image1[image1$V1 %% 3==1 & image1$V2 %% 3 ==2, ]
image1_8=image1[image1$V1 %% 3==2 & image1$V2 %% 3 ==1, ]
image1_9=image1[image1$V1 %% 3==2 & image1$V2 %% 3 ==2, ]

image2_1=image2[image2$V1 %% 3==0 & image2$V2 %% 3 ==0, ]
image2_2=image2[image2$V1 %% 3==1 & image2$V2 %% 3 ==0, ]
image2_3=image2[image2$V1 %% 3==2 & image2$V2 %% 3 ==0, ]
image2_4=image2[image2$V1 %% 3==0 & image2$V2 %% 3 ==1, ]
image2_5=image2[image2$V1 %% 3==0 & image2$V2 %% 3 ==2, ]
image2_6=image2[image2$V1 %% 3==1 & image2$V2 %% 3 ==1, ]
image2_7=image2[image2$V1 %% 3==1 & image2$V2 %% 3 ==2, ]
image2_8=image2[image2$V1 %% 3==2 & image2$V2 %% 3 ==1, ]
image2_9=image2[image2$V1 %% 3==2 & image2$V2 %% 3 ==2, ]

image3_1=image3[image3$V1 %% 3==0 & image3$V2 %% 3 ==0, ]
image3_2=image3[image3$V1 %% 3==1 & image3$V2 %% 3 ==0, ]
image3_3=image3[image3$V1 %% 3==2 & image3$V2 %% 3 ==0, ]
image3_4=image3[image3$V1 %% 3==0 & image3$V2 %% 3 ==1, ]
image3_5=image3[image3$V1 %% 3==0 & image3$V2 %% 3 ==2, ]
image3_6=image3[image3$V1 %% 3==1 & image3$V2 %% 3 ==1, ]
image3_7=image3[image3$V1 %% 3==1 & image3$V2 %% 3 ==2, ]
image3_8=image3[image3$V1 %% 3==2 & image3$V2 %% 3 ==1, ]
image3_9=image3[image3$V1 %% 3==2 & image3$V2 %% 3 ==2, ]

image_all=list(image1_1,image1_2, image1_3, image1_4, image1_5, image1_6, image1_7, image1_8, image1_9,image2_1,image2_2, image2_3, image2_4, image2_5, image2_6, image2_7, image2_8, image2_9,image3_1,image3_2, image3_3, image3_4, image3_5, image3_6, image3_7, image3_8, image3_9 )

image1_1_new=data.frame(matrix(ncol=0,nrow=0))
image1_2_new=data.frame(matrix(ncol=0,nrow=0))
image1_3_new=data.frame(matrix(ncol=0,nrow=0))
image1_4_new=data.frame(matrix(ncol=0,nrow=0))
image1_5_new=data.frame(matrix(ncol=0,nrow=0))
image1_6_new=data.frame(matrix(ncol=0,nrow=0))
image1_7_new=data.frame(matrix(ncol=0,nrow=0))
image1_8_new=data.frame(matrix(ncol=0,nrow=0))
image1_9_new=data.frame(matrix(ncol=0,nrow=0))
image2_1_new=data.frame(matrix(ncol=0,nrow=0))
image2_2_new=data.frame(matrix(ncol=0,nrow=0))
image2_3_new=data.frame(matrix(ncol=0,nrow=0))
image2_4_new=data.frame(matrix(ncol=0,nrow=0))
image2_5_new=data.frame(matrix(ncol=0,nrow=0))
image2_6_new=data.frame(matrix(ncol=0,nrow=0))
image2_7_new=data.frame(matrix(ncol=0,nrow=0))
image2_8_new=data.frame(matrix(ncol=0,nrow=0))
image2_9_new=data.frame(matrix(ncol=0,nrow=0))
image3_1_new=data.frame(matrix(ncol=0,nrow=0))
image3_2_new=data.frame(matrix(ncol=0,nrow=0))
image3_3_new=data.frame(matrix(ncol=0,nrow=0))
image3_4_new=data.frame(matrix(ncol=0,nrow=0))
image3_5_new=data.frame(matrix(ncol=0,nrow=0))
image3_6_new=data.frame(matrix(ncol=0,nrow=0))
image3_7_new=data.frame(matrix(ncol=0,nrow=0))
image3_8_new=data.frame(matrix(ncol=0,nrow=0))
image3_9_new=data.frame(matrix(ncol=0,nrow=0))

new_image=list(image1_1_new,image1_2_new, image1_3_new, image1_4_new, image1_5_new, image1_6_new, image1_7_new, image1_8_new, image1_9_new,image2_1_new,image2_2_new, image2_3_new, image2_4_new, image2_5_new, image2_6_new, image2_7_new, image2_8_new, image2_9_new,image3_1_new,image3_2_new, image3_3_new, image3_4_new, image3_5_new, image3_6_new, image3_7_new, image3_8_new, image3_9_new)

##subset images to 3 small images by modulos
t=rep(0,27)
for(m in 1:27){
  t[m]=(nrow(image_all[[m]]))%/%27
}
##taking sections from original image without replacement
for(i in 1:length(new_image)){
  for(k in 1:length(image_all)){
    samp=sample_n(as.data.frame(image_all[[k]]), t[k], replace = FALSE)
    image_all[k]=setdiff(image_all[k],samp)
    new_image[[i]]=rbind(new_image[[i]], samp)
  }
}
##randomly select 9 of them to compose training, testing and validation
image1_new=data.frame(matrix(ncol=0,nrow=0))
image2_new=data.frame(matrix(ncol=0,nrow=0))
image3_new=data.frame(matrix(ncol=0,nrow=0))
t=c(1:27)
f1=sample(t, 27, replace = F)
for (m in 1:9){
  image1_new=rbind(image1_new, new_image[[f1[m]]])
}
for (m in 10:18){
  image2_new=rbind(image2_new, new_image[[f1[m]]])
}
for (m in 19:27){
  image3_new=rbind(image3_new, new_image[[f1[m]]])
}

train=image1_new
test=image2_new
valid=image3_new
colnames(train)=c('x','y','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
colnames(test)=c('x','y','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
colnames(valid)=c('x','y','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')

library(corrplot)
colnames(image1)=c('x','y','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
colnames(image2)=c('x','y','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
colnames(image3)=c('x','y','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
corrplot(cor(image1), method = 'number')
corrplot(cor(image2), method = 'number')
corrplot(cor(image3), method = 'number')
library(caret)
##remove label=0
train=subset.data.frame(train, train$label!=0)
test=subset.data.frame(test,test$label!=0)
valid=subset.data.frame(valid, valid$label!=0)

train11=train
test_11=test 
valid_11=valid

train=train[c(3,4,5,6)]
test=test[c(3,4,5,6)]
valid=valid[c(3,4,5,6)]

train=rbind(train, valid)

##super pixel: 9 observations are merged to one by taking average
spl1=split(image1, rep(1:12803, each=9))
index=c()
for(m in 1:12803){
  if(length(unique(spl1[[m]]$label))>1){
    index=c(index, m)
  }
}
spl1=spl1[-index]

super_1=data.frame()
for(g in 1:length(spl1)){
  mean_x=mean(spl1[[g]]$x)
  mean_y=mean(spl1[[g]]$y)
  mean_label=mean(spl1[[g]]$label)
  mean_NDAI=mean(spl1[[g]]$NDAI)
  mean_SD=mean(spl1[[g]]$SD)
  mean_CORR=mean(spl1[[g]]$CORR)
  mean_DF=mean(spl1[[g]]$DF)
  mean_CF=mean(spl1[[g]]$CF)
  mean_BF=mean(spl1[[g]]$BF)
  mean_AF=mean(spl1[[g]]$AF)
  mean_AN=mean(spl1[[g]]$AN)
  s=list(mean_x,mean_y,mean_label,mean_NDAI,mean_SD,mean_CORR,mean_DF,mean_CF,mean_BF,mean_AF, mean_AN)
  super_1=rbind(super_1,s)
}

spl2=split(image2, rep(1:12803, each=9))
index=c()
for(m in 1:12803){
  if(length(unique(spl2[[m]]$label))>1){
    index=c(index, m)
  }
}
spl2=spl2[-index]

super_2=data.frame()
for(g in 1:length(spl2)){
  mean_x=mean(spl2[[g]]$x)
  mean_y=mean(spl2[[g]]$y)
  mean_label=mean(spl2[[g]]$label)
  mean_NDAI=mean(spl2[[g]]$NDAI)
  mean_SD=mean(spl2[[g]]$SD)
  mean_CORR=mean(spl2[[g]]$CORR)
  mean_DF=mean(spl2[[g]]$DF)
  mean_CF=mean(spl2[[g]]$CF)
  mean_BF=mean(spl2[[g]]$BF)
  mean_AF=mean(spl2[[g]]$AF)
  mean_AN=mean(spl2[[g]]$AN)
  s=list(mean_x,mean_y,mean_label,mean_NDAI,mean_SD,mean_CORR,mean_DF,mean_CF,mean_BF,mean_AF, mean_AN)
  super_2=rbind(super_2,s)
}
super_22=super_2

spl3=split(image3, rep(1:12803, each=9))
index=c()
for(m in 1:12803){
  if(length(unique(spl3[[m]]$label))>1){
    index=c(index, m)
  }
}
spl3=spl3[-index]

super_3=data.frame()
for(g in 1:length(spl3)){
  mean_x=mean(spl3[[g]]$x)
  mean_y=mean(spl3[[g]]$y)
  mean_label=mean(spl3[[g]]$label)
  mean_NDAI=mean(spl3[[g]]$NDAI)
  mean_SD=mean(spl3[[g]]$SD)
  mean_CORR=mean(spl3[[g]]$CORR)
  mean_DF=mean(spl3[[g]]$DF)
  mean_CF=mean(spl3[[g]]$CF)
  mean_BF=mean(spl3[[g]]$BF)
  mean_AF=mean(spl3[[g]]$AF)
  mean_AN=mean(spl3[[g]]$AN)
  s=list(mean_x,mean_y,mean_label,mean_NDAI,mean_SD,mean_CORR,mean_DF,mean_CF,mean_BF,mean_AF, mean_AN)
  super_3=rbind(super_3,s)
}

colnames(super_1)=c('x','y','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
colnames(super_2)=c('x','y','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
colnames(super_3)=c('x','y','label','NDAI','SD','CORR','DF','CF','BF','AF','AN')
super_11=super_1
super_22=super_2
super_33=super_3
super_1=super_1[c(3:6)]
super_2=super_2[c(3:6)]
super_3=super_3[c(3:6)]

train_super=super_1
test_super=super_2
valid_super=super_3
train_super=subset.data.frame(train_super, train_super$label!=0)
test_super=subset.data.frame(test_super,test_super$label!=0)
valid_super=subset.data.frame(valid_super, valid_super$label!=0)
train_super=rbind(train_super, valid_super)
##finding accuracy of trivial classifier
valid_tri=valid
valid_tri$label[valid_tri$label!=-1]=-1
test_tri=test
test_tri$label[test_tri$label!=-1]=-1
train_tri=rbind(train, valid_tri)

library(caret)
confusionMatrix(as.factor(test_tri$label), as.factor(train$label[1:69100]))

test_super_tri=test_super
valid_super_tri=valid_super
train_super_tri=train_super
test_super_tri$label[test_super_tri$label!=-1]=-1
valid_super_tri$label[valid_super_tri$label!=-1]=-1
train_super_tri=rbind(train_super_tri, valid_super_tri)

confusionMatrix(as.factor(test_super_tri$label), as.factor(train_super_tri$label[1:8110]))

confusionMatrix(as.factor(test_tri$label[c(1:69191)]), as.factor(train$label))

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
##logistic regression CV
GLM=CVgeneric(multinom.new, train[c(2:4)], train[["label"]], 10, zero_one)
##LDA
lda_cv=CVgeneric(lda.new, train[c(2:4)], train[["label"]], 10, zero_one)

qqnorm(train$SD)
qqnorm(train$CORR)
qqnorm(train$NDAI)
##QDA
qda_cv=CVgeneric(qda.new, train[c(2:4)], train[["label"]], 10, zero_one)
qda_cv
##Decision tree
rpart_cv=CVgeneric(rpart.new, train[c(2:4)], train[["label"]], 10, zero_one)
rpart_cv
#find ROC for all four methods
library(pROC)
qda.predict=qda(data=train, label~.)
qda_predict=predict(qda.predict, newdata=test, probability=TRUE)
rocobj=roc(response=test$label, predictor = qda_predict$posterior[,"1"])
plot(rocobj)

library(MASS)
lda.predict=lda(data=train, label~.)
lda_predict=predict(lda.predict, newdata=test, probability=TRUE)
rocobj=roc(response=test$label, predictor = lda_predict$posterior[,"1"])
plot(rocobj)

new_train=train
new_test=test
new_train$label[new_train$label==-1] = 0
glm.predict=glm(data=new_train, label~., family = 'binomial')
glm_predict=predict(glm.predict, newdata=new_test, type=c('response'))
new_test$predict=glm_predict
library(pROC)
g=roc(label~predict, data=new_test)
plot(g)

plot.dat=data.frame(logodds=predict(glm.predict, newdata = new_test), SD=new_test$SD, CORR=new_test$CORR, NDAI=new_test$NDAI)
ggplot(plot.dat)+geom_point(aes(x=SD, y=logodds))
ggplot(plot.dat)+geom_point(aes(x=CORR, y=logodds))
ggplot(plot.dat)+geom_point(aes(x=NDAI, y=logodds))

tree_train=train
tree_test=test
model=rpart::rpart(formula=label~.,data=tree_train,method="class" )
tree_predict=predict(model, newdata = tree_test, method='class')
tree_train=train[c(1:69100),]
tree_train$predict=tree_predict[,2]
library(pROC)
f=roc(label~predict, data=tree_train)
plot(f)

GLM_super=CVgeneric(multinom.new, train_super[c(2:4)], train_super[["label"]], 10, zero_one)

lda_cv_sup=CVgeneric(lda.new, train_super[c(2:4)], train_super[["label"]], 10, zero_one)
lda_cv_sup

qda__sup_cv=CVgeneric(qda.new, train_super[c(2:4)], train_super[["label"]], 10, zero_one)
qda__sup_cv



library(pROC)
qda.predict_sup=qda(data=train_super, label~.)
qda_predict_sup=predict(qda.predict_sup, newdata=test_super, probability=TRUE)
rocobj=roc(response=test_super$label, predictor = qda_predict_sup$posterior[,"1"])
plot(rocobj)
coords(rocobj, "best")
rocobj$auc

library(MASS)
lda.predict=lda(data=train_super, label~.)
lda_predict_sup=predict(lda.predict, newdata=test_super, probability=TRUE)
rocobj=roc(response=test_super$label, predictor = lda_predict_sup$posterior[,"1"])
plot(rocobj)
coords(rocobj, "best")
rocobj$auc

new_train=train_super
new_test=test_super
new_train$label[new_train$label==-1] = 0
glm.predict=glm(data=new_train, label~., family = 'binomial')
glm_predict_sup=predict(glm.predict, newdata=new_test, type=c('response'))
new_test$predict=glm_predict_sup
library(pROC)
g=roc(label~predict, data=new_test)
plot(g)
coords(g, "best")
g$auc



tree_train_sup=train_super
tree_test_sup=test_super
model=rpart::rpart(formula=label~.,data=tree_train_sup,method="class" )
tree_predict_sup=predict(model, newdata = tree_test_sup, method='class')
tree_train_sup=train_super[c(1:8110),]

tree_train_sup$predict=tree_predict_sup[,2]
library(pROC)
f=roc(label~predict, data=tree_train_sup)
plot(f)
coords(f, "best")
f$auc

rpart_cv_sup=CVgeneric(rpart.new, train_super[c(2:4)], train_super[["label"]], 10, zero_one)
rpart_cv_sup

##Comparing accuracy of CV for two methods
glm_plot_mod=GLM$X0.887523492843718[c(1:10)]
glm_plot_sup=GLM_super$X0.955137481910275[c(1:10)]
b2=data.frame(glm_plot_mod, glm_plot_sup)
ggplot(data=b2)+geom_line(aes(x=c(1:10), y=b2$glm_plot_mod, colour='red'))+geom_line(aes(x=c(1:10), y=b2$glm_plot_sup, colour='blue'))

lda_plot_mod=lda_cv$X0.901300578034682[c(1:10)]
lda_plot_sup=lda_cv_sup$X0.950795947901592[c(1:10)]
b=data.frame(lda_plot_mod, lda_plot_sup)
ggplot(data=b)+geom_line(aes(x=c(1:10), y=b$lda_plot_mod, colour='red'))+geom_line(aes(x=c(1:10), y=b$lda_plot_sup, colour='blue'))

qda_plot_mod=qda_cv$X0.892470010117069[c(1:10)]
qda_plot_sup=qda__sup_cv$X0.959361393323657[c(1:10)]
b1=data.frame(qda_plot_mod, qda_plot_sup)
ggplot(data=b1)+geom_line(aes(x=c(1:10), y=b1$qda_plot_mod, colour='red'))+geom_line(aes(x=c(1:10), y=b1$qda_plot_sup, colour='blue'))

tree_plot_mod=rpart_cv$X0.900433526011561[c(1:10)]
tree_plot_sup=rpart_cv_sup$X0.952173913043478[c(1:10)]
b3=data.frame(tree_plot_mod, tree_plot_sup)
ggplot(data=b3)+geom_line(aes(x=c(1:10), y=b3$tree_plot_mod, colour='red'))+geom_line(aes(x=c(1:10), y=b3$tree_plot_sup, colour='blue'))

```{r}
tree_train_sup=train_super
tree_test_sup=test_super
model=rpart::rpart(formula=label~.,data=tree_train_sup,method="class" )
tree_predict_sup=predict(model, newdata = tree_test_sup, method='class')
tree_train_sup=train_super[c(1:8110),]

tree_train_sup$predict=tree_predict_sup[,2]
library(pROC)
f=roc(label~predict, data=tree_train_sup)
plot(f)
coords(f, "best")
f$auc

##Finding Confusion matrix for four methods
a=tree_predict_sup
for(k in 1:length(a[,1])){
  if(a[,1][k]>0.5){
    a[,1][k]=1
  }
  else{a[,1][k]=-1}
}
for(k in 1:length(a[,2])){
  if(a[,2][k]>0.5){
    a[,2][k]=1
  }
  else{a[,2][k]=-1}
}

table(a, train$label[c(1:16220)])

cfm_qda_sup=confusionMatrix(qda_predict_sup$class, as.factor(test_super$label))
cfm_lda_sup=confusionMatrix(lda_predict_sup$class, as.factor(test_super$label))
l_sup=test_super$label
for (f in 1:length(test_super$label)){
  if (l_sup[f]==-1){
    l_sup[f]=0
  }
}
cfm_glm_sup=confusionMatrix(as.factor(as.numeric(glm_predict_sup>0.5)), as.factor(l_sup))

##Plotting convergence graph for superpixel
library(pROC)
k=seq(1000, 6500, 10)
m=rep(0, length(k))
f=rep(0,length(k))
for(a in 1:length(k)){
  qda.predict_sup=qda(data=train_super[1:k[a],], label~.)
  qda_predict_sup=predict(qda.predict_sup, newdata=test_super[1:k[a],], probability=TRUE)
  rocobj=roc(response=test_super$label[1:k[a]], predictor = qda_predict_sup$posterior[,"1"])
  m[a]=rocobj$auc
  f[a]=coords(rocobj, 'best')[3]
}
t=data.frame(k, m)
ggplot(data=t)+geom_line(aes(x=k, y=1-m))+labs(x="train size")+labs(y='error')
t=data.frame(k, f)
ggplot(data=t)+geom_line(aes(x=k, y=f))+labs(x="train size")+labs(y='cutoff')

##Comparing qda accuracy to the testing data

v=qda_predict_sup$class
v1=as.numeric(as.character(v))
v_t=prop.table(table(v))
m_t=prop.table(table(test_super$label))
m_v=table(v_t, m_t)

##plotting predicted graph and compare to the original graph to find their differences
f=super_22
ggplot(data=f, aes(x=x, y=y, colour=label))+geom_point()
h=super_22
k=1
t=1
u=na.omit(h$label)
while (k<=length(u)){
  if(u[k]!=0)
  {
    h$label[k]=v1[t]
    k=k+1
    t=t+1
  }
  else{k=k+1}
}
ggplot(data=h, aes(x=x, y=y, colour=label))+geom_point()
f$label=f$label-h$label
ggplot(data=subset.data.frame(f, f$label!=0), aes(x=x, y=y, colour=label))+geom_point()
l=subset.data.frame(f, f$label!=0)
summary(l[4:6])
summary(super_22[4:6])

##Plotting histograms of misclassified data

ggplot(data=l, aes(x=NDAI))+geom_histogram(data=subset(l, label==2), fill='red', alpha=0.2)+geom_histogram(data=subset(l, label==-2), fill='blue', alpha=0.2)+scale_color_manual("", breaks=c(2, -2), values = c('red', 'blue'))
ggplot(data=l, aes(x=CORR))+geom_histogram(data=subset(l, label==2), fill='red', alpha=0.2)+geom_histogram(data=subset(l, label==-2), fill='blue', alpha=0.2)+scale_color_manual("", breaks=c(2, -2), values = c('red', 'blue'))
ggplot(data=l, aes(x=SD))+geom_histogram(data=subset(l, label==2), fill='red', alpha=0.2)+geom_histogram(data=subset(l, label==-2), fill='blue', alpha=0.2)+scale_color_manual("", breaks=c(2, -2), values = c('red', 'blue'))
ggplot(data=l, aes(x=DF))+geom_histogram(data=subset(l, label==2), fill='red', alpha=0.2)+geom_histogram(data=subset(l, label==-2), fill='blue', alpha=0.2)+scale_color_manual("", breaks=c(2, -2), values = c('red', 'blue'))
ggplot(data=l, aes(x=CF))+geom_histogram(data=subset(l, label==2), fill='red', alpha=0.2)+geom_histogram(data=subset(l, label==-2), fill='blue', alpha=0.2)+scale_color_manual("", breaks=c(2, -2), values = c('red', 'blue'))
ggplot(data=l, aes(x=BF))+geom_histogram(data=subset(l, label==2), fill='red', alpha=0.2)+geom_histogram(data=subset(l, label==-2), fill='blue', alpha=0.2)+scale_color_manual("", breaks=c(2, -2), values = c('red', 'blue'))
ggplot(data=l, aes(x=AF))+geom_histogram(data=subset(l, label==2), fill='red', alpha=0.2)+geom_histogram(data=subset(l, label==-2), fill='blue', alpha=0.2)+scale_color_manual("", breaks=c(2, -2), values = c('red', 'blue'))
ggplot(data=l, aes(x=AN))+geom_histogram(data=subset(l, label==2), fill='red', alpha=0.2)+geom_histogram(data=subset(l, label==-2), fill='blue', alpha=0.2)+scale_color_manual("", breaks=c(2, -2), values = c('red', 'blue'))

##Improving features
train_super_mod=train_super
train_super_mod$NDAI=log(abs(train_super_mod$NDAI+0.6))
qda_sup_cv_1=CVgeneric(qda.new, train_super_mod[c(2:4)], train_super[["label"]], 10, zero_one)
qda_sup_cv_1

##Finding accuraacy of modulo and the testing data
v=qda_predict$class
v1=as.numeric(as.character(v))
v_t=prop.table(table(v))
m_t=prop.table(table(test$label))

##ploting convergence graph

library(pROC)
library(MASS)
k=seq(10000, 60000, 1000)
m=rep(0, length(k))
f=rep(0,length(k))
for(a in 1:length(k)){
  qda.predict=qda(data=train[1:k[a],], label~.)
  qda_predict=predict(qda.predict, newdata=test[1:k[a],], probability=TRUE)
  rocobj=roc(response=test$label[1:k[a]], predictor = qda_predict$posterior[,"1"])
  m[a]=rocobj$auc
  f[a]=coords(rocobj, 'best')[3]
}

t=data.frame(k, m)
ggplot(data=t)+geom_line(aes(x=k, y=1-m))+labs(x="train size")+labs(y='error')



