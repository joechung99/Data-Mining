
#Split the data randomly to training data and test data ( 70% / 30% )
data <- read.csv("C:/Users/joe/Desktop/DM/data.csv", header = T, sep = ",")
data$default.payment.next.month=as.factor(credit$default.payment.next.month)
#(c)Convert categorical variables to factor
for (i in c(3:5,7:12)){
  data[,i]=as.factor(data[,i])
}
set.seed(1)
train_rows = sample.int(nrow(credit), nrow(credit) * 0.7)
train = credit[train_rows, ]
test = credit[-train_rows, ]
##logistic regression
model1<- glm(default.payment.next.month ~ .-ID,data=train,family = binomial)
predict <- predict(model1,test)

library(knitr)
library(ROCR)
model1_confusion=table(test$default.payment.next.month, predict>0.5)
kable(model1_confusion)
model1_accuracy=(model1_confusion[1]+model1_confusion[4])/nrow(test)
model1_accuracy
pre <- predict(model1,type='response')

pred <- prediction(pre,train$default.payment.next.month)
perf <- performance(pred,'tpr','fpr')
plot(perf,col=rainbow(10))
lines(c(0,1),c(0,1))


#KNN
library(class)
train.def <- factor(train$default.payment.next.month)
test.def <-factor(test$default.payment.next.month)
model2<-knn(train, test, train.def, k = 20, prob=TRUE)
model2_confusion<-table(model2,test.def)
model2_confusion
model2_accuracy=(model2_confusion[1]+model2_confusion[4])/nrow(test)
model2_accuracy
model2_recall=model2_confusion[1]/(model2_confusion[1]+model2_confusion[2])
model2_recall
model2_precision=model2_confusion[1]/(model2_confusion[1]+model2_confusion[3])
model2_precision
#Naive Bayes
library(e1071)
model3 <- naiveBayes(default.payment.next.month ~ .-ID, data = train)
predict <- predict(model3, test)
table(predict, test$default.payment.next.month)
model3_confusion<-table(predict, test$default.payment.next.month)
model3_accuracy=(model3_confusion[1]+model3_confusion[4])/nrow(test)
model3_accuracy
#Random forest
library(randomForest)
model4 <- randomForest(default.payment.next.month ~ .-ID, data = train)
predict <- predict(model4, test)
table(predict, test$default.payment.next.month)
model4_confusion<-table(predict, test$default.payment.next.month)
model4_accuracy=(model4_confusion[1]+model4_confusion[4])/nrow(test)
model4_accuracy
#SVM
library(e1071)
model5 <- svm(default.payment.next.month ~ .-ID, data = train)
predict2<-predict(model5, test)
table(predict2,test$default.payment.next.month)
model5_confusion<-table(predict2, test$default.payment.next.month)
model5_accuracy=(model5_confusion[1]+model5_confusion[4])/nrow(test)
model5_accuracy
library(e1071)
tune_model<-svm(default.payment.next.month ~ .-ID, data = train,kernel="sigmoid")
predict3<-predict(tune_model, test)
table(predict3,test$default.payment.next.month)
tune_model_confusion<-table(predict3, test$default.payment.next.month)
tune_model_accuracy=(tune_model_confusion[1]+tune_model_confusion[4])/nrow(test)
tune_model_accuracy

#Kmeans
set.seed(101)
mydata <- data
changedata<-mydata[,3:5]
wss <- (nrow(changedata)-1)*sum(apply(changedata,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(changedata,centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",pch=20, cex=2)
