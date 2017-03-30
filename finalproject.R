
set.seed(777)
train <- read.csv("C:/Users/joe/Downloads/dodoData-adjust variables(20161219)/Predict parking space/(train) 穝狶カ.csv", header = T, sep = ",")
test <- read.csv("C:/Users/joe/Downloads/dodoData-adjust variables(20161219)/Predict parking space/(test) 穝狶カ.csv", header = T, sep = ",")


#Random forest
library(randomForest)
model4 <- randomForest(cars ~ .-Date.Time -X, data = train, ntree=500)
predict <- predict(model4, test)
table1 <- cbind(test[c("Date.Time")],predict,test[c("cars")])
write.table(table1, file = "C:/Users/joe/desktop/DM/DM1222/穝狶カ500.CSV",  sep = ",")
library(forecast)
accuracy(test$cars,predict)
accuracy(train$cars,predict)



train <- read.csv("C:/Users/joe/Downloads/dodoData-adjust variables(20161219)/Predict waiting time/(train) ず打风 _waiting_time.csv", header = T, sep = ",")
test <- read.csv("C:/Users/joe/Downloads/dodoData-adjust variables(20161219)/Predict waiting time/(test) ず打风 _waiting_time.csv", header = T, sep = ",")
library(randomForest)
model5 <- randomForest(waiting_time ~ .-Date.Time -X, data = train, ntree=500)
predict <- predict(model5, test)
table5 <- cbind(test[c("Date.Time")],predict,test[c("cars")])
write.table(table5, file = "ず打风space500.CSV",  sep = ",")
library(forecast)
accuracy(test$waiting_time,predict)
accuracy(train$waiting_time,predict)
