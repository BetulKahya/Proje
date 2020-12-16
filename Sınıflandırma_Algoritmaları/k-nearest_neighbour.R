library(caret)
library(pROC)
library(mlbench)

#Veri Okuma
data <- read.csv(file.choose(),header = T)
str(data)
data$death_event <- factor(data$death_event)

#Test Train Veri Bölümü
set.seed(123)
ind <- sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
training <- data[ind==1,]
test <- data[ind==2,]

#knn Model
trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)
set.seed(222)
fit <- train(death_event~.,
             data = training,
             method='knn',
             tuneLength=20,
             trControl=trControl)

#Confusion Matrix & model performance
plot(fit)
varImp(fit)
pred <- predict(fit,newdata=test)
confusionMatrix(pred,test$death_event)


tab <- table(pred,test$death_event)

#Ýstatistiksel Oranlar
P <- sum(tab[,2])
N <- sum(tab[,1])
FP <- tab[2,1]
FN <- tab[1,2]
TN <- tab[1,1]
TP <- tab[2,2]
f1_score <- 2*TP/(2*TP+FP+FN)
TP_Rate <- TP/P
TN_Rate <- TN/N

mcc <- ((TP*TN)-(FP*FN))/
  sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
mcc

accuracy <- (TP+TN)/(TP+FN+TN+FP)

