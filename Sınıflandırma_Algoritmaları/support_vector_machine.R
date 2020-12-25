#Veri Okuma
library(e1071)
data <- read.csv(file.choose(),header = T)
str(data)
data$death_event <- factor(data$death_event)

#Test Train Veri Bölümü
set.seed(123)
s <- sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
training <- data[s==1,]
test <- data[s==2,]

#Support Vector Machine
library(caret)
trctrl <- trainControl(method="repeatedcv",number=10,repeats=3)
svm_Linear <- train(death_event~.,data=training,method="svmLinear",
                    trControl=trctrl,
                    tuneLength=10)
svm_Linear

test_pred <- predict(svm_Linear,newdata=test)
test_pred

#SVM Görselleþtirme
library(ggplot2)
qplot(serum_creatinine,ejection_fraction,data=data,
      color=death_event)

mymodel <- svm(death_event~.,data=data)
summary(mymodel)
plot(mymodel,data=data,
     ejection_fraction~serum_creatinine)

#Confusiom Matrix 
confusionMatrix(table(test_pred,test$death_event))
tab <- table(test_pred,test$death_event)


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
