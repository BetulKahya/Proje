library(readr)
library(OneR)
library(caTools)

#Veri Okuma
data <- read.csv(file.choose(),header = T)
dim(data)
data$death_event <- factor(data$death_event)
summary(data)

#Test Train Veri Bölümü
set.seed(123)
random <- sample(1:nrow(data),0.8*nrow(data))
data_train <- optbin(data[random,],method="infogain")
data_test <- data[-random,]

#One Rule
model_train <- OneR(data_train,verbose=TRUE)
summary(model_train)

plot(model_train)

#Prediction & Confusiom matrix 
prediction1 <- predict(model_train,data_train)
eval_model(prediction1,data_train)

prediction_binary <- as.numeric(prediction1)
patient_data_test_pred_binary <- data.frame(prediction1)
patient_data_test_pred_binary

library(caret)
confusionMatrix(table(prediction1,data_train$death_event))
tab <- table(prediction1,data_train$death_event)

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
