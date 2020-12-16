library(readr)
library(randomForest)
library(caTools)

#Veri Okuma
data <- read.csv(file.choose(),header = T)
data$death_event <- factor(data$death_event)
summary(data)

#Test Train Veri Bölümü
sample= sample.split(data$death_event,SplitRatio=0.8)
train = subset(data,sample==TRUE)
test = subset(data,sample==FALSE)

#Random Forest & Confusion Matrix
rf <- randomForest(death_event~.,data=train)
rf
pred=predict(rf,newdata = test[-12])
confm= table(test[,12],pred)
confm
importance(rf)
varImpPlot(rf)

library(nnet)
mymodel <- multinom(death_event~.,data=data)
p <- predict(mymodel,data)
tab <- table(p,data$death_event)
tab


#Prediction & ROC
library(ROCR)
pred <- predict(mymodel,data,type='prob')
pred <- prediction(pred,data$death_event)
eval <- performance(pred,"acc")
plot(eval)
roc <- performance(pred,"tpr","fpr")
plot(roc)
abline(a=0,b=1)
auc <- performance(pred,"auc")
ROC_auc <- unlist(slot(auc,"y.values"))
ROC_auc <- round(ROC_auc,4)
ROC_auc

#İstatistiksel Oranlar
library(MLmetrics)
library(dplyr)
set.seed(123)
mod_log <- glm(death_event~.,family = binomial(link = "logit"),data=data)
model <- glm(formula(train),train,family = "binomial")
AUC(mod_log$fitted.values,data$death_event)

Precision(data$death_event,ifelse(mod_log$fitted.values>=.5,1,0),positive = 0)
Recall(data$death_event,ifelse(mod_log$fitted.values>=.5,1,0),positive = 0)
Accuracy(ifelse(mod_log$fitted.values>=.34,1,0),data$death_event)

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





