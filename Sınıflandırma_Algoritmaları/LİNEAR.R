#Veri Okuma
library(readr)
data <- read.csv(file.choose(),header = T)
head(data)


#multiple linear regresion
results <- lm(death_event~.,data)
results
summary(results)



#Test Train Veri Bölümü
set.seed(123)
ind <- sample(2,nrow(data),replace = TRUE,prob = c(0.8,0.2))

training <- data[ind==1,]
testing <- data[ind==2,]

model <- lm(death_event~.,data=testing)
model
summary(model)
pred <- predict(model,training)

#confusion matrix- train
pred1 <- ifelse(pred>0.5,1,0)
tab <- table(Predicted=pred1,Actual=training$death_event)
tab

#İstatistiksel Oranlar
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
