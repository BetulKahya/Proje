library(readr)
library(dplyr)
library(naivebayes)
library(ggplot2)
library(psych)

#Veri Okuma
data <- read.csv(file.choose(),header = T)
str(data)
data$death_event <- as.factor(data$death_event)
str(data)

#Görselleþtirme
pairs.panels(data[7:12])
data %>%
  ggplot(aes(x=ejection_fraction,y=serum_creatinine,fill=death_event))+
  geom_boxplot()+
  ggtitle("Box Plot")
data %>%
  ggplot(aes(x=serum_creatinine,fill=death_event))+
  geom_density(alpha=0.8,color='black')+
  ggtitle("density plot")

#Test Train Veri Bölümü
set.seed(123)
ind <- sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
train <- data[ind==1,]
test <- data[ind==2,]

#Naive Bayes Model
model <- naive_bayes(death_event~.,data = train)
model


#Confusion matrix-test data
p1 <- predict(model,test)
tab1 <- table(p1,test$death_event)


#Ýstatistiksel Oranlar
P <- sum(tab1[,2])
N <- sum(tab1[,1])
FP <- tab1[2,1]
FN <- tab1[1,2]
TN <- tab1[1,1]
TP <- tab1[2,2]
f1_score <- 2*TP/(2*TP+FP+FN)
TP_Rate <- TP/P
TN_Rate <- TN/N

mcc <- ((TP*TN)-(FP*FN))/
  sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
mcc


accuracy <- (TP+TN)/(TP+FN+TN+FP)


