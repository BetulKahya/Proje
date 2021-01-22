#Veri Okuma
library(readr)
data <- read.csv(file.choose(),header = T)
str(data)
data$death_event <- factor(data$death_event)

#Test Train Veri Bölümü
set.seed(123)
pd <- sample(2,nrow(data),replace = TRUE,prob = c(0.8,0.2))
train <- data[pd==1,]
validate <- data[pd==2,]

#decision tree
library(party)
tree <- ctree(death_event~.,data=train,controls=ctree_control(mincriterion = 0.9,minsplit = 200))
tree
plot(tree)

#predict
predict(tree,validate,type='prob')



#Confusion matrix-validate data
testPred <- predict(tree,newdata=validate)
tab1 <- table(testPred,validate$death_event)
print(tab1)
1-sum(diag(tab1))/sum(tab1)
sum(diag(tab1))/sum(tab1) #acc


#İstatistiksel Oranlar
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
