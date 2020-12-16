#Veri Okuma
data <- read.csv(file.choose(),header = T)
str(data)
data$sex <- factor(data$sex)
data$smoking <- factor(data$smoking)
data$diabetes <- factor(data$diabetes)
data$high_blood_pressure <- factor(data$high_blood_pressure)
data$anaemia <- factor(data$anaemia)
data$death_event <- factor(data$death_event)
str(data)

#Test Train Bölümü
set.seed(123)
ind <- sample(2,nrow(data),replace = T,prob = c(0.8,0.2))
train <- data[ind==1,]
test <- data[ind==2,]

#logistic regresion model
mymodel <- glm(death_event~.,data=train,family = 'binomial')
summary(mymodel)

#prediction
pred <- predict(mymodel,train,type = 'response')

#confusion matrix- train
pred1 <- ifelse(pred>0.5,1,0)
tab <- table(Predicted=pred1,Actual=train$death_event)
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

