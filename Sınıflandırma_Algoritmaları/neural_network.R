#Veri Okuma
library(readr)
data <- read.csv(file.choose(),header = T)
str(data)


#Test Train Veri Bölümü
set.seed(123)
ind <- sample(2,nrow(data),replace = TRUE,prob = c(0.8,0.2))
training <- data[ind==1,]
testing <- data[ind==2,]


#NEURAL NETWORK
library(neuralnet)
set.seed(333)
netw <- neuralnet(death_event~.,
               data=training,
               hidden=1,
               err.fct="ce",
               linear.output=FALSE)
plot(netw)



#Prediction-Confusion matrix-testing data
output <- compute(netw,testing[,-12])
p <- output$net.result
pred <- ifelse(p>0.5,1,0)
tab <- table(pred,testing$death_event)
tab
sum(diag(tab))/sum(tab)


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

