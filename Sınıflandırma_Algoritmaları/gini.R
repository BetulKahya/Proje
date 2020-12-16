library(dplyr)
library(readr)
library(tibble)
library(tidyr)
library(ggplot2)

data <- read.csv(file.choose(),header = T)
data$death_event <- factor(data$death_event)
set.seed(123)
train1 <- data %>% sample_frac(.80)
train1
test1 <- anti_join(data,train1)
test1

library(randomForest)
model1 <- randomForest(death_event~.,train1[1:12])
model1
summary(model1)


k <- importance(model1)
k <- tibble(Degisken=as.vector(row.names(k)),Ortalama_Gini_Katsayýsý=as.vector(importance(model1))) %>% arrange(desc(Ortalama_Gini_Katsayýsý))
format.data.frame(k)

