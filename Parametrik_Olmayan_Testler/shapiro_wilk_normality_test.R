library(readr)

#Veri Okuma
data <- read.csv(file.choose(),header = T)
attach(data)
names(data)
data$death_event <- as.factor(data$death_event)

#Shapiro Wilk Normality Test
shapiro.test(data$sex)
shapiro.test(data$smoking)
shapiro.test(data$diabetes)
shapiro.test(data$high_blood_pressure)
shapiro.test(data$anaemia)
shapiro.test(data$age)
shapiro.test(data$ejection_fraction)
shapiro.test(data$serum_sodium)
shapiro.test(data$serum_creatinine)
shapiro.test(data$platelets)
shapiro.test(data$creatinine_phosphokinase)
shapiro.test(data$death_event)
