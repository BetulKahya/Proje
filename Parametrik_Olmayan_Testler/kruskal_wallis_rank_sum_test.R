library(readr)

#Veri Okuma
data <- read.csv(file.choose(),header = T)
attach(data)
names(data)
data$death_event <- as.factor(data$death_event)


#Kruskal Wallis Rank Sum Test
kruskal.test(sex ~ death_event)
kruskal.test(smoking ~ death_event)
kruskal.test(diabetes ~ death_event)
kruskal.test(high_blood_pressure ~ death_event)
kruskal.test(anaemia ~ death_event)
kruskal.test(age ~ death_event)
kruskal.test(ejection_fraction ~ death_event)
kruskal.test(serum_sodium ~ death_event)
kruskal.test(serum_creatinine ~ death_event)
kruskal.test(platelets ~ death_event)
kruskal.test(creatinine_phosphokinase ~ death_event)
