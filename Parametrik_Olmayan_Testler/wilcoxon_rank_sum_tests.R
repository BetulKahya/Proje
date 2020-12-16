library(readr)

#Veri Okuma
data <- read.csv(file.choose(),header = T)
attach(data)
names(data)
data$death_event <- as.factor(data$death_event)

#Wilcoxon Rank Sum Test
wilcox.test(sex ~ death_event,mu=0,alt="two.sided",conf.int=T,
            conf.level=0.95,paired=F,correct=T)
wilcox.test(smoking ~ death_event,mu=0,alt="two.sided",conf.int=T,
            conf.level=0.95,paired=F,correct=T)
wilcox.test(diabetes ~ death_event,mu=0,alt="two.sided",conf.int=T,
            conf.level=0.95,paired=F,correct=T)
wilcox.test(high_blood_pressure ~ death_event,mu=0,alt="two.sided",conf.int=T,
            conf.level=0.95,paired=F,correct=T)
wilcox.test(anaemia ~ death_event,mu=0,alt="two.sided",conf.int=T,
            conf.level=0.95,paired=F,correct=T)
wilcox.test(age ~ death_event,mu=0,alt="two.sided",conf.int=T,
            conf.level=0.95,paired=F,correct=T)
wilcox.test(ejection_fraction ~ death_event,mu=0,alt="two.sided",conf.int=T,
            conf.level=0.95,paired=F,correct=T)
wilcox.test(serum_sodium~ death_event,mu=0,alt="two.sided",conf.int=T,
            conf.level=0.95,paired=F,correct=T)
wilcox.test(serum_creatinine ~ death_event,mu=0,alt="two.sided",conf.int=T,
            conf.level=0.95,paired=F,correct=T)
wilcox.test(platelets ~ death_event,mu=0,alt="two.sided",conf.int=T,
            conf.level=0.95,paired=F,correct=T)
wilcox.test(creatinine_phosphokinase~ death_event,mu=0,alt="two.sided",conf.int=T,
            conf.level=0.95,paired=F,correct=T)
