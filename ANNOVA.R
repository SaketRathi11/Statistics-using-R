setwd("C:/Spring_Sem_2017/Stats/HW/HW12") 
data<-read.csv("HW12.csv",header = T)
my_lm <- lm(formula=Height_cM~ Line + as.factor(Column) + as.factor(Row), data=data)

summary(my_lm)

# Latin Sqaure
my_lm <- lm(formula=Height_cM~ Line + as.factor(Column) + as.factor(Row), data=data)
anova(my_lm)

# Mean Seperation test Using two different functions
a1 <- aov(data$Height_cM~ data$Line + as.factor(data$Column) + as.factor(data$Row))
library(agricolae)
v<-HSD.test(a1,'data$Line',group= TRUE)
v
posthoc <- TukeyHSD(x=a1, 'data$Line', conf.level=0.95)

posthoc


# RCBD
my_lm1 <- lm(formula=Height_cM~ Line + as.factor(Column), data=data)
anova(my_lm1)
b1 <- aov(data$Height_cM~ data$Line + as.factor(data$Column))

library(agricolae)
v<-HSD.test(b1,'data$Line',group= TRUE)
v
posthoc1 <- TukeyHSD(x=b1, 'data$Line', conf.level=0.95)

posthoc1


# CRD
my_lm2 <- lm(formula=Height_cM~ Line, data=data)
anova(my_lm2)

c1 <- aov(data$Height_cM~ data$Line)

library(agricolae)
x<-HSD.test(c1,'data$Line',group= TRUE)
x
posthoc1 <- TukeyHSD(x=c1, 'data$Line', conf.level=0.95)

posthoc1









a1 <- aov(chocolate$Sabor ~ chocolate$Tipo + chocolate$Provador)
posthoc <- TukeyHSD(x=a1, 'data$Line', conf.level=0.95)
posthoc








a1 <- aov(data$Height_cM~ data$Line + as.factor(data$Column) + as.factor(data$Row))


library(agricolae)
v<-HSD.test(a1,'data$Line',group= TRUE)
v

install.packages("PMCMR")
library("PMCMR")
SimTestDiff(a1, grp = "Line", resp = "Height_cM", type = "Tukey", covar.equal =TRUE)
TukeyHSD(x=a1,'data$Line', conf.level=0.95)
plot(TukeyHSD(a1,data$Line))



#Conduct the Tukey
install.packages("SimComp")
library(SimComp)

SimTestDiff(data, grp = "Line", resp = "Height_cM", type = "Tukey", covar.equal =TRUE)


qf(.95, df1=18, df2=25) 
