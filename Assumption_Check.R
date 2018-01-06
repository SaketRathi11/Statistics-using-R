setwd("C:/Spring_Sem_2017/Stats/HW/HW9") 
the.data <- read.csv("Hw9.csv", head = TRUE)
the.data

# ANOVA other way

# Final Plotting

ANOVA.model1 <- aov(the.data$Time~(the.data$Treatment)) 

#Look at the ANOVA table 

summary(ANOVA.model1) 

#Obtain the predicted values and residuals
residuals <- ANOVA.model1$residuals
predicted.values <- ANOVA.model1$fitted.values 

####Assess the assumption of constant variance 

#Plot residuals against predicted values 
plot(residuals~predicted.values) 
abline(h = 0, lty = 2) 

#Make a QQ-plot of the residuals 
qqnorm(residuals) 


#Make a histogram of the residuals 
hist(residuals) 



#check assumptions
shapiro.test(residuals)

library(lawstat)

levene.test(as.numeric(the.data[,3]),(the.data[,2]))



install.packages("SimComp")
library(SimComp) 
SimTestDiff(data = the.data, grp = "Treatment",alternative = "two.sided", resp = "Time", type = "Dunnett", base = 1, covar.equal = TRUE)
SimTestDiff(data=the.data, grp = "Treatment", resp = "Time", type = "Tukey", covar.equal =TRUE)

#Sceffes test
install.packages("agricolae")
library(agricolae)
scheffe.test(y=the.data[,3], trt = the.data[,2], DFerror = 24, MSerror = 0.05 , Fc = 242, alpha = 0.05, group = TRUE, main = FALSE, console = TRUE )
??scheffe.test()

Dunnett


qf(p = 0.975, df1 = 1, df2 = 24, lower.tail = TRUE)
