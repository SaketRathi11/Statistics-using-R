setwd("C:/Spring_Sem_2017/Stats/HW/HW8") 
the.data <- read.csv("HW8.Q1.csv", head = TRUE)
the.data


#Separate the data into 4 subsets: 
Audi_S7 <- the.data[which(the.data[,2] == "Audi_S7"),]
Cadillac_CTS <- the.data[which(the.data[,2] == "Cadillac_CTS"),]
Maserati_Ghibli <- the.data[which(the.data[,2] == "Maserati_Ghibli"),]
Honda_Civic <- the.data[which(the.data[,2] == "Honda_Civic"),]


#Conduct the Shapiro-Wilk test for normality
shapiro.test(as.numeric(Audi_S7[,3]))
shapiro.test(as.numeric(Cadillac_CTS[,3]))
shapiro.test(as.numeric(Maserati_Ghibli[,3]))
shapiro.test(as.numeric(Honda_Civic[,3]))


# Calcualte variance for each case For Fmax Test
a <-var(as.numeric(Audi_S7[,3]))
b <-var(as.numeric(Cadillac_CTS[,3]))
c <-var(as.numeric(Maserati_Ghibli[,3]))
d <-var(as.numeric(Honda_Civic[,3]))

#Max and Min Variance        
S1 <- max(a,b,c,d)
S2  <- min(a,b,c,d)

Fmax = (S1/S2)
Fmax

#Calculate the means so that we can include them in the box plot
means <- by(as.numeric(the.data[,3]),the.data[,2], mean,header= FALSE) 
means


Variance <- by(as.numeric(the.data[,3]),the.data[,2],var) 
Variance


#Make a box plot. This will be saved into your working directory
pdf("Boxplot_HW8_Q1.pdf")
boxplot(the.data[,3]~the.data[,2], col = "Red", 
        ylab = "MPG", xlab = "1 = Audi , 2 = Cadillac , 3 = Honda_civic ,4 = Maserati")
points(c(1:length(means)), means, pch = 3, cex = 0.75) #Adds the means, which are indicated by the "+" symbol
dev.off()

?boxcox()


#write linear model for data
#ANOVA 1
my_lm<-lm(as.numeric(the.data$MPG) ~ (the.data$Automobile))
anova(my_lm)

# ANOVA other way

# Final Plotting

ANOVA.model1 <- aov(the.data$MPG~(the.data$Automobile)) 

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
shapiro.test(residuals(my_lm))

library(lawstat)

levene.test(as.numeric(the.data[,3]),(the.data[,2]))




#the boxcox function helps us detrimine the optimal transformation
library(MASS)
boxcox(my_lm)



new.variable<-(as.numeric(the.data[,3]))^1.5

the.data<-cbind(the.data,new.variable)

my_lm<-lm(the.data$new.variable~(the.data$Automobile))
anova(my_lm)

summary(my_lm)

shapiro.test(residuals(my_lm))

levene.test(as.numeric(the.data$new.variable),the.data$Automobile)



#Separate the data into two subsets: one for LactNumb = 1, and one for LactNumb = 3
Audi_S7 <- the.data[which(the.data[,2] == "Audi_S7"),]
Cadillac_CTS <- the.data[which(the.data[,2] == "Cadillac_CTS"),]
Maserati_Ghibli <- the.data[which(the.data[,2] == "Maserati_Ghibli"),]
Honda_Civic <- the.data[which(the.data[,2] == "Honda_Civic"),]

# Calcualte variance for each case
a <-var(as.numeric(Audi_S7$new.variable))
b <-var(as.numeric(Cadillac_CTS$new.variable))
c <-var(as.numeric(Maserati_Ghibli$new.variable))
d <-var(as.numeric(Honda_Civic$new.variable))

S1 <- max(a,b,c,d)
S2  <- min(a,b,c,d)

Fmax = (S1/S2)
Fmax


# Final Plotting

ANOVA.model <- aov(the.data$new.variable~(the.data$Automobile)) 

#Look at the ANOVA table 

summary(ANOVA.model) 

#Obtain the predicted values and residuals
residuals <- ANOVA.model$residuals
predicted.values <- ANOVA.model$fitted.values 

residuals

?shapiro.test()
shapiro.test(residuals)
####Assess the assumption of constant variance 

#Plot residuals against predicted values 
plot(residuals~predicted.values) 
abline(h = 0, lty = 2) 

#Make a QQ-plot of the residuals 
qqnorm(residuals) 


#Make a histogram of the residuals 
hist(residuals) 

boxplot(residuals, col = "Red")
points(c(1:length(mean(residuals))), means, pch = 3, cex = 0.75) #Adds the means, which are indicated by the "+" symbol
dev.off()



##Conduct the Kruskal-Wallis test
kruskal.test(MPG ~ Automobile, data = the.data) 




qf(p = 0.975, df1 = 7, df2 = 10, lower.tail = TRUE) 
qf(p = 0.05, df1 = 9, df2 = 15, lower.tail = TRUE)
