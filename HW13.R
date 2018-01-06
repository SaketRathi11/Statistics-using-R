setwd("C:/Spring_Sem_2017/Stats/HW/HW13") 
data<-read.csv("HW13.csv",header = T)


X	<- data$BAC
Y	<- data$X5K_Time

#start by ploting the data and visualize what kind of trend
plot(Y ~ X,
     xlab = "X",
     ylab = "Y",
     main = "X v. Y")
#For simple linear regression the coding is still the same
#as previous models
my_lm<-lm(Y~X)
summary(my_lm)





#Read in the data
setwd("C:/Spring_Sem_2017/Stats/HW/HW13") 
the.data <- read.csv("HW13_Q2.csv")
Y <- the.data$Half_Marathon_Time_Champaign_Urbana
X <- the.data$Half_Marathon_Time_Indianapolis




confint(my_lm, 'X',level=0.95)

confint(my_lm,'(Intercept)',level = 0.95)
#Estimate of X is the slope (beta1) estimate
#intercept is the Beta0 estimate
#The output includes a t test result testing if it is significantly different than o

#Use the linear model for predicting values
new<-data.frame(X=0.02)
predict(object = my_lm, new ,interval = "confidence", level = 0.95) 

predict(object = my_lm, new, interval = "prediction", level = 0.95) 


#higher order models?
my_quad_lm<-lm(Y~poly(X,3,raw=TRUE))
summary(my_quad_lm)



Lack-of-fit test
Reduced=lm(Y~X)#fit reduced model
Full=lm(Y~0+as.factor(X)) #fit full model
anova(Reduced, Full) #get lack-of-fit test

#To visualize the relationship 
#start by ploting the data and visualize what kind of trend
plot(Y ~ X,
     xlab = "X",
     ylab = "Y",
     main = "X v. Y")


#TO get MSE we need to run an ANOVA
anova(lm(Y~X))



ANOVA.model <- lm(Y~as.factor(X))

#Conduct an F lack of fit test on the linear model
linear.model <- lm(Y~X)
anova(linear.model,ANOVA.model)



#Conduct an F lack of fit test on a quadratic model.
X.2 <- X^2
quadratic.model <- lm(Y~X+X.2)
anova(quadratic.model,ANOVA.model)

anova(quadratic.model)
#Conduct an F lack of fit test on a cubic model
X.3 <- X^3
cubic.model <- lm(Y~X+X.2+X.3)
anova(cubic.model,ANOVA.model)


anova(cubic.model)
