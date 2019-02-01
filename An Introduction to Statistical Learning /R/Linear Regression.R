# Simple Linear Regression
library(MASS)
attach(Boston)

names(Boston)
lm.fit=lm(medv~lstat)
summary(lm.fit)
plot(lm.fit)

plot(lstat,medv,main = "Scatterplot",xlab = "Lstat",ylab = "Median value")
abline(lm.fit,col = 'red', lwd = 6)
# Multiple Linear Regression
pairs(Boston)
pairs(Boston[,c(1,3,7)])
lm.fit2 = lm(medv~lstat+age)
summary(lm.fit2)
lm.fit3 = lm(medv~.,data=Boston)
summary(lm.fit3)
lm.fit4 = lm(medv~.-age,data=Boston)
summary(lm.fit4)
# Interaction Terms
lm.fit5 = lm(medv~lstat*age)
summary(lm.fit5)

lm.fit6 = lm(medv~lstat + lstat:age)
summary(lm.fit6)
#It is easy to include interaction terms in a linear model using the lm() function. 
#The syntax lstat:black tells R to include an interaction term between lstat and black. 
#The syntax lstat*age simultaneously includes lstat, age, 
#and the interaction term lstat × age as predictors;
#it is a shorthand for lstat + age + lstat:age.

# Non-Linear Transformations of Predictors
lm.fit7 = lm(medv~poly(lstat,2))
summary(lm.fit7)

lm.fit7 = lm(medv~poly(lstat,7))
summary(lm.fit7)


