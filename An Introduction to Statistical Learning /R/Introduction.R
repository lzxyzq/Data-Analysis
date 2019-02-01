
Auto = read.csv(file = '../Data sets/Auto.csv',header = TRUE,na.strings = '?')
head(Auto)
Auto = na.omit(Auto)
names(Auto)
# Graph 
attach(Auto)
plot(cylinders,mpg)
cylinders_factor = as.factor(cylinders)
plot(cylinders_factor,mpg,xlab = 'cylinders',ylab = 'mpg',main = 'I Am a Boxplot')

