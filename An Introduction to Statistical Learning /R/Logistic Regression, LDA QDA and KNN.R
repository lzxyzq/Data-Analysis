library(ISLR)
attach(Smarket)
train = Year < 2005
test = !train

names(Smarket)
training_data = Smarket[train, -8]
testing_data = Smarket[test, -8]

test_y = Direction[test]

logistic_model = glm(Direction ~ .,
                     data = training_data,
                     family = "binomial")
summary(logistic_model)

logistic_probs = predict(logistic_model, testing_data, type = "response") 
head(logistic_probs)

logistic_pred_y = rep("Down", length(test_y)) 
## the function rep(), repeats "Down" 252 times
logistic_pred_y[logistic_probs > 0.5] = "Up"

## the following command creates the confusion matrix 
table(logistic_pred_y, test_y)
mean(logistic_pred_y != test_y)
--
library(MASS)
## create an LDA model. The lda() function takes a forumla and the name of the training data set as its argument

lda_model = lda(Direction~., data = training_data)
lda_pred = predict(lda_model,testing_data) 
names(lda_pred)
lda_pred_y = lda_pred$class
## compute the confusion matrix 
table(lda_pred_y, testing_y)
## compute the misclassification error rate
mean(lda_pred_y != testing_y)

--KNN 
library(class)
data = scale(Smarket[,-c(8,9)])

train = Year < 2005
test = !train

training_data = data[train,]
testing_data = data[test,]

training_y = Smarket$Direction[train]
testing_y = Smarket$Direction[test]

set.seed(1)
knn_pred_y = knn(training_data, testing_data, training_y, k = 1) 
table(knn_pred_y, testing_y)
mean(knn_pred_y != testing_y)

knn_pred_y = NULL
error_rate = NULL
for(i in 1:300){
  set.seed(1)
  knn_pred_y = knn(training_data,testing_data,training_y,k=i)
  ### find the minimum error rate 
  error_rate[i] = mean(testing_y != knn_pred_y) 
}
min_error_rate = min(error_rate) 
print(min_error_rate)
K = which(error_rate == min_error_rate)
print(K)

library(ggplot2)
qplot(1:300, error_rate, xlab = "K",
      ylab = "Error Rate", geom=c("point", "line"))


