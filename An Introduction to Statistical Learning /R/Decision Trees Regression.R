
library(MASS)
library(tree)
attach(Boston)

#Split data into testing and training using
set.seed(1)
train = sample(1:nrow(Boston),nrow(Boston)/2)
test = -train
train_data = Boston[train,]
test_data = Boston[test,]
testing_medv = medv[test]

# fit a tree based on training data 
tree_model = tree(medv~.,train_data)
tree_model
plot(tree_model)
text(tree_model,pretty = 0)

# check the model is doing using the testing dataset

tree_predict = predict(tree_model,test_data)
mean((tree_predict-testing_medv)^2)  #25%

#cross validation for pruning the tree 
cv_tree = cv.tree(tree_model)
plot(cv_tree$size,cv_tree$dev,type = 'b',xlab = 'Tree Size',ylab = 'MSE')
which.min(cv_tree$dev)
cv_tree$size[1]

#prune the tree to size 4 

prune_model = prune.tree(tree_model,best = 4)
plot(prune_model)
text(prune_model)

#check the accuracy of the model using testing data
tree_pred = predict(prune_model,test_data)
mean((tree_pred-testing_medv)^2)

