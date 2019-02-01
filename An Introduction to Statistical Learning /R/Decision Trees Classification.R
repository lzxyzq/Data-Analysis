
## Fitting Classification Tree Models
## We are going to use the Carseats dataset from the ILSR ibrary

library(ISLR)
library(tree)

attach(Carseats)
head(Carseats)
range(Sales)
#Create a categorical variables bases on Sales
High = ifelse(Sales>=8,"YES","NO")
#Appends High to Carseat dataset
Carseats = data.frame(Carseats,High)
Carseats = Carseats[,-1]
#Split data into testing and training using
set.seed(2)
train = sample(1:nrow(Carseats),nrow(Carseats)/2)
test = -train
train_data = Carseats[train,]
test_data = Carseats[test,]
testing_High = High[test]

# Fitting the tree model using training data

tree_model = tree(High~.,data = train_data)
plot(tree_model)
text(tree_model,pretty = 0)     

# check how the model is doing using the testing data

tree_pred = predict(tree_model,test_data,type = 'class')
mean(tree_pred != testing_High) #28.5%

# cross valition to check where to stop pruning
set.seed(3)
cv_tree = cv.tree(tree_model,FUN=prune.misclass)
names(cv_tree)
plot(cv_tree$size,cv_tree$dev,type = 'b')

# prune the model 
pruned_model = prune.misclass(tree_model,best = 9)
plot(pruned_model)
text(pruned_model,pretty = 0)
tree_pred = predict(pruned_model,test_data,type = 'class')
mean(tree_pred != testing_High)   #23%
