# Step 1 Load data, and run numberical and grahical summeries

library(ISLR)
attach(Smarket)

summary(Smarket)
cor(Smarket[,-9])
pairs(Smarket[,-9])      
# Step 2 Split the data into training and testing data 

training = (Year < 2005)
testing = !training

training_data = Smarket[training,]
testing_data = Smarket[testing,]

Direction_test = Direction[testing]

# Step 3 Fit a logistic regression model using training data

stock_model = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                 data = training_data,
                 family = "binomial")
summary(stock_model)

# Step 4 Use the fitted model to do predictions for test data
model_pred_probs = predict(stock_model,testing_data,type = 'response')

model_pred_Direction = rep("Down",252)
model_pred_Direction[model_pred_probs > 0.5] = "Up"

# Step 5 Create the confusion matrix, and compute the misclassification rate
table(model_pred_Direction,Direction_test)
mean(model_pred_Direction != Direction_test)
