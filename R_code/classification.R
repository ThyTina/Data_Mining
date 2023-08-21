#load library
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(corrplot)
library(pROC)
# decision tree
library(rpart)
library(tree)
library(ISLR)
library(rpart.plot)
# random forest
library(randomForest)
library(class)
# neutral network
library(tidyverse)
library(neuralnet)

# CLASSIFICATION:
# 1. LOGISTIC REGRESSION
# 2. DECISION TREE
# 3. RANDOM FOREST
# 4. NEUTRAL NETWORK


# ===== Read data =====
setwd('D:\\')
data <- read.csv(file ='data_processed.csv')
head(data)

summary(data)


# ========= Create the training and test data =========
set.seed(42)
n= nrow(data)
trainIndex = sample(1:n, size= round(0.7*n), replace=FALSE)
train = data[trainIndex,]
test = data[-trainIndex,]

# Rows of training data and test data
nrow(train)
nrow(test)

# ===== LOGISTIC REGRESSION ==================================================
# Estimates a logistic regression model using the glm
set.seed(42)
model <- glm(Response ~ ., data = train, family = "binomial")
summary(model)
# AIC: 5007.2
# Number of Fisher Scoring iterations: 5

model

coef_df <- as.data.frame(summary(model)$coefficients)
coef_df$Variable <- rownames(coef_df)
coef_df

ggplot(coef_df, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Estimate - 1.96 * `Pr(>|z|)`, ymax = Estimate + 1.96 * `Pr(>|z|)`), width = 0.2) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "Predictor Variables", y = "Coefficient Estimate")



# ===== TRAIN DATA
# Predict train data
pred <- predict(model, newdata = train, type = "response")
pred_value <- ifelse(pred > 0.5, 1, 0)
result <- table(pred_value, train$Response)
result
# pred_value    0    1
#           0 5460  932
#           1    2    0

# ===== Calculate accuracy, F1-score
TN <- result[1, 1]
TP <- result[2, 2]
FP <- result[1, 2]
FN <- result[2, 1]

accuracy <- (TN + TP) / (TN + TP + FP + FN)
accuracy
# [1] 0.8539256

precision <- TP / (TP + FP)
precision
# [1] 0

recall <- TP / (TP + FN)
recall
# [1] 0

f1_score <- 2 * (precision * recall) / (precision + recall)
f1_score
# [1] NaN

# ==>>> (TP) value is 0, it means that the model did not correctly predict any positive instances.
# Both precision and recall will also be 0, and the F1 score cannot be calculated.


# ===== TEST DATA
# Predict test data
pred2 <- predict(model, newdata = test, type = "response")
pred_value2 <- ifelse(pred2 > 0.5, 1, 0)
result <- table(pred_value2, test$Response)
result
# pred_value    0    1
#         0 2363  376
#         1    1    0

# ===== Calculate accuracy, F1-score
TN <- result[1, 1]
TP <- result[2, 2]
FP <- result[1, 2]
FN <- result[2, 1]

accuracy <- (TN + TP) / (TN + TP + FP + FN)
accuracy
# [1] 0.8624088

precision <- TP / (TP + FP)
precision
# [1] 0

recall <- TP / (TP + FN)
recall
# [1] 0

f1_score <- 2 * (precision * recall) / (precision + recall)
f1_score
# [1] NaN

# ===== ROC
# Create ROC curves for train and test data
roc_data <- roc(train$Response, pred)
roc_data2 <- roc(test$Response, pred2)

# Combine ROC curves into a single plot
plot(roc_data, col = "blue", main = "ROC Curve for Logistic Regression Model")
lines(roc_data2, col = "red")
legend("bottomright", legend = c("Train", "Test"), col = c("blue", "red"), lty = 1)




# =========== DECISION TREE ==================================================
set.seed(42)
data.tree = rpart(Response ~ .,data = train, method="class")
data.tree
summary(data.tree)

# plot tree
prp(data.tree, type = 2, extra = "auto", nn = TRUE, branch = 1, varlen = 0, yesno = 2)

# Get the number of nodes
num_nodes <- data.tree$n
num_nodes
# [1] 4

# Get variable importance
var_importance <- data.tree$variable.importance
var_importance

# Create a bar plot of variable importance with rotated x-axis labels
barplot(var_importance, main = "Variable Importance in Decision Tree",
        xlab = "Variable", ylab = "Importance",
        names.arg = names(var_importance), las = 2)

# ==== DECISION TREE TRAIN DATA
# Predict train data
pred = predict(data.tree, train, type = 'prob')
pred_value <- ifelse(pred[, "1"] > 0.5, 1, 0)
# accuracy train data
result = table(pred_value, train$Response)
result
# y_pred    0    1
#       0 5420  744
#       1   42  188

# Calculate accuracy, F1-score
TN <- result[1, 1]
TP <- result[2, 2]
FP <- result[1, 2]
FN <- result[2, 1]

accuracy <- (TN + TP) / (TN + TP + FP + FN)
accuracy
# [1] 0.8770723

precision <- TP / (TP + FP)
precision
# [1] 0.2017167

recall <- TP / (TP + FN)
recall
# [1] 0.8173913

f1_score <- 2 * (precision * recall) / (precision + recall)
f1_score
# [1] 0.32358


# ==== TEST DATA
# Predict test data
pred2 = predict(data.tree, test, type = 'prob')
pred_value2 <- ifelse(pred2[, "1"] > 0.5, 1, 0)
# accuracy test data
result = table(pred_value2, test$Response)
result
# y_pred    0    1
#       0 2340  312
#       1   24   64

# Calculate F1-score
TN <- result[1, 1]
TP <- result[2, 2]
FP <- result[1, 2]
FN <- result[2, 1]

accuracy <- (TN + TP) / (TN + TP + FP + FN)
accuracy
# [1] 0.8773723

precision <- TP / (TP + FP)
precision
# [1] 0.1702128

recall <- TP / (TP + FN)
recall
# [1] 0.7272727

f1_score <- 2 * (precision * recall) / (precision + recall)
f1_score
# [1] 0.2758621

# ===== ROC
# Predict probabilities of the positive class
# y_probs <- predict(data.tree, train, type = 'prob')[, "1"]
# y_probs2 <- predict(data.tree, test, type = 'prob')[, "1"]

# Create ROC data
roc_data <- roc(train$Response, pred_value)
roc_data2 <- roc(test$Response, pred_value2)

# Combine ROC curves into a single plot
plot(roc_data, col = "blue", main = "ROC Curve for Decision Tree Model")
lines(roc_data2, col = "red")
legend("bottomright", legend = c("Train", "Test"), col = c("blue", "red"), lty = 1)



# ===== RANDOM FOREST ========================================================

# Fine tuning parameters of Random Forest model
set.seed(42)
train$Response <- factor(train$Response)
model_rf <- randomForest(Response ~ ., data = train, ntree = 15, mtry = 7, importance = TRUE)
model_rf
summary(model_rf)

varImpPlot(model_rf)

library(randomForest)
tree_plot <- getTree(model_rf)
plot(tree_plot)

# ===== TRAIN DATA
# Predicting on train set
pred <- predict(model_rf, train, type = "response")
# Checking classification accuracy
result = table(pred, train$Response)
result
# predTrain    0    1
#         0 5462    1
#         1    0  931

# ===== Calculate accuracy, F1-score
TN <- result[1, 1]
TP <- result[2, 2]
FP <- result[1, 2]
FN <- result[2, 1]

accuracy <- (TN + TP) / (TN + TP + FP + FN)
accuracy
# [1] 0.9998436

precision <- TP / (TP + FP)
precision
# [1] 0.998927

recall <- TP / (TP + FN)
recall
# [1] 1

f1_score <- 2 * (precision * recall) / (precision + recall)
f1_score
# [1] 0.9994632


# ===== TEST DATA
# Predicting on test set
pred2 <- predict(model_rf, test, type = "class")
# Checking classification accuracy
result = table(pred2, test$Response)
result
# predTest    0    1
#         0 2348   12
#         1   16  364

# ===== Calculate accuracy, F1-score
TN <- result[1, 1]
TP <- result[2, 2]
FP <- result[1, 2]
FN <- result[2, 1]

accuracy <- (TN + TP) / (TN + TP + FP + FN)
accuracy
# [1] 0.989781

precision <- TP / (TP + FP)
precision
# [1] 0.9680851

recall <- TP / (TP + FN)
recall
# [1] 0.9578947

f1_score <- 2 * (precision * recall) / (precision + recall)
f1_score
# [1] 0.962963


# ===== ROC
# Create a binary vector indicating if the predicted class is 1 or 0
pred_class <- as.numeric(pred == "1")
pred_class2 <- as.numeric(pred2 == "1")

# Create ROC data
roc_data <- roc(train$Response, pred_class)
roc_data2 <- roc(test$Response, pred_class2)

# Combine ROC curves into a single plot
plot(roc_data, col = "blue", main = "ROC Curve for Random Forest Model")
lines(roc_data2, col = "red")
legend("bottomright", legend = c("Train", "Test"), col = c("blue", "red"), lty = 1)




# ===== NEUTRAL NETWORK =======================================================
set.seed(123)
model = neuralnet(Response ~ ., data=train, hidden=c(4,2),linear.output = FALSE, act.fct = "logistic")
summary(model)

plot(model,rep = "best")

# ===== TRAIN DATA
# Predicting on train set
pred <- predict(model, newdata = train)
pred_class <- ifelse(pmax(pred) > 0.5, 1, 0)[,2]
result <- table(train$Response, pred_class)
result

# pred_class
#     0    1
# 0 5265  197
# 1  645  287

# ===== Calculate accuracy, F1-score
TN <- result[1, 1]
TP <- result[2, 2]
FP <- result[1, 2]
FN <- result[2, 1]

accuracy <- (TN + TP) / (TN + TP + FP + FN)
accuracy
# [1] 0.868314

precision <- TP / (TP + FP)
precision
# [1] 0.5929752

recall <- TP / (TP + FN)
recall
# [1] 0.3079399

f1_score <- 2 * (precision * recall) / (precision + recall)
f1_score
# [1] 0.4053672



# ===== TEST DATA
# Predicting on test set
pred2 <- predict(model, newdata = test)
pred_class <- ifelse(pmax(pred2) > 0.5, 1, 0)[,2]
result <- table(test$Response, pred_class)
result
# pred_class
#     0    1
# 0 2237  127
# 1  265  111

# ===== Calculate accuracy, F1-score
TN <- result[1, 1]
TP <- result[2, 2]
FP <- result[1, 2]
FN <- result[2, 1]

accuracy <- (TN + TP) / (TN + TP + FP + FN)
accuracy
# [1] 0.8569343

precision <- TP / (TP + FP)
precision
# [1] 0.4663866

recall <- TP / (TP + FN)
recall
# [1] 0.2952128

f1_score <- 2 * (precision * recall) / (precision + recall)
f1_score
# [1] 0.3615635


# ===== ROC

# Create ROC curve
roc_data <- roc(train$Response, pred[,2])
roc_data2 <- roc(test$Response, pred2[,2])

# Combine ROC curves into a single plot
plot(roc_data, col = "blue", main = "ROC Curve for Neural Network Model")
lines(roc_data2, col = "red")
legend("bottomright", legend = c("Train", "Test"), col = c("blue", "red"), lty = 1)

