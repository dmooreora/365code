install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")
install.packages("rsample")
install.packages("randomForest")
install.packages("caTools")
install.packages("ROSE")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("yardstick")
install.packages("DescTools")
install.packages("margins")
install.packages("gbm")
install.packages("ggpubr")
install.packages("e1071")
install.packages("rmarkdown")
library(caret)
library(e1071)
library(tidyverse)
library(dplyr)
library(cutpointr)
library(rsample)
# Decision Tree
library(rpart)
library(rpart.plot)

# Random Forest Model
library(randomForest)

# Gradient Boosting Model
library(gbm)

# Compute RMSE for Regression
library(yardstick)

# Draw ROC Curve and Compute AUC for Classification
library(cutpointr)

train <- read.csv("c:/Dev/r/NetworkDetection/Train_data.csv",header=TRUE)
test <- read.csv("c:/Dev/r/NetworkDetection/Test_data.csv",header=TRUE)

str(train)
summary(train)

# Visualization and Description

ggplot(data = train) +
  geom_point(mapping = aes(y=count, x=duration, color=service)) +
  facet_wrap(~ protocol_type, nrow = 2) +
  scale_x_log10()

ggplot(data = train) +
  geom_smooth(mapping = aes(y=count, x=duration, color=protocol_type), show.legend = FALSE) +
  scale_x_log10()

ggplot(data=train) +
  geom_point(mapping = aes(y=count, x=duration)) +
  geom_smooth(mapping = aes(y=count, x=duration)) +
  scale_x_log10() +
  scale_y_log10()


ggplot(data=train, mapping=aes(x=duration, y=count)) +
  geom_point(mapping = aes(color=service)) +
  geom_smooth() +
  scale_x_log10() +
  scale_y_log10()


ggplot(data = train) +
  geom_bar(mapping = aes(x=class, fill=service),
          position = "dodge"
  )

ggplot(data = train) +
  geom_boxplot(mapping = aes(x=service, y=dst_host_count)) +
  coord_flip()


#  Data Manipulation and Cleaning

sapply(train, function(x) sum(is.na(x)))
train <- train[complete.cases(train),]

str(train)
set.seed(645)

network_split <- initial_split(train, prop = 0.7)

network_train <- training(network_split)
network_test <- testing(network_split)


# Create a binary target variable

network_train <- network_train %>% 
  mutate(binary_class = if_else(class=="normal", 0, 1))

network_test <- network_test %>% 
  mutate(binary_class = if_else(class=="normal", 0, 1))


# Regression Analysis

str(network_train)

regression1 <- glm(binary_class ~ . - class - is_host_login - is_guest_login - num_outbound_cmds, data = network_train, family="binomial")

summary(regression1)

# Predict on test data

predicted_value <- regression1 %>% 
  predict(newdata = network_test, type = "response")

network_test <- network_test %>% 
  mutate(.fitted = predicted_value)

roc <- roc(network_test, x = .fitted, class = binary_class, pos_class = 1, neg_class=0, direction = ">=")
plot(roc)
auc(roc)

plot(roc) +
  geom_line(data=roc, color="red") +
  geom_abline(slope=1) +
  labs(title = "ROC Curve for Logistical Regression for Binomial Predictor")

# Gradient Boosting Model

network_gbm <- gbm(binary_class ~ . - class - is_host_login - is_guest_login, data = network_train, distribution = "bernoulli",  n.trees = 1000)

# Outlier detection

out <- boxplot.stats(train$dst_bytes)$out
out_ind <- which(train$dst_bytes %in% c(out))

str(out_ind)
train[out_ind,"dst_bytes"]
train$dst_bytes

boxplot(train$dst_bytes,
        ylab = "dst_bytes",
        main = "Boxplot of dst bytes"
)
mtext(paste("Outliers: ", paste(out, collapse =",")))


# Decision Trees Classification

network_ctree <- rpart(as.factor(binary_class) ~ . - class, data = network_train, method = "class")

network_dtree <- rpart(binary_class ~ . - class, data = network_train, method = "class")

rpart.plot(network_dtree, cex=0.8)

# Support Vector Machine (SVM)

data("iris")
head(iris)
x <- iris[,-5]
y <- iris[5]

model_svm <- svm(Species ~ ., data=iris)
summary(model_svm)
pred <- predict(model_svm,x)
confusionMatrix(pred, y$Species)

x <- network_train[,1:42]
y <- network_train[43]
y

#model_svm <- svm(binary_class ~ ., data = network_train, type='C-classification', kernel='linear')
model_svm <- svm(binary_class ~ network_train$srv_serror_rate + network_train$srv_rerror_rate, data = network_train, type='C-classification', kernel='linear')

pred <- predict(model_svm,x) 
summary(pred)

#confusionMatrix(as.factor(pred$))

network_predict <- predict(model_svm, newdata=network_test) 

str(network_test)
str(network_train)

