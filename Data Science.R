#===================================================================
# ISM645 / IAF601   Principle of Predictive Analytics
# Final Exam        Predictive Modeling
# Due Date          December 1, 11:59 pm
# Student           Derek Moore 
# Student ID        882113958
#===================================================================


#===================================================================
############################ Final Exam ############################
#===================================================================

install.packages("cutpointr")
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
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(randomForest)
library(cutpointr)
library(rsample)
library(caret)
library(caTools)
library(ROSE)
library(rpart)
library(rpart.plot)
library(broom)
library(yardstick)
library(DescTools)
library(margins)
library(gbm)
library(ggpubr)

# Import the student_performance.csv and explore it.
#====================== Write R code HERE ==========================

students <- read.csv("c:/Dev/R/student_performance.csv",header=TRUE)
str(students)
summary(students)

summarize(students, avg_age = mean(age, na.rm=TRUE))

by_values <- group_by(students, studytime, failures, traveltime)
summarize(by_values, avg_travel=mean(studytime, na.rm = TRUE))

#===================================================================



#====================================
#======= Question 1 (1 Point) =======
# Answer the following questions by presenting a relevant plot (visualization).
# Q1-1. Do grades of Mathematics and Portuguese differ by gender?
# Q1-2. Does study time matter to get better grades of Mathematics and Portuguese?
# Q1-3. Does access to Internet influence students' grades of Mathematics and Portuguese?

## There could be various approaches to answer to these questions.

#====================== Write R code HERE ==========================

# Answer the following questions by presenting a relevant plot (visualization).
# Q1-1. Do grades of Mathematics and Portuguese differ by gender?
  
ggplot(data=students, aes(x=subject, y=total_grade, fill=sex)) +
    geom_bar(position="dodge",stat="identity") +
    ylab("total grade") +
    xlab("age")
  
# For each age category, the grades of Mathematics and Portuguese Subject grades differ by Gender.  With 
# Females having less of an overall total grade for Mathematics than Male students, and 
# Males having less of an overall total grade for Portuguese than Female students.

# Q1-2. Does study time matter to get better grades of Mathematics and Portuguese?

ggplot(data=students, aes(x=studytime, y=total_grade)) +
  geom_point() +
  geom_abline(color="red")

# Using a Scatterplot with initial data fitting "red" line, we can see that the overall total grades
# are trending upward as studytime hours increase.

# Q1-3. Does access to Internet influence students' grades of Mathematics and Portuguese?


ggplot(students, aes(x=subject, y=total_grade, fill=internet)) +
         geom_bar(position="dodge",stat="identity")
    
#  For mathematics, Internet access does appear to have a slight impact.  
#   Overall for Portuguese, on average, having access to the internet does not have
#  as much of an impact.

#===================================================================



#====================================
#======= Question 2 (2 Point) =======
# Q2-1. Split the data into two separate data frames for each subject: Mathematics and Portuguese. Then, remove a variable "subject".
# Q2-2. Build linear regression models to predict total_grade of Mathematics and Portuguese, respectively, based on time spending patterns (travel time, study time, free time after school, and going out with friends)
# Q2-3. Based on your model, make prediction for a new student's final grades of Mathematics and Portuguese, based on the following information.
#       You can write your prediction results by using comment (#).

#------------------ New Student's Information
#------------------ traveltime = 2
#------------------ studytime = 4
#------------------ freetime = 3
#------------------ goout = 2

#====================== Write R code HERE ==========================

# Q2-1. Split the data into two separate data frames for each subject: Mathematics and Portuguese. Then, remove a variable "subject".
str(students)

s_math <- students %>%
         filter(subject=="Mathematics")

s_math <- subset(s_math, select= -subject)
  
s_portuguese <- students %>%
         filter(subject=="Portuguese")

s_portuguese <- subset(s_portuguese, select =-subject)

str(s_portuguese)
str(s_math)

# Q2-2. Build linear regression models to predict total_grade of Mathematics and Portuguese, respectively, based on time spending patterns (travel time, study time, free time after school, and going out with friends)

reg_s_math <- lm(total_grade ~ traveltime + studytime + freetime + goout, data=s_math)

reg_s_port <- lm(total_grade ~ traveltime + studytime + freetime + goout, data=s_portuguese)

# Q2-3. Based on your model, make prediction for a new student's final grades of Mathematics and Portuguese, based on the following information.
#       You can write your prediction results by using comment (#).

#------------------ New Student's Information
#------------------ traveltime = 2
#------------------ studytime = 4
#------------------ freetime = 3
#------------------ goout = 2

 
print(reg_s_math)
print(reg_s_port)

#Linear Regression Model Prediction y=mx+b

new.student <- data.frame(traveltime=c(2),studytime=c(4),freetime=c(3),goout=c(2))

 

math_predict<- predict(reg_s_math, newdata=new.student) 

print(math_predict)

port_predict <- predict(reg_s_port, newdata=new.student)

print(port_predict)


# Predicted Math Grades
#  Total_Grade = 35.8727 

# Predicted Portuguese Grades
# Total_Grade = 39.78389
#===================================================================



#### For the subsequent questions (Q3 to Q5), use the dataset for grades of Mathematics.

#====================================
#======= Question 3 (2 Point) =======

# Q3-1. Create a new binary variable, "pass", defined as TRUE if total_grade > 30 and FALSE otherwise.
#       To avoid any error, remove the variable, "subject".
# Q3-2. Split data into 70% train and 30% test data.
# Q3-3. Based on train data, build a logistic regression model to predict "pass" using all available predictor variables.
#       Note that "total_grade" should not be included as a predictor.
# Q3-4. Based on test data, draw a ROC curve and calculate AUC for the model trained in Q3-3.

#====================== Write R code HERE ==========================

# Q3-1. Create a new binary variable, "pass", defined as TRUE if total_grade > 30 and FALSE otherwise.
#       To avoid any error, remove the variable, "subject".

s_math <- s_math %>%
  mutate(pass = if_else(total_grade > 30, TRUE, FALSE))


str(s_math)

# Q3-2. Split data into 70% train and 30% test data.

set.seed(645)

math_split <- initial_split(s_math, prop=0.7)
math_train <- training(math_split)
math_test <- testing(math_split)

# Q3-3. Based on train data, build a logistic regression model to predict "pass" using all available predictor variables.
#       Note that "total_grade" should not be included as a predictor.


reg_math <- glm(pass ~ . - total_grade, data = math_train)

# Q3-4. Based on test data, draw a ROC curve and calculate AUC for the model trained in Q3-3.

#math_test <- math_test %>%
#  mutate(binary_grade=if_else(binary_grade=TRUE, 1, 0))

pred_math<- reg_math %>%
  predict(newdata = math_test, type = "response")

summary(pred_math)

math_test <- math_test %>%
  mutate(.fitted = pred_math)

math_test <- math_test %>%
  mutate(predicted_class = .fitted)

test_roc <- roc(math_test, x=.fitted, class=pass, pos_class=TRUE, neg_class=FALSE, direction=">=")

plot(test_roc)
auc(test_roc)

plot(test_roc) +
  geom_line(data=test_roc, color="red") +
  geom_abline(slope=1) +
  labs(title = "ROC Curve for Logistical Regression for Binomial Predictor")

#===================================================================



#====================================
#======= Question 4 (2 Point) =======
# Q4-1. Based on train data, build a random forest model to predict "pass" using all available predictor variables.
# Q4-2. Based on test data, draw a ROC curve and calculate AUC for the model trained in Q4-1.
# Q4-3. According to the results of Q3 and Q4, which model do you think is better? Why?
#       Write your answer below by using comment (#).

#====================== Write R code HERE ==========================

# Q4-1. Based on train data, build a random forest model to predict "pass" using all available predictor variables.

str(math_train)
math_rf  <- randomForest(as.factor(pass) ~ . - total_grade, data = math_train, ntree=1000, importance = TRUE)

# Q4-2. Based on test data, draw a ROC curve and calculate AUC for the model trained in Q4-1.

math_rf
 
varImpPlot(math_rf)

pred_math_rf <- math_rf %>%
   predict(newdata = math_test, type = "prob")

math_test <- math_test %>%
  mutate(.fitted_rf = pred_math_rf[,TRUE])

 

test_roc <- roc(math_test, x=.fitted_rf, class=pass, pos_class=TRUE, neg_class=FALSE, direction=">=")
plot(test_roc)
auc(test_roc)

plot(test_roc) +
  geom_line(data=test_roc, color="blue") +
  geom_abline(slope=1) +
  labs(title = "ROC Curve for Random Forest for Binomial Predictor")


# Q4-3. According to the results of Q3 and Q4, which model do you think is better? Why?
#       Write your answer below by using comment (#).

# The regression model is better, it has a higher AUC that is closer to 1 and also has a better balance between
# Sensitivity and Specificity as the ROC Curve changes.  Having AUCs closer to 1 represents there is a good measure
# Of separability.  The higher area under the curve, the better the classification.


#===================================================================


#====================================
#======= Question 5 (2 Point) =======
# For this question, use the random forest model as in Q4.

# Q5. One might argue that background of parents and family structure are important in predicting children's math performance. 
#     Build another random forest model with only family-related factors (Pstatus, Medu, Mjob, Fedu, Fjob, guardian, famsize, famrel, and famsup).
#     Based on your analysis, do you agree or disagree with this argument?
#     Write your answer below by using comment (#).

#====================== Write R code HERE ==========================

# Q5. One might argue that background of parents and family structure are important in predicting children's math performance. 
#     Build another random forest model with only family-related factors (Pstatus, Medu, Mjob, Fedu, Fjob, guardian, famsize, famrel, and famsup).
#     Based on your analysis, do you agree or disagree with this argument?
#     Write your answer below by using comment (#).

str(math_train)
math_rf_2  <- randomForest(as.factor(pass) ~ Pstatus + Medu + Mjob + Fedu + Fjob + guardian + famsize + famrel + famsup, data = math_train, ntree=1000, importance = TRUE)

math_rf_2


varImpPlot(math_rf_2)

pred_math_rf <- math_rf_2 %>%
  predict(newdata = math_test, type = "prob")

math_test <- math_test %>%
  mutate(.fitted_rf_2 = pred_math_rf[,TRUE])



test_roc <- roc(math_test, x=.fitted_rf_2, class=pass, pos_class=TRUE, neg_class=FALSE, direction=">=")
plot(test_roc)
auc(test_roc)

plot(test_roc) +
  geom_line(data=test_roc, color="blue") +
  geom_abline(slope=1) +
  labs(title = "ROC Curve for Random Forest for Binomial Predictor (family)")

#  I do not agree with that family factors have a good predictive ability.  If you look at the ROC/AUC chart.
#  The values are near 0.50 for both sensitivity and specificity, meaning that the classifier is not 
# about to distinguish between - and + class points.  The prediction is random  or constant for all 
# of the data points. There isn't much of a impact for these variables on the overall 
# probability of correctly classifying the binary variable.



#===================================================================


#====================================
#======= Question 6 (1 Point) =======
# Q6. Suggest one possible application of this predictive analytics to improve education.
#     Write your answer below by using comment (#), up to 10 lines.

#====================== Write R code HERE ==========================


#   The possible usage of these model can help create multi-variable predictions which will allow 
#   Educators to modify specific measures (metrics) to create a better balance for a student's time and circumstances.
#   This is where random forest and decision trees can come into better usage.  Although my observations
#   is that regression analysis for binomial prediction will be good to get an overall picture of predicting
#   final grades, a random forest with the right number of iterations and predictors could dig a little 
#   deeper into HOW a student could improve their grade (more time studying, less going out, etc.)
#   Such a tool could be used to then guide the overall regression model, thus giving educators a powerful
#   tool to help students achieve.





#===================================================================



#===========================================================================================================
# Before submission, I recommend you restart your RStudio and run all codes.
# Please ensure that there is no error message in your code. Any code errors will get some points deduction without exception.
#===========================================================================================================


# Congratulations on your completion of the predictive analytics course!
# Wish you the best of luck!
