# Daniel Handojo
# Capstone Project Havardx
# Telecom Churn Prediction
# 04-14-2021



if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos ="http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("readr", repos ="http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("randomForest", repos ="http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("e1071", repos ="http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("gbm", repos ="http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(readr)
library(randomForest)
library(e1071)
library(gbm)
options(digits=3)



#### load the data ####
urlfile="https://raw.githubusercontent.com/SirPopper/havard_DS_1/main/telecom_users.csv"
df <-read_csv(url(urlfile))

head(df)


#### data cleaning ####
str(df)

# changing all character to factors
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
summary(df)

# transform customer id back
df$customerID <- as.character(df$customerID)
df$SeniorCitizen <- as.factor(df$SeniorCitizen)

#removing x1 column
df$X1 <- NULL
df$Churn <- ifelse(df$Churn =="Yes", 1, 0)


# checking NA's
sum(is.na(df))

# removing rows with NA's
df <- df[!is.na(df$TotalCharges),]


### Data exploration ####


# customerID - customer id
# gender - client gender (male / female)
# SeniorCitizen - is the client retired (1, 0)
# Partner - is the client married (Yes, No)
# tenure - how many months a person has been a client of the company
# PhoneService - is the telephone service connected (Yes, No)
# MultipleLines - are multiple phone lines connected (Yes, No, No phone service)
# InternetService - client's Internet service provider (DSL, Fiber optic, No)
# OnlineSecurity - is the online security service connected (Yes, No, No internet service)
# OnlineBackup - is the online backup service activated (Yes, No, No internet service)
# DeviceProtection - does the client have equipment insurance (Yes, No, No internet service)
# TechSupport - is the technical support service connected (Yes, No, No internet service)
# StreamingTV - is the streaming TV service connected (Yes, No, No internet service)
# StreamingMovies - is the streaming cinema service activated (Yes, No, No internet service)
# Contract - type of customer contract (Month-to-month, One year, Two year)
# PaperlessBilling - whether the client uses paperless billing (Yes, No)
# PaymentMethod - payment method (Electronic check, Mailed check, Bank transfer (automatic), Credit card (automatic))
# MonthlyCharges - current monthly payment
# TotalCharges - the total amount that the client paid for the services for the entire time
# Churn - whether there was a churn (Yes or No)



sex <- df %>% group_by(gender) %>% summarize(n=n())

pie(sex$n, labels = c("Female", "Male"), col=brewer.pal(2,"Blues"))
?brewer.pal

# let's check out some interesting plots
sum(df$Churn == "Yes")
df %>% filter(Churn == "Yes") %>% group_by(gender) %>% summarize(n=n())



# check whether gender is says anything about churn 
# Seems like both have about the exact churn which is funny

df %>% ggplot(aes(gender, fill=as.character(Churn))) +
  geom_bar() +
  labs(fill ="Churn") +
  ggtitle("Female vs. Male by Churn")

# check wether being a Senior Citizen makes a difference
#Senior Citizen have a higher churn share
df %>% ggplot(aes(gender, fill=as.character(Churn))) +
  geom_bar() +
  facet_wrap(~ SeniorCitizen) +
  labs(fill ="Churn") +
  ggtitle("Senior Citizens")

# Senior Citizens account for 25.3% of the total churn
df %>% filter(SeniorCitizen == 1, Churn == "Yes") %>% summarize(n=n()/sum(df$Churn=="Yes"))


# checking relationship
# People without a partner and not being a senior citizen tend to have a higher churn
# having a partner reduces the churn?
df %>% ggplot(aes(Partner, fill=as.character(Churn))) +
  geom_bar() +
  facet_wrap(~ SeniorCitizen) +
  labs(fill ="Churn") +
  ggtitle("Senior Citizen")


# It is evident that the longer the tenure the less likely the Churn, which make sense
df %>% ggplot(aes(tenure, fill =as.character(Churn))) +
  geom_bar() +
  facet_wrap(~ Churn) +
  labs(fill ="Churn") +
  ggtitle("Tenure splitted by Churn")


# Let's check whether there is a difference in gender, e.g. if men have less patient
# there is no evident difference in the churn considering gender and tenure
df %>% ggplot(aes(tenure, fill =as.character(Churn))) +
  geom_histogram() +
  facet_wrap(~ gender) +
  labs(fill ="Churn") +
  ggtitle("Tenure splitted by Gender")


# Does Phoneservice change the churn?
df %>% ggplot(aes(PhoneService, fill =as.character(Churn))) +
  geom_bar() +
  labs(fill ="Churn") +
  ggtitle("Phone Service")


# How about streaming tv
# interesting, no internet service has the lowest churn share for streamingtv and movies
df %>% ggplot(aes(StreamingTV, fill =as.character(Churn))) +
  geom_bar() +
  labs(fill ="Churn") +
  ggtitle("StreamingTV")

df %>% ggplot(aes(StreamingMovies, fill =as.character(Churn))) +
  geom_bar() +
  labs(fill ="Churn") +
  ggtitle("StreamingMovies")


# contract type
# the longer the contract the less churn
df %>% ggplot(aes(Contract, fill =as.character(Churn))) +
  geom_bar() +
  labs(fill ="Churn") +
  ggtitle("Contract")

df %>% ggplot(aes(tenure, fill =as.character(Churn))) +
  geom_bar() +
  facet_grid(~Contract) + 
  labs(fill ="Churn") +
  ggtitle("Contract")



#Paperless billing
df %>% ggplot(aes(PaperlessBilling, fill =as.character(Churn))) +
  geom_bar() +
  labs(fill ="Churn") +
  ggtitle("Paperless Billing")


df %>% group_by(PaperlessBilling) %>% 
  (percentage = sum(Churn =="Yes")/n()) %>% 
  select(percentage) %>% 
  slice(1)


# payment method
#  electronic check has the highest churning share
df %>% ggplot(aes(PaymentMethod, fill =as.character(Churn))) +
  geom_bar() +
  labs(fill ="Churn") +
  ggtitle("Payment method")

df %>% ggplot(aes(PaymentMethod, fill =as.character(Churn))) +
  geom_bar() +
  facet_grid(~PaperlessBilling) + 
  labs(fill ="Churn") +
  ggtitle("PaperlessBilling")

df %>% group_by(PaymentMethod) %>% 
  mutate(percentage = sum(Churn =="Yes")/n()) %>% 
  select(percentage) %>% 
  slice(1)


# monthly charges
# there is a zone where it increases
df %>% ggplot(aes(MonthlyCharges, fill =as.character(Churn))) +
  geom_histogram(bins = 30) +
  labs(fill ="Churn") +
  ggtitle("Monthly Charges")


# Total charges
df %>% ggplot(aes(TotalCharges, fill =as.character(Churn))) +
  geom_histogram() +
  labs(fill ="Churn") +
  ggtitle("Total Charges")


df %>% ggplot(aes(log(TotalCharges), fill =as.character(Churn))) +
  geom_histogram() +
  labs(fill ="Churn") +
  ggtitle("Total Charges in Log Scale")

#payment method and total charges
df %>% ggplot(aes(TotalCharges, fill =as.character(Churn))) +
  geom_histogram() +
  facet_wrap(~PaymentMethod) +
  labs(fill ="Churn") +
  ggtitle("Payment Method")

df %>% ggplot(aes(TotalCharges, fill =as.character(Churn))) +
  geom_histogram() +
  facet_wrap(~PaperlessBilling) +
  labs(fill ="Churn") +
  ggtitle("PaperlessBilling")


# payperless billing and payment method has high share of churn
df %>% ggplot(aes(TotalCharges, fill =as.character(Churn))) +
  geom_histogram() +
  facet_wrap(~PaperlessBilling + PaymentMethod) +
  labs(fill ="Churn") +
  ggtitle("PaperlessBilling & Payment Method")




### feature engineering
# create contract + total charges feature

df %>% ggplot(aes(TotalCharges, fill =as.character(Churn))) +
  geom_histogram() +
  facet_grid(~Contract) +
  labs(fill ="Churn") +
  ggtitle("Contract")

feat1 <- df %>% filter(Contract == "Month-to-month") %>% mutate(Contract_Charges_feature = TotalCharges) %>% select(customerID, Contract_Charges)
df <- left_join(df, feat1, by ="customerID")

df$Contract_Charges_feature <- ifelse(is.na(df$Contract_Charges_feature), 0, df$Contract_Charges_feature)



# monthly charges above 60
feat3 <- df %>% filter(MonthlyCharges > 60) %>% mutate(MonthlyCharges_feature = 1) %>% select(customerID, MonthlyCharges_feature)
df <- left_join(df, feat3, by ="customerID")
df$MonthlyCharges_feature <- ifelse(is.na(df$MonthlyCharges_feature), 0, df$MonthlyCharges_feature)


# create payment method + paperless billing variable
feat4 <- df %>% filter(PaperlessBilling=="Yes", PaymentMethod == "Electronic check") %>% 
  mutate(tenure_PB_PM_feature = tenure) %>% 
  select(customerID, tenure_PB_PM_feature)
df <- left_join(df, feat4, by ="customerID")
df$tenure_PB_PM_feature <- ifelse(is.na(df$tenure_PB_PM_feature), 0, df$tenure_PB_PM_feature)


# feature tenure and total charges
feat5 <- df %>% mutate(TCperTenure_feature = TotalCharges/tenure) %>% select(customerID, TCperTenure_feature)
df <- left_join(df, feat5, by="customerID")



# high correlation between total charges and tenure, might be multicollinear
index_num <- unlist(lapply(df, is.numeric))
cor(df[, index_num])


#### Model Building ####



# partition the data


set.seed(1)
test_index <- createDataPartition(y = df$Churn, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- df[-test_index,]
test_set <- df[test_index,]

train_set$customerID <- NULL
test_set$customerID <- NULL




## logistic regression 1
lr <- glm(as.factor(Churn) ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + MultipleLines + 
            InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + 
            StreamingTV + StreamingMovies + Contract + tenure + MonthlyCharges + TotalCharges, data=train_set, family=binomial(link="logit"))
summary(lr)

#prediction
pred <- predict(lr, newdata=test_set, type="response")
pred <- if_else(pred >0.5, 1, 0)

# Balanced Accuracy
CM <- confusionMatrix(as.factor(pred), as.factor(test_set$Churn))
CM
BA_results <- tibble(method = "logistic regression", Accuracy = CM$overall[1], BalancedAccuracy = CM$byClass[11])
BA_results


## logistic regression 2
lr <- glm(as.factor(Churn) ~ ., data=train_set, family=binomial(link="logit"))
summary(lr)

McFaddenR <- 1- lr$deviance/lr$null.deviance
McFaddenR

#prediction
pred <- predict(lr, newdata=test_set, type="response")
pred <- if_else(pred >0.5, 1, 0)

CM <- confusionMatrix(as.factor(pred), as.factor(test_set$Churn))
CM
BA_results <- BA_results %>% add_row(method = "logistic regression with feature engineering",  Accuracy = CM$overall[1], BalancedAccuracy = CM$byClass[11])
BA_results



## logistic regression 3
# balancing the training set 
set.seed(2021)
train_Churn <- train_set %>% filter(Churn == 1)
n <- train_set %>% filter(Churn == 1) %>% nrow()

train_noChurn <- train_set %>% filter(Churn == 0)
train_noChurn <- train_noChurn[sample(seq(1:n), size = n, replace = FALSE), ]

train_new <- rbind(train_Churn, train_noChurn)
train_set <- train_new[sample(seq(1:nrow(train_new)), size = nrow(train_new), replace =FALSE), ]


# train model
lr <- glm(as.factor(Churn) ~ ., data=train_set, family=binomial(link="logit"))
summary(lr)

McFaddenR <- 1- lr$deviance/lr$null.deviance
McFaddenR


# prediction 
# increased BA by a 4% when balancing -> boosting might help
pred_lr <- predict(lr, newdata=test_set, type="response")
pred_lr <- as.factor(if_else(pred_lr >0.5, 1, 0))

CM <- confusionMatrix(as.factor(pred_lr), as.factor(test_set$Churn))
CM

BA_results <- BA_results %>% add_row(method = "logistic regression + balanced train_set",  Accuracy = CM$overall[1], BalancedAccuracy = CM$byClass[11])
BA_results



## logistic regression 4 
# eliminating all NA's bc of dummy variable trap -> multicollinearity
# interestingly the drop is not huge, evidence of multicollinearity
lr <- glm(as.factor(Churn) ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + 
            InternetService + Contract + tenure + MonthlyCharges + TotalCharges, data=train_set, family=binomial(link="logit"))
summary(lr)

#prediction
pred <- predict(lr, newdata=test_set, type="response")
pred <- if_else(pred >0.5, 1, 0)

McFaddenR <- 1- lr$deviance/lr$null.deviance
McFaddenR

# Balanced Accuracy
CM <- confusionMatrix(as.factor(pred), as.factor(test_set$Churn))
CM
BA_results <- BA_results %>% add_row(method = "logistig regression + balanced train_set + w/o NA variables",  Accuracy = CM$overall[1], BalancedAccuracy = CM$byClass[11])
BA_results




## Random Forest 1

rf_train <- train_set %>% select(-Churn)
rf_label <- as.factor(train_set$Churn)
rf_test <- test_set %>% select(-Churn)

set.seed(2021)
rf <- randomForest(x= rf_train, y = rf_label, importance = TRUE, ntree =1000)
rf

varImpPlot(rf)


pred_rf <- predict(rf, newdata = rf_test, type="response")

CM <- confusionMatrix(as.factor(pred_rf), as.factor(test_set$Churn))
CM
BA_results <- BA_results %>% add_row(method = "random forest",  Accuracy = CM$overall[1], BalancedAccuracy = CM$byClass[11])
BA_results



## random Forest fine tuning

# plot out of back error rate
oob.error.data <- data.frame(
  Trees =rep(1:nrow(rf$err.rate), times =3),
  Type=rep(c("OOB", "0", "1"), each=nrow(rf$err.rate)),
  Error=c(rf$err.rate[,"OOB"],
          rf$err.rate[,"0"],
          rf$err.rate[,"1"])
)

ggplot(data =oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(col=Type))


# check how many random features (mtry) is the best 

oob.values <- vector(length=10)
for(i in 1:10){
  temp.model <- randomForest(x = rf_train, y = rf_label, mtry = i, importance = TRUE, ntree = 2000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}

#mtry = 2 best
which.min(oob.values)


# random forest 2
set.seed(2021)
rf <- randomForest(x= rf_train, y = rf_label, importance = TRUE, ntree =2000, mtry = 2)
rf

varImpPlot(rf)


pred <- predict(rf, newdata = rf_test, type="response")

CM <- confusionMatrix(as.factor(pred), as.factor(test_set$Churn))
CM
BA_results <- BA_results %>% add_row(method = "random forest + mtry = 2",  Accuracy = CM$overall[1], BalancedAccuracy = CM$byClass[11])
BA_results


# Auto Gradient Boosting
# gradient boosting combines weak learners to create a strong learner
# predictions are sequential, while each subsequent predictor learns from the errors of the previous predictors

gb_ctrl_specs <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

set.seed(2021)

gb <- train(as.factor(Churn)~., data=train_set, 
            method="gbm", 
            distribution="adaboost", 
            trControl=gb_ctrl_specs,
            verbose=FALSE)


pred_gb <- predict(gb, newdata=test_set)
CM <- confusionMatrix(as.factor(pred_gb), as.factor(test_set$Churn))
CM
BA_results <- BA_results %>% add_row(method = "gradient boosting",  Accuracy = CM$overall[1], BalancedAccuracy = CM$byClass[11])
BA_results



# takes about 20 minutes
# tuning parameters with grid

# grid <- expand.grid(
#   n.trees = c(100, 150, 200),
#   interaction.depth = c(1, 2, 3),
#   shrinkage = c(0.05,0.1,0.2),
#   n.minobsinnode = c(8, 10, 12)
# )
# 
# set.seed(2021)
# gb_tuned <- train(as.factor(Churn)~., data=train_set,
#                   method="gbm",
#                   distribution="adaboost",
#                   trControl=gb_ctrl_specs,
#                   verbose =FALSE,
#                   tuneGrid= grid)
# 
# grid2 <- gb_tuned$bestTune

# n.trees =  200, interaction.depth = 1, shrinkage 0.05, n.minobsinnode = 8
# grid2
# 
# set.seed(2021)
# gb <- train(as.factor(Churn)~., data=train_set, 
#             method="gbm", 
#             distribution="adaboost", 
#             trControl=gb_ctrl_specs,
#             tuneGrid = grid2,
#             verbose=FALSE)
# 
# pred <- predict(gb, newdata=test_set)
# CM <- confusionMatrix(as.factor(pred), as.factor(test_set$Churn))
# CM
# BA_results <- BA_results %>% add_row(method = "gradient boosting + tuned",  Accuracy = CM$overall[1], BalancedAccuracy = CM$byClass[11])
# BA_results



## Lasso regression

# set control specs

ctrl_specs <- trainControl(method ="cv",
                           savePredictions = "all",
                           number = 10)


# create vector for lambdas
lambda <- 10^seq(-5,5,length =500)

lassoreg <- train(as.factor(Churn) ~., data=train_set,
                  method="glmnet",
                  tuneGrid = expand.grid(alpha=1, lambda=lambda),
                  trControl = ctrl_specs,
                  preProcess=c("center", "scale"))

lassoreg$bestTune


# lasso regression coefficients
# lasso does feature selection, those without a number are zeroed out
round(coef(lassoreg$finalModel, lassoreg$bestTune$lambda), 3)

# importance of the variables
varImp(lassoreg)

# plot variable importance
ggplot(varImp(lassoreg)) + 
  ggtitle("Lasso regression variable importance")

#predict
# same BA but random forest is better in predicting churn
pred_lasso <- predict(lassoreg, newdata=test_set)
CM <- confusionMatrix(as.factor(pred_lasso), as.factor(test_set$Churn))
CM
BA_results <- BA_results %>% add_row(method = "lasso regression",  Accuracy = CM$overall[1], BalancedAccuracy = CM$byClass[11])
BA_results


## ridge regression

set.seed(2021)

# set another control specs but alpha = 0 and not 1
ridge_ctrl_specs <-trainControl(method = "cv",
                                savePredictions = "all",
                                number = 10)


ridgereg <- train(as.factor(Churn) ~., data=train_set,
                  method="glmnet",
                  tuneGrid = expand.grid(alpha=0, lambda=lambda),
                  trControl = ridge_ctrl_specs,
                  preProcess=c("center", "scale"))

ridgereg$bestTune

# no feature selection with ridge regression
round(coef(ridgereg$finalModel, ridgereg$bestTune$lambda), 3)

#variable importance
varImp(ridgereg)

#plot 
ggplot(varImp(ridgereg)) + 
  ggtitle("Ridge regression variable importance")

#predict
# generally worse BA but better Sensitivity for ridge regression than lasso
pred_ridge <- predict(ridgereg, newdata=test_set)
CM <- confusionMatrix(as.factor(pred_ridge), as.factor(test_set$Churn))
CM
BA_results <- BA_results %>% add_row(method = "ridge regression",  Accuracy = CM$overall[1], BalancedAccuracy = CM$byClass[11])
BA_results


## Naive Bayes

nb <- naiveBayes(as.factor(Churn) ~., data=train_set, laplace=1)

pred_nb <- predict(nb, newdata=test_set)
CM <- confusionMatrix(as.factor(pred), as.factor(test_set$Churn))
CM
BA_results <- BA_results %>% add_row(method = "naive bayes", Accuracy = CM$overall[1],   BalancedAccuracy = CM$byClass[11])
BA_results





# ensemble
ensemble <- cbind(logreg = pred_lr == 1, rf = pred_rf == 1, adaboost = pred_gb == 1, 
                  lasso = pred_lasso == 1, ridge = pred_ridge == 1, nb = pred_nb == 1)

ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, 1, 0)
CM <- confusionMatrix(as.factor(ensemble_preds), as.factor(test_set$Churn))
CM
BA_results <- BA_results %>% add_row(method = "ensemble",  Accuracy = CM$overall[1], BalancedAccuracy = CM$byClass[11])
BA_results


