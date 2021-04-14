# Daniel Handojo
# Capstone Project Havardx
# Telecom Churn Prediction
# 04-14-2021


# 1. Introduction


# load all relevant libraries and the data
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
if(!require(RColorBrewer)) install.packages("knitr", repos ="http://cran.us.r-project.org")

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
library(knitr)



#### 1. load the data ####
urlfile="https://raw.githubusercontent.com/SirPopper/havard_DS_1/main/telecom_users.csv"
df <-read_csv(url(urlfile))


## 1.1 Data cleaning


# customerID - customer id\
# gender - client gender (male / female)\
# SeniorCitizen - is the client retired (1, 0)\
# Partner - is the client married (Yes, No)\
# tenure - how many months a person has been a client of the company\
# PhoneService - is the telephone service connected (Yes, No)\
# MultipleLines - are multiple phone lines connected (Yes, No, No phone service)\
# InternetService - client's Internet service provider (DSL, Fiber optic, No)\
# OnlineSecurity - is the online security service connected (Yes, No, No internet service)\
# OnlineBackup - is the online backup service activated (Yes, No, No internet service)\
# DeviceProtection - does the client have equipment insurance (Yes, No, No internet service)\
# TechSupport - is the technical support service connected (Yes, No, No internet service)\
# StreamingTV - is the streaming TV service connected (Yes, No, No internet service)\
# StreamingMovies - is the streaming cinema service activated (Yes, No, No internet service)\
# Contract - type of customer contract (Month-to-month, One year, Two year)\
# PaperlessBilling - whether the client uses paperless billing (Yes, No)\
# PaymentMethod - payment method (Electronic check, Mailed check, Bank transfer (automatic), Credit card (automatic))\
# MonthlyCharges - current monthly payment\
# TotalCharges - the total amount that the client paid for the services for the entire time\
# Churn - whether there was a churn (Yes or No)\



#Cleaning the data.\

str(df)

df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)





#Transofrm back customerID into character
df$customerID <- as.character(df$customerID)

#Transform senior citizen into factor
df$SeniorCitizen <- as.factor(df$SeniorCitizen)

# eliminate X1 variable as we don't need it
df$X1 <- NULL

# Change the Churn into binary variable
df$Churn <- as.factor(ifelse(df$Churn =="Yes", 1, 0))

# checking NA's
sum(is.na(df))

# removing rows with NA's
df <- df[!is.na(df$TotalCharges),]




#### 2. Analysis ####

## 2.1 Data exploration
sex <- df %>% group_by(gender) %>% summarize(n=round(100*n()/nrow(df),1))
pie(sex$n, main ="Gender distribution", labels = sex$gender, col=brewer.pal(3,"Blues"))


# All Churns in our dataset
df %>% filter(Churn == 1)  %>% nrow() %>% 
  knitr::kable(format="pipe", caption="Number of Churns")

# Churn divided by gender
df %>% filter(Churn == 1) %>% group_by(gender) %>% summarize(n=n()) %>% 
  knitr::kable(format="pipe", caption="Churn divided by gender")


# It doesn't seem that gender has an impact on the churn rate.\
df %>% ggplot(aes(gender, fill=as.character(Churn))) +
  geom_bar() +
  labs(fill ="Churn") +
  ggtitle("Female vs. Male by Churn") +
  scale_fill_brewer(palette = "Greens")  +
  theme_minimal()


# check wether being a Senior Citizen makes a difference
df %>% ggplot(aes(gender, fill=as.character(Churn))) +
  geom_bar() +
  facet_wrap(~ SeniorCitizen) +
  labs(fill ="Churn") +
  ggtitle("Senior Citizens by gender") +
  scale_fill_brewer(palette = "Greens")  +
  theme_minimal()


# Senior Citizens account for 25.3% of the total churn\
df %>% filter(SeniorCitizen == 1, Churn == 1) %>% summarize(n=n()/sum(df$Churn==1)) %>% 
  knitr::kable(format="pipe", caption="Percentage Senior Citizen Churn")


# When checking for the relationship status, we see that people without a partner and not being a senior citizen tend to have a higher churn share. Does having a partner reduce the churn?\
df %>% ggplot(aes(Partner, fill=as.character(Churn))) +
  geom_bar() +
  facet_wrap(~ SeniorCitizen) +
  labs(fill ="Churn") +
  ggtitle("Senior Citizen by Partner")  +
  scale_fill_brewer(palette = "Greens")  +
  theme_minimal()


#It is evident that the longer the tenure the less likely the Churn, which makes sense. People with a long tenure are probably satisfied with their contract or are just too lazy to look for a new one.\
df %>% ggplot(aes(tenure, fill =as.character(Churn))) +
  geom_bar() +
  facet_wrap(~ Churn) +
  labs(fill ="Churn") +
  ggtitle("Tenure splitted by Churn") +
  scale_fill_brewer(palette = "PuBu")  +
  theme_minimal()


# There is no evident difference in the churn considering gender and tenure.\
df %>% ggplot(aes(tenure, fill =as.character(Churn))) +
  geom_histogram() +
  facet_wrap(~ gender) +
  labs(fill ="Churn") +
  ggtitle("Tenure splitted by Gender") + 
  scale_fill_brewer(palette = "PuBu")  +
  theme_minimal()


# Does having a phone service change the churn? 
df %>% ggplot(aes(PhoneService, fill =as.character(Churn))) +
  geom_bar() +
  labs(fill ="Churn") +
  ggtitle("Phone Service by category") +
  scale_fill_brewer(palette = "Greens")  +
  theme_minimal()


#Interesting, no internet service has the lowest churn share for streaming tv and movies.\

df %>% ggplot(aes(StreamingTV, fill =as.character(Churn))) +
  geom_bar() +
  labs(fill ="Churn") +
  ggtitle("StreamingTV by category") +
  scale_fill_brewer(palette = "Greens")  +
  theme_minimal()

df %>% ggplot(aes(StreamingMovies, fill =as.character(Churn))) +
  geom_bar() +
  labs(fill ="Churn") +
  ggtitle("StreamingMovies by category") +
  scale_fill_brewer(palette = "Greens")  +
  theme_minimal()


# The longer the contract the less the churn. This might be interesting for our random forest later, as it will take each category as own branch. Recognizing that a Month-to-month contract has a quite high churn rate in comparison to the two other contract lengths, it might be even reasonable to combine this variable with another variable to create a feature that could be more meaningful. \
df %>% ggplot(aes(Contract, fill =as.character(Churn))) +
  geom_bar() +
  labs(fill ="Churn") +
  ggtitle("Contract by category") +
  scale_fill_brewer(palette = "Greens")  +
  theme_minimal()


df %>% ggplot(aes(tenure, fill =as.character(Churn))) +
  geom_bar() +
  facet_grid(~Contract) + 
  labs(fill ="Churn") +
  ggtitle("Contract splitted by category with tenure as x axis") +
  scale_fill_brewer(palette = "PuBu")  +
  theme_minimal()


#One can assume that paperless billing may increase the churn rate, as this would be related to online contract management. It should be easier to cancel contracts this way.\
df %>% ggplot(aes(PaperlessBilling, fill =as.character(Churn))) +
  geom_bar() +
  labs(fill ="Churn") +
  ggtitle("Paperless Billing by category") +
  scale_fill_brewer(palette = "Greens")  +
  theme_minimal()



#Indeed, almost twice as many customers cancel their phone service when they have paperless billing than no paperless billing. Canceling contracts when billing is managed digital might me more convenient. \
df %>% group_by(PaperlessBilling) %>% 
  mutate(percentage = sum(Churn == 1)/n()) %>% 
  select(percentage) %>% 
  slice(1) %>% 
  knitr::kable(format ="pipe", caption="Percentage of Churn by Paperless Billing")



#The same scenario can be found when looking at the payment method: Even though Bank transfer and credit cards can also be managed online, the electronic check seems the most convenient way to manage ones telecom subscription. Unfortunately, it is also the most convenient to cancel the subscription. \
df %>% ggplot(aes(PaymentMethod, fill =as.character(Churn))) +
  geom_bar() +
  labs(fill ="Churn") +
  ggtitle("Payment method") +
  scale_fill_brewer(palette = "Greens")  +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Calculating the churn rates by method, we see that 45% of people churn! That is a lot. Surprisingly, the churn rate for mailed check is higher than for automatic bank transfer and automatic credit card payments. \
df %>% group_by(PaymentMethod) %>% 
  mutate(percentage = sum(Churn ==1)/n()) %>% 
  select(percentage) %>% 
  slice(1) %>% 
  knitr::kable(format = "pipe", caption ="Churn rate by Payment Method")


#I hoped to generate a group of variables where the majority is made out of churned customers. However, putting payment method and paperless billing doesn't give us more knowledge than before. Nevertheless it might be interesting to connect these two variables into a new feature. \
df %>% ggplot(aes(PaymentMethod, fill =as.character(Churn))) +
  geom_bar() +
  facet_grid(~PaperlessBilling) + 
  labs(fill ="Churn") +
  ggtitle("PaperlessBilling splitted by Payment Methods") +
  scale_fill_brewer(palette = "Greens")  +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#When looking the monthly charges, it seems that there is a zone at above 60, that leads to an increase of the churn share.\
df %>% ggplot(aes(MonthlyCharges, fill =as.character(Churn))) +
  geom_histogram(bins = 30) +
  labs(fill ="Churn") +
  ggtitle("Monthly Charges") +
  scale_fill_brewer(palette = "Greens")  +
  theme_minimal()


#I am a fan of numerical variables, as they will definitely help our models later, especially something like total charges.\
df %>% ggplot(aes(TotalCharges, fill =as.character(Churn))) +
  geom_histogram() +
  labs(fill ="Churn") +
  ggtitle("Total Charges") +
  scale_fill_brewer(palette = "Greens")  +
  theme_minimal()



### Let's also investigate interrelationships between other promising variables

# There is a very high share in electronic check as payment method as we saw earlier. It seems that this churn share is distributed through all total charges.\
df %>% ggplot(aes(TotalCharges, fill =as.character(Churn))) +
  geom_histogram() +
  facet_wrap(~PaymentMethod) +
  labs(fill ="Churn") +
  ggtitle("Payment Method") +
  scale_fill_brewer(palette = "PuBu")  +
  theme_minimal()


#Plotting paperless billing against total charges, we can perceive the distribution of churn rates on the total charges. If a customer has paperless billing, the churn rate decreases with increasing total charges. However, without paperless billing the churn rate stays approximately the same after total charges of approximately 1200. \
df %>% ggplot(aes(TotalCharges, fill =as.character(Churn))) +
  geom_histogram() +
  facet_wrap(~PaperlessBilling) +
  labs(fill ="Churn") +
  ggtitle("PaperlessBilling") +
  scale_fill_brewer(palette = "PuBu")  +
  theme_minimal()


#Now we plot paperless billing and payment methods across total charges. When we see that somebody goes fully digital, meaning having an electronic check as payment method and paperless billing, we see that the churn share is pretty high.\
df %>% ggplot(aes(TotalCharges, fill =as.character(Churn))) +
  geom_histogram() +
  facet_wrap(~PaperlessBilling + PaymentMethod) +
  labs(fill ="Churn") +
  ggtitle("PaperlessBilling & Payment Method") +
  scale_fill_brewer(palette = "PuBu")  +
  theme_minimal()




### 2.2 Feature engineering 

# With our gained insights, we now are going to create some new features that might help our models to perform better. The algorithms won't take into account combinations of variables by itself. Therefore, we have to think of smart combinations which might boost the balanced accuracy of our models later on.\
df %>% ggplot(aes(TotalCharges, fill =as.character(Churn))) +
  geom_histogram() +
  facet_grid(~Contract) +
  labs(fill ="Churn") +
  ggtitle("Contract") +
  scale_fill_brewer(palette = "PuBu")  +
  theme_minimal()


#We create a feature that accounts for month-to-month contract with the respective charges in order to give that relationship more weight.\
# create new feature
feat1 <- df %>% 
  filter(Contract == "Month-to-month") %>% 
  mutate(Contract_Charges_feature = TotalCharges) %>% 
  select(customerID, Contract_Charges_feature)

# join with dataset
df <- left_join(df, feat1, by ="customerID")

# transform all NA's
df$Contract_Charges_feature <- ifelse(is.na(df$Contract_Charges_feature), 0, 
                                      df$Contract_Charges_feature)



#We saw earlier that monthly charges at above 60 has a higher churn share. Let's also create a variable for that.\
# create new feature
feat2 <- df %>% 
  filter(MonthlyCharges > 60) %>% 
  mutate(MonthlyCharges_feature = 1) %>% 
  select(customerID, MonthlyCharges_feature)

# join with dataset
df <- left_join(df, feat2, by ="customerID")

# transform NA's
df$MonthlyCharges_feature <- ifelse(is.na(df$MonthlyCharges_feature), 0, 
                                    df$MonthlyCharges_feature)



#We will also account for the fully digital people, that is customers with paperless billing and electronic check as payment method.\
# create feature
feat3 <- df %>% 
  filter(PaperlessBilling=="Yes", PaymentMethod == "Electronic check") %>%
  mutate(tenure_PB_PM_feature = tenure) %>%
  select(customerID, tenure_PB_PM_feature)

# join with dataset
df <- left_join(df, feat3, by ="customerID")

# transform NA's
df$tenure_PB_PM_feature <- ifelse(is.na(df$tenure_PB_PM_feature), 0, 
                                  df$tenure_PB_PM_feature)



# Next, we create a monthly charges per tenure and total charges per tenure variable.\
# Total charges per tenure
feat4 <- df %>% 
  mutate(TCperTenure_feature = TotalCharges/tenure) %>% 
  select(customerID, TCperTenure_feature)

#join dataset
df <- left_join(df, feat4, by="customerID")

# Monthly charges per tenur 
feat5 <- df %>% 
  mutate(MCperTenure_feature = MonthlyCharges/tenure) %>% 
  select(customerID, MCperTenure_feature)

#join datset
df <- left_join(df, feat5, by="customerID")


#### 3. Model building and results ####

index_num <- unlist(lapply(df, is.numeric))
cor(df[, index_num])

heatmap(cor(df[, index_num]), scale="column")


## Data Split
To check which models are better we create a 80/20 train-test-split. This split seems reasonable as the dataset is not very huge and we need some data to train our model with.\

set.seed(2021)
test_index <- createDataPartition(y = df$Churn, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- df[-test_index,]
test_set <- df[test_index,]

train_set$customerID <- NULL
test_set$customerID <- NULL


## 3.1 Logistic Regression 1

## logistic regression 1
lr <- glm(as.factor(Churn) ~ gender + SeniorCitizen + Partner + Dependents + 
            PhoneService + MultipleLines + InternetService + OnlineSecurity + 
            OnlineBackup + DeviceProtection + TechSupport + StreamingTV + 
            StreamingMovies + Contract + tenure + MonthlyCharges + TotalCharges, 
          data=train_set, family=binomial(link="logit"))


# McFadden
McFaddenR <- 1- lr$deviance/lr$null.deviance
McFaddenR


#prediction
pred <- predict(lr, newdata=test_set, type="response")
pred <- if_else(pred >0.5, 1, 0)

# Balanced Accuracy
CM <- confusionMatrix(as.factor(pred), as.factor(test_set$Churn))



# keping track of the results
BA_results <- tibble(method = "(1) logistic regression", 
                     Accuracy = CM$overall[1], 
                     BalancedAccuracy = CM$byClass[11])

BA_results %>% knitr::kable(format="pipe", caption="Model results")


## 3.2 Logistic Regression 2

#In this model, we want to take into account our created features as well. \

## logistic regression 2
lr <- glm(as.factor(Churn) ~ ., data=train_set, family=binomial(link="logit"))



# Our McFaddenR increased!

McFaddenR <- 1- lr$deviance/lr$null.deviance
McFaddenR


# We were able to increase our BA by 0.01%. Our Contract Charges feature and monthly charge per tenure feature are significant at a 0.01%-significance level, while the others are not significant at a 10%-signifcance level. \
#prediction
pred <- predict(lr, newdata=test_set, type="response")
pred <- if_else(pred >0.5, 1, 0)

CM <- confusionMatrix(as.factor(pred), as.factor(test_set$Churn))


#storing the results
BA_results <- BA_results %>% 
  add_row(method = "(2) logistic regression with feature engineering",  
          Accuracy = CM$overall[1], BalancedAccuracy = CM$byClass[11])
BA_results %>% knitr::kable(format="pipe", caption="Model results")



## 3.3 Logistic regression 3

#As there are a lot more non-churn customers than churn customers, we balance the train set, such that our model has the chance to have a larger train share for the churned customers. Balancing the dataset is a common procedure, as having too less data on one label makes it difficult for the algorithms to learn. Thus the models might overfit on the label that has the bigger share in the train set. \

# logistic regression 3
# balancing the training set 
train_Churn <- train_set %>% filter(Churn == 1)
n <- train_set %>% filter(Churn == 1) %>% nrow()

train_noChurn <- train_set %>% filter(Churn == 0)

set.seed(2021)
train_noChurn <- train_noChurn[sample(seq(1:n), 
                                      size = n, 
                                      replace = FALSE), ]

train_new <- rbind(train_Churn, train_noChurn)

set.seed(2021)
train_set <- train_new[sample(seq(1:nrow(train_new)), 
                              size = nrow(train_new), 
                              replace =FALSE), ]


# train model
lr <- glm(as.factor(Churn) ~ ., data=train_set, family=binomial(link="logit"))


# We could increase our McFaddenR by 2% which is not bad! \


McFaddenR <- 1- lr$deviance/lr$null.deviance
McFaddenR


### From now, we will use the balanced train set for all following models. \


# prediction 
# increased BA by a 4% when balancing -> boosting might help
pred_lr <- predict(lr, newdata=test_set, type="response")
pred_lr <- as.factor(if_else(pred_lr >0.5, 1, 0))

CM <- confusionMatrix(as.factor(pred_lr), as.factor(test_set$Churn))



#storing the results
BA_results <- BA_results %>% 
  add_row(method = "(3) logistic regression + balanced train_set",
          Accuracy = CM$overall[1], BalancedAccuracy = CM$byClass[11])
BA_results %>% knitr::kable(format="pipe", caption="Model results")



## 3.4 Logistic Regression 4
#How will our model change, if we take out all the insignificant variables and the dummy variable trap? \


## logistic regression 4 
# eliminating all NA's bc of dummy variable trap -> multicollinearity
# interestingly the drop is not huge, evidence of multicollinearity
lr <- glm(as.factor(Churn) ~ gender + SeniorCitizen + Partner + Dependents + 
            PhoneService + InternetService + Contract + tenure + MonthlyCharges + 
            TotalCharges + Contract_Charges_feature + MCperTenure_feature , 
          data=train_set, family=binomial(link="logit"))


#Our McFaddenR decreased by 3%. \
McFaddenR <- 1- lr$deviance/lr$null.deviance
McFaddenR


#However, we could increase our BA to 74%. We are on the right track!\

#prediction
pred <- predict(lr, newdata=test_set, type="response")
pred <- if_else(pred >0.5, 1, 0)

# Balanced Accuracy
CM <- confusionMatrix(as.factor(pred), as.factor(test_set$Churn))



BA_results <- BA_results %>% 
  add_row(method = "(4) logistic regression + balanced train_set + w/o NA variables",  
          Accuracy = CM$overall[1], BalancedAccuracy = CM$byClass[11])
BA_results %>% knitr::kable(format="pipe", caption="Model results")




## 3.5 Random Forest 1

#Let's check how random forest will do on our dataset. \


## Random Forest 1
rf_train <- train_set %>% select(-Churn)
rf_label <- as.factor(train_set$Churn)
rf_test <- test_set %>% select(-Churn)

# train model
set.seed(2021)
rf <- randomForest(x= rf_train, y = rf_label, importance = TRUE, ntree =1000)
rf

# varImplot
varImpPlot(rf)

#prediction
pred_rf <- predict(rf, newdata = rf_test, type="response")

CM <- confusionMatrix(as.factor(pred_rf), as.factor(test_set$Churn))




BA_results <- BA_results %>% add_row(method = "(5) random forest",  
                                     Accuracy = CM$overall[1], 
                                     BalancedAccuracy = CM$byClass[11])
BA_results %>% knitr::kable(format="pipe", caption="Model results")



## 3.6 Random Forest with fine tuning

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
  temp.model <- randomForest(x = rf_train, y = rf_label,
                             mtry = i, 
                             importance = TRUE, 
                             ntree = 1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}

which.min(oob.values)


# Training the model after hyperparameter tuning.
# random forest 2
set.seed(2021)
rf <- randomForest(x= rf_train, y = rf_label, 
                   importance = TRUE, ntree =2000, 
                   mtry = which.min(oob.values))
rf



# Tenure got more important in both rankings. However, we see the same variables in the upper ranks as in our previous random forest. \
varImpPlot(rf)

pred <- predict(rf, newdata = rf_test, type="response")

CM <- confusionMatrix(as.factor(pred), as.factor(test_set$Churn))
CM



BA_results <- BA_results %>% add_row(method = "(6) random forest + mtry = 2",  
                                     Accuracy = CM$overall[1], 
                                     BalancedAccuracy = CM$byClass[11])
BA_results %>% knitr::kable(format="pipe", caption="Model results")


## 3.7 Gradient Boosting
# gradient boosting combines weak learners to create a strong learner
# predictions are sequential, while each subsequent predictor learns from 
# the errors of the previous predictors

# to control nuances of train function
set.seed(2021)
gb_ctrl_specs <- trainControl(method = "cv", number = 10)

#train the gradient boosting with the "auto" function
set.seed(2021)
gb <- train(as.factor(Churn)~., data=train_set, 
            method="gbm", 
            distribution="adaboost", 
            trControl=gb_ctrl_specs,
            verbose=FALSE)

# predict and confusion matrix
pred_gb <- predict(gb, newdata=test_set)
CM <- confusionMatrix(as.factor(pred_gb), as.factor(test_set$Churn))


BA_results <- BA_results %>% add_row(method = "(7) gradient boosting",  
                                     Accuracy = CM$overall[1], 
                                     BalancedAccuracy = CM$byClass[11])
BA_results %>% knitr::kable(format="pipe", caption="Model results")


## 3.8 Gradient Boosting with Fine Tuning

#lets check the hyperparameters of the previous model first 
gb$bestTune



# I could have set up some vectors for the grid search. However, this part would take a lot of time (about 30 minutes on my laptop). 
# Therefore, I skipped this part here and just show the results that were outputted as the best hyperparameters. 


set.seed(2021)

grid <- expand.grid(
  n.trees = 3000,
  interaction.depth = 1,
  shrinkage = 0.001,
  n.minobsinnode = 20
)


set.seed(2021)
gb_tuned <- train(as.factor(Churn)~., data=train_set,
                  method="gbm",
                  distribution="adaboost",
                  trControl=gb_ctrl_specs,
                  verbose =FALSE,
                  tuneGrid= grid)

pred <- predict(gb_tuned, newdata=test_set)
CM <- confusionMatrix(as.factor(pred), as.factor(test_set$Churn))



BA_results <- BA_results %>% add_row(method = "(8) gradient boosting + tuned",
Accuracy = CM$overall[1], BalancedAccuracy = CM$byClass[11])
BA_results %>% knitr::kable(format="pipe", caption="Model results")



## 3.9 Lasso regression

# set control sepcs
set.seed(2021)
ctrl_specs <- trainControl(method ="cv",
                           savePredictions = "all",
                           number = 10)


# create vector for lambdas
lambda <- 10^seq(-5,5,length =500)

set.seed(2021)
lassoreg <- train(as.factor(Churn) ~., data=train_set,
                  method="glmnet",
                  tuneGrid = expand.grid(alpha=1, lambda=lambda),
                  trControl = ctrl_specs,
                  preProcess=c("center", "scale"))

lassoreg$bestTune


# lasso regression coefficients
# lasso does feature selection, those without a number are zeroed out
round(coef(lassoreg$finalModel, lassoreg$bestTune$lambda), 3)


# plot variable importance
ggplot(varImp(lassoreg)) + 
  ggtitle("Lasso regression variable importance")


#predict
# same BA but random forest is better in predicting churn
pred_lasso <- predict(lassoreg, newdata=test_set)
CM <- confusionMatrix(as.factor(pred_lasso), as.factor(test_set$Churn))




BA_results <- BA_results %>% add_row(method = "(9) lasso regression",  
                                     Accuracy = CM$overall[1], 
                                     BalancedAccuracy = CM$byClass[11])
BA_results %>% knitr::kable(format="pipe", caption="Model results")


## 3.10 Ridge regression


set.seed(2021)
# set another control specs but alpha = 0 and not 1
ridge_ctrl_specs <-trainControl(method = "cv",
                                savePredictions = "all",
                                number = 10)

set.seed(2021)
ridgereg <- train(as.factor(Churn) ~., data=train_set,
                  method="glmnet",
                  tuneGrid = expand.grid(alpha=0, lambda=lambda),
                  trControl = ridge_ctrl_specs,
                  preProcess=c("center", "scale"))

ridgereg$bestTune


# no feature selection with ridge regression
round(coef(ridgereg$finalModel, ridgereg$bestTune$lambda), 3)


#plot
ggplot(varImp(ridgereg)) +
  ggtitle("Ridge regression variable importance")


pred_ridge <- predict(ridgereg, newdata=test_set)
CM <- confusionMatrix(as.factor(pred_ridge), as.factor(test_set$Churn))



BA_results <- BA_results %>% add_row(method = "(10) ridge regression",  
                                     Accuracy = CM$overall[1], 
                                     BalancedAccuracy = CM$byClass[11])
BA_results %>% knitr::kable(format="pipe", caption="Model results")



## 3.11 Naive Bayes
set.seed(2021)
nb <- naiveBayes(as.factor(Churn) ~., data=train_set, laplace=1)

pred_nb <- predict(nb, newdata=test_set)
CM <- confusionMatrix(as.factor(pred), as.factor(test_set$Churn))




BA_results <- BA_results %>% add_row(method = "(11) naive bayes", 
                                     Accuracy = CM$overall[1],   
                                     BalancedAccuracy = CM$byClass[11])
BA_results %>% knitr::kable(format="pipe", caption="Model results")


## 3.12 Ensemble
# ensemble
ensemble <- cbind(logreg = pred_lr == 1, rf = pred_rf == 1, adaboost = pred_gb == 1, 
                  lasso = pred_lasso == 1, ridge = pred_ridge == 1, nb = pred_nb == 1)

ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, 1, 0)
CM <- confusionMatrix(as.factor(ensemble_preds), as.factor(test_set$Churn))



BA_results <- BA_results %>% add_row(method = "(12) ensemble",  
                                     Accuracy = CM$overall[1], 
                                     BalancedAccuracy = CM$byClass[11])
BA_results %>% knitr::kable(format="pipe", caption="Model results")



##### 4. Results ####

kable(BA_results %>% arrange(desc(BalancedAccuracy)))

