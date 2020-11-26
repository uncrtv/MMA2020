
#Import packages and libraries
easypackages::packages("data.table", "DT", "haven", "lubridate", "summarytools", "Hmisc", "dplyr", "RCurl","sqldf","dummies","tidyr","purrr","mice","caret","tidyverse","readxl")
options(scipen = 999)

install.packages("ROSE")

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} 

pacman::p_load('sqldf', 'readxl', 'mice', 'dummies', 'car', "caret","ROCR","lift","glmnet","MASS","e1071","randomForest","nnet","xgboost") 
library(sqldf)
library(readxl)
library(dplyr)
library(mice)
library(dummies)
library(caret)
library(car)
library(ROSE)

# setwd("C:\\Users\\sunny\\Desktop\\MMA\\MMA 867\\Assignment 3\\")
source('mma867a3/mma867a3.R')

#Import credit data
#credit<-read_excel(file.choose())
credit <- read_excel("MMA867 A3 -- credit data.xlsx")
#There are 24000 records in credit data
dim(credit)

#Import new applicants data
#new<-read_excel(file.choose())
new <- read_excel("MMA867 A3 -- new applications.xlsx")
#There are 1000 records in credit data
dim(new)

#Add default_0 column to new applicant dataset and set all decault value to 0
new$default_0<-0

#Combine credit data and new data
data<-rbind(credit,new)

# Check missing values in data. No missing values.
md.pattern(data) 

#Check format of the data
str(data)

# skew.score <- function(c, x) (skewness(log(x + c)))^2

## Feature engineering
#Covert sex to male indicator
data$male_indicator<-ifelse(data$SEX==1,1,0)

#Add marrige indicator
data$marriage_indicator<-ifelse(data$MARRIAGE==1,1,0)



#Add positive bill balance indiactor
data$positive_balance_1<-ifelse(data$BILL_AMT1>0,1,0)
data$positive_balance_2<-ifelse(data$BILL_AMT2>0,1,0)
data$positive_balance_3<-ifelse(data$BILL_AMT3>0,1,0)
data$positive_balance_4<-ifelse(data$BILL_AMT4>0,1,0)
data$positive_balance_5<-ifelse(data$BILL_AMT5>0,1,0)
data$positive_balance_6<-ifelse(data$BILL_AMT6>0,1,0)

# num of positive balance
data$positive_balance_count<-data$positive_balance_1+data$positive_balance_2+data$positive_balance_3+data$positive_balance_4+data$positive_balance_5+data$positive_balance_6



## Feel free to add more new variables and do more data enginnering here (before data partition)##
#Below are the features added by Chelsea
#difference between bill amount and paid amount
data$diff_1 <- data$BILL_AMT2 - data$PAY_AMT1
data$diff_2 <- data$BILL_AMT3 - data$PAY_AMT2
data$diff_3 <- data$BILL_AMT4 - data$PAY_AMT3
data$diff_4 <- data$BILL_AMT5 - data$PAY_AMT4
data$diff_5 <- data$BILL_AMT6 - data$PAY_AMT5

# Average % paid off
data$avg_diff <- apply(data[,35:39], 1, mean, na.rm = TRUE)
data$max_diff <- apply(data[,35:39], 1, max, na.rm = TRUE)

# % of bill paid off each month 
data$diff_p1 <- ifelse(is.nan(data$PAY_AMT1/data$BILL_AMT2),0,data$PAY_AMT1/data$BILL_AMT2)
data$diff_p1 <- ifelse(is.infinite(data$diff_p1),0,data$diff_p1)
data$diff_p2 <- ifelse(is.nan(data$PAY_AMT2/data$BILL_AMT3),0,data$PAY_AMT2/data$BILL_AMT3)
data$diff_p2 <- ifelse(is.infinite(data$diff_p2),0,data$diff_p2)
data$diff_p3 <- ifelse(is.nan(data$PAY_AMT3/data$BILL_AMT4),0,data$PAY_AMT3/data$BILL_AMT4)
data$diff_p3 <- ifelse(is.infinite(data$diff_p3),0,data$diff_p3)
data$diff_p4 <- ifelse(is.nan(data$PAY_AMT4/data$BILL_AMT5),0,data$PAY_AMT4/data$BILL_AMT5)
data$diff_p4 <- ifelse(is.infinite(data$diff_p4),0,data$diff_p4)
data$diff_p5 <- ifelse(is.nan(data$PAY_AMT5/data$BILL_AMT6),0,data$PAY_AMT5/data$BILL_AMT6)
data$diff_p5 <- ifelse(is.infinite(data$diff_p5),0,data$diff_p5)

# Average % paid off
data$AVG_PAIDOFF <- apply(data[,42:46], 1, mean, na.rm = TRUE)

#add negative indicator to diff
data$diff_n1 <- ifelse(data$diff_1 <= 0,1,0)
data$diff_n2 <- ifelse(data$diff_2 <= 0,1,0)
data$diff_n3 <- ifelse(data$diff_3 <= 0,1,0)
data$diff_n4 <- ifelse(data$diff_4 <= 0,1,0)
data$diff_n5 <- ifelse(data$diff_5 <= 0,1,0)

# negative indicator to diff
data$negative_diff_count<-data$diff_n1+data$diff_n2+data$diff_n3+data$diff_n4+data$diff_n5
#best.1 <- optimise(skew.score, c(0, 1000000), x = data$diff_1)$minimum
#data$diff_1 <- log(data$diff_1+best.1)
#best.2 <- optimise(skew.score, c(0, 10000000), x = data$diff_2)$minimum
#data$diff_2 <- log(data$diff_2+best.2)
#best.3 <- optimise(skew.score, c(0, 10000000), x = data$diff_3)$minimum
#data$diff_3 <- log(data$diff_3+best.3)
#best.4 <- optimise(skew.score, c(0, 10000000), x = data$diff_4)$minimum
#data$diff_4 <- log(data$diff_4+best.4)
#best.5 <- optimise(skew.score, c(0, 10000000), x = data$diff_5)$minimum
#data$diff_5 <- log(data$diff_5+best.5)


#percentage of used limit
data$limit_use <- data$BILL_AMT1 / data$LIMIT_BAL

#create buckets for age
hist(data$AGE)
data$age_26 <- ifelse(data$AGE <= 26,1,0)
data$age_27_31 <- ifelse(data$AGE >=27 & data$AGE <= 31,1,0)
data$age_32_36 <- ifelse(data$AGE >=32 & data$AGE <= 36,1,0)
data$age_37_43 <- ifelse(data$AGE >=37 & data$AGE <= 43,1,0)
data$age_44 <- ifelse(data$AGE >=44,1,0)

#create buckets for limit
hist(data$LIMIT_BAL)
data$limit_5 <- ifelse(data$LIMIT_BAL <= 50000,1,0)
data$limit_6_11 <- ifelse(data$LIMIT_BAL > 50000 & data$AGE <= 110000,1,0)
data$limit_12_18 <- ifelse(data$LIMIT_BAL > 110000 & data$AGE <= 180000,1,0)
data$limit_19_27 <- ifelse(data$LIMIT_BAL > 180000 & data$AGE <= 270000,1,0)
data$limit_28 <- ifelse(data$LIMIT_BAL > 270000,1,0)

# Feature Engineering NICK ------------------------------------------------
# # Converting EDUCATION into education level
# data$EDUCATION <- ifelse(data$EDUCATION == 1, "Graduate", data$EDUCATION)
# data$EDUCATION <- ifelse(data$EDUCATION == 2, "Undergraduate", data$EDUCATION)
# data$EDUCATION <- ifelse(data$EDUCATION == 3, "High School", data$EDUCATION)
# data$EDUCATION <- ifelse(data$EDUCATION == 4, "Other", data$EDUCATION)
# data$EDUCATION <- ifelse(data$EDUCATION == 5, "Unknown", data$EDUCATION)
# data$EDUCATION <- ifelse(data$EDUCATION == 6, "Unknown", data$EDUCATION)
# 
# # Converting MARRIAGE into marital status
# data$MARRIAGE <- ifelse(data$MARRIAGE == 1, "Married", data$MARRIAGE)
# data$MARRIAGE <- ifelse(data$MARRIAGE == 2, "Single", data$MARRIAGE)
# data$MARRIAGE <- ifelse(data$MARRIAGE == 3, "Other", data$MARRIAGE)
# 
# # Converting PAY_x to what it actually means
# data$PAY_1 <- ifelse(data$PAY_1 == -2, "Zero Balance", data$PAY_1)
# data$PAY_1 <- ifelse(data$PAY_1 == -1, "Paid in Full", data$PAY_1)
# data$PAY_1 <- ifelse(data$PAY_1 == 0, "Revolving Credit", data$PAY_1)
# data$PAY_1 <- ifelse(data$PAY_1 == 1, "Delay 1mo", data$PAY_1)
# data$PAY_1 <- ifelse(data$PAY_1 == 2, "Delay 2mo", data$PAY_1)
# data$PAY_1 <- ifelse(data$PAY_1 == 3, "Delay 3mo", data$PAY_1)
# data$PAY_1 <- ifelse(data$PAY_1 == 4, "Delay 4mo", data$PAY_1)
# data$PAY_1 <- ifelse(data$PAY_1 == 5, "Delay 5mo", data$PAY_1)
# data$PAY_1 <- ifelse(data$PAY_1 == 6, "Delay 6mo", data$PAY_1)
# data$PAY_1 <- ifelse(data$PAY_1 == 7, "Delay 7mo", data$PAY_1)
# data$PAY_1 <- ifelse(data$PAY_1 == 8, "Delay 8mo", data$PAY_1)
# 
# data$PAY_2 <- ifelse(data$PAY_2 == -2, "Zero Balance", data$PAY_2)
# data$PAY_2 <- ifelse(data$PAY_2 == -1, "Paid in Full", data$PAY_2)
# data$PAY_2 <- ifelse(data$PAY_2 == 0, "Revolving Credit", data$PAY_2)
# data$PAY_2 <- ifelse(data$PAY_2 == 1, "Delay 1mo", data$PAY_2)
# data$PAY_2 <- ifelse(data$PAY_2 == 2, "Delay 2mo", data$PAY_2)
# data$PAY_2 <- ifelse(data$PAY_2 == 3, "Delay 3mo", data$PAY_2)
# data$PAY_2 <- ifelse(data$PAY_2 == 4, "Delay 4mo", data$PAY_2)
# data$PAY_2 <- ifelse(data$PAY_2 == 5, "Delay 5mo", data$PAY_2)
# data$PAY_2 <- ifelse(data$PAY_2 == 6, "Delay 6mo", data$PAY_2)
# data$PAY_2 <- ifelse(data$PAY_2 == 7, "Delay 7mo", data$PAY_2)
# data$PAY_2 <- ifelse(data$PAY_2 == 8, "Delay 8mo", data$PAY_2)
# 
# data$PAY_3 <- ifelse(data$PAY_3 == -2, "Zero Balance", data$PAY_3)
# data$PAY_3 <- ifelse(data$PAY_3 == -1, "Paid in Full", data$PAY_3)
# data$PAY_3 <- ifelse(data$PAY_3 == 0, "Revolving Credit", data$PAY_3)
# data$PAY_3 <- ifelse(data$PAY_3 == 1, "Delay 1mo", data$PAY_3)
# data$PAY_3 <- ifelse(data$PAY_3 == 2, "Delay 2mo", data$PAY_3)
# data$PAY_3 <- ifelse(data$PAY_3 == 3, "Delay 3mo", data$PAY_3)
# data$PAY_3 <- ifelse(data$PAY_3 == 4, "Delay 4mo", data$PAY_3)
# data$PAY_3 <- ifelse(data$PAY_3 == 5, "Delay 5mo", data$PAY_3)
# data$PAY_3 <- ifelse(data$PAY_3 == 6, "Delay 6mo", data$PAY_3)
# data$PAY_3 <- ifelse(data$PAY_3 == 7, "Delay 7mo", data$PAY_3)
# data$PAY_3 <- ifelse(data$PAY_3 == 8, "Delay 8mo", data$PAY_3)
# 
# data$PAY_4 <- ifelse(data$PAY_4 == -2, "Zero Balance", data$PAY_4)
# data$PAY_4 <- ifelse(data$PAY_4 == -1, "Paid in Full", data$PAY_4)
# data$PAY_4 <- ifelse(data$PAY_4 == 0, "Revolving Credit", data$PAY_4)
# data$PAY_4 <- ifelse(data$PAY_4 == 1, "Delay 1mo", data$PAY_4)
# data$PAY_4 <- ifelse(data$PAY_4 == 2, "Delay 2mo", data$PAY_4)
# data$PAY_4 <- ifelse(data$PAY_4 == 3, "Delay 3mo", data$PAY_4)
# data$PAY_4 <- ifelse(data$PAY_4 == 4, "Delay 4mo", data$PAY_4)
# data$PAY_4 <- ifelse(data$PAY_4 == 5, "Delay 5mo", data$PAY_4)
# data$PAY_4 <- ifelse(data$PAY_4 == 6, "Delay 6mo", data$PAY_4)
# data$PAY_4 <- ifelse(data$PAY_4 == 7, "Delay 7mo", data$PAY_4)
# data$PAY_4 <- ifelse(data$PAY_4 == 8, "Delay 8mo", data$PAY_4)
# 
# data$PAY_5 <- ifelse(data$PAY_5 == -2, "Zero Balance", data$PAY_5)
# data$PAY_5 <- ifelse(data$PAY_5 == -1, "Paid in Full", data$PAY_5)
# data$PAY_5 <- ifelse(data$PAY_5 == 0, "Revolving Credit", data$PAY_5)
# data$PAY_5 <- ifelse(data$PAY_5 == 1, "Delay 1mo", data$PAY_5)
# data$PAY_5 <- ifelse(data$PAY_5 == 2, "Delay 2mo", data$PAY_5)
# data$PAY_5 <- ifelse(data$PAY_5 == 3, "Delay 3mo", data$PAY_5)
# data$PAY_5 <- ifelse(data$PAY_5 == 4, "Delay 4mo", data$PAY_5)
# data$PAY_5 <- ifelse(data$PAY_5 == 5, "Delay 5mo", data$PAY_5)
# data$PAY_5 <- ifelse(data$PAY_5 == 6, "Delay 6mo", data$PAY_5)
# data$PAY_5 <- ifelse(data$PAY_5 == 7, "Delay 7mo", data$PAY_5)
# data$PAY_5 <- ifelse(data$PAY_5 == 8, "Delay 8mo", data$PAY_5)
# 
# data$PAY_6 <- ifelse(data$PAY_6 == -2, "Zero Balance", data$PAY_6)
# data$PAY_6 <- ifelse(data$PAY_6 == -1, "Paid in Full", data$PAY_6)
# data$PAY_6 <- ifelse(data$PAY_6 == 0, "Revolving Credit", data$PAY_6)
# data$PAY_6 <- ifelse(data$PAY_6 == 1, "Delay 1mo", data$PAY_6)
# data$PAY_6 <- ifelse(data$PAY_6 == 2, "Delay 2mo", data$PAY_6)
# data$PAY_6 <- ifelse(data$PAY_6 == 3, "Delay 3mo", data$PAY_6)
# data$PAY_6 <- ifelse(data$PAY_6 == 4, "Delay 4mo", data$PAY_6)
# data$PAY_6 <- ifelse(data$PAY_6 == 5, "Delay 5mo", data$PAY_6)
# data$PAY_6 <- ifelse(data$PAY_6 == 6, "Delay 6mo", data$PAY_6)
# data$PAY_6 <- ifelse(data$PAY_6 == 7, "Delay 7mo", data$PAY_6)
# data$PAY_6 <- ifelse(data$PAY_6 == 8, "Delay 8mo", data$PAY_6)
# 
# data$SEX <- as.factor(data$SEX)
# data$EDUCATION <- as.factor(data$EDUCATION)
# data$MARRIAGE <- as.factor(data$MARRIAGE)
# data$PAY_1 <- as.factor(data$PAY_1)
# data$PAY_2 <- as.factor(data$PAY_2)
# data$PAY_3 <- as.factor(data$PAY_3)
# data$PAY_4 <- as.factor(data$PAY_4)
# data$PAY_5 <- as.factor(data$PAY_5)
# data$PAY_6 <- as.factor(data$PAY_6)
# 
# data$default_0 <- as.factor(data$default_0)

# Spending variables: the amount a client spent in a month. This is this bill - last bill + payment
data$SPEND_1 <- data$BILL_AMT1 - data$BILL_AMT2 + data$PAY_AMT1
data$SPEND_2 <- data$BILL_AMT2 - data$BILL_AMT3 + data$PAY_AMT2
data$SPEND_3 <- data$BILL_AMT3 - data$BILL_AMT4 + data$PAY_AMT3
data$SPEND_4 <- data$BILL_AMT4 - data$BILL_AMT5 + data$PAY_AMT4
data$SPEND_5 <- data$BILL_AMT5 - data$BILL_AMT6 + data$PAY_AMT5

# Average spending
data$AVG_SPEND <- (data$SPEND_1 + data$SPEND_2 + data$SPEND_3 + data$SPEND_4 + data$SPEND_5)/5
# data$AVG_SPEND <- apply(data[,46:50], 1, mean)
# As a % of credit limit
data$AVG_SPEND_PCT <- data$AVG_SPEND/data$LIMIT_BAL

# Average Bill
data$AVG_BILL <- (data$BILL_AMT1 + data$BILL_AMT2 + data$BILL_AMT3 + data$BILL_AMT4 + data$BILL_AMT5 + data$BILL_AMT6)/6
# data$AVG_BILL <- apply(data[,13:18], 1, mean)
# As % of credit limit
data$AVG_BILL_PCT <- data$AVG_BILL / data$LIMIT_BAL

# Highest Bill
data$MAX_BILL <- apply(data[,13:18], 1, max)
# As % of credit limit
data$MAX_BILL_PCT <- data$MAX_BILL / data$LIMIT_BAL



# Number of months with delay payment
data$DELAY <- ((data$PAY_1 > 0) + (data$PAY_2 > 0) + (data$PAY_3 > 0)
               + (data$PAY_4 > 0) + (data$PAY_5 > 0) + (data$PAY_6 > 0))
# Number of months with rolling credit or worse
data$RC <- ((data$PAY_1 >= 0) + (data$PAY_2 >= 0) + (data$PAY_3 >= 0)
            + (data$PAY_4 >= 0) + (data$PAY_5 >= 0) + (data$PAY_6 >= 0))


#Sunny added additonal features
#constant non-zero paymebt
data$constant_payment<-data$PAY_AMT1==data$PAY_AMT2 & data$PAY_AMT2==data$PAY_AMT3 &data$PAY_AMT3==data$PAY_AMT4 &data$PAY_AMT4==data$PAY_AMT5 &data$PAY_AMT5==data$PAY_AMT6 & data$PAY_AMT1>0
data$constant_payment<-as.numeric(data$constant_payment)

# num of zero payment in last 6 payments
data$zero_payment_count<-rowSums(data[19:24]==0)





#Remove ID
data<-data[,-1]

#Remove sex,marriage
# data<-data%>%dplyr::select(-SEX,-MARRIAGE)%>%distinct()

#Convert all character variable into factor
data=data%>% mutate_if(is.character, as.factor)

# Create another a custom function to combine rare categories into "Other."+the name of the original variavle (e.g., Other.State)
# This function has two arguments: the name of the dataframe and the count of observation in a category to define "rare"
combinerarecategories<-function(data_frame,mincount){ 
  for (i in 1 : ncol(data_frame)){
    a<-data_frame[,i]
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame)[i],sep=".")
    data_frame[,i]<-a }
  return(data_frame) }


#Apply the fixNAs and combinerarecategories functions to the data and then split it into testing and training data.
data<-combinerarecategories(data,250) #combine categories with <250 values in STCdata into "Other"

##Data partition
observed<-data[1:24000,]
prediction<-data[24001:25000,]

set.seed(77850) #set a random number generation seed to ensure that the split is the same everytime
inTrain <- createDataPartition(y = observed$default_0,
                               p = 19200/24000, list = FALSE)#20%/80% split
training <- observed[ inTrain,]
testing <- observed[ -inTrain,]

print('Number of default in train dataset before applying sampling methods')
# Roughfully 22% of records have defult
table(training$default_0)

#Apply re-sampling methods. Feel free to change method = "both"to "over" or "under"
# Reference: https://rdrr.io/cran/ROSE/man/ovun.sample.html
training_balanced <- ovun.sample(default_0 ~ ., data = training, method = "both",p=0.5)$data

print('Number of default in train dataset before applying re-sampling methods')
#50% or records have defult
table(training_balanced$default_0)


#Lasso regression on logistic regerssion
# Dumy code categorical predictor variables
x <- model.matrix(default_0~.+EDUCATION*SEX+EDUCATION*marriage_indicator+SEX*marriage_indicator+
                    DELAY*EDUCATION+DELAY*SEX+DELAY*marriage_indicator, training_balanced)[,-1]
# Convert the outcome (class) to a numerical variable
y <- training_balanced$default_0

glmnet(x, y, family = "binomial", alpha = 1, lambda = NULL)

library(glmnet)
set.seed(123)
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso)

lasso.model <- glmnet(x, y, alpha = 1, family = "binomial",
                      lambda = cv.lasso$lambda.min)

saveRDS(lasso.model, "model_logistic.rds")
# Make prediction on test data
x.test <- model.matrix(default_0 ~.+EDUCATION*SEX+EDUCATION*marriage_indicator+SEX*marriage_indicator+
                         DELAY*EDUCATION+DELAY*SEX+DELAY*marriage_indicator, testing)[,-1]

glm_pred <- lasso.model %>% predict(newx = x.test, type = 'response')
glm_classification <- ifelse(glm_pred > 0.5, 1, 0)
glm_classification<-as.factor(glm_classification)

###Confusion matrix  
testing$default_0 <- as.factor(testing$default_0)
confusionMatrix(glm_classification,testing$default_0,positive = "1") 

####ROC Curve
glm_ROC_prediction <- prediction(glm_pred, testing$default_0)
glm_ROC <- performance(glm_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(glm_ROC, main="True Positives versus False Positives",
     xlab="False Positives Rate", ylab="True Positives Rate") 

####AUC (area under curve)
auc_glm <- performance(glm_ROC_prediction,"auc") #Create AUC data
glm_auc_testing <- as.numeric(auc_glm@y.values) #Calculate AUC
glm_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(glm_pred, testing$default_0, cumulative = TRUE, n.buckets = 10) 

#Calculate profit
a3.model_test(glm_pred, testing$default_0)
a3.max_profit(glm_pred, testing$default_0)

############### Random Forest  ##################
model_forest <- randomForest(default_0~PAY_1+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+diff_1+diff_2+diff_3+diff_4+diff_5
                             + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6
                             + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6
                             +age_26+age_27_31+age_44+age_37_43+age_32_36+limit_5+
                              limit_6_11+limit_12_18+limit_19_27+limit_28+
                             +positive_balance_count+negative_diff_count+male_indicator+marriage_indicator+AVG_PAIDOFF+limit_use+AGE+LIMIT_BAL+AVG_SPEND+AVG_SPEND_PCT
                             +AVG_BILL+AVG_BILL_PCT+MAX_BILL+MAX_BILL_PCT+DELAY+RC+constant_payment+zero_payment_count+avg_diff+max_diff,data=training_balanced,
                             importance=TRUE,proximity=TRUE,
                             cutoff = c(0.5, 0.5),type="classification") #cutoffs need to be determined for class 0 and class 1. By default 50/50, but need not be those necessarily


# saveRDS(model_forest, "model_forest.rds")
print(model_forest)   
plot(model_forest)
importance(model_forest)
varImpPlot(model_forest, main = "RandomForest Variable Importance")



###Finding predicitons: probabilities and classification
forest_probabilities<-predict(model_forest,newdata=testing) 
# write.csv(forest_probabilities,"rf_prob.csv")
forest_classification<-rep("0",4800)
forest_classification[forest_probabilities>0.5]="1" 
forest_classification<-as.factor(forest_classification)

testing$default_0 <- as.factor(testing$default_0)
confusionMatrix(forest_classification,testing$default_0, positive="1")


#Calculate profit
a3.model_test(forest_probabilities, testing$default_0)
a3.max_profit(forest_probabilities, testing$default_0)

####ROC Curve
forest_ROC_prediction <- prediction(forest_probabilities, testing$default_0) #Calculate errors
forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(forest_ROC) #Plot ROC curve

####AUC (area under curve)
AUC_rf <- performance(forest_ROC_prediction,"auc") #Create AUC data
forest_AUC <- as.numeric(AUC_rf@y.values) #Calculate AUC
forest_AUC #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(forest_probabilities,  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_forest <- performance(forest_ROC_prediction,"lift","rpp")
plot(Lift_forest)


############ XGboost  #################
training.x <-model.matrix(default_0~ ., data = training_balanced)
testing.x <-model.matrix(default_0~ ., data = testing)

model_XGboost<-xgboost(data = data.matrix(training.x[,-1]), 
                       label = as.numeric(as.character(training_balanced$default_0)), 
                       eta = 0.1,
                       max_depth = 20, 
                       nround=50, 
                       objective = "binary:logistic")

XGboost_prediction<-predict(model_XGboost,newdata=testing.x[,-1], type="response") 
#write.csv(XGboost_prediction,"C:\\Users\\yaora\\Desktop\\MMA\\MMA867\\A3\\xg_test.csv")
confusionMatrix(as.factor(ifelse(XGboost_prediction>0.5,1,0)),testing$default_0,positive="1") 

####ROC Curve
XGboost_pred_testing <- prediction(XGboost_prediction, testing$default_0) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_pred_testing,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_pred_testing,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(XGboost_prediction, testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

#Calculate profit
a3.model_test(XGboost_prediction, testing$default_0)
a3.max_profit(XGboost_prediction, testing$default_0)


# Ensemble ----------------------------------------------------------------
ensemble_prob <- (glm_pred + forest_probabilities + XGboost_prediction)/3
summary(ensemble_prob)
a3.max_profit(ensemble_prob, testing$default_0)
a3.model_test(ensemble_prob, testing$default_0) # Ideal threshold 0.45
a3.max_profit(ensemble_prob, testing$default_0)/4.8

plot(glm_ROC, main="True Positives versus False Positives",
     xlab="False Positives Rate", ylab="True Positives Rate")
plot(forest_ROC, add = TRUE, col = "red")
plot(XGboost_ROC_testing, add = TRUE, col = "green")
legend("right", legend=c("GLM", "RandomForest", "XGBoost"), col=c("black", "red", "green"), lty=1:2, cex=0.8)

# Make prediction
prediction_x_glm <- model.matrix(default_0~.+EDUCATION*SEX+EDUCATION*marriage_indicator+SEX*marriage_indicator+
                               DELAY*EDUCATION+DELAY*SEX+DELAY*marriage_indicator, prediction)[,-1]
prediction_x_xgboost <- model.matrix(default_0~ ., data = prediction)
final_glm_pred <- lasso.model %>% predict(newx = prediction_x_glm, type = 'response')
final_forest_pred <- predict(model_forest, newdata = prediction) 
write.csv(final_forest_pred, "final_forest_withgender.csv")
final_XGBoost_pred <- predict(model_XGboost, newdata = prediction_x_xgboost[,-1], type="response") 
final_ensemble_prob <- (final_glm_pred + final_forest_pred + final_XGBoost_pred)/3

ensemble_classification <- ifelse(final_ensemble_prob > 0.43, 1, 0)
ensemble_classification <- as.factor(ensemble_classification)
extend_credit <- ifelse(ensemble_classification == 1, 0, 1)
extend_credit <- as.factor(extend_credit)
write.csv(extend_credit, "final.csv")

# minus sex ---------------------------------------------------------------

model_forest_ns <- randomForest(default_0~PAY_1+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6+diff_1+diff_2+diff_3+diff_4+diff_5
                             + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6
                             + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6
                             +age_26+age_27_31+age_44+age_37_43+age_32_36+limit_5+
                               limit_6_11+limit_12_18+limit_19_27+limit_28+
                               +positive_balance_count+negative_diff_count+marriage_indicator+AVG_PAIDOFF+limit_use+AGE+LIMIT_BAL+AVG_SPEND+AVG_SPEND_PCT
                             +AVG_BILL+AVG_BILL_PCT+MAX_BILL+MAX_BILL_PCT+DELAY+RC+constant_payment+zero_payment_count+avg_diff+max_diff,data=training_balanced,
                             importance=TRUE,proximity=TRUE,
                             cutoff = c(0.5, 0.5),type="classification") #cutoffs need to be determined for class 0 and class 1. By default 50/50, but need not be those necessarily


# saveRDS(model_forest, "model_forest.rds")
print(model_forest_ns)   
plot(model_forest_ns)
importance(model_forest_ns)
varImpPlot(model_forest_ns)

###Finding predicitons: probabilities and classification
forest_probabilities_ns<-predict(model_forest_ns,newdata=testing) 
# write.csv(forest_probabilities,"rf_prob.csv")
forest_classification_ns<-rep("0",4800)
forest_classification_ns[forest_probabilities_ns>0.5]="1" 
forest_classification_ns<-as.factor(forest_classification_ns)

# testing$default_0 <- as.factor(testing$default_0)
confusionMatrix(forest_classification_ns,testing$default_0, positive="1")

#Calculate profit
a3.model_test(forest_probabilities_ns, testing$default_0)
a3.max_profit(forest_probabilities_ns, testing$default_0)

####ROC Curve
forest_ROC_prediction_ns <- prediction(forest_probabilities_ns, testing$default_0) #Calculate errors
forest_ROC_ns <- performance(forest_ROC_prediction_ns,"tpr","fpr") #Create ROC curve data
plot(forest_ROC_ns) #Plot ROC curve

####AUC (area under curve)
AUC_rf <- performance(forest_ROC_prediction_ns,"auc") #Create AUC data
forest_AUC <- as.numeric(AUC_rf@y.values) #Calculate AUC
forest_AUC #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(forest_probabilities,  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_forest <- performance(forest_ROC_prediction,"lift","rpp")
plot(Lift_forest)

plot(forest_ROC_ns, col = "blue", main="True Positives versus False Positives",
     xlab="False Positives Rate", ylab="True Positives Rate")
plot(forest_ROC, add = TRUE, col = "red")
legend("right", legend=c("Gender Agnostic", "With Gender"), col=c("blue", "red"), lty=1:2, cex=0.8)

# Make predictions
final_forest_ns_pred <- predict(model_forest_ns, newdata = prediction) 
write.csv(final_forest_ns_pred, "final_forest_withoutgender.csv")
